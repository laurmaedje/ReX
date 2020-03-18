use crate::error::Error;
use crate::font::FontContext;
use crate::dimensions::*;
use crate::layout::{LayoutNode, LayoutVariant, Alignment, Style, LayoutSettings, Layout};
use crate::parser::{color::RGBA};

pub struct Renderer<'a> {
    pub ctx: FontContext<'a>,
    pub font_size: Scale<Px, Em>,
    pub strict: bool,
    pub style: Style,
    pub debug: bool,
}

#[derive(Copy, Clone, Default)]
pub struct Cursor {
    pub x: f64,
    pub y: f64,
}

impl Cursor {
    pub fn translate(self, dx: f64, dy: f64) -> Cursor {
        Cursor {
            x: self.x + dx,
            y: self.y + dy,
        }
    }
    pub fn left(self, dx: f64) -> Cursor {
        Cursor {
            x: self.x - dx,
            y: self.y,
        }
    }
    pub fn right(self, dx: f64) -> Cursor {
        Cursor {
            x: self.x + dx,
            y: self.y,
        }
    }
    pub fn up(self, dy: f64) -> Cursor {
        Cursor {
            x: self.x,
            y: self.y - dy,
        }
    }
    pub fn down(self, dy: f64) -> Cursor {
        Cursor {
            x: self.x,
            y: self.y + dy,
        }
    }
}

pub trait Backend {
    fn bbox(&mut self, _pos: Cursor, _width: f64, _height: f64, role: Role) {}
    fn symbol(&mut self, pos: Cursor, gid: u16, scale: f64);
    fn rule(&mut self, pos: Cursor, width: f64, height: f64);
    fn begin_color(&mut self, color: RGBA);
    fn end_color(&mut self);
}

pub enum Role {
    Glyph,
    VBox,
    HBox,
}

impl<'a> Renderer<'a> {
    pub fn new(ctx: FontContext<'a>, font_size: f64) -> Self {
        Renderer {
            font_size: Scale::new(font_size, Px, Em),
            ctx,
            strict: true,
            style: Style::Display,
            debug: false,
        }
    }
    pub fn layout(&self, tex: &str) -> Result<Layout, Error> {
        use crate::parser::parse;
        use crate::layout::engine::layout;


        let mut parse = parse(tex)?;
        let layout_settings = LayoutSettings::new(&self.ctx, self.font_size, self.style);
        Ok(layout(&mut parse, layout_settings))
    }
    // (x0, y0, x1, y1)
    pub fn size(&self, layout: &Layout) -> (f64, f64, f64, f64) {
        (
            0.0,
            layout.depth / Px,
            layout.width / Px,
            layout.height / Px
        )
    }
    pub fn render(&self, layout: &Layout, out: &mut impl Backend) {
        let pos = Cursor {
            x: 0.0,
            y: (layout.height + layout.depth) / Px,
        };
        self.render_hbox(out, pos, &layout.contents, layout.height / Px, layout.width / Px, Alignment::Default);
    }

    fn render_hbox(&self, out: &mut impl Backend, mut pos: Cursor, nodes: &[LayoutNode], height: f64, nodes_width: f64, alignment: Alignment) {
        if let Alignment::Centered(w) = alignment {
            pos.x += (nodes_width - w / Px) * 0.5;
        }

        if self.debug {
            out.bbox(pos.up(height), nodes_width, height, Role::HBox);
        }

        for node in nodes {
            match node.node {
                LayoutVariant::Glyph(ref gly) => {
                    if self.debug {
                        out.bbox(pos.up(node.height / Px), node.width / Px, (node.height - node.depth) / Px, Role::Glyph);
                    }
                    out.symbol(pos, gly.gid, gly.scale * self.font_size.factor);
                }

                LayoutVariant::Rule => out.rule(pos.up(node.height / Px), node.width / Px, node.height / Px),

                LayoutVariant::VerticalBox(ref vbox) => {
                    if self.debug {
                        out.bbox(pos.up(node.height / Px), node.width / Px, (node.height - node.depth) / Px, Role::VBox);
                    }
                    self.render_vbox(out, pos.up(node.height / Px), &vbox.contents);
                }

                LayoutVariant::HorizontalBox(ref hbox) => {
                    self.render_hbox(out, pos, &hbox.contents, node.height / Px, node.width / Px, hbox.alignment);
                }

                LayoutVariant::Color(ref clr) => {
                    out.begin_color(clr.color);
                    self.render_hbox(out, pos, &clr.inner, node.height / Px, node.width / Px, Alignment::Default);
                    out.end_color();
                }

                LayoutVariant::Kern => { /* NOOP */ }
            } // End macth

            pos.x += node.width / Px;
        }
    }
    fn render_vbox(&self, out: &mut impl Backend, mut pos: Cursor, nodes: &[LayoutNode]) {
        for node in nodes {
            match node.node {
                LayoutVariant::Rule => out.rule(pos, node.width / Px, node.height / Px),

                LayoutVariant::HorizontalBox(ref hbox) => {
                    self.render_hbox(out,
                                     pos.down(node.height / Px),
                                     &hbox.contents,
                                     node.height / Px,
                                     node.width / Px,
                                     hbox.alignment)
                }

                LayoutVariant::VerticalBox(ref vbox) => {
                    if self.debug {
                        out.bbox(pos, node.width / Px, (node.height - node.depth) / Px, Role::VBox);
                    }
                    self.render_vbox(out, pos, &vbox.contents);
                }

                LayoutVariant::Glyph(ref gly) => {
                    if self.debug {
                        out.bbox(pos, node.width / Px, (node.height - node.depth) / Px, Role::Glyph);
                    }
                    out.symbol(pos.down(node.height / Px), gly.gid, gly.scale * self.font_size.factor);
                }

                LayoutVariant::Color(_) => panic!("Shouldn't have a color in a vertical box???"),

                LayoutVariant::Kern => { /* NOOP */ }
            }

            pos.y += node.height / Px;
        }
    }
}

pub mod scene;
pub use scene::SceneWrapper;
