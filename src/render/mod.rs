use serde::{Serialize, Deserialize};

use crate::error::Error;
use crate::font::FontContext;
use crate::dimensions::*;
use crate::layout::{LayoutNode, LayoutVariant, Alignment, Style, LayoutSettings, Layout};
use crate::parser::{parse, color::RGBA};

const HBOX_COLOR: &str = "blue";
const VBOX_COLOR: &str = "red";
const GLYPH_COLOR: &str = "green";

pub struct RenderSettings<'a> {
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

impl<'a> RenderSettings<'a> {
    fn new(ctx: FontContext<'a>, font_size: f64) -> Self {
        RenderSettings {
            font_size: Scale::new(font_size, Px, Em),
            ctx,
            strict: true,
            style: Style::Display,
            debug: false,
        }
    }
}

pub trait Backend {
    fn bbox(&mut self, _pos: Cursor, _width: f64, _height: f64, _color: &str) {}
    fn symbol(&self, pos: Cursor, gid: u16, scale: f64);
    fn rule(&self, pos: Cursor, width: f64, height: f64);
    fn color(&self, color: RGBA, contents: impl FnMut(&mut Self));
}

pub struct Renderer<'a> {
    settings: &'a RenderSettings<'a>,
    layout_settings: LayoutSettings<'a>,
}
impl<'a> Renderer<'a> {
    pub fn new(settings: &'a RenderSettings<'a>) -> Self {
        Renderer {
            layout_settings: LayoutSettings::new(&settings.ctx, settings.font_size, settings.style),
            settings
        }
    }
    pub fn size(&self, layout: &LayoutNode) -> (f64, f64) {
        (layout.width / Px, layout.height / Px)
    }
    pub fn render(&self, layout: &Layout, out: &mut impl Backend) {
        let pos = Cursor {
            x: 0.0,
            y: layout.height / Px,
        };
        self.render_hbox(out, pos, &layout.contents, layout.height / Px, layout.width / Px, Alignment::Default);
    }

    fn render_hbox(&self, out: &mut impl Backend, mut pos: Cursor, nodes: &[LayoutNode], height: f64, nodes_width: f64, alignment: Alignment) {
        if let Alignment::Centered(w) = alignment {
            pos.x += (nodes_width - w / Px) * 0.5;
        }

        if self.settings.debug {
            out.bbox(pos.up(height), nodes_width, height, HBOX_COLOR);
        }

        for node in nodes {
            match node.node {
                LayoutVariant::Glyph(ref gly) => {
                    if self.settings.debug {
                        out.bbox(pos.up(node.height / Px), node.width / Px, (node.height - node.depth) / Px, GLYPH_COLOR);
                    }
                    out.symbol(pos, gly.gid, f64::from(gly.scale));
                }

                LayoutVariant::Rule => out.rule(pos.up(node.height / Px), node.width / Px, node.height / Px),

                LayoutVariant::VerticalBox(ref vbox) => {
                    if self.settings.debug {
                        out.bbox(pos.up(node.height / Px), node.width / Px, (node.height - node.depth) / Px, VBOX_COLOR);
                    }
                    self.render_vbox(out, pos.up(node.height / Px), &vbox.contents);
                }

                LayoutVariant::HorizontalBox(ref hbox) => {
                    self.render_hbox(out, pos, &hbox.contents, node.height / Px, node.width / Px, hbox.alignment);
                }

                LayoutVariant::Color(ref clr) => {
                    out.color(clr.color, |out| {
                        self.render_hbox(out, pos, &clr.inner, node.height / Px, node.width / Px, Alignment::Default);
                    });
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
                    if self.settings.debug {
                        out.bbox(pos, node.width / Px, (node.height - node.depth) / Px, VBOX_COLOR);
                    }
                    self.render_vbox(out, pos, &vbox.contents);
                }

                LayoutVariant::Glyph(ref gly) => {
                    if self.settings.debug {
                        out.bbox(pos, node.width / Px, (node.height - node.depth) / Px, GLYPH_COLOR);
                    }
                    out.symbol(pos.down(node.height / Px), gly.gid, gly.scale);
                }

                LayoutVariant::Color(_) => panic!("Shouldn't have a color in a vertical box???"),

                LayoutVariant::Kern => { /* NOOP */ }
            }

            pos.y += node.height / Px;
        }
    }
}
