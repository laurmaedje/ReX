
use pathfinder_renderer::scene::Scene;
use pathfinder_content::outline::Outline as PaOutline;
use pathfinder_geometry::transform2d::Transform2F;
use super::{Backend, Cursor, Role};
use vector::{PathBuilder, Rect, Vector, Surface, PathStyle, Paint, FillRule, Outline};
use font::OpenTypeFont;
use crate::parser::{color::RGBA};

fn v_cursor(c: Cursor) -> Vector {
    Vector::new(c.x as f32, c.y as f32)
}
fn v_xy(x: f64, y: f64) -> Vector {
    Vector::new(x as f32, y as f32)
}

pub struct SceneWrapper<'a> {
    scene: &'a mut Scene,
    font: &'a OpenTypeFont<PaOutline>,
    style: <Scene as Surface>::Style,
    color_stack: Vec<<Scene as Surface>::Style>,
}
impl<'a> SceneWrapper<'a> {
    pub fn new(scene: &'a mut Scene, font: &'a OpenTypeFont<PaOutline>) -> Self {
        SceneWrapper {
            style: scene.build_style(PathStyle {
                fill: Some(Paint::black()),
                stroke: None,
                fill_rule: FillRule::NonZero
            }),
            scene,
            font,
            color_stack: Vec::new()
        }
    }
}

impl<'a> Backend for SceneWrapper<'a> {
    fn bbox(&mut self, pos: Cursor, width: f64, height: f64, role: Role) {
        let color = match role {
            Role::Glyph => (0, 200, 0, 255),
            Role::HBox => (200, 0, 0, 255),
            Role::VBox => (0, 0, 200, 255),
        };
        let style = self.scene.build_style(PathStyle {
            fill: None,
            stroke: Some((Paint::Solid(color), 0.1)),
            fill_rule: FillRule::NonZero
        });
        let mut b = PathBuilder::new();
        b.rect(Rect::new(v_cursor(pos), v_xy(width, height)));
        self.scene.draw_path(b.into_outline(), &style, None);
    }
    fn symbol(&mut self, pos: Cursor, gid: u16, scale: f64) {
        use font::{Font, GlyphId};
        let path = self.font.glyph(GlyphId(gid as u32)).unwrap().path;
        let tr = Transform2F::from_translation(v_cursor(pos))
            * Transform2F::from_scale(v_xy(scale, -scale))
            * self.font.font_matrix();
        
        self.scene.draw_path(path.transform(tr), &self.style, None);
    }
    fn rule(&mut self, pos: Cursor, width: f64, height: f64) {
        let origin = v_cursor(pos);
        let size = v_xy(width, height);

        let mut b = PathBuilder::new();
        b.rect(Rect::new(origin, size));

        self.scene.draw_path(b.into_outline(), &self.style, None);
    }
    fn begin_color(&mut self, RGBA(r, g, b, a): RGBA) {
        let new_style = self.scene.build_style(PathStyle {
            fill: Some(Paint::Solid((r, g, b, a))),
            stroke: None,
            fill_rule: FillRule::NonZero
        });

        self.color_stack.push(std::mem::replace(&mut self.style, new_style));
    }
    fn end_color(&mut self) {
        self.style = self.color_stack.pop().unwrap();
    }
}

use super::{RenderSettings, Renderer};
use crate::font::FontContext;
use pathfinder_export::{Export, FileFormat};

pub fn svg(font: &[u8], tex: &str) -> Vec<u8> {
    let font = OpenTypeFont::parse(font);
    let ctx = FontContext::new(&font);
    let mut settings = RenderSettings::new(ctx, 48.);
    settings.debug = true;
    let renderer = Renderer::new(&settings);
    let layout = renderer.layout(tex).unwrap();
    let (x0, y0, x1, y1) = dbg!(renderer.size(&layout));
    let mut scene = Scene::new();
    scene.set_view_box(Rect::from_points(v_xy(x0, y0), v_xy(x1, y1)));
    let mut backend = SceneWrapper::new(&mut scene, &font);
    renderer.render(&layout, &mut backend);

    let mut buf = Vec::new();
    scene.export(&mut buf, FileFormat::SVG).unwrap();
    buf
}