use std::fs;
use rex::*;
use rex::render::scene::svg;

fn main() {
    let mut args = std::env::args().skip(1);
    let font = args.next().expect("font");
    let tex = args.next().expect("tex");

    env_logger::init();
    let font = fs::read(font).unwrap();

    fs::write("tex.svg", &svg(&font, &tex)).unwrap();
}
