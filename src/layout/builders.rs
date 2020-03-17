#![allow(dead_code)]
use super::{VerticalBox, HorizontalBox, LayoutNode, LayoutVariant, Alignment};
use std::cmp::{max, min};
use crate::dimensions::*;

#[derive(Default)]
pub struct VBox {
    pub width: Length<Px>,
    pub height: Length<Px>,
    pub depth: Length<Px>,
    node: VerticalBox,
}

impl VBox {
    pub fn new() -> VBox {
        VBox::default()
    }

    pub fn insert_node(&mut self, idx: usize, node: LayoutNode) {
        self.width = max(self.width, node.width);
        self.height += node.height;
        self.node.contents.insert(idx, node);
    }

    pub fn add_node(&mut self, node: LayoutNode) {
        self.width = max(self.width, node.width);
        self.height += node.height;
        self.node.contents.push(node);
    }

    pub fn set_offset(&mut self, offset: Length<Px>) {
        self.node.offset = offset;
    }

    pub fn build(mut self) -> LayoutNode {
        // The depth only depends on the depth
        // of the last element and offset.
        if let Some(node) = self.node.contents.last() {
            self.depth = node.depth;
        }

        self.depth -= self.node.offset;
        self.height -= self.node.offset;

        LayoutNode {
            width: self.width,
            height: self.height,
            depth: self.depth,
            node: LayoutVariant::VerticalBox(self.node),
        }
    }
}

macro_rules! vbox {
    (offset: $offset:expr; $($node:expr),*) => ({
        let mut _vbox = builders::VBox::new();
        $( _vbox.add_node($node); )*
        _vbox.set_offset($offset);
        _vbox.build()
    });

    ( $($node:expr),* ) => ({
        let mut _vbox = builders::VBox::new();
        $( _vbox.add_node($node); )*
        _vbox.build()
    });
}

#[derive(Default)]
pub struct HBox {
    pub width: Length<Px>,
    pub height: Length<Px>,
    pub depth: Length<Px>,
    pub node: HorizontalBox,
    pub alignment: Alignment,
}

impl HBox {
    pub fn new() -> HBox {
        HBox::default()
    }

    pub fn add_node(&mut self, node: LayoutNode) {
        self.width += node.width;
        self.height = max(self.height, node.height);
        self.depth = min(self.depth, node.depth);
        self.node.contents.push(node);
    }

    pub fn set_offset(&mut self, offset: Length<Px>) {
        self.node.offset = offset;
    }

    pub fn set_alignment(&mut self, align: Alignment) {
        self.node.alignment = align;
    }

    pub fn set_width(&mut self, width: Length<Px>) {
        self.width = width;
    }

    pub fn build(mut self) -> LayoutNode {
        self.depth -= self.node.offset;
        self.height -= self.node.offset;

        LayoutNode {
            width: self.width,
            height: self.height,
            depth: self.depth,
            node: LayoutVariant::HorizontalBox(self.node),
        }
    }
}

macro_rules! hbox {
    (offset: $offset:expr; $($node:expr),*) => ({
        let mut _hbox = builders::HBox::new();
        $( _hbox.add_node($node); )*
        _hbox.set_offset($offset);
        _hbox.build()
    });

    (align: $align:expr; width: $width:expr; $($node:expr),*) => ({
        let mut _hbox = builders::HBox::new();
        let align = $align;
        let width = $width;
        $( _hbox.add_node($node); )*
        _hbox.set_alignment(align);
        _hbox.set_width(width);
        _hbox.build()
    });

    ( $($node:expr),* ) => ({
        let mut _hbox = builders::HBox::new();
        $( _hbox.add_node($node); )*
        _hbox.build()
    });
}

macro_rules! rule {
    (width: $width:expr, height: $height:expr) => (
        rule!(width: $width, height: $height, depth: Length::zero())
    );

    (width: $width:expr, height: $height:expr, depth: $depth:expr) => (
        LayoutNode {
            width:  $width,
            height: $height,
            depth:  $depth,
            node: LayoutVariant::Rule,
        }
    );
}

macro_rules! kern {
    (vert: $height:expr) => (
        LayoutNode {
            width:  Length::zero(),
            height: $height,
            depth:  Length::zero(),
            node:   LayoutVariant::Kern,
        }
    );

    (horz: $width:expr) => (
        LayoutNode {
            width:   $width,
            height: Length::zero(),
            depth:  Length::zero(),
            node:   LayoutVariant::Kern,
        }
    );
}

macro_rules! color {
    ($layout:expr, $color:expr) => (
        LayoutNode {
            width: $layout.width,
            height: $layout.height,
            depth: $layout.depth,
            node: LayoutVariant::Color(ColorChange {
                color: $color.color,
                inner: $layout.contents,
            }),
        }
    )
}
