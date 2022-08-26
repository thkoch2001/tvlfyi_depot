//! This module implements the runtime representation of functions.
use std::rc::Rc;

use crate::chunk::Chunk;

#[derive(Clone, Debug)]
pub struct Lambda {
    // name: Option<NixString>,
    pub(crate) chunk: Rc<Chunk>,
    pub(crate) upvalue_count: usize,
}

impl Lambda {
    pub fn new_anonymous() -> Self {
        Lambda {
            // name: None,
            chunk: Default::default(),
            upvalue_count: 0,
        }
    }

    pub fn chunk(&mut self) -> &mut Rc<Chunk> {
        &mut self.chunk
    }
}

#[derive(Clone, Debug)]
pub struct Closure {
    pub lambda: Lambda,
}
