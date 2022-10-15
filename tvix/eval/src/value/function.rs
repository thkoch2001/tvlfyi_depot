//! This module implements the runtime representation of functions.
use std::rc::Rc;

use crate::{chunk::Chunk, upvalues::Upvalues};

#[derive(Debug, PartialEq)]
pub struct Lambda {
    // name: Option<NixString>,
    pub(crate) chunk: Chunk,
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

    pub fn chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub lambda: Rc<Lambda>,
    pub upvalues: Upvalues,
}

impl Closure {
    pub fn new(lambda: Rc<Lambda>) -> Self {
        Closure {
            upvalues: Upvalues::with_capacity(lambda.upvalue_count),
            lambda,
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.lambda.chunk
    }

    pub fn lambda(&self) -> Rc<Lambda> {
        self.lambda.clone()
    }

    pub fn upvalues(&self) -> &Upvalues {
        &self.upvalues
    }
}
