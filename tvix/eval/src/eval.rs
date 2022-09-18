use std::path::PathBuf;

use crate::{
    builtins::global_builtins,
    errors::EvalResult,
    value::Value,
    vm::VM,
};

/// Runtime options for the Tvix interpreter
#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "repl", derive(clap::Parser))]
pub struct Options {
    /// Dump the bytecode to stdout before evaluating
    #[cfg_attr(feature = "repl", clap(long, env = "TVIX_DUMP_BYTECODE"))]
    pub dump_bytecode: bool,

    /// Trace the runtime of the VM
    #[cfg_attr(feature = "repl", clap(long, env = "TVIX_TRACE_RUNTIME"))]
    pub trace_runtime: bool,
}

pub fn interpret(code: &str, location: Option<PathBuf>, options: Options) -> EvalResult<Value> {
    let mut vm = VM::new(global_builtins(), options);
    let result = vm.compile(code, location)?;

    for error in &result.errors {
        let codemap = vm.codemap();
        let file = codemap.find_file(error.span.low());

        eprintln!(
            "compiler error: {:?} at `{}`[line {}]",
            error.kind,
            file.source_slice(error.span),
            file.find_line(error.span.low()) + 1
        );
    }

    if let Some(err) = result.errors.last() {
        return Err(err.clone());
    }

    vm.call_toplevel(result.lambda)
}
