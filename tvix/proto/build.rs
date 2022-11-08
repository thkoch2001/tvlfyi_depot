use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(
        &["tvix/proto/castore.proto", "tvix/proto/pathinfo.proto"],
        &["../../"],
    )?;
    Ok(())
}
