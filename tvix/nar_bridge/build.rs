use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(
        &["tvix/proto/castore.proto", "tvix/proto/pathinfo.proto"],
        &["../../"],
    )?;
    //prost_build::compile_protos(&["../proto/pathinfo.proto"], &["../proto/"])?;

    //tonic_build::configure()
    ////.build_server(false)
    //.compile(&["../../tvix/proto/rpc_directory.proto"], &["../.."])?;
    Ok(())
}
