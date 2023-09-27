use anyhow::bail;
use clap::Parser;
use futures_util::StreamExt;
use nix_compat::store_path::StorePath;
use petgraph;
use petgraph::stable_graph::StableGraph;
use std::collections::HashMap;
use tvix_castore::blobservice;
use tvix_castore::directoryservice;
use tvix_castore::proto::Directory;
use tvix_castore::B3Digest;
use tvix_store::pathinfoservice;

/// Consumes a Store Path and prints out the Directory Graph in DOT format.
#[derive(Parser)]
struct Cli {
    #[arg(long, env, default_value = "sled:///var/lib/tvix-store/blobs.sled")]
    blob_service_addr: String,

    #[arg(
        long,
        env,
        default_value = "sled:///var/lib/tvix-store/directories.sled"
    )]
    directory_service_addr: String,

    #[arg(long, env, default_value = "sled:///var/lib/tvix-store/pathinfo.sled")]
    path_info_service_addr: String,

    #[clap(value_name = "STORE_PATH", value_parser = |s: &str| StorePath::from_absolute_path(s.as_bytes()))]
    store_path: StorePath,
}

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();

    // initialize stores
    let blob_service = blobservice::from_addr(&cli.blob_service_addr)?;
    let directory_service = directoryservice::from_addr(&cli.directory_service_addr)?;
    let path_info_service = pathinfoservice::from_addr(
        &cli.path_info_service_addr,
        blob_service,
        directory_service.clone(),
    )?;

    // extract the output path
    let digest = cli.store_path.digest.clone();

    // lookup the root node for this store path.
    let path_info = match path_info_service.get(digest).await? {
        None => {
            bail!("{} not found", cli.store_path.to_absolute_path());
        }
        Some(path_info) => path_info,
    };

    // ensure it's a directory at the root.
    let directory_node = match path_info.node.unwrap().node.unwrap() {
        tvix_castore::proto::node::Node::Directory(dn) => dn,
        _ => bail!("root node is no directory"),
    };

    // instantiate a graph for all directories, using B3digest for nodes, and BString for edge weights.
    let mut g_directories: StableGraph<B3Digest, bstr::BString> = Default::default();

    let root_directory_digest: B3Digest = directory_node.digest.try_into().unwrap();

    // Receive all directories from the directory service, and collect them into
    // a lookup table B3Digest -> Directory.
    // This will also dedup Directories that are received multiple times
    let directories: HashMap<B3Digest, Directory> = {
        let mut directories: HashMap<B3Digest, Directory> = Default::default();
        let mut directories_it = directory_service.get_recursive(&root_directory_digest);
        while let Some(e) = directories_it.next().await {
            let directory = e.unwrap();
            directories.insert(directory.digest(), directory);
        }
        directories
    };

    // First pass, add all directories as nodes to the graph,
    // and populate a lookup table from digest to node index
    let directory_dgst_to_idx: HashMap<B3Digest, _> = {
        let mut directory_dgst_to_idx = HashMap::with_capacity(directories.len());
        for (directory_digest, _) in directories.iter() {
            let node_idx = g_directories.add_node(directory_digest.clone());
            directory_dgst_to_idx.insert(directory_digest.clone(), node_idx);
        }
        directory_dgst_to_idx
    };

    // Second pass, traverse all directories, and for every child directory, create an edge.
    for (directory_digest, directory) in directories {
        for child_directory in directory.directories {
            let from_edge = directory_dgst_to_idx[&directory_digest];
            let to_digest: B3Digest = child_directory.digest.clone().try_into().unwrap();
            let to_edge = directory_dgst_to_idx[&to_digest];

            g_directories.add_edge(from_edge, to_edge, child_directory.name.to_vec().into());
        }
    }

    // Print out the graph.
    println!(
        "{}",
        petgraph::dot::Dot::with_config(&g_directories, &[petgraph::dot::Config::NodeNoLabel])
    );

    Ok(())
}
