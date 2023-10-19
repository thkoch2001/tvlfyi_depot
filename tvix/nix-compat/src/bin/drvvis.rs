use fdg_macroquad::macroquad;
use fdg_sim::{ForceGraph, ForceGraphHelper};
use nix_compat::derivation::Derivation;
use petgraph::Undirected;
use std::collections::{HashMap, VecDeque};

fn parse_drv_path(drv_path: &str) -> Derivation {
    let b = std::fs::read(drv_path).unwrap_or_else(|_| panic!("unable to read {} from disk", drv_path));

    Derivation::from_aterm_bytes(&b).unwrap_or_else(|_| panic!("unable to parse derivation at {}", drv_path))
}

fn generate_graph(root_drv_path: String) -> ForceGraph<(), (), Undirected> {
    let mut queue: VecDeque<String> = VecDeque::from([root_drv_path]);

    // keep a list of all derivations we have so far
    let mut seen_drvs: HashMap<String, Derivation> = HashMap::new();

    while !queue.is_empty() {
        let drv_path = queue.pop_front().unwrap();
        let derivation = parse_drv_path(&drv_path);

        // insert
        seen_drvs.insert(drv_path.clone(), derivation.clone());

        // add all input_derivation paths to the queue if not already in there.
        for child_drv_path in derivation.input_derivations.keys() {
            if seen_drvs.contains_key(child_drv_path) || queue.contains(child_drv_path) {
                continue;
            }
            queue.push_back(child_drv_path.clone());
        }
    }

    let mut force_graph: ForceGraph<(), (), Undirected> = ForceGraph::default();

    // lookup table from drv path to node index in the graph
    let mut node_idxs: HashMap<String, _> = HashMap::new();

    // Make each drv path an actual node in the graph, and populate the
    // node_idxs lookup table with the Derivation struct.
    for (drv_path, _drv) in &seen_drvs {
        // let node_idx = force_graph.add_force_node(drv_path.clone(), drv_path.clone());
        let node_idx = force_graph.add_force_node(drv_path.clone(), ());

        node_idxs.insert(drv_path.clone(), node_idx);
    }

    // Create edges for all nodes
    for (drv_path, drv) in &seen_drvs {
        for input_drv_path in drv.input_derivations.keys() {
            force_graph.add_edge(node_idxs[drv_path], node_idxs[input_drv_path], ());
        }
    }

    force_graph
}

#[macroquad::main("Nix Derivation visualization")]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let graph = generate_graph(args[1].clone());

    fdg_macroquad::run_window(&graph).await;
}
