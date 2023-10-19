use std::collections::{HashMap, VecDeque};

use eframe::{run_native, App, CreationContext};
use egui::Context;
use egui_graphs::{Graph, GraphView, SettingsInteraction, SettingsStyle};
use fdg_sim::{ForceGraph, ForceGraphHelper, Simulation, SimulationParameters};
use nix_compat::{derivation::Derivation, store_path::StorePath};
use petgraph::{stable_graph::StableGraph, Directed};

pub struct BasicInteractiveApp {
    g: Graph<String, (), Directed>,
}

impl BasicInteractiveApp {
    fn new(_: &CreationContext<'_>) -> Self {
        let g = generate_graph();
        Self { g }
    }
}

impl App for BasicInteractiveApp {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let interaction_settings = &SettingsInteraction::new()
                .with_dragging_enabled(true)
                .with_clicking_enabled(true)
                .with_selection_enabled(true)
                .with_selection_multi_enabled(true);
            // .with_selection_depth(i32::MAX)
            // .with_folding_depth(usize::MAX);
            ui.add(
                &mut GraphView::new(&mut self.g)
                    .with_interactions(interaction_settings)
                    .with_styles(
                        &SettingsStyle::default()
                            .with_edge_radius_weight(0.0)
                            .with_labels_always(true),
                    ),
            );
        });
    }
}

fn parse_drv_path(drv_path: &str) -> Derivation {
    let b = std::fs::read(&drv_path).expect(&format!("unable to read {} from disk", drv_path));

    Derivation::from_aterm_bytes(&b).expect(&format!("unable to parse derivation at {}", drv_path))
}

// fn generate_graph(drvs: HashMap<String, Derivation>) -> Graph<(), (), Directed> {
fn generate_graph() -> Graph<String, (), Directed> {
    let args: Vec<String> = std::env::args().collect();

    let mut queue: VecDeque<String> = VecDeque::from([args[1].to_string()]);

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

    // let mut g: StableGraph<String, ()> = StableGraph::new();
    let mut force_graph: ForceGraph<String, (), Directed> = ForceGraph::new();

    // lookup table from drv path to node index in the graph
    let mut node_idxs: HashMap<String, _> = HashMap::new();

    // Make each drv path an actual node in the graph, and populate the
    // node_idxs lookup table with the Derivation struct.
    for (drv_path, _drv) in &seen_drvs {
        let node_idx = force_graph.add_force_node(drv_path.clone(), drv_path.clone());

        node_idxs.insert(drv_path.clone(), node_idx);
    }

    // Create edges for all nodes
    for (drv_path, drv) in &seen_drvs {
        for input_drv_path in drv.input_derivations.keys() {
            force_graph.add_edge(node_idxs[drv_path], node_idxs[input_drv_path], ());
        }
    }

    let mut simulation = Simulation::from_graph(force_graph, SimulationParameters::default());

    for _frame in 0..20 {
        // update the nodes positions based on force algorithm
        simulation.update(0.035);

        // render (print) your nodes new locations.
        // println!("---- frame {frame} ----");
        // for node in simulation.get_graph().node_weights() {
        //     println!("\"{}\" - {:?}", node.name, node.location);
        // }
        // println!("-----------------------")
    }

    // get out a ref to the force graph
    let force_graph = simulation.get_graph();

    // now we need to recreate a plain graph, because egui_graps doesn't like fdg-sim nodes (?)
    let mut locations: HashMap<String, egui::Vec2> = HashMap::new();
    let graph = {
        let mut g: StableGraph<String, ()> = StableGraph::new();

        for n in force_graph.node_weights() {
            // grab the location data from the force graph
            locations.insert(n.name.clone(), egui::Vec2::new(n.location.x, n.location.y));
            // create the node in our display graph
            g.add_node(n.name.clone());
        }

        // Create edges for all nodes
        for (drv_path, drv) in &seen_drvs {
            for input_drv_path in drv.input_derivations.keys() {
                g.add_edge(node_idxs[drv_path], node_idxs[input_drv_path], ());
            }
        }

        g
    };

    egui_graphs::to_graph_custom(
        &graph,
        |_idx, data| {
            let location = locations.get(data).unwrap().clone();
            let storepath = StorePath::from_absolute_path(data.as_bytes()).unwrap();
            egui_graphs::Node::new(location, data.clone()).with_label(storepath.name)
            // egui_graphs::default_node_transform(idx, data).with_label(data.to_string())
        },
        |_idx, _e| egui_graphs::Edge::default().with_width(0.1),
    )
}

// read a .drv from stdin, assume all input drvs to be inside /nix/store.
fn main() {
    let native_options = eframe::NativeOptions::default();
    run_native(
        "egui_graphs_basic_interactive_demo",
        native_options,
        Box::new(|cc| Box::new(BasicInteractiveApp::new(cc))),
    )
    .unwrap();
}
