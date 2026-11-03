use calyx_libm_utils as utils;
use cranelift_entity::{EntityList, ListPool, PrimaryMap, entity_impl};
use itertools::Itertools;
use malachite::{Natural, Rational};
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::slice::IterMut;
use utils::Format;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(u32);
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EdgeId(u32);
entity_impl!(NodeId, "n");
entity_impl!(EdgeId, "e");

#[derive(Debug)]
pub struct Dfg {
    pub nodes: PrimaryMap<NodeId, Node>,
    pub edges: PrimaryMap<EdgeId, Edge>,
    pub edge_pool: ListPool<EdgeId>,
    output_nodes: Vec<NodeId>,
}

impl Dfg {
    pub fn new() -> Self {
        Dfg {
            nodes: PrimaryMap::new(),
            edges: PrimaryMap::new(),
            edge_pool: ListPool::new(),
            output_nodes: Vec::new(),
        }
    }

    pub fn with_nodes(nodes: PrimaryMap<NodeId, Node>) -> Self {
        Dfg {
            nodes,
            edges: PrimaryMap::new(),
            edge_pool: ListPool::new(),
            output_nodes: Vec::new(),
        }
    }

    ///Adds an edge to the Dfg. Updates connected nodes to contain this edge
    ///as an neighbor
    pub fn add_edge(&mut self, edge: Edge) -> EdgeId {
        let e_id = self.edges.push(edge);
        self.nodes[self.edges[e_id].output]
            .inputs
            .push(e_id, &mut self.edge_pool);
        self.nodes[self.edges[e_id].input]
            .outputs
            .push(e_id, &mut self.edge_pool);
        e_id
    }

    pub fn remove_edge(&mut self, e_id: EdgeId) {
        let input = self.edges[e_id].input;
        let output = self.edges[e_id].output;

        let input_pos = self.nodes[input]
            .outputs
            .as_slice(&self.edge_pool)
            .iter()
            .position(|e| *e == e_id)
            .expect("edge should be in input node's outputs");
        self.nodes[input]
            .outputs
            .swap_remove(input_pos, &mut self.edge_pool);

        let output_pos = self.nodes[output]
            .inputs
            .as_slice(&self.edge_pool)
            .iter()
            .position(|e| *e == e_id)
            .expect("edge should be in output node's inputs");
        self.nodes[output]
            .inputs
            .remove(output_pos, &mut self.edge_pool);
    }

    pub fn nodes_mut(&mut self) -> IterMut<'_, Node> {
        self.nodes.values_mut()
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        self.nodes.push(node)
    }

    pub fn add_const(&mut self, value: Rational) -> NodeId {
        self.nodes.push(Node {
            node_type: NodeKind::Const(value),
            inputs: EntityList::new(),
            outputs: EntityList::new(),
            props: Vec::new(),
        })
    }

    pub fn inputs(&self, node: NodeId) -> &[EdgeId] {
        self[node].inputs.as_slice(&self.edge_pool)
    }

    pub fn node_ids(&self) -> impl Iterator<Item = NodeId> {
        self.nodes.keys()
    }

    /// Iterate over connected edges by looking for each node's inputs.
    pub fn active_edge_ids(&self) -> impl Iterator<Item = EdgeId> + '_ {
        self.nodes.values().flat_map(|node| {
            node.inputs.as_slice(&self.edge_pool).iter().copied()
        })
    }

    pub fn input_nodes(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].node_type, NodeKind::Input))
    }

    pub fn output_nodes(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.output_nodes.iter().copied()
    }

    pub fn add_output(&mut self, node: NodeId) {
        assert!(self.nodes.is_valid(node), "invalid output node");
        self.output_nodes.push(node);
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn to_dot(&self, name: &str) -> String {
        let mut dot: String = String::new();

        dot.push_str(&format!(
            "subgraph {name} {{\n\
            \trankdir=BT;\n\
            \tordering=out;\n\
            \tnode [fontname=\"Helvetica\"];\n\
            \tedge [fontname=\"Helvetica\"];\n\n",
        ));

        for (n_id, node) in self.nodes.iter() {
            dot.push_str(&format!(
                "\t{} [label=\"{}\", shape={}];\n",
                n_id,
                node,
                if node.node_type == NodeKind::Input
                    || self.output_nodes.contains(&n_id)
                {
                    "box"
                } else {
                    "circle"
                }
            ));
        }

        for edge_id in self.active_edge_ids() {
            let edge = &self[edge_id];
            dot.push_str(&format!("\t{} -> {};\n", edge.input, edge.output));
        }

        dot.push_str("}\n");

        dot
    }

    /// Replace a node with exactly one incoming edge using a DFG
    /// with exactly one input node and one declared output.
    pub fn replace_node_with_dfg(&mut self, node_id: NodeId, dfg: Dfg) {
        assert!(
            !matches!(self[node_id].node_type, NodeKind::Input),
            "cannot replace a graph input"
        );

        let (input_id,) = dfg
            .input_nodes()
            .collect_tuple()
            .expect("DFG must have exactly one input node");
        let (output_id,) = dfg
            .output_nodes()
            .collect_tuple()
            .expect("DFG must have exactly one declared output");

        assert!(
            dfg.inputs(input_id).is_empty(),
            "replacement input must not have incoming edges"
        );

        let (incoming_edge,) = self
            .inputs(node_id)
            .iter()
            .copied()
            .collect_tuple()
            .expect("target must have exactly one incoming edge");

        let replacement_input = self[incoming_edge].input;
        assert_ne!(replacement_input, node_id, "target has a self-loop");

        let outgoing_edges =
            self[node_id].outputs.as_slice(&self.edge_pool).to_vec();

        let mut node_map = HashMap::new();
        node_map.insert(input_id, replacement_input);

        for (old_id, node) in dfg.nodes.iter() {
            if old_id == input_id {
                continue;
            }

            let mut new_node = node.clone();
            new_node.inputs = EntityList::new();
            new_node.outputs = EntityList::new();

            node_map.insert(old_id, self.add_node(new_node));
        }

        for edge_id in dfg.active_edge_ids() {
            let edge = &dfg[edge_id];

            self.add_edge(Edge {
                input: node_map[&edge.input],
                output: node_map[&edge.output],
                props: edge.props.clone(),
                edge_type: edge.edge_type.clone(),
            });
        }

        let replacement_output = node_map[&output_id];

        for edge_id in outgoing_edges {
            // Preserve the edge ID and its position in the consumer's
            // input list: that position can identify an operand.
            self[edge_id].input = replacement_output;
            self.nodes[replacement_output]
                .outputs
                .push(edge_id, &mut self.edge_pool);
        }
        self.nodes[node_id].outputs.clear(&mut self.edge_pool);

        for output in &mut self.output_nodes {
            if *output == node_id {
                *output = replacement_output;
            }
        }

        self.remove_edge(incoming_edge);
        // The replaced node remains allocated but disconnected.
    }
}

macro_rules! dfg_index_impl {
    ($field:ident, $idx:ty, $out:ty) => {
        impl Index<$idx> for Dfg {
            type Output = $out;

            #[inline]
            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index]
            }
        }

        impl IndexMut<$idx> for Dfg {
            #[inline]
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }
    };
}

dfg_index_impl!(edges, EdgeId, Edge);
dfg_index_impl!(nodes, NodeId, Node);

impl Default for Dfg {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub inputs: EntityList<EdgeId>,
    pub outputs: EntityList<EdgeId>,
    pub props: Vec<String>,
    pub node_type: NodeKind,
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub input: NodeId,
    pub output: NodeId,
    pub props: Vec<String>,
    pub edge_type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Sin,
    Cos,
    Asin,
    Acos,
    Asinh,
    Acosh,
    Abs,
    Pow,
    Min,
    Max,
    Floor,
    Exp,
    Gain { gain: f64 },
    Block { name: String },
    Other { name: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Input,
    Const(Rational),
    Op(ArithOp),
    Rom(RomData),
}

#[derive(Clone, Debug)]
pub enum Type {
    Int { signed: bool, bits: usize },
    Fixed(Format),
    F64,
    F32,
    F16,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RomData {
    pub data: Vec<Natural>,
}

impl std::fmt::Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithOp::Add => write!(f, "+"),
            ArithOp::Sub => write!(f, "-"),
            ArithOp::Mul => write!(f, "*"),
            ArithOp::Div => write!(f, "/"),
            ArithOp::Neg => write!(f, "neg"),
            ArithOp::Abs => write!(f, "abs"),
            ArithOp::Pow => write!(f, "pow"),
            ArithOp::Min => write!(f, "min"),
            ArithOp::Max => write!(f, "max"),
            ArithOp::Floor => write!(f, "floor"),
            ArithOp::Exp => write!(f, "exp"),
            ArithOp::Sin => write!(f, "sin"),
            ArithOp::Cos => write!(f, "cos"),
            ArithOp::Asin => write!(f, "asin"),
            ArithOp::Acos => write!(f, "acos"),
            ArithOp::Asinh => write!(f, "asinh"),
            ArithOp::Acosh => write!(f, "acosh"),
            ArithOp::Gain { gain: g } => write!(f, "gain({})", g),
            ArithOp::Block { name: n } => write!(f, "{}", n),
            ArithOp::Other { name: n } => write!(f, "{}", n),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int { signed: s, bits: b } => {
                write!(f, "{}{}", if *s { "int" } else { "uint" }, b)
            }
            Type::Fixed(form) => write!(
                f,
                "{}fix<{},{}>",
                if form.is_signed { "s" } else { "u" },
                form.width,
                form.scale
            ),
            Type::F64 => write!(f, "double"),
            Type::F32 => write!(f, "float"),
            Type::F16 => write!(f, "f16"),
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.node_type {
            NodeKind::Input => write!(f, "Input"),
            NodeKind::Op(o) => write!(f, "{o}"),
            NodeKind::Const(c) => write!(f, "{c}"),
            NodeKind::Rom(r) => write!(f, "{r}"),
        }
    }
}

impl std::fmt::Display for RomData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for r in &self.data {
            write!(f, "{r}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Dfg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Nodes:")?;

        for id in self.nodes.keys() {
            let node = &self.nodes[id];

            write!(f, "  {id}: {:?} ", node.node_type)?;

            let inputs = node.inputs.as_slice(&self.edge_pool);
            let outputs = node.outputs.as_slice(&self.edge_pool);

            write!(f, "in=[")?;
            for (i, e) in inputs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{e}")?;
            }

            write!(f, "] out=[")?;
            for (i, e) in outputs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{e}")?;
            }

            writeln!(f, "]")?;
        }

        writeln!(f, "\nEdges:")?;

        for id in self.active_edge_ids() {
            let edge = &self.edges[id];

            writeln!(
                f,
                "  {id}: {:?} {:?} -> {:?}",
                edge.edge_type, edge.input, edge.output
            )?;
        }

        writeln!(f, "\nOutputs: {:?}", self.output_nodes)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(dfg: &mut Dfg, kind: NodeKind) -> NodeId {
        dfg.add_node(Node {
            node_type: kind,
            inputs: EntityList::new(),
            outputs: EntityList::new(),
            props: Vec::new(),
        })
    }

    fn connect(dfg: &mut Dfg, source: NodeId, destination: NodeId) -> EdgeId {
        dfg.add_edge(Edge {
            input: source,
            output: destination,
            props: vec!["test edge".into()],
            edge_type: Type::F32,
        })
    }

    fn unary_graph() -> (Dfg, NodeId, NodeId) {
        let mut dfg = Dfg::new();
        let input = node(&mut dfg, NodeKind::Input);
        let output = node(&mut dfg, NodeKind::Op(ArithOp::Neg));
        connect(&mut dfg, input, output);
        dfg.add_output(output);
        (dfg, input, output)
    }

    fn assert_consistent(dfg: &Dfg) {
        for (id, node) in dfg.nodes.iter() {
            for &edge in node.inputs.as_slice(&dfg.edge_pool) {
                assert_eq!(dfg[edge].output, id);
                assert!(
                    dfg[dfg[edge].input]
                        .outputs
                        .as_slice(&dfg.edge_pool)
                        .contains(&edge)
                );
            }
            for &edge in node.outputs.as_slice(&dfg.edge_pool) {
                assert_eq!(dfg[edge].input, id);
                assert!(dfg.inputs(dfg[edge].output).contains(&edge));
            }
        }
    }

    #[test]
    fn outputs_preserve_order_duplicates_and_non_sink_nodes() {
        let (mut dfg, input, output) = unary_graph();
        dfg.add_output(input);
        dfg.add_output(output);
        assert_eq!(
            dfg.output_nodes().collect::<Vec<_>>(),
            [output, input, output]
        );
        assert_eq!(dfg.input_nodes().collect::<Vec<_>>(), [input]);
    }

    #[test]
    #[should_panic(expected = "invalid output node")]
    fn rejects_invalid_output() {
        Dfg::new().add_output(NodeId::from_u32(0));
    }

    #[test]
    fn removing_edge_preserves_other_operands_and_hides_disconnected_edge() {
        let (mut dfg, input, output) = unary_graph();
        let removed = dfg.inputs(output)[0];
        let other = dfg.add_const(Rational::from(2));
        let retained = connect(&mut dfg, other, output);
        dfg.remove_edge(removed);

        assert_eq!(dfg.inputs(output), [retained]);
        assert!(dfg[input].outputs.as_slice(&dfg.edge_pool).is_empty());
        assert_eq!(dfg.active_edge_ids().collect::<Vec<_>>(), [retained]);
        assert!(
            !dfg.to_dot("test")
                .contains(&format!("{input} -> {output};"))
        );
        assert!(!dfg.to_string().contains(&format!("  {removed}:")));
        assert!(dfg.to_string().contains(&format!("Outputs: [{output}]")));
        assert_consistent(&dfg);
    }

    #[test]
    fn replacement_rewires_fanout_and_declared_outputs_preserving_operand_positions()
     {
        let (mut dfg, input, target) = unary_graph();
        let constant = dfg.add_const(Rational::from(3));
        let consumer = node(&mut dfg, NodeKind::Op(ArithOp::Sub));
        let first = connect(&mut dfg, constant, consumer);
        let second = connect(&mut dfg, target, consumer);
        let another = node(&mut dfg, NodeKind::Op(ArithOp::Abs));
        let third = connect(&mut dfg, target, another);
        dfg.add_output(consumer);
        dfg.add_output(target);
        let (replacement, _, _) = unary_graph();

        dfg.replace_node_with_dfg(target, replacement);

        let outputs = dfg.output_nodes().collect::<Vec<_>>();
        let new_output = outputs[0];
        assert_ne!(new_output, target);
        assert_eq!(outputs, [new_output, consumer, new_output]);
        assert_eq!(dfg[new_output].node_type, NodeKind::Op(ArithOp::Neg));
        assert_eq!(dfg[dfg.inputs(new_output)[0]].input, input);
        assert_eq!(dfg.inputs(consumer), [first, second]);
        assert_eq!(dfg[second].input, new_output);
        assert_eq!(dfg[third].input, new_output);
        assert_eq!(dfg[second].props, ["test edge"]);
        assert!(matches!(dfg[second].edge_type, Type::F32));
        assert!(dfg.inputs(target).is_empty());
        assert!(dfg[target].outputs.as_slice(&dfg.edge_pool).is_empty());
        assert_eq!(dfg.input_nodes().collect::<Vec<_>>(), [input]);
        assert_consistent(&dfg);
    }

    #[test]
    fn replacement_handles_output_without_consumers() {
        let (mut dfg, input, target) = unary_graph();
        let (replacement, _, _) = unary_graph();
        dfg.replace_node_with_dfg(target, replacement);
        let output = dfg.output_nodes().next().unwrap();
        assert_ne!(output, target);
        assert_eq!(dfg[dfg.inputs(output)[0]].input, input);
        assert_eq!(dfg.active_edge_ids().count(), 1);
        assert_consistent(&dfg);
    }

    #[test]
    fn identity_replacement_reuses_existing_input() {
        let (mut dfg, input, target) = unary_graph();
        let consumer = node(&mut dfg, NodeKind::Op(ArithOp::Abs));
        let edge = connect(&mut dfg, target, consumer);
        let mut identity = Dfg::new();
        let identity_input = node(&mut identity, NodeKind::Input);
        identity.add_output(identity_input);
        let original_node_count = dfg.len();

        dfg.replace_node_with_dfg(target, identity);

        assert_eq!(dfg.len(), original_node_count);
        assert_eq!(dfg.output_nodes().collect::<Vec<_>>(), [input]);
        assert_eq!(dfg[edge].input, input);
        assert_eq!(dfg.active_edge_ids().collect::<Vec<_>>(), [edge]);
        assert_consistent(&dfg);
    }

    #[test]
    fn replacement_preserves_internal_operand_order_and_skips_removed_edges() {
        let (mut dfg, input, target) = unary_graph();
        let mut replacement = Dfg::new();
        let replacement_input = node(&mut replacement, NodeKind::Input);
        let constant = replacement.add_const(Rational::from(5));
        let subtract = node(&mut replacement, NodeKind::Op(ArithOp::Sub));
        let removed = connect(&mut replacement, replacement_input, subtract);
        replacement.remove_edge(removed);
        // Operand order deliberately differs from source node allocation order.
        connect(&mut replacement, constant, subtract);
        connect(&mut replacement, replacement_input, subtract);
        replacement[constant].props.push("constant metadata".into());
        replacement.add_output(subtract);

        dfg.replace_node_with_dfg(target, replacement);

        let output = dfg.output_nodes().next().unwrap();
        assert_eq!(dfg[output].node_type, NodeKind::Op(ArithOp::Sub));
        let operands = dfg.inputs(output);
        assert_eq!(operands.len(), 2);
        let mapped_constant = dfg[operands[0]].input;
        assert_eq!(
            dfg[mapped_constant].node_type,
            NodeKind::Const(Rational::from(5))
        );
        assert_eq!(dfg[mapped_constant].props, ["constant metadata"]);
        assert_eq!(dfg[operands[1]].input, input);
        assert_eq!(dfg[operands[0]].props, ["test edge"]);
        assert!(matches!(dfg[operands[0]].edge_type, Type::F32));
        assert_eq!(dfg.active_edge_ids().count(), 2);
        assert_consistent(&dfg);
    }

    #[test]
    #[should_panic(expected = "DFG must have exactly one declared output")]
    fn replacement_rejects_multiple_outputs() {
        let (mut dfg, _, target) = unary_graph();
        let (mut replacement, input, _) = unary_graph();
        replacement.add_output(input);
        dfg.replace_node_with_dfg(target, replacement);
    }

    #[test]
    #[should_panic(expected = "target must have exactly one incoming edge")]
    fn replacement_rejects_target_with_multiple_inputs() {
        let (mut dfg, input, target) = unary_graph();
        connect(&mut dfg, input, target);
        let (replacement, _, _) = unary_graph();
        dfg.replace_node_with_dfg(target, replacement);
    }
}
