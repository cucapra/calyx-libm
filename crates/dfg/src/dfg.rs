use cranelift_entity::{EntityList, ListPool, PrimaryMap, entity_impl};
use malachite::Rational;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::slice::IterMut;

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
}

impl Dfg {
    pub fn new() -> Self {
        Dfg {
            nodes: PrimaryMap::new(),
            edges: PrimaryMap::new(),
            edge_pool: ListPool::new(),
        }
    }

    pub fn with_nodes(nodes: PrimaryMap<NodeId, Node>) -> Self {
        Dfg {
            nodes,
            edges: PrimaryMap::new(),
            edge_pool: ListPool::new(),
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
        let input_new_edges = self.nodes[self.edges[e_id].input]
            .outputs
            .as_slice(&self.edge_pool)
            .iter()
            .copied()
            .filter(|e| *e != e_id)
            .collect::<Vec<_>>();
        self.nodes[self.edges[e_id].input]
            .outputs
            .clear(&mut self.edge_pool);
        for edgeid in input_new_edges {
            self.nodes[self.edges[e_id].input]
                .outputs
                .push(edgeid, &mut self.edge_pool);
        }
        let output_new_edges = self.nodes[self.edges[e_id].output]
            .inputs
            .as_slice(&self.edge_pool)
            .iter()
            .copied()
            .filter(|e| *e != e_id)
            .collect::<Vec<_>>();
        self.nodes[self.edges[e_id].output]
            .inputs
            .clear(&mut self.edge_pool);
        for edgeid in output_new_edges {
            self.nodes[self.edges[e_id].output]
                .inputs
                .push(edgeid, &mut self.edge_pool);
        }
        todo!();
        //figure out what to do with orphaned edge
    }

    pub fn node_iter(&mut self) -> IterMut<'_, Node> {
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

    pub fn input_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].node_type, NodeKind::Input))
            .collect()
    }

    pub fn output_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].node_type, NodeKind::Output))
            .collect()
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
                    || node.node_type == NodeKind::Output
                {
                    "box"
                } else {
                    "circle"
                }
            ));
        }

        for edge in self.edges.values() {
            dot.push_str(&format!("\t{} -> {};\n", edge.input, edge.output));
        }

        dot.push_str("}\n");

        dot
    }

    ///The dfg should have exactly one input and one output node
    pub fn replace_node_with_dfg(
        self,
        node_id: NodeId,
        dfg: Dfg,
    ) -> Result<Dfg, &'static str> {
        if self.input_nodes().len() != 1 {
            return Err("Not exactly one input");
        } else if self.output_nodes().len() != 1 {
            return Err("Not exactly one output");
        }
        let input_id: NodeId = *self.input_nodes().first().unwrap();
        let replacement_input: NodeId =
            self[self[node_id].inputs.first(&self.edge_pool).unwrap()].input;
        let output_id: NodeId = *self.output_nodes().first().unwrap();
        let replacement_output: NodeId =
            self[self[node_id].outputs.first(&self.edge_pool).unwrap()].output;
        let mut final_dfg = Dfg::with_nodes(self.nodes);
        let mut node_map = HashMap::new();

        for (old_id, node) in dfg.nodes.iter() {
            let mut new_node = node.clone();
            new_node.inputs = EntityList::new();
            new_node.outputs = EntityList::new();

            let new_id = final_dfg.add_node(new_node);
            node_map.insert(old_id, new_id);
        }
        for edge in dfg.edges.values() {
            final_dfg.add_edge(Edge {
                input: if edge.input == input_id {
                    replacement_input
                } else {
                    *node_map.get(&edge.input).unwrap()
                },
                output: if edge.output == output_id {
                    replacement_output
                } else {
                    *node_map.get(&edge.output).unwrap()
                },
                props: edge.props.clone(),
                edge_type: edge.edge_type.clone(),
            });
        }
        final_dfg.remove_edge(
            final_dfg[node_id]
                .inputs
                .first(&final_dfg.edge_pool)
                .unwrap(),
        );
        final_dfg.remove_edge(
            final_dfg[node_id]
                .outputs
                .first(&final_dfg.edge_pool)
                .unwrap(),
        );
        //TODO figure out what to do with orphaned node
        Ok(final_dfg)
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
    Output,
    Const(Rational),
    Op(ArithOp),
}

#[derive(Clone, Debug)]
pub enum Type {
    Int {
        signed: bool,
        bits: usize,
    },
    Fixed {
        signed: bool,
        bits: usize,
        exp: isize,
    },
    F64,
    F32,
    F16,
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
            Type::Fixed {
                signed: s,
                bits: b,
                exp: e,
            } => write!(f, "{}fix<{},{}>", if *s { "s" } else { "u" }, b, e),
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
            NodeKind::Output => write!(f, "Output"),
            NodeKind::Op(o) => write!(f, "{o}"),
            NodeKind::Const(c) => write!(f, "{c}"),
        }
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

        for id in self.edges.keys() {
            let edge = &self.edges[id];

            writeln!(
                f,
                "  {id}: {:?} {:?} -> {:?}",
                edge.edge_type, edge.input, edge.output
            )?;
        }

        Ok(())
    }
}
