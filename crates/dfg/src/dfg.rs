use cranelift_entity::{EntityList, ListPool, PrimaryMap, entity_impl};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(u32);
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EdgeId(u32);
entity_impl!(NodeId, "n");
entity_impl!(EdgeId, "e");

#[derive(Debug)]
pub struct DFG {
    pub nodes: PrimaryMap<NodeId, Node>,
    pub edges: PrimaryMap<EdgeId, Edge>,
    pub edge_pool: ListPool<EdgeId>,
}

impl DFG {
    pub fn new() -> Self {
        DFG {
            nodes: PrimaryMap::new(),
            edges: PrimaryMap::new(),
            edge_pool: ListPool::new(),
        }
    }

    pub fn new_with_nodes(nodes: PrimaryMap<NodeId, Node>) -> Self {
        DFG {
            nodes,
            edges: PrimaryMap::new(),
            edge_pool: ListPool::new(),
        }
    }

    pub fn add_edge(&mut self, edge: Edge) -> EdgeId {
        self.edges.push(edge)
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        self.nodes.push(node)
    }

    pub fn add_input(&mut self, input_edge: EdgeId) -> NodeId {
        let mut n = Node {
            node_type: NodeType::Input,
            inputs: EntityList::new(),
            outputs: EntityList::new(),
            props: Vec::new(),
        };
        n.outputs.push(input_edge, &mut self.edge_pool);
        self.nodes.push(n)
    }

    pub fn add_output(&mut self, output_edge: EdgeId) -> NodeId {
        let mut n = Node {
            node_type: NodeType::Output,
            inputs: EntityList::new(),
            outputs: EntityList::new(),
            props: Vec::new(),
        };
        n.inputs.push(output_edge, &mut self.edge_pool);
        self.nodes.push(n)
    }

    pub fn push_node_input(&mut self, node_id: NodeId, edge_id: EdgeId) {
        self.nodes[node_id]
            .inputs
            .push(edge_id, &mut self.edge_pool);
    }

    pub fn push_node_output(&mut self, node_id: NodeId, edge_id: EdgeId) {
        self.nodes[node_id]
            .outputs
            .push(edge_id, &mut self.edge_pool);
    }

    pub fn add_const(&mut self, value: f64) -> NodeId {
        self.nodes.push(Node {
            node_type: NodeType::Const(value),
            inputs: EntityList::new(),
            outputs: EntityList::new(),
            props: Vec::new(),
        })
    }

    pub fn inputs(&self, node: NodeId) -> &[EdgeId] {
        self.nodes[node].inputs.as_slice(&self.edge_pool)
    }

    pub fn node_ids(&self) -> impl Iterator<Item = NodeId> {
        self.nodes.keys()
    }

    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id]
    }

    pub fn edge(&self, id: EdgeId) -> &Edge {
        &self.edges[id]
    }

    pub fn source_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].node_type, NodeType::Input))
            .collect()
    }

    pub fn sink_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].node_type, NodeType::Output))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn to_dot(&self, name: String) -> String {
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
                if node.node_type == NodeType::Input || node.node_type == NodeType::Output {
                    "box"
                } else {
                    "circle"
                }
            ));
        }

        for (_, edge) in self.edges.iter() {
            dot.push_str(&format!("\t{} -> {};\n", edge.input, edge.output));
        }

        dot.push_str("\n");

        dot.push_str("}\n");

        dot
    }
}

#[derive(Debug)]
pub struct Node {
    pub inputs: EntityList<EdgeId>,
    pub outputs: EntityList<EdgeId>,
    pub props: Vec<String>,
    pub node_type: NodeType,
}

#[derive(Debug)]
pub struct Edge {
    pub input: NodeId,
    pub output: NodeId,
    pub props: Vec<String>,
    pub edge_type: EdgeType,
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
pub enum NodeType {
    Input,
    Output,
    Const(f64),
    Op(ArithOp),
}

#[derive(Clone, Debug)]
pub enum EdgeType {
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

impl std::fmt::Display for EdgeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeType::Int { signed: s, bits: b } => {
                write!(f, "{}{}", if *s { "int" } else { "uint" }, b)
            }
            EdgeType::Fixed {
                signed: s,
                bits: b,
                exp: e,
            } => write!(f, "{}fix<{},{}>", if *s { "s" } else { "u" }, b, e),
            EdgeType::F64 => write!(f, "double"),
            EdgeType::F32 => write!(f, "float"),
            EdgeType::F16 => write!(f, "f16"),
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.node_type {
            NodeType::Input => write!(f, "Input"),
            NodeType::Output => write!(f, "Output"),
            NodeType::Op(o) => write!(f, "{o}"),
            NodeType::Const(c) => write!(f, "{c}"),
        }
    }
}

impl std::fmt::Display for DFG {
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
