use cranelift_entity::{EntityList, ListPool, PrimaryMap, entity_impl};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(u32);
entity_impl!(NodeId, "node");

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Sqrt,
    Abs,
    Pow,
    Min,
    Max,
}

#[derive(Clone, Copy, Debug)]
pub enum NodeKind {
    Input,
    Output,
    Op(ArithOp),
    Const(i64),
}

#[derive(Clone, Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub inputs: EntityList<NodeId>,
}

pub struct Dfg {
    pub nodes: PrimaryMap<NodeId, Node>,
    pub pool: ListPool<NodeId>,
}

impl Dfg {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            pool: ListPool::new(),
        }
    }

    pub fn add_input(&mut self) -> NodeId {
        self.nodes.push(Node {
            kind: NodeKind::Input,
            inputs: EntityList::new(),
        })
    }

    pub fn add_const(&mut self, value: i64) -> NodeId {
        self.nodes.push(Node {
            kind: NodeKind::Const(value),
            inputs: EntityList::new(),
        })
    }

    pub fn add_op(&mut self, op: ArithOp, input_ids: &[NodeId]) -> NodeId {
        let mut inputs = EntityList::new();
        for &id in input_ids {
            inputs.push(id, &mut self.pool);
        }
        self.nodes.push(Node {
            kind: NodeKind::Op(op),
            inputs,
        })
    }

    pub fn add_output(&mut self, source: NodeId) -> NodeId {
        let mut inputs = EntityList::new();
        inputs.push(source, &mut self.pool);
        self.nodes.push(Node {
            kind: NodeKind::Output,
            inputs,
        })
    }

    pub fn inputs(&self, node: NodeId) -> &[NodeId] {
        self.nodes[node].inputs.as_slice(&self.pool)
    }

    pub fn node_ids(&self) -> impl Iterator<Item = NodeId> {
        self.nodes.keys()
    }

    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id]
    }

    pub fn source_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].kind, NodeKind::Input))
            .collect()
    }

    pub fn sink_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .keys()
            .filter(|&id| matches!(self.nodes[id].kind, NodeKind::Output))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn successor_map(&self) -> PrimaryMap<NodeId, Vec<NodeId>> {
        let mut succs: PrimaryMap<NodeId, Vec<NodeId>> = PrimaryMap::new();

        for _ in self.nodes.keys() {
            succs.push(Vec::new());
        }

        for node_id in self.nodes.keys() {
            for &pred in self.inputs(node_id) {
                succs[pred].push(node_id);
            }
        }

        succs
    }

    pub fn topological_order(&self) -> Vec<NodeId> {
        let mut in_degree: PrimaryMap<NodeId, u32> = PrimaryMap::new();

        for _ in self.nodes.keys() {
            in_degree.push(0);
        }
        for node_id in self.nodes.keys() {
            in_degree[node_id] = self.inputs(node_id).len() as u32;
        }

        let mut queue: Vec<NodeId> =
            self.nodes.keys().filter(|&id| in_degree[id] == 0).collect();

        let succs = self.successor_map();
        let mut order = Vec::with_capacity(self.nodes.len());

        while let Some(node) = queue.pop() {
            order.push(node);
            for &succ in &succs[node] {
                in_degree[succ] -= 1;
                if in_degree[succ] == 0 {
                    queue.push(succ);
                }
            }
        }

        assert_eq!(
            order.len(),
            self.nodes.len(),
            "DFG has a cycle — topological sort failed"
        );

        order
    }
}

impl Default for Dfg {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithOp::Add => write!(f, "+"),
            ArithOp::Sub => write!(f, "-"),
            ArithOp::Mul => write!(f, "*"),
            ArithOp::Div => write!(f, "/"),
            ArithOp::Neg => write!(f, "neg"),
            ArithOp::Sqrt => write!(f, "sqrt"),
            ArithOp::Abs => write!(f, "abs"),
            ArithOp::Pow => write!(f, "pow"),
            ArithOp::Min => write!(f, "min"),
            ArithOp::Max => write!(f, "max"),
        }
    }
}

impl std::fmt::Display for Dfg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for id in self.nodes.keys() {
            let node = &self.nodes[id];
            let inputs = self.inputs(id);
            match &node.kind {
                NodeKind::Input => writeln!(f, "{id}: INPUT")?,
                NodeKind::Output => {
                    writeln!(f, "{id}: OUTPUT <- {}", inputs[0])?
                }
                NodeKind::Const(v) => writeln!(f, "{id}: CONST({v})")?,
                NodeKind::Op(op) => {
                    let args: Vec<String> =
                        inputs.iter().map(|n| format!("{n}")).collect();
                    writeln!(f, "{id}: {op}({})", args.join(", "))?;
                }
            }
        }
        Ok(())
    }
}
