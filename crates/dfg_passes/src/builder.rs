use calyx_libm_dfg as dfg;
use cranelift_entity::PrimaryMap;
use dfg::*;
use std::ops::{Index, IndexMut};
/*
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq)]
pub struct BuilderEdgeId(u32);
entity_impl!(BuilderEdgeId, "be");
*/

pub struct DfgBuilder {
    nodes: PrimaryMap<NodeId, Node>,
    edges: PrimaryMap<EdgeId, Edge>,
}

impl DfgBuilder {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            edges: PrimaryMap::new(),
        }
    }

    pub fn finish(self) -> Dfg {
        let mut dfg: Dfg = Dfg::with_nodes(self.nodes);

        for edge in self.edges.values() {
            let n_out = edge.output;
            let n_in = edge.input;
            dfg.add_edge(Edge {
                input: n_in,
                output: n_out,
                props: edge.props.clone(),
                edge_type: edge.edge_type.clone(),
            });
        }

        dfg
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        self.nodes.push(node)
    }

    pub fn connect_nodes(
        &mut self,
        input: NodeId,
        output: NodeId,
        props: Vec<String>,
        edge_type: Type,
    ) -> EdgeId {
        self.edges.push(Edge {
            input,
            output,
            props,
            edge_type,
        })
    }
}

impl Default for DfgBuilder {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! dfg_index_impl {
    ($field:ident, $idx:ty, $out:ty) => {
        impl Index<$idx> for DfgBuilder {
            type Output = $out;

            #[inline]
            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index]
            }
        }

        impl IndexMut<$idx> for DfgBuilder {
            #[inline]
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }
    };
}

dfg_index_impl!(nodes, NodeId, Node);
