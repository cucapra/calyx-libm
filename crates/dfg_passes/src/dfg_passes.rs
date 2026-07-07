use calyx_libm_dfg as dfg;
use dfg::*;

pub fn pass(dfg: &mut Dfg) -> Result<(), &'static str>{
    for node in dfg.node_iter() {
        let kind = node.node_type.clone();
        match kind {
            NodeKind::Input => continue,
            NodeKind::Output => continue,
            NodeKind::Const(_) => continue,
            NodeKind::Op(op) => parse_operator(op),
        }
    }
    Ok(())
}

fn parse_operator(op: ArithOp) {

}
