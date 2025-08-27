use julials::{ast::{Node, NodeData, NodeList, Span, AST}, parser::NodeKind};

const N: usize = 50;

#[test]
fn avl_tree() {
    let mut ast = AST::default();
    for i in 0..N {
        ast.push(Node {
            kind: NodeKind::IntLitL,
            data: NodeData::None,
            span: Span::new(0, (i, i), (i, i))
        });
    }
    let mut nodes = NodeList::default();
    for i in 0..N {
        nodes.push(i, ast[i].span.clone());
    }
    nodes.verify();
    eprintln!("{nodes}");
}
