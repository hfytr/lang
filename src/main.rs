use std::io::Read;

use julials::parser::{make_parser, NodeKind, ParseState};

fn main() {
    unsafe {backtrace_on_stack_overflow::enable();}
    let mut engine = make_parser().unwrap();
    let mut f = std::fs::File::open("test.jl").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let mut state = ParseState::default();
    state.ast.add_file("test.jl".into());
    // let mut a = s.as_str();
    // dbg!(engine.lex(&mut a, &mut state));
    // dbg!(&state);
    let result = engine.parse(NodeKind::ExprList, &s, &mut state).unwrap();
    dbg!(result);
    dbg!(&state);
}
