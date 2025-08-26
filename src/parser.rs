use crate::ast::{Node, NodeData, Span, AST, NodeList};

#[derive(Debug, Default)]
pub struct ParseState {
    pub file_ind: usize,
    pub errors: Vec<String>,
    pub line: usize,
    pub col: usize,
    pub ast: AST,
    // parser ensures brackets match
    pub brackets: usize,
}

fn make_lit<'a>(
    state: &'a mut ParseState,
    text: &'_ str,
    kind: NodeKind,
) -> Option<(usize, usize)> {
    let start = (state.line, state.col);
    state.col += text.len();
    let node = Node {
        kind,
        span: Span::new(state.file_ind, start, (state.line, state.col)),
        data: NodeData::None,
    };
    Some((state.ast.push(node), kind as usize))
}

fn new_line(state: &mut ParseState) -> Option<(usize, usize)>{
    let start = (state.line, state.col);
    state.col = 0;
    state.line += 1;
    (state.brackets == 0).then(|| {
        let result = state.ast.push(Node {
            kind: NodeKind::TermL,
            span: Span::new(state.file_ind, start, (state.line, state.col)),
            data: NodeData::None,
        });
        (result, NodeKind::TermL as usize)
    })
}

fn make_bracket(state: &mut ParseState, open: bool, kind: NodeKind) -> Option<(usize, usize)> {
    if open {
        state.brackets += 1
    } else {
        state.brackets -= 1
    }
    make_lit(state, " ", kind)
}

fn int_lit(state: &mut ParseState, mut text: &str, base: u32) -> Option<(usize, usize)> {
    let start = (state.line, state.col);
    state.col += text.len();
    let neg = text.as_bytes()[0] == b'-';
    if neg {
        text = &text[1..];
    }
    if base != 10 {
        text = &text[2..]
    }
    let neg = if neg { -1 } else { 1 };
    let (kind, data) = if let Ok(x) = i128::from_str_radix(text, base) {
        (NodeKind::IntLitL, NodeData::IntLit(x * neg))
    } else if let Ok(x) = u128::from_str_radix(text, base) {
        (NodeKind::UIntLitL, NodeData::UIntLit(x))
    } else {
        state
            .errors
            .push(format!("found too large integer literal"));
        (NodeKind::IntLitL, NodeData::Error)
    };
    let node = Node {
        kind,
        data,
        span: Span::new(state.file_ind, start, (state.line, state.col)),
    };
    Some((state.ast.push(node), kind as usize))
}

fn float_lit(state: &mut ParseState, text: &str, base: u32) -> Option<(usize, usize)> {
    let start = (state.line, state.col);
    state.col += text.len();
    let pred = if base == 16 {
        |c: char| c == 'p' || c == 'P' || c == '.'
    } else {
        |c: char| c == '.'
    };
    let (dec_point, dec_char) = text
        .chars()
        .enumerate()
        .filter(|(_, c)| pred(*c))
        .next()
        .unwrap();
    let whole_part = u64::from_str_radix(&text[..dec_point], base).unwrap() as f64;
    let exp_neg = text[dec_point + 1..]
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == '-' || *c == '+')
        .next()
        .map(|x| (x.0, x.1 == '-'));
    let (exp_point, exp_char) = text[dec_point + 1..]
        .chars()
        .enumerate()
        .filter(|(_, c)| ['e', 'f', 'p', 'P'].contains(c))
        .next()
        .unzip();
    let dec_part = u64::from_str_radix(&text[dec_point + 1..exp_point.unwrap_or(text.len())], base)
        .unwrap() as f64;
    let mut val = whole_part + dec_part / (base as f64).powi((text.len() - dec_point - 1) as i32);
    if let Some(exp_point) = exp_point
        && let Some(exp_char) = exp_char
    {
        let neg = if exp_neg.is_some() { -1 } else { 1 };
        if dec_char != '.' {
            let lit_type = if base == 16 { "hex" } else { "decimal" };
            state.errors.push(format!("{lit_type} float literals with an exponent suffix must use a . decimal point (not {dec_char})"));
        }
        if base == 16 && exp_char != 'p' && exp_char != 'P' {
            state.errors.push(format!(
                "hex float literals must use a p or P as the exponentiation suffix (not {exp_char})"
            ));
        }
        let exp = neg * text[exp_point + 1..].parse::<i32>().unwrap();
        val = val * (base as f64).powi(exp)
    } else if base == 16 && dec_char != 'p' && dec_char != 'P' {
        state.errors.push(format!("hex float without exponent suffix must use p or P as the decimal point (not {dec_char})"));
    }

    let node = Node {
        kind: NodeKind::FloatLitL,
        span: Span::new(state.file_ind, start, (state.line, state.col)),
        data: NodeData::FloatLit(val),
    };
    Some((state.ast.push(node), NodeKind::FloatLitL as usize))
}

fn str_lit(state: &mut ParseState, text: &'_ str) -> Option<(usize, usize)> {
    let start = (state.line, state.col);
    state.col += text.len();
    let node = Node {
        kind: NodeKind::StrLitL,
        data: NodeData::String(text.to_string()),
        span: Span::new(state.file_ind, start, (state.line, state.col)),
    };
    Some((state.ast.push(node), NodeKind::StrLitL as usize))
}

fn expr_list(state: &mut ParseState, expr_id: usize, expr_list_id: usize) -> usize {
    let [expr, expr_list] = state.ast.get_mut([expr_id, expr_list_id]);
    if let Node {
        span,
        data: NodeData::VarKids(kids),
        ..
    } = expr_list
    {
        span.merge_with(&expr.span);
        kids.push(expr_id, expr.span.clone());
        expr_list_id
    } else {
        panic!();
    }
}

fn make_binop(
    state: &mut ParseState,
    expr_id: usize,
    expr_list_id: usize,
    kind: NodeKind,
) -> usize {
    let span = state.ast[expr_id].span.clone();
    if let NodeData::VarKids(ref mut kids) = state.ast[expr_list_id].data {
        kids.push(expr_id, span);
        expr_list_id
    } else {
        let node = Node {
            kind,
            span: state.ast[expr_id].span.merge(&state.ast[expr_id].span),
            data: NodeData::VarKids(NodeList::from_nodes(
                &state.ast,
                [expr_id, expr_list_id].into_iter(),
            )),
        };
        state.ast.push(node)
    }
}

parser::parser! {
    State(ParseState),
    Output(usize),
    Kind(pub NodeKind),
    GeneratedFn(pub make_parser),
    _ => Regex("[ \t]*" |state: &mut ParseState, text: &str| {state.col += text.len(); None}),
    _ => Regex("#=([^=]|=[^#])*=#" |_, _| { None }),
    _ => Regex("#[^\\n]*" |state: &mut ParseState, text: &str| {state.col += text.len(); None}),

    ExprList => Rule(
        Expr,
        TermL,
        TermL ExprList |_, _, elist| elist,
        Expr TermL ExprList |state, e, _, elist| expr_list(state, e, elist)
    ),

    Expr => Rule(IdentifierL),

    AssignExpr => Rule(
        PairExpr,
        PairExpr AssignOp AssignExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::AssignExpr)
    ),
    AssignOp => Rule(
        EqL, DivEqL, ModEqL, AndEqL, OrEqL, MulEqL, LShiftEqL, RShiftEqL, ULShiftEqL, URShiftEqL,
        RevDivEqL, AddEqL, SubEqL, BNotEqL, ExpEqL,
    ),

    PairExpr => Rule(
        QuestionExpr,
        QuestionExpr PairL PairExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::PairExpr)
    ),

    QuestionExpr => Rule(
        LOrExpr,
        LOrExpr QuestionL QuestionExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::QuestionExpr)
    ),

    LOrExpr => Rule(
        LAndExpr,
        LAndExpr LOrL LOrExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::LOrExpr)
    ),

    LAndExpr => Rule(
        CompareExpr,
        CompareExpr LAndL LAndExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::LAndExpr)
    ),

    CompareExpr => Rule(
        PipeLeftExpr,
        PipeLeftExpr CompareOp CompareExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::CompareExpr)
    ),
    CompareOp => Rule(
        TripleEqL, NotTripleEqL, DoubleEqL, NotEqL, MoreEqL, LessEqL, MoreL, LessL, SubTypeL,
        SupTypeL, InL, IsAL
    ),

    PipeLeftExpr => Rule(
        PipeRightExpr,
        PipeRightExpr PipeLeftL PipeLeftExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::PipeLeftExpr)
    ),

    PipeRightExpr => Rule(
        ColonExpr,
        ColonExpr PipeRightL PipeRightExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::PipeRightExpr)
    ),

    ColonExpr => Rule(
        AddExpr,
        AddExpr ColonL ColonExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::ColonExpr)
    ),

    AddExpr => Rule(
        MulExpr,
        MulExpr AddOp AddExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::AddExpr)
    ),
    AddOp => Rule(AddL, SubL, OrL),

    MulExpr => Rule(
        IdentifierL,
        IdentifierL MulOp MulExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::MulExpr)
    ),
    MulOp => Rule(DivL, ModL, AndL, MulL, RevDivL),

    RationalExpr => Rule(
        ShiftExpr,
        ShiftExpr RationalL RationalExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::RationalExpr)
    ),

    ShiftExpr => Rule(
        PowExpr,
        PowExpr ShiftOp ShiftExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::ShiftExpr)
    ),
    ShiftOp => Rule(LShiftL, RShiftL, URShiftL),

    PowExpr => Rule(
        DColonExpr,
        DColonExpr PowL PowExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::PowExpr)
    ),

    DColonExpr => Rule(
        BaseExpr,
        BaseExpr DColonL DColonExpr |state, e, _, elist| make_binop(state, e, elist, NodeKind::DColonExpr)
    ),

    BaseExpr => Rule(
        IdentifierL,
        UIntLitL,
        IntLitL,
        FloatLitL,
        StrLitL, 
        LParenL Expr RParenL |state: &mut ParseState, lp: usize, expr, rp| {
            state.ast[expr].span = state.ast[lp].span.merge(&state.ast[rp].span);
            expr
        }
    ),

    IdentifierL => Regex("[a-zA-Z_][a-zA-Z0-9_]*" |state: &mut ParseState, text: &str| {
        let start = (state.line, state.col);
        state.col += text.len();
        let node = Node {
            kind: NodeKind::IdentifierL,
            span: Span::new(state.file_ind, start, (state.line, state.col)),
            data: NodeData::String(text.to_string())
        };
        Some((state.ast.push(node), NodeKind::IdentifierL as usize))
    }),

    UIntLitL,
    IntLitL => Regex("0b[01_]*" |state, text| int_lit(state, text, 2)),
    IntLitL => Regex("0o[0-7_]*" |state, text| int_lit(state, text, 8)),
    IntLitL => Regex("[0-9_]*" |state, text| int_lit(state, text, 10)),
    IntLitL => Regex("0x[0-9a-fA-F_]*" |state, text| int_lit(state, text, 16)),
    FloatLitL => Regex("[0-9_]*\\.[0-9_]*([ef](-|+|)[0-9][0-9]*|)" |state, text| float_lit(state, text, 10)),
    FloatLitL => Regex("0x[0-9a-fA-F_]*[pP.][0-9a-fA-F_]*([pP](-|+|)[0-9]|)*" |state, text: &str| float_lit(state, &text[2..], 16)),
    StrLitL => Regex("\"([^\"]|\\\\\")*\"" |state, text: &str| str_lit(state, &text[1..text.len()-1])),
    StrLitL => Regex("\"\"\"([^\"]|\"[^\"]|\"\"[^\"])*\"\"\"" |state, text: &str| str_lit(state, &text[3..text.len()-3])),

    FunctionL => Literal("function" |state, text| make_lit(state, text, NodeKind::FunctionL)),
    BeginL => Literal("begin" |state, text| make_lit(state, text, NodeKind::BeginL)),
    EndL => Literal("end" |state, text| make_lit(state, text, NodeKind::EndL)),
    IfL => Literal("if" |state, text| make_lit(state, text, NodeKind::IfL)),
    ElseL => Literal("else" |state, text| make_lit(state, text, NodeKind::ElseL)),
    ElseIfL => Literal("elseif" |state, text| make_lit(state, text, NodeKind::ElseIfL)),
    ForL => Literal("for" |state, text| make_lit(state, text, NodeKind::ForL)),
    InL => Literal("in" |state, text| make_lit(state, text, NodeKind::InL)),
    IsAL => Literal("isa" |state, text| make_lit(state, text, NodeKind::IsAL)),
    ModuleL => Literal("module" |state, text| make_lit(state, text, NodeKind::ModuleL)),
    StructL => Literal("struct" |state, text| make_lit(state, text, NodeKind::StructL)),
    MutableL => Literal("mutable" |state, text| make_lit(state, text, NodeKind::MutableL)),
    WhileL => Literal("while" |state, text| make_lit(state, text, NodeKind::WhileL)),
    BreakL => Literal("break" |state, text| make_lit(state, text, NodeKind::BreakL)),
    ContinueL => Literal("continue" |state, text| make_lit(state, text, NodeKind::ContinueL)),
    ReturnL => Literal("return" |state, text| make_lit(state, text, NodeKind::ReturnL)),
    TrueL => Literal("true" |state, text| make_lit(state, text, NodeKind::TrueL)),
    FalseL => Literal("false" |state, text| make_lit(state, text, NodeKind::FalseL)),
    NothingL => Literal("nothing" |state, text| make_lit(state, text, NodeKind::NothingL)),
    ConstL => Literal("const" |state, text| make_lit(state, text, NodeKind::ConstL)),
    GlobalL => Literal("global" |state, text| make_lit(state, text, NodeKind::GlobalL)),
    LocalL => Literal("local" |state, text| make_lit(state, text, NodeKind::LocalL)),

    LParenL => Literal("(" |state, _| make_bracket(state, true, NodeKind::LParenL)),
    RParenL => Literal(")" |state, _| make_bracket(state, false, NodeKind::RParenL)),
    LCurlL => Literal("{" |state, _| make_bracket(state, true, NodeKind::LCurlL)),
    RCurlL => Literal("}" |state, _| make_bracket(state, false, NodeKind::RCurlL)),
    LSquareL => Literal("[" |state, _| make_bracket(state, true, NodeKind::LSquareL)),
    RSquareL => Literal("]" |state, _| make_bracket(state, false, NodeKind::RSquareL)),

    CommaL => Literal("," |state, text| make_lit(state, text, NodeKind::CommaL)),
    TermL => Literal(";" |state, text| make_lit(state, text, NodeKind::TermL)),
    TermL => Literal("\n" |state: &mut ParseState, _| new_line(state)),

    PairL => Literal("=>" |state, text| make_lit(state, text, NodeKind::PairL)),

    LArrowL => Literal("<-" |state, text| make_lit(state, text, NodeKind::LArrowL)),
    RArrowL => Literal("->" |state, text| make_lit(state, text, NodeKind::RArrowL)),

    LAndL => Literal("&&" |state, text| make_lit(state, text, NodeKind::LAndL)),
    LOrL => Literal("||" |state, text| make_lit(state, text, NodeKind::LOrL)),

    PipeLeftL => Literal("<|" |state, text| make_lit(state, text, NodeKind::PipeLeftL)),
    PipeRightL => Literal("|>" |state, text| make_lit(state, text, NodeKind::PipeRightL)),

    PowL => Literal("^" |state, text| make_lit(state, text, NodeKind::PowL)),

    DivL => Literal("/" |state, text| make_lit(state, text, NodeKind::DivL)),
    ModL => Literal("÷" |state, text| make_lit(state, text, NodeKind::ModL)),
    ModL => Literal("%" |state, text| make_lit(state, text, NodeKind::ModL)),
    AndL => Literal("&" |state, text| make_lit(state, text, NodeKind::AndL)),
    MulL => Literal("⋅" |state, text| make_lit(state, text, NodeKind::MulL)),
    MulL => Literal("*" |state, text| make_lit(state, text, NodeKind::MulL)),

    OrL => Literal("|" |state, text| make_lit(state, text, NodeKind::OrL)),
    AddL => Literal("+" |state, text| make_lit(state, text, NodeKind::AddL)),
    SubL => Literal("-" |state, text| make_lit(state, text, NodeKind::SubL)),

    LShiftL => Literal("<<" |state, text| make_lit(state, text, NodeKind::LShiftL)),
    RShiftL => Literal(">>" |state, text| make_lit(state, text, NodeKind::RShiftL)),
    URShiftL => Literal(">>>" |state, text| make_lit(state, text, NodeKind::URShiftL)),
    RevDivL => Literal("\\\\" |state, text| make_lit(state, text, NodeKind::RevDivL)),

    RationalL => Literal("//" |state, text| make_lit(state, text, NodeKind::RationalL)),

    BNotL => Literal("~" |state, text| make_lit(state, text, NodeKind::BNotL)),

    EqL => Literal("=" |state, text| make_lit(state, text, NodeKind::EqL)),
    DivEqL => Literal("/=" |state, text| make_lit(state, text, NodeKind::DivEqL)),
    ModEqL => Literal("÷=" |state, text| make_lit(state, text, NodeKind::ModEqL)),
    ModEqL => Literal("%=" |state, text| make_lit(state, text, NodeKind::ModEqL)),
    AndEqL => Literal("&=" |state, text| make_lit(state, text, NodeKind::AndEqL)),
    OrEqL => Literal("|=" |state, text| make_lit(state, text, NodeKind::OrEqL)),
    MulEqL => Literal("⋅=" |state, text| make_lit(state, text, NodeKind::MulEqL)),
    MulEqL => Literal("*=" |state, text| make_lit(state, text, NodeKind::MulEqL)),
    LShiftEqL => Literal("<<=" |state, text| make_lit(state, text, NodeKind::LShiftEqL)),
    RShiftEqL => Literal(">>=" |state, text| make_lit(state, text, NodeKind::RShiftEqL)),
    ULShiftEqL => Literal("<<<=" |state, text| make_lit(state, text, NodeKind::ULShiftEqL)),
    URShiftEqL => Literal(">>>=" |state, text| make_lit(state, text, NodeKind::URShiftEqL)),
    RevDivEqL => Literal("\\=" |state, text| make_lit(state, text, NodeKind::RevDivEqL)),
    AddEqL => Literal("+=" |state, text| make_lit(state, text, NodeKind::AddEqL)),
    SubEqL => Literal("-=" |state, text| make_lit(state, text, NodeKind::SubEqL)),
    BNotEqL => Literal("~=" |state, text| make_lit(state, text, NodeKind::BNotEqL)),
    ExpEqL => Literal("^=" |state, text| make_lit(state, text, NodeKind::ExpEqL)),

    TripleEqL => Literal("===" |state, text| make_lit(state, text, NodeKind::TripleEqL)),
    NotTripleEqL => Literal("!==" |state, text| make_lit(state, text, NodeKind::NotTripleEqL)),
    DoubleEqL => Literal("==" |state, text| make_lit(state, text, NodeKind::DoubleEqL)),
    NotEqL => Literal("!=" |state, text| make_lit(state, text, NodeKind::NotEqL)),
    MoreEqL => Literal(">=" |state, text| make_lit(state, text, NodeKind::MoreEqL)),
    LessEqL => Literal("<=" |state, text| make_lit(state, text, NodeKind::LessEqL)),
    MoreL => Literal(">" |state, text| make_lit(state, text, NodeKind::MoreL)),
    LessL => Literal("<" |state, text| make_lit(state, text, NodeKind::LessL)),
    SubTypeL => Literal("<:" |state, text| make_lit(state, text, NodeKind::SubTypeL)),
    SupTypeL => Literal(":>" |state, text| make_lit(state, text, NodeKind::SupTypeL)),

    ColonL => Literal(":" |state, text| make_lit(state, text, NodeKind::ColonL)),
    DColonL => Literal("::" |state, text| make_lit(state, text, NodeKind::DColonL)),
    QuestionL => Literal("?" |state, text| make_lit(state, text, NodeKind::QuestionL)),
    DollarL => Literal("$" |state, text| make_lit(state, text, NodeKind::DollarL)),
    AtL => Literal("@" |state, text| make_lit(state, text, NodeKind::AtL)),
    SplatL => Literal("..." |state, text| make_lit(state, text, NodeKind::SplatL)),
}
