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

lr_rust::parser! {
    State(ParseState),
    Output(usize),
    Kind(pub NodeKind),
    GeneratedFn(pub make_parser),

    _ => Regex("[ \t]*" |state: &mut ParseState, text: &str| {state.col += text.len(); None}),
    _ => Regex("#=([^=]|=[^#])*=#" |_, _| { None }),
    _ => Regex("#[^\\n]*" |state: &mut ParseState, text: &str| {state.col += text.len(); None}),

TranslationUnit => Rule(
    TopLevelDecl,
    TranslationUnit TopLevelDecl |state, tu, td| make_binary(state, tu, td, TranslationUnit)
),

TopLevelDecl => Rule(
    Decl,
    FunctionDefSpec CompoundStmt |state, fs, cs| make_binary(state, fs, cs, FunctionDefinition)
),

Decl => Rule(
    BaseType DecltrList SemicolonL |state, bt, dl, _| make_binary(state, bt, dl, Declaration),
    SemicolonL |state, _| make_nullary(state, EmptyDeclaration)
),

FunctionDefSpec => Rule(BaseType Decltr |state, bt, d| make_binary(state, bt, d, FunctionDefSpec)),

DecltrList => Rule(
    Decltr,
    DecltrList CommaL Decltr |state, dl, _, d| make_ternary(state, dl, d, DecltrList)
),

Decltr => Rule(
    DirectDecltr,
    MulL Decltr |state, _, d| make_unary(state, d, PointerDecltr)
),

DirectDecltr => Rule(
    FunctionDecltr,
    IdentifierL |state, id| make_unary(state, id, IdentifierDecltr),
    LParenL Decltr RParenL |state, _, d, _| make_unary(state, d, ParenDecltr),
    DirectDecltr LSquareL RSquareL |state, dd, _, _| make_unary(state, dd, ArrayDecltr),
    DirectDecltr LSquareL ConditionalExpr RSquareL |state, dd, _, ce, _| make_binary(state, dd, ce, ArrayDecltr)
),

FunctionDecltr => Rule(
    IdentifierL LParenL Params RParenL |state, id, _, p, _| make_binary(state, id, p, FunctionDecltr)
),

Params => Rule(
    ParamDecl,
    VoidL |state, _| make_nullary(state, VoidParams),
    Params CommaL ParamDecl |state, ps, _, pd| make_ternary(state, ps, pd, Params)
),

ParamDecl => Rule(BaseType Decltr |state, bt, d| make_binary(state, bt, d, ParamDecl)),

AbstractType => Rule(
    PtrType,
),

PtrType => Rule(
    BaseType,
    PtrType MulL |state, pt, _| make_unary(state, pt, PointerType)
),

Expr => Rule(PostfixExpr),
CommaExpr => Rule(
    AssignExpr,
    CommaExpr CommaL AssignExpr |state, ce, _, ae| make_ternary(state, ce, ae, CommaExpr)
),

AssignOp => Rule(EqL, AddEqL, SubEqL, MulEqL, DivEqL, RemEqL, LShiftEqL, RShiftEqL, AndEqL, XorEqL, OrEqL),
AssignExpr => Rule(
    ConditionalExpr,
    UnaryExpr AssignOp AssignExpr |state, ue, ao, ae| make_ternary(state, ue, ao, ae)
),

ConditionalExpr => Rule(
    LogicalOrExpr,
    LogicalOrExpr QuestionL CommaExpr ColonL ConditionalExpr |state, loe, _, ce, _, cond| make_ternary(state, loe, ce, cond, ConditionalExpr)
),

LogicalOrExpr => Rule(
    LogicalAndExpr,
    LogicalOrExpr DoubleOrL LogicalAndExpr |state, loe, op, lae| make_ternary(state, loe, op, lae)
),

LogicalAndExpr => Rule(
    BitwiseOrExpr,
    LogicalAndExpr DoubleAndL BitwiseOrExpr |state, lae, op, boe| make_ternary(state, lae, op, boe)
),

BitwiseOrExpr => Rule(
    BitwiseXorExpr,
    BitwiseOrExpr OrL BitwiseXorExpr |state, boe, op, bxe| make_ternary(state, boe, op, bxe)
),

BitwiseXorExpr => Rule(
    BitwiseAndExpr,
    BitwiseXorExpr XorL BitwiseAndExpr |state, bxe, op, bae| make_ternary(state, bxe, op, bae)
),

BitwiseAndExpr => Rule(
    EqualityExpr,
    BitwiseAndExpr AndL EqualityExpr |state, bae, op, ee| make_ternary(state, bae, op, ee)
),

EqualityOp => Rule(DoubleEqL, NotEqL),
EqualityExpr => Rule(
    RelationalExpr,
    EqualityExpr EqualityOp RelationalExpr |state, ee, op, re| make_ternary(state, ee, op, re)
),

RelationalOp => Rule(LessL, GreaterL, LessEqL, GreaterEqL),
RelationalExpr => Rule(
    ShiftExpr,
    RelationalExpr RelationalOp ShiftExpr |state, re, op, se| make_ternary(state, re, op, se)
),

ShiftOp => Rule(LShiftL, RShiftL),
ShiftExpr => Rule(
    AddExpr,
    ShiftExpr ShiftOp AddExpr |state, se, op, ae| make_ternary(state, se, op, ae)
),

AddOp => Rule(AddL, SubL),
AddExpr => Rule(
    MulExpr,
    AddExpr AddOp MulExpr |state, ae, op, me| make_ternary(state, ae, op, me)
),

MulOp => Rule(MulL, DivL, RemL),
MulExpr => Rule(
    CastExpr,
    MulExpr MulOp CastExpr |state, me, op, ce| make_ternary(state, me, op, ce)
),

CastExpr => Rule(
    UnaryExpr,
    LParenL AbstractType RParenL CastExpr |state, _, at, _, ce| make_binary(state, at, ce, CastExpr)
),

UnaryExpr => Rule(
    PostfixExpr,
    SubL CastExpr |state, _, ce| make_unary(state, ce, NegExpr),
    AddL CastExpr |state, _, ce| make_unary(state, ce, PosExpr),
    BangL CastExpr |state, _, ce| make_unary(state, ce, NotExpr),
    TildeL CastExpr |state, _, ce| make_unary(state, ce, BitwiseNotExpr),
    AndL CastExpr |state, _, ce| make_unary(state, ce, AddrOfExpr),
    MulL CastExpr |state, _, ce| make_unary(state, ce, DerefExpr),
    PPlusL UnaryExpr |state, _, ue| make_unary(state, ue, PreIncExpr),
    MMinusL UnaryExpr |state, _, ue| make_unary(state, ue, PreDecExpr)
),

PostfixExpr => Rule(
    PrimaryExpr,
    PostfixExpr LSquareL Expr RSquareL |state, pe, _, ce, _| make_binary(state, pe, ce, SubscriptExpr),
    PostfixExpr LParenL ExpressionList RParenL |state, pe, _, el, _| make_binary(state, pe, el, CallExpr),
    PostfixExpr LParenL RParenL |state, pe, _, _| make_unary(state, pe, CallExpr),
    PostfixExpr PPlusL |state, pe, _| make_unary(state, pe, PostIncExpr),
    PostfixExpr MMinusL |state, pe, _| make_unary(state, pe, PostDecExpr)
),

PrimaryExpr => Rule(
    IdentifierL, IntLitL, StrLitL,
    LParenL Expr RParenL |_, _, e, _| e
),

ExpressionList => Rule(
    Expr,
    ExpressionList CommaL Expr |state, el, _, ae| make_ternary(state, el, ae, ExpressionList)
),

CompoundStmt => Rule(
    LCurlL RCurlL |state, _, _| make_nullary(state, EmptyCompoundStmt),
    LCurlL DeclOrStmtList RCurlL |state, _, dsl, _| make_unary(state, dsl, CompoundStmt)
),

DeclOrStmtList => Rule(
    Decl,
    Stmt,
    DeclOrStmtList Decl |state, dsl, d| make_binary(state, dsl, d, DeclOrStmtList),
    DeclOrStmtList Stmt |state, dsl, s| make_binary(state, dsl, s, DeclOrStmtList)
),

ElseStmt, ElseIfStmt,
IfChain => Rule(
    ElseL IfL CommaExpr CompoundStmt |state, _, _, ce, cs| make_ternary(state, ce, cs, None, ElseIfStmt),
    ElseL CommaExpr CompoundStmt |state, _, _, ce, cs| make_ternary(state, ce, cs, None, ElseStmt),
    ElseL IfL CommaExpr CompoundStmt IfChain |state, _, _, ce, cs, ic| make_ternary(state, ce, cs, ic, ElseIfStmt),
    ElseL CommaExpr CompoundStmt IfChain |state, _, _, ce, cs, ic| make_ternary(state, ce, cs, ic, ElseStmt),
),

ForStmtElem => Rule(CommaExpr SemicolonL |_, ce, _| ce),
Stmt => Rule(
    CompoundStmt,
    IfChain,
    WhileL CommaExpr CompoundStmt |state, _, ce, s| make_binary(state, ce, s, WhileStmt),
    CommaExpr SemicolonL |state, ce, _| make_unary(state, ce, ExprStmt),
    IdentifierL ColonL Stmt |state, id, _, s| make_binary(state, id, s, LabelStmt),
    ForL ForStmtElem ForStmtElem ForStmtElem CompoundStmt |state, _, e1, e2, e3, s| make_ternary(state, e1, e2, e3, s, ForStmt),
    BreakL SemicolonL |state, _, _| make_nullary(state, BreakStmt),
    ContinueL SemicolonL |state, _, _| make_nullary(state, ContinueStmt),
    ReturnL CommaExpr SemicolonL |state, _, ce, _| make_unary(state, ce, ReturnStmt),
    ReturnL SemicolonL |state, _, _| make_nullary(state, ReturnVoidStmt),
    GotoL IdentifierL SemicolonL |state, _, id, _| make_unary(state, id, GotoStmt)
),

BaseType => Rule(U64L, U32L, U16L, I64L, I32L, I16L, F64L, F32L),

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

    BangL => Literal("!" |state, text| make_lit(state, text, NodeKind::BangL)),
    RemL => Literal("%" |state, text| make_lit(state, text, NodeKind::RemL)),
    XorL => Literal("^" |state, text| make_lit(state, text, NodeKind::XorL)),
    AndL => Literal("&" |state, text| make_lit(state, text, NodeKind::AndL)),
    MulL => Literal("*" |state, text| make_lit(state, text, NodeKind::MulL)),
    SubL => Literal("-" |state, text| make_lit(state, text, NodeKind::SubL)),
    AddL => Literal("+" |state, text| make_lit(state, text, NodeKind::AddL)),
    EqL => Literal("=" |state, text| make_lit(state, text, NodeKind::EqL)),
    TildeL => Literal("~" |state, text| make_lit(state, text, NodeKind::TildeL)),
    OrL => Literal("|" |state, text| make_lit(state, text, NodeKind::OrL)),
    DivL => Literal("/" |state, text| make_lit(state, text, NodeKind::DivL)),
    QuestionL => Literal("?" |state, text| make_lit(state, text, NodeKind::QuestionL)),
    LShiftL => Literal("<<" |state, text| make_lit(state, text, NodeKind::LShiftL)),
    RShiftL => Literal(">>" |state, text| make_lit(state, text, NodeKind::RShiftL)),

    AddEqL => Literal("+=" |state, text| make_lit(state, text, NodeKind::AddEqL)),
    SubEqL => Literal("-=" |state, text| make_lit(state, text, NodeKind::SubEqL)),
    MulEqL => Literal("*=" |state, text| make_lit(state, text, NodeKind::MulEqL)),
    DivEqL => Literal("/=" |state, text| make_lit(state, text, NodeKind::DivEqL)),
    RemEqL => Literal("%=" |state, text| make_lit(state, text, NodeKind::RemEqL)),
    LShiftEqL => Literal("<<=" |state, text| make_lit(state, text, NodeKind::LShiftEqL)),
    RShiftEqL => Literal(">>=" |state, text| make_lit(state, text, NodeKind::RShiftEqL)),
    AndEqL => Literal("&=" |state, text| make_lit(state, text, NodeKind::AndEqL)),
    XorEqL => Literal("^=" |state, text| make_lit(state, text, NodeKind::XorEqL)),
    OrEqL => Literal("|=" |state, text| make_lit(state, text, NodeKind::OrEqL)),
    PPlusL => Literal("++" |state, text| make_lit(state, text, NodeKind::PPlusL)),
    MMinusL => Literal("--" |state, text| make_lit(state, text, NodeKind::MMinusL)),

    LessL => Literal("<" |state, text| make_lit(state, text, NodeKind::LessL)),
    GreaterL => Literal(">" |state, text| make_lit(state, text, NodeKind::GreaterL)),
    LessEqL => Literal("<=" |state, text| make_lit(state, text, NodeKind::LessEqL)),
    GreaterEqL => Literal(">=" |state, text| make_lit(state, text, NodeKind::GreaterEqL)),
    DoubleEqL => Literal("==" |state, text| make_lit(state, text, NodeKind::DoubleEqL)),
    NotEqL => Literal("!=" |state, text| make_lit(state, text, NodeKind::NotEqL)),
    DoubleAndL => Literal("&&" |state, text| make_lit(state, text, NodeKind::DoubleAndL)),
    DoubleOrL => Literal("||" |state, text| make_lit(state, text, NodeKind::DoubleOrL)),

    NewLineL,
    LParenL => Literal("(" |state, text| make_lit(state, text, NodeKind::LParenL)),
    RParenL => Literal(")" |state, text| make_lit(state, text, NodeKind::RParenL)),
    LSquareL => Literal("[" |state, text| make_lit(state, text, NodeKind::LSquareL)),
    RSquareL => Literal("]" |state, text| make_lit(state, text, NodeKind::RSquareL)),
    LCurlL => Literal("{" |state, text| make_lit(state, text, NodeKind::LCurlL)),
    RCurlL => Literal("}" |state, text| make_lit(state, text, NodeKind::RCurlL)),

    DotL => Literal("." |state, text| make_lit(state, text, NodeKind::DotL)),
    RArrowL => Literal("->" |state, text| make_lit(state, text, NodeKind::RArrowL)),
    CommaL => Literal("," |state, text| make_lit(state, text, NodeKind::CommaL)),
    SemicolonL => Literal(";" |state, text| make_lit(state, text, NodeKind::SemicolonL)),
    ColonL => Literal(":" |state, text| make_lit(state, text, NodeKind::ColonL)),

    ForL => Literal("for" |state, text| make_lit(state, text, NodeKind::ForL)),
    ReturnL => Literal("return" |state, text| make_lit(state, text, NodeKind::ReturnL)),
    BreakL => Literal("break" |state, text| make_lit(state, text, NodeKind::BreakL)),
    ShortL => Literal("short" |state, text| make_lit(state, text, NodeKind::ShortL)),
    ElseL => Literal("else" |state, text| make_lit(state, text, NodeKind::ElseL)),
    GotoL => Literal("goto" |state, text| make_lit(state, text, NodeKind::GotoL)),
    IfL => Literal("if" |state, text| make_lit(state, text, NodeKind::IfL)),
    VoidL => Literal("void" |state, text| make_lit(state, text, NodeKind::VoidL)),
    ContinueL => Literal("continue" |state, text| make_lit(state, text, NodeKind::ContinueL)),
    WhileL => Literal("while" |state, text| make_lit(state, text, NodeKind::WhileL)),
    SizeofL => Literal("sizeof" |state, text| make_lit(state, text, NodeKind::SizeofL)),
    CaseL => Literal("case" |state, text| make_lit(state, text, NodeKind::CaseL)),
    DefaultL => Literal("default" |state, text| make_lit(state, text, NodeKind::DefaultL)),
    SwitchL => Literal("switch" |state, text| make_lit(state, text, NodeKind::SwitchL)),

    MutL => Literal("mut" |state, text| make_lit(state, text, NodeKind::MutL)),
    ImmutL => Literal("immut" |state, text| make_lit(state, text, NodeKind::ImmutL)),
    ConstL => Literal("const" |state, text| make_lit(state, text, NodeKind::ConstL)),
    StructL => Literal("struct" |state, text| make_lit(state, text, NodeKind::StructL)),
    UnionL => Literal("union" |state, text| make_lit(state, text, NodeKind::UnionL)),

    U64L => Literal("u64" |state, text| make_lit(state, text, NodeKind::U64L)),
    U32L => Literal("u32" |state, text| make_lit(state, text, NodeKind::U32L)),
    U16L => Literal("u16" |state, text| make_lit(state, text, NodeKind::U16L)),
    I64L => Literal("i64" |state, text| make_lit(state, text, NodeKind::I64L)),
    I32L => Literal("i32" |state, text| make_lit(state, text, NodeKind::I32L)),
    I16L => Literal("i16" |state, text| make_lit(state, text, NodeKind::I16L)),
    F64L => Literal("f64" |state, text| make_lit(state, text, NodeKind::F64L)),
    F32L => Literal("f32" |state, text| make_lit(state, text, NodeKind::F32L)),
}
