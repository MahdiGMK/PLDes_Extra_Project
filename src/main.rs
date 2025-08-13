macro_rules! b {
    ($x:expr) => {
        Box::new($x)
    };
}
#[derive(Clone, Debug, PartialEq)]
enum Value {
    IntVal(i32),
    BoolVal(bool),
    StrVal(String),
    RecVal(Vec<(String, Value)>),
    ListVal(Vec<Value>),
}
#[derive(Clone, Debug, PartialEq)]
enum Expr {
    VarExpr(String),
    ConstExpr(Value),
    FuncEvalExpr(String, Vec<Expr>),
    SumExpr(Box<Expr>, Box<Expr>),
    NotExpr(Box<Expr>),
    IfElseExpr(Box<Expr>, Box<Expr>, Box<Expr>),
    FieldExpr(Box<Expr>, String),
    // IndexExpr(Box<Expr>, Box<Expr>),
}
#[derive(Clone, Debug, PartialEq)]
enum STMT {
    VarDef(VarType, String, Box<Expr>),
    FuncDef(String, Vec<(VarType, String, TypeInfo)>, Box<Expr>),
    RecDef(String, Vec<(String, TypeInfo)>),
    Expr(Box<Expr>),
}
#[derive(Clone, Copy, Debug, PartialEq)]
enum VarType {
    Affine,
    Relevant,
}
#[derive(Clone, Debug, PartialEq)]
enum TypeInfo {
    Int,
    String,
    Boolean,
    Record(Vec<(String, TypeInfo)>),
    List(Box<TypeInfo>),
}
#[derive(Clone, Debug, PartialEq)]
struct Program(Vec<STMT>);

use std::collections::HashMap;

use TypeInfo::Boolean as BoolT;
use TypeInfo::Int as IntT;
use TypeInfo::List as ListT;
use TypeInfo::Record as RecordT;
use TypeInfo::String as StrT;

fn main() {}
struct Context {
    // funcs: HashMap<String>,
}
impl Context {}

fn eval_expr(expr: &Expr, ctx: &mut Context) -> Result<Value, (EvalError, String)> {
    match expr {
        Expr::ConstExpr(val) => Ok(val.clone()),
        Expr::IfElseExpr(cond_expr, true_expr, false_expr) => {
            let cond_expr_val = eval_expr(cond_expr.as_ref(), ctx)?;
            if let Value::BoolVal(cond) = cond_expr_val {
                if cond {
                    eval_expr(true_expr.as_ref(), ctx)
                } else {
                    eval_expr(false_expr.as_ref(), ctx)
                }
            } else {
                Err((
                    EvalError::MismatchedType,
                    format!(
                        "Expected if condition to be bool but was {:?}",
                        cond_expr_val
                    ),
                ))
            }
        }
        Expr::NotExpr(expr) => {
            todo!()
        }
        Expr::SumExpr(lhs_expr, rhs_expr) => {
            let lhs_val = eval_expr(lhs_expr.as_ref(), ctx)?;
            let rhs_val = eval_expr(rhs_expr.as_ref(), ctx)?;
            match lhs_val {
                Value::IntVal(lhs) => {
                    if let Value::IntVal(rhs) = rhs_val {
                        return Ok(Value::IntVal(rhs + lhs));
                    }
                }
                Value::BoolVal(lhs) => {
                    if let Value::BoolVal(rhs) = rhs_val {
                        return Ok(Value::BoolVal(lhs | rhs));
                    }
                }
                Value::StrVal(lhs) => {
                    if let Value::StrVal(rhs) = rhs_val {
                        return Ok(Value::StrVal(lhs + rhs.as_str()));
                    }
                }
                Value::ListVal(mut lhs) => {
                    if let Value::ListVal(rhs) = rhs_val {
                        lhs.extend(rhs);
                        return Ok(Value::ListVal(lhs));
                    }
                }
                Value::RecVal(mut lhs) => {
                    if let Value::RecVal(rhs) = rhs_val {
                        // asume sorted names
                        todo!();
                        return Ok(Value::RecVal(lhs));
                    }
                }
            }
            return Err((
                EvalError::MismatchedType,
                format!("Lhs and Rhs type missmatched"),
            ));
        }
        Expr::VarExpr(varname) => {
            todo!()
        }
        Expr::FieldExpr(expr, fieldname) => {
            let expr_val = eval_expr(expr.as_ref(), ctx)?;
            if let Value::RecVal(val) = expr_val {
                for (fname, fval) in val {
                    if fname.eq(fieldname) {
                        return Ok(fval);
                    }
                }
                Err((
                    EvalError::MismatchedType,
                    format!("Expected record type with field {}", fieldname),
                ))
            } else {
                Err((
                    EvalError::MismatchedType,
                    format!(
                        "Expected record type with field {}, found : {:?}",
                        fieldname, expr_val
                    ),
                ))
            }
        }
        Expr::FuncEvalExpr(funcname, fields) => {
            todo!()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum EvalError {
    MismatchedType,
    InvalidField,
    AffineVarMoreThanOnce,
    RelevantVarUnused,
}
fn eval_prog(prog: Program) -> Result<Value, (EvalError, String)> {
    todo!()
}
#[derive(Clone, Debug, PartialEq)]
enum STMTParserState {
    Idle,
    Variable(VarType),
    VariableName(VarType, String),
    VariableNameEq(VarType, String),
    Fn,
    FnName(String),
    Rec,
    RecName(String),
}
fn capture_next_tok(code: &str) -> (&str, &str) {
    let mut bg = None;
    for (i, ch) in code.char_indices() {
        if ch.is_whitespace() {
            if let Some(x) = bg {
                return (code.get(x..i).unwrap(), code.get(i..).unwrap());
            }
            continue;
        }
        if ch == '{'
            || ch == '}'
            || ch == '['
            || ch == ']'
            || ch == '('
            || ch == ')'
            || ch == ';'
            || ch == ','
            || ch == '.'
            || ch == '+'
            || ch == '='
        {
            return if let Some(x) = bg {
                (code.get(x..i).unwrap(), code.get(i..).unwrap())
            } else {
                (code.get(i..i + 1).unwrap(), code.get(i + 1..).unwrap())
            };
        };
        if bg == None {
            bg = Some(i);
        }
    }
    return if let Some(x) = bg {
        (code.get(x..).unwrap(), "")
    } else {
        ("", "")
    };
}

#[derive(Clone, Debug, PartialEq)]
enum SyntaxError {
    UnexpectedToken,
}
fn expect_token<'a>(code: &'a str, tok: &str) -> Result<&'a str, (SyntaxError, String)> {
    let (tk, cc) = capture_next_tok(code);
    if tk.eq(tok) {
        Ok(cc)
    } else {
        Err((
            SyntaxError::UnexpectedToken,
            format!("expected {tok}, found {tk}"),
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
enum TypeName {
    Int,
    Bool,
    List(Box<TypeName>),
    Record(String),
}
fn capture_type(mut code: &str) -> Result<(TypeName, &str), (SyntaxError, String)> {
    let (tname, mut cc) = capture_next_tok(code);
    match tname {
        "int" => Ok((TypeName::Int, cc)),
        "bool" => Ok((TypeName::Bool, cc)),
        "List" => {
            cc = expect_token(cc, "(")?;
            let (subt, mut cc) = capture_type(cc)?;
            cc = expect_token(cc, ")")?;
            Ok((TypeName::List(b!(subt)), cc))
        }
        _ => Ok((TypeName::Record(tname.into()), cc)),
    }
}
fn capture_expr(mut code: &str) -> Result<(Expr, &str), (SyntaxError, String)> {
    todo!()
}
fn parse_source_code(mut code: &str) -> Result<Program, (SyntaxError, String)> {
    let mut stmts = Vec::<STMT>::new();
    let mut idx: usize = 0;
    let mut state = STMTParserState::Idle;

    while !code.is_empty() {
        let (tok, cc) = capture_next_tok(code);
        code = cc;
        match tok {
            "fn" => {
                let (fnname, mut cc) = capture_next_tok(code);
                cc = expect_token(cc, "(")?;

                let mut args = Vec::<(VarType, String, TypeName)>::new();

                loop {
                    let (mut vart_or_end, mut cc) = capture_next_tok(cc);
                    let vart = match vart_or_end {
                        ")" => break,
                        "," => continue,
                        "affine" => VarType::Affine,
                        "relative" => VarType::Relevant,
                        _ => {
                            return Err((
                                SyntaxError::UnexpectedToken,
                                format!(
                                    "unexpected token {vart_or_end}, should have been one of : \"affine\"/\"relative\"/\")\""
                                ),
                            ));
                        }
                    };

                    let (mut varname, mut cc) = capture_next_tok(cc);
                    cc = expect_token(cc, ":")?;
                    let (vartype, cc) = capture_type(cc)?;

                    args.push((vart, varname.into(), vartype));
                    code = cc;
                }

                cc = expect_token(code, "=")?;

                let (expr, mut cc) = capture_expr(cc)?;

                todo!("combine fnname, args and expr into STMT::function");
            }
            "record" => {
                let (recname, mut cc) = capture_next_tok(code);
                let mut args = Vec::<(String, TypeName)>::new();

                cc = expect_token(cc, "{")?;
                loop {
                    let (mut varname_or_end, mut cc) = capture_next_tok(cc);
                    let vart = match varname_or_end {
                        "}" => break,
                        "," => continue,
                        _ => {
                            cc = expect_token(cc, ":")?;
                            let (vartype, cc) = capture_type(cc)?;
                            args.push((varname_or_end.into(), vartype));
                        }
                    };
                }
                todo!("combine recname and args into STMT::record");
            }
            "relevant" => {
                let (varname, mut cc) = capture_next_tok(code);
                cc = expect_token(cc, "=")?;
                let (expr, mut cc) = capture_expr(cc)?;
                todo!("combine vartype, varname and expr into STMT::var");
            }
            "affine" => {
                let (varname, mut cc) = capture_next_tok(code);
                cc = expect_token(cc, "=")?;
                let (expr, mut cc) = capture_expr(cc)?;
                todo!("combine vartype, varname and expr into STMT::var");
            }
            _ => {
                println!("ERR!");
            }
        }
    }
    // while idx < src.len() {
    //     if src.get(idx)
    // }

    Ok(Program(stmts))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_system() {
        let lof_lof_int = ListT(b!(ListT(b!(IntT))));
        let lof_pii = ListT(b!(RecordT(vec![("x".into(), IntT), ("y".into(), IntT),])));
        println!("{:?}", lof_lof_int);
        println!("{:?}", lof_pii);
    }

    #[test]
    fn source_code_parsing_test() {
        println!(
            "{:?}",
            parse_source_code(
                "
relevant x = 4;
relevant lst = [1 , 2 , 3];
affine a = lst + [x];
fn list_append(relevant list : List(int) , affine item : int) = list + [item];
affine test_list_append = list_append(a, 5);
record Pair { x : int, y : int};
fn get_x(affine pair : Pair) = pair.x;
fn get_y(affine pair : Pair) = pair.y;
fn get_sum(relevant pair : Pair) = get_x(pair) + get_y(pair);
"
                .into()
            )
        );
    }
}
