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
    FuncDef(Vec<(VarType, String, TypeInfo)>, Box<Expr>),
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
struct Prog(Vec<STMT>);

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
        Expr::SumExpr(lhs, rhs) => {
            todo!()
        }
        Expr::VarExpr(varname) => {
            todo!()
        }
        Expr::FieldExpr(expr, fieldname) => {
            todo!()
        }
        Expr::FuncEvalExpr(funcname, fields) => {
            todo!()
        }
    }
}
enum EvalError {
    MismatchedType,
    InvalidField,
    AffineVarMoreThanOnce,
    RelevantVarUnused,
}
fn eval_prog(prog: Prog) -> Result<Value, (EvalError, String)> {
    todo!()
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
}
