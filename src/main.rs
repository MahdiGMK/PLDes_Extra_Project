#![allow(warnings)]

use serde_derive::{Deserialize, Serialize};
macro_rules! b {
    ($x:expr) => {
        Box::new($x)
    };
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum Value {
    IntVal(i32),
    BoolVal(bool),
    StrVal(String),
    RecVal(Vec<(String, Value)>),
    ListVal(Vec<Value>),
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum Expr {
    Empty,
    VarExpr(String),
    ConstExpr(Value),
    ListExpr(Vec<Expr>),
    RecExpr(Vec<(String, Expr)>),
    FuncEvalExpr(String, Vec<Expr>),
    SumExpr(Box<Expr>, Box<Expr>),
    NotExpr(Box<Expr>),
    IfElseExpr(Box<Expr>, Box<Expr>, Box<Expr>),
    FieldExpr(Box<Expr>, String),
    // IndexExpr(Box<Expr>, Box<Expr>),
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum STMT {
    VarDef(VarType, String, Box<Expr>),
    FuncDef(String, Vec<(VarType, String, TypeInfo)>, Box<Expr>),
    RecDef(String, Vec<(String, TypeInfo)>),
    Expr(Box<Expr>),
}
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
enum VarType {
    Affine,
    Relevant,
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum TypeInfo {
    Int,
    String,
    Boolean,
    Record(Vec<(String, TypeInfo)>),
    RecordName(String),
    List(Box<TypeInfo>),
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct Program(Vec<STMT>);

use std::cmp::{max, min};
use std::collections::HashMap;
use std::future::Pending;
use std::hash::Hash;
use std::io::Error;
use std::net::TcpListener;
use std::ops::Add;
use std::ops::DerefMut;
use std::ops::Index;
use std::string;
use std::sync::mpsc::SyncSender;

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

struct TypeContext {
    infunction: bool,
    vars: HashMap<String, (VarType, TypeInfo)>,
    local_vars: HashMap<String, (VarType, TypeInfo)>,
    funcs: HashMap<String, (Vec<(VarType, String, TypeInfo)>, TypeInfo)>,
    records: HashMap<String, Vec<(String, TypeInfo)>>,

    var_visit: Vec<HashMap<String, (u32, u32)>>,
}
impl TypeContext {
    pub fn begin_branch(&mut self) {
        self.var_visit.push(HashMap::new());
    }
    pub fn else_branch(&mut self) {
        self.var_visit.push(HashMap::new());
    }
    pub fn end_branch(&mut self) {
        let else_visits = self.var_visit.pop().unwrap();
        let then_visits = self.var_visit.pop().unwrap();
        let par_visits = self.var_visit.last_mut().unwrap();
        let vars = if self.infunction {
            &self.local_vars
        } else {
            &self.vars
        };

        for (vname, _) in vars {
            let elsecnt = else_visits.get(vname).unwrap_or(&(0, 0));
            let thencnt = then_visits.get(vname).unwrap_or(&(0, 0));
            if let Some((min, max)) = par_visits.get_mut(vname) {
            } else {
                par_visits.insert(
                    vname.clone(),
                    (min(elsecnt.0, thencnt.0), max(elsecnt.1, thencnt.1)),
                );
            }
        }
    }

    fn resolve_var(&mut self, varname: &str) -> Result<TypeInfo, (EvalError, String)> {
        let vars = if self.infunction {
            &self.local_vars
        } else {
            &self.vars
        };
        let current_level_visits = self.var_visit.last_mut().unwrap();
        if let Some((min, max)) = current_level_visits.get_mut(varname) {
            *min = *min + 1u32;
            *max = *max + 1u32;
        } else {
            current_level_visits.insert(varname.into(), (1, 1));
        }
        Ok(IntT)
    }

    fn resolve_typename(&self, typename: &str) -> Result<TypeInfo, (EvalError, String)> {
        Ok(TypeInfo::Record(
            self.records
                .get(typename)
                .ok_or((
                    EvalError::InvalidFuncEval,
                    format!("There is no record called {typename}"),
                ))
                .cloned()?,
        ))
    }

    fn resolve_func(
        &self,
        funcname: &str,
    ) -> Result<(Vec<(VarType, String, TypeInfo)>, TypeInfo), (EvalError, String)> {
        self.funcs
            .get(funcname)
            .ok_or((
                EvalError::InvalidFuncEval,
                format!("There is no function called {funcname}"),
            ))
            .cloned()
    }

    fn new() -> Self {
        Self {
            infunction: false,
            funcs: HashMap::new(),
            local_vars: HashMap::new(),
            vars: HashMap::new(),
            records: HashMap::new(),
            var_visit: vec![HashMap::new()],
        }
    }

    fn define_var(
        &mut self,
        var_type: VarType,
        name: String,
        Tinfo: TypeInfo,
    ) -> Result<(), (EvalError, String)> {
        if self.vars.contains_key(&name) {
            return Err((
                EvalError::NameDuplication,
                format!("variable with name {name} already exists"),
            ));
        }
        self.vars.insert(name, (var_type, Tinfo));
        Ok(())
    }

    fn define_func(
        &mut self,
        name: String,
        params: Vec<(VarType, String, TypeInfo)>,
        expr: &Expr,
    ) -> Result<(), (EvalError, String)> {
        if self.funcs.contains_key(&name) {
            return Err((
                EvalError::NameDuplication,
                format!("function with name {name} already exists"),
            ));
        }
        self.infunction = true;
        self.local_vars.clear();
        for (vart, varname, Typ) in &params {
            if self.local_vars.contains_key(varname) {
                return Err((
                    EvalError::NameDuplication,
                    format!("localvar with name {name} already exists"),
                ));
            }
            self.local_vars
                .insert(varname.clone(), (vart.clone(), Typ.clone()));
        }
        let Ret = eval_expr_type(expr, self)?;
        self.infunction = false;

        self.funcs.insert(name, (params, Ret));
        Ok(())
    }

    fn define_rec(
        &mut self,
        name: String,
        fields: Vec<(String, TypeInfo)>,
    ) -> Result<(), (EvalError, String)> {
        if self.records.contains_key(&name) {
            return Err((
                EvalError::NameDuplication,
                format!("record with name {name} already exists"),
            ));
        }
        self.records.insert(name, fields);
        Ok(())
    }
}

// fn eval_expr(expr: &Expr, ctx: &mut Context) -> Result<Value, (EvalError, String)> {
//     match expr {
//         Expr::Empty => Err((EvalError::NoneExpr, "Found \"None\" expression!".into())),
//         Expr::ConstExpr(val) => Ok(val.clone()),
//         Expr::IfElseExpr(cond_expr, true_expr, false_expr) => {
//             let cond_expr_val = eval_expr(cond_expr.as_ref(), ctx)?;
//             if let Value::BoolVal(cond) = cond_expr_val {
//                 if cond {
//                     eval_expr(true_expr.as_ref(), ctx)
//                 } else {
//                     eval_expr(false_expr.as_ref(), ctx)
//                 }
//             } else {
//                 Err((
//                     EvalError::MismatchedType,
//                     format!(
//                         "Expected if condition to be bool but was {:?}",
//                         cond_expr_val
//                     ),
//                 ))
//             }
//         }
//         Expr::NotExpr(expr) => {
//             todo!()
//         }
//         Expr::SumExpr(lhs_expr, rhs_expr) => {
//             let lhs_val = eval_expr(lhs_expr.as_ref(), ctx)?;
//             let rhs_val = eval_expr(rhs_expr.as_ref(), ctx)?;
//             match lhs_val {
//                 Value::IntVal(lhs) => {
//                     if let Value::IntVal(rhs) = rhs_val {
//                         return Ok(Value::IntVal(rhs + lhs));
//                     }
//                 }
//                 Value::BoolVal(lhs) => {
//                     if let Value::BoolVal(rhs) = rhs_val {
//                         return Ok(Value::BoolVal(lhs | rhs));
//                     }
//                 }
//                 Value::StrVal(lhs) => {
//                     if let Value::StrVal(rhs) = rhs_val {
//                         return Ok(Value::StrVal(lhs + rhs.as_str()));
//                     }
//                 }
//                 Value::ListVal(mut lhs) => {
//                     if let Value::ListVal(rhs) = rhs_val {
//                         lhs.extend(rhs);
//                         return Ok(Value::ListVal(lhs));
//                     }
//                 }
//                 Value::RecVal(mut lhs) => {
//                     if let Value::RecVal(rhs) = rhs_val {
//                         // asume sorted names
//                         todo!();
//                         return Ok(Value::RecVal(lhs));
//                     }
//                 }
//             }
//             return Err((
//                 EvalError::MismatchedType,
//                 format!("Lhs and Rhs type missmatched"),
//             ));
//         }
//         Expr::VarExpr(varname) => {
//             todo!()
//         }
//         Expr::FieldExpr(expr, fieldname) => {
//             let expr_val = eval_expr(expr.as_ref(), ctx)?;
//             if let Value::RecVal(val) = expr_val {
//                 for (fname, fval) in val {
//                     if fname.eq(fieldname) {
//                         return Ok(fval);
//                     }
//                 }
//                 Err((
//                     EvalError::MismatchedType,
//                     format!("Expected record type with field {}", fieldname),
//                 ))
//             } else {
//                 Err((
//                     EvalError::MismatchedType,
//                     format!(
//                         "Expected record type with field {}, found : {:?}",
//                         fieldname, expr_val
//                     ),
//                 ))
//             }
//         }
//         Expr::FuncEvalExpr(funcname, fields) => {
//             todo!()
//         }
//         Expr::ListExpr(exprs) => todo!(),
//         Expr::RecExpr(items) => todo!(),
//     }
// }
fn eval_value_type(value: &Value, ctx: &mut TypeContext) -> Result<TypeInfo, (EvalError, String)> {
    match value {
        Value::BoolVal(_) => Ok(BoolT),
        Value::IntVal(_) => Ok(IntT),
        Value::StrVal(_) => Ok(StrT),
        Value::ListVal(x) => {
            if let Some(v0) = x.first() {
                let T0 = eval_value_type(v0, ctx)?;
                for vi in x {
                    let Ti = eval_value_type(vi, ctx)?;
                    if T0 != Ti {
                        return Err((
                            EvalError::MismatchedType,
                            format!("Inconsistant list typed value : {:?} != {:?}", T0, Ti),
                        ));
                    }
                }
                Ok(ListT(b!(T0)))
            } else {
                Err((EvalError::InvalidField, format!("Unknown List elem type")))
            }
        }
        Value::RecVal(x) => {
            let mut res = vec![];
            for (k, v) in x {
                res.push((k.clone(), eval_value_type(v, ctx)?));
            }
            Ok(RecordT(res))
        }
    }
}
fn eval_expr_type(expr: &Expr, ctx: &mut TypeContext) -> Result<TypeInfo, (EvalError, String)> {
    match expr {
        Expr::Empty => Err((EvalError::NoneExpr, "Found \"None\" expression!".into())),
        Expr::ConstExpr(val) => Ok(eval_value_type(val, ctx)?),
        Expr::IfElseExpr(cond_expr, true_expr, false_expr) => {
            let Cond = eval_expr_type(cond_expr, ctx)?;
            if Cond != BoolT {
                return Err((
                    EvalError::MismatchedType,
                    format!("Expected bool in if condition, found {:?}", Cond),
                ));
            }
            ctx.begin_branch();
            let True = eval_expr_type(true_expr, ctx)?;
            ctx.else_branch();
            let False = eval_expr_type(false_expr, ctx)?;
            ctx.end_branch();

            if True != False {
                return Err((
                    EvalError::MismatchedType,
                    format!(
                        "Expected if to have matched types on the then and else sides, found {:?} != {:?}",
                        True, False
                    ),
                ));
            }
            Ok(True)
        }
        Expr::NotExpr(expr) => {
            let T = eval_expr_type(expr, ctx)?;
            if T == BoolT {
                Ok(BoolT)
            } else {
                Err((
                    EvalError::MismatchedType,
                    format!("Expected not operator to take bool not {:?}", T),
                ))
            }
        }
        Expr::SumExpr(lhs_expr, rhs_expr) => {
            let Lhs = eval_expr_type(lhs_expr, ctx)?;
            let Rhs = eval_expr_type(rhs_expr, ctx)?;
            if Lhs != Rhs {
                Err((
                    EvalError::MismatchedType,
                    format!("Lhs and Rhs type missmatched, {:?} != {:?}", Lhs, Rhs),
                ))
            } else {
                Ok(Lhs)
            }
        }
        Expr::VarExpr(varname) => ctx.resolve_var(varname),
        Expr::FieldExpr(expr, fieldname) => {
            let Expr = eval_expr_type(expr, ctx)?;
            let Resolved = if let TypeInfo::RecordName(typename) = Expr.clone() {
                ctx.resolve_typename(&typename)?
            } else {
                Expr.clone()
            };

            if let RecordT(table) = Resolved {
                for (name, tinfo) in table {
                    if name.eq(fieldname) {
                        return Ok(tinfo);
                    }
                }
            }
            return Err((
                EvalError::InvalidField,
                format!("Type {:?} doesnt have a field called {}", Expr, fieldname),
            ));
        }
        Expr::FuncEvalExpr(funcname, fields) => {
            let (params, Ret) = ctx.resolve_func(funcname)?;
            if params.len() != fields.len() {
                return Err((
                    EvalError::InvalidFuncEval,
                    format!(
                        "argument count missmatch : {} required, {} was given",
                        params.len(),
                        fields.len()
                    ),
                ));
            }
            for (fexpr, (_, _, Param)) in fields.iter().zip(params) {
                let Fexpr = eval_expr_type(fexpr, ctx)?;
                if Fexpr != Param {
                    return Err((
                        EvalError::MismatchedType,
                        format!(
                            "argument type missmatch : {:?} required, {:?} was given",
                            Param, Fexpr
                        ),
                    ));
                }
            }
            Ok(Ret)
        }
        Expr::ListExpr(exprs) => {
            if let Some(e0) = exprs.first() {
                let T0 = eval_expr_type(e0, ctx)?;
                for ei in exprs {
                    let Ti = eval_expr_type(ei, ctx)?;
                    if Ti != T0 {
                        return Err((
                            EvalError::MismatchedType,
                            format!("Inconsistant list typed value : {:?} != {:?}", T0, Ti),
                        ));
                    }
                }
                Ok(ListT(b!(T0)))
            } else {
                Err((EvalError::InvalidField, format!("Unknown List elem type")))
            }
        }
        Expr::RecExpr(items) => {
            let mut vc = vec![];
            for (name, expr) in items {
                let Expr = eval_expr_type(expr, ctx)?;
                vc.push((name.clone(), Expr));
            }
            Ok(RecordT(vc))
        }
    }
}
fn eval_prog_type(prog: &Program) -> Result<TypeInfo, (EvalError, String)> {
    let mut ctx = TypeContext::new();
    let mut optRes = Option::<TypeInfo>::None;
    for stmt in &prog.0 {
        match stmt {
            STMT::Expr(expr) => {
                if let Some(_) = optRes {
                    return Err((
                        EvalError::MultipleEntryPoints,
                        "Expected a single entry expression, found multiple".into(),
                    ));
                }
                optRes = Some(eval_expr_type(expr.as_ref(), &mut ctx)?);
            }
            STMT::VarDef(var_type, name, expr) => {
                let T = eval_expr_type(expr.as_ref(), &mut ctx)?;
                ctx.define_var(var_type.clone(), name.clone(), T)?;
            }
            STMT::FuncDef(name, params, expr) => {
                ctx.define_func(name.clone(), params.clone(), expr.as_ref())?;
            }
            STMT::RecDef(name, fields) => {
                ctx.define_rec(name.clone(), fields.clone())?;
            }
        }
    }

    optRes.ok_or((
        EvalError::NoEntryPoint,
        "Expected a single entry expression, found none".into(),
    ))
}

#[derive(Clone, Debug, PartialEq)]
enum EvalError {
    MismatchedType,
    InvalidField,
    IncompleteType,
    InvalidFuncEval,
    NameDuplication,
    AffineVarMoreThanOnce,
    RelevantVarUnused,
    NoEntryPoint,
    MultipleEntryPoints,
    NoneExpr,
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
        match ch {
            '{' | '}' | '[' | ']' | '(' | ')' | ';' | ',' | '.' | '+' | '=' => {
                return if let Some(x) = bg {
                    (code.get(x..i).unwrap(), code.get(i..).unwrap())
                } else {
                    (code.get(i..i + 1).unwrap(), code.get(i + 1..).unwrap())
                };
            }
            _ => {}
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum TypeName {
    Int,
    Bool,
    List(Box<TypeName>),
    Record(String),
}

// fn capture_type(mut code: &str) -> Result<(TypeName, &str), (SyntaxError, String)> {
//     let (tname, mut cc) = capture_next_tok(code);
//     match tname {
//         "int" => Ok((TypeName::Int, cc)),
//         "bool" => Ok((TypeName::Bool, cc)),
//         "List" => {
//             cc = expect_token(cc, "(")?;
//             let (subt, mut cc) = capture_type(cc)?;
//             cc = expect_token(cc, ")")?;
//             Ok((TypeName::List(b!(subt)), cc))
//         }
//         _ => Ok((TypeName::Record(tname.into()), cc)),
//     }
// }
// fn capture_singular_expr(mut code: &str) -> Result<(Expr, &str), (SyntaxError, String)> {
//     let (tname, mut cc) = capture_next_tok(code);
//     if let Ok(intval) = tname.parse::<i32>() {
//         return Ok((Expr::ConstExpr(Value::IntVal(intval)), cc));
//     }
//     match tname {
//         "true" => return Ok((Expr::ConstExpr(Value::BoolVal(true)), cc)),
//         "false" => return Ok((Expr::ConstExpr(Value::BoolVal(false)), cc)),
//         "(" => return capture_expr(cc, ")"),
//         "[" => {
//             capture_expr(cc, ",");
//         }
//         "\"" => {
//             todo!()
//         }
//         _ => {

//             // return Err((
//             //     SyntaxError::UnexpectedToken,
//             //     format!("Expression expected, found {tname}"),
//             // ));
//         }
//     };
// }
// fn capture_expr<'a>(
//     mut code: &'a str,
//     ending_tok: &str,
// ) -> Result<(Expr, &'a str), (SyntaxError, String)> {
//     let (tname, mut cc) = capture_next_tok(code);
//     // let mut current_captured = Expr::Empty;
//     if let Ok(intval) = tname.parse::<i32>() {
//         let intexp = Expr::ConstExpr(Value::IntVal(intval));

//         let (tname, mut cc) = capture_next_tok(cc);
//         match tname {
//             "+" => {
//                 let (following_expr, cc) = capture_expr(cc, ending_tok)?;
//                 return Ok((Expr::SumExpr(b!(intexp), b!(following_expr)), cc));
//             }
//             ending_tok => return Ok((intexp, cc)),
//         }
//     }
//     // match tname {
//     //     "!" => {}
//     //     "(" => {
//     //         let (internal_expr, cc) = capture_expr(cc, ")")?;
//     //     }
//     //     "\"" => {}
//     //     "[" => {}
//     //     "true" => {}
//     //     "false" => {}
//     //     "if" => {}
//     //     // final_tok => return;
//     //     _ => {}
//     // }

//     println!("first nxt tok : {tname} -- \n***\n{cc}\n***\n");
//     return Err((SyntaxError::UnexpectedToken, "random error".into()));
// }
// fn parse_source_code(mut code: &str) -> Result<Program, (SyntaxError, String)> {
//     let mut stmts = Vec::<STMT>::new();
//     let mut idx: usize = 0;
//     let mut state = STMTParserState::Idle;

//     while !code.is_empty() {
//         let (tok, cc) = capture_next_tok(code);
//         code = cc;
//         match tok {
//             "fn" => {
//                 let (fnname, mut cc) = capture_next_tok(code);
//                 cc = expect_token(cc, "(")?;

//                 let mut args = Vec::<(VarType, String, TypeName)>::new();

//                 loop {
//                     let (mut vart_or_end, mut cc) = capture_next_tok(cc);
//                     let vart = match vart_or_end {
//                         ")" => break,
//                         "," => continue,
//                         "affine" => VarType::Affine,
//                         "relative" => VarType::Relevant,
//                         _ => {
//                             return Err((
//                                 SyntaxError::UnexpectedToken,
//                                 format!(
//                                     "unexpected token {vart_or_end}, should have been one of : \"affine\"/\"relative\"/\")\""
//                                 ),
//                             ));
//                         }
//                     };

//                     let (mut varname, mut cc) = capture_next_tok(cc);
//                     cc = expect_token(cc, ":")?;
//                     let (vartype, cc) = capture_type(cc)?;

//                     args.push((vart, varname.into(), vartype));
//                     code = cc;
//                 }

//                 cc = expect_token(code, "=")?;

//                 let (expr, mut cc) = capture_expr(cc, ";")?;

//                 todo!("combine fnname, args and expr into STMT::function");
//             }
//             "record" => {
//                 let (recname, mut cc) = capture_next_tok(code);
//                 let mut args = Vec::<(String, TypeName)>::new();

//                 cc = expect_token(cc, "{")?;
//                 loop {
//                     let (mut varname_or_end, mut cc) = capture_next_tok(cc);
//                     let vart = match varname_or_end {
//                         "}" => break,
//                         "," => continue,
//                         _ => {
//                             cc = expect_token(cc, ":")?;
//                             let (vartype, cc) = capture_type(cc)?;
//                             args.push((varname_or_end.into(), vartype));
//                         }
//                     };
//                 }
//                 todo!("combine recname and args into STMT::record");
//             }
//             "relevant" => {
//                 let (varname, mut cc) = capture_next_tok(code);
//                 cc = expect_token(cc, "=")?;
//                 let (expr, mut cc) = capture_expr(cc, ";")?;
//                 todo!("combine vartype, varname and expr into STMT::var");
//             }
//             "affine" => {
//                 let (varname, mut cc) = capture_next_tok(code);
//                 cc = expect_token(cc, "=")?;
//                 let (expr, mut cc) = capture_expr(cc, ";")?;
//                 todo!("combine vartype, varname and expr into STMT::var");
//             }
//             _ => {
//                 println!("ERR!");
//             }
//         }
//     }
//     // while idx < src.len() {
//     //     if src.get(idx)
//     // }

//     Ok(Program(stmts))
// }

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn type_system() {
        let lof_lof_int = ListT(b!(ListT(b!(IntT))));
        let lof_pii = ListT(b!(RecordT(vec![("x".into(), IntT), ("y".into(), IntT),])));

        println!("{:?}", lof_lof_int);
        println!("{:?}", lof_pii);

        println!("{}", serde_json::to_string_pretty(&lof_lof_int).unwrap());
        println!("{}", serde_json::to_string_pretty(&lof_pii).unwrap());
    }

    #[test]
    fn test1() {
        let prog1 = Program(vec![
            STMT::VarDef(
                VarType::Relevant,
                "x".into(),
                b!(Expr::ConstExpr(Value::IntVal(4))),
            ),
            STMT::VarDef(
                VarType::Relevant,
                "lst".into(),
                b!(Expr::ConstExpr(Value::ListVal(vec![
                    Value::IntVal(1),
                    Value::IntVal(2),
                    Value::IntVal(3)
                ]))),
            ),
            STMT::VarDef(
                VarType::Relevant,
                "begin".into(),
                b!(Expr::ConstExpr(Value::StrVal("salam be \"".into()))),
            ),
            STMT::VarDef(
                VarType::Relevant,
                "end".into(),
                b!(Expr::ConstExpr(Value::StrVal("\" aziz del".into()))),
            ),
            STMT::VarDef(
                VarType::Affine,
                "a".into(),
                b!(Expr::SumExpr(
                    b!(Expr::VarExpr("lst".into())),
                    b!(Expr::ListExpr(vec![Expr::VarExpr("x".into())]))
                )),
            ),
            STMT::VarDef(
                VarType::Affine,
                "s".into(),
                b!(Expr::SumExpr(
                    b!(Expr::VarExpr("begin".into())),
                    b!(Expr::SumExpr(
                        b!(Expr::ConstExpr(Value::StrVal("to".into()))),
                        b!(Expr::VarExpr("end".into()))
                    ))
                )),
            ),
            STMT::FuncDef(
                "list_append".into(),
                vec![
                    (VarType::Relevant, "list".into(), ListT(b!(IntT))),
                    (VarType::Affine, "item".into(), IntT),
                ],
                b!(Expr::SumExpr(
                    b!(Expr::VarExpr("list".into())),
                    b!(Expr::ListExpr(vec![Expr::VarExpr("item".into())]))
                )),
            ),
            STMT::VarDef(
                VarType::Affine,
                "test_list_append".into(),
                b!(Expr::FuncEvalExpr(
                    "list_append".into(),
                    vec![Expr::VarExpr("a".into()), Expr::ConstExpr(Value::IntVal(5))]
                )),
            ),
            STMT::RecDef("Pair".into(), vec![("x".into(), IntT), ("y".into(), IntT)]),
            STMT::FuncDef(
                "get_x".into(),
                vec![(
                    VarType::Affine,
                    "pair".into(),
                    TypeInfo::RecordName("Pair".into()),
                )],
                b!(Expr::FieldExpr(
                    b!(Expr::VarExpr("pair".into())),
                    "x".into()
                )),
            ),
            STMT::FuncDef(
                "get_y".into(),
                vec![(
                    VarType::Affine,
                    "pair".into(),
                    TypeInfo::RecordName("Pair".into()),
                )],
                b!(Expr::FieldExpr(
                    b!(Expr::VarExpr("pair".into())),
                    "y".into()
                )),
            ),
            STMT::FuncDef(
                "get_sum".into(),
                vec![(
                    VarType::Relevant,
                    "pair".into(),
                    TypeInfo::RecordName("Pair".into()),
                )],
                b!(Expr::SumExpr(
                    b!(Expr::FuncEvalExpr(
                        "get_x".into(),
                        vec![Expr::VarExpr("pair".into())]
                    )),
                    b!(Expr::FuncEvalExpr(
                        "get_y".into(),
                        vec![Expr::VarExpr("pair".into())]
                    )),
                )),
            ),
        ]);

        let progfromjson =
            serde_json::from_str::<Program>(include_str!("testing/sample1.json")).unwrap();
        println!("{:?}", progfromjson);

        // println!("{}", serde_json::to_string_pretty(&prog1).unwrap());
    }
}
