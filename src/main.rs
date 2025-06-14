macro_rules! b {
    ($x:expr) => {
        Box::new($x)
    };
}
enum Value {
    IntVal(i32),
    BoolVal(bool),
    StrVal(String),
    RecVal(Vec<(String, Value)>),
    ListVal(Vec<Value>),
}
enum Expr {
    VarExpr(String),
    ConstExpr(Value),
    FuncEvalExpr(String, Vec<Expr>),
    SumExpr(Box<Expr>, Box<Expr>),
    NotExpr(Box<Expr>),
    IfElseExpr(Box<Expr>, Box<Expr>, Box<Expr>),
    FieldExpr(Box<Expr>, String),
    IndexExpr(Box<Expr>, Box<Expr>),
}
enum STMT {
    VarDef(VarType, String, Box<Expr>),
    FuncDef(Vec<(VarType, String, TypeInfo)>, Box<Expr>),
    RecDef(String, Vec<(String, TypeInfo)>),
    Expr(Box<Expr>),
}
#[derive(Debug, PartialEq)]
enum VarType {
    Affine,
    Relevant,
}
#[derive(Debug, PartialEq)]
enum TypeInfo {
    Int,
    String,
    Boolean,
    Record(Vec<(String, TypeInfo)>),
    List(Box<TypeInfo>),
}
struct Prog(Vec<STMT>);

use TypeInfo::Boolean as BoolT;
use TypeInfo::Int as IntT;
use TypeInfo::List as ListT;
use TypeInfo::Record as RecordT;
use TypeInfo::String as StrT;

fn main() {}

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
