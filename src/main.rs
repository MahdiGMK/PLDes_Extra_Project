macro_rules! b {
    ($x:expr) => {
        Box::new($x)
    };
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
fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_system() {
        let lof_lof_int = TypeInfo::List(b!(TypeInfo::List(b!(TypeInfo::Int))));
        let lof_pii = TypeInfo::List(b!(TypeInfo::Record(vec![
            ("x".into(), TypeInfo::Int),
            ("y".into(), TypeInfo::Int),
        ])));
        println!("{:?}", lof_lof_int);
        println!("{:?}", lof_pii);
    }
}
