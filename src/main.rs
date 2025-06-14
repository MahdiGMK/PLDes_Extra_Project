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
    Record(Vec<TypeInfo>),
    List(Box<TypeInfo>),
}
fn main() {
    let lof_lof_int = TypeInfo::List(Box::new(TypeInfo::List(Box::new(TypeInfo::Int))));
    print!("{:?}", lof_lof_int);
}
