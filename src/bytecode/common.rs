use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum VMValue {
    Num(i64),
    Bool(bool),
    Ptr(usize),
    Loc(usize),
    Canary,
}
#[derive(PartialEq, Debug, Clone, Hash)]
pub enum HeapValue {
    HV_closure(Label),
}

type Var = String;
type Label = String;

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Instruction {
    CANARY,
    NUM(i64),
    BOOL(bool),
    PUSH(Var),
    POP(Var),
    LOOKUP(Var),
    ADD,
    CALL(Label),
    RET,
    JMP(Label),
    JMP_TRUE(Label),
    JMP_FALSE(Label),
}

#[derive(PartialEq, Debug, Clone)]
pub struct BytecodeProgram {
    pub instructions: Vec<Instruction>,
    pub func_tab: HashMap<Label, FuncInfo>,
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct FuncInfo {
    pub name: String,
    pub arg_names: Vec<String>,
    pub loc: usize,
}
impl FuncInfo {
    pub fn arg_count(&self) -> usize {
        self.arg_names.len()
    }
}
