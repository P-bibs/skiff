use crate::bytecode::common::{BytecodeProgram, HeapValue, Instruction, VMValue};
pub struct VMError;

pub fn vm_interp(program: BytecodeProgram) -> Result<VMValue, VMError> {
    let mut stack = vec![];
    let mut heap = vec![];
    let mut pc = 0;

    while pc < program.instructions.len() {
        let instruction = &program.instructions[pc];
        match instruction {
            Instruction::NUM(n) => stack.push(VMValue::Num(*n)),
            Instruction::BOOL(b) => stack.push(VMValue::Bool(*b)),
            Instruction::POP(id) => {
                let val = stack.pop().unwrap();
                stack.push(val);
            }
            Instruction::LOOKUP(id) => {
                let val = stack.pop().unwrap();
                stack.push(val);
            }
            Instruction::ADD => {
                let val1 = stack.pop().unwrap();
                let val2 = stack.pop().unwrap();
                match (val1, val2) {
                    (VMValue::Num(n1), VMValue::Num(n2)) => stack.push(VMValue::Num(n1 + n2)),
                    _ => return Err(VMError),
                }
            }
            Instruction::CALL(label) => {
                let ptr = stack.pop().unwrap();
                match ptr {
                    VMValue::Ptr(ptr) => {
                        let func = heap[ptr];

                        match func {
                            HeapValue::HV_closure(label) => {
                                let func = &program.func_tab[&label];

                                let args = vec![];

                                for _ in 0..func.arg_count() {
                                    let arg = stack.pop().unwrap();
                                    if arg == VMValue::Canary {
                                        // not enough args
                                        return Err(VMError);
                                    }

                                    args.push(arg);
                                }
                                let canary = stack.pop().unwrap();
                                if canary != VMValue::Canary {
                                    // too many args
                                    return Err(VMError);
                                }

                                stack.push(VMValue::Num(pc));

                                for arg in args.iter().rev() {
                                    stack.push(*arg);
                                }
                            }
                            _ => panic!("not a closure"),
                        }
                    }
                    _ => panic!("CALL: not a pointer"),
                }
            }
            Instruction::RET => {
                let val = stack.pop().unwrap();
                let new_pc = stack.pop().unwrap();
                stack.push(val);
                match new_pc {
                    VMValue::Loc(new_pc) => {
                        pc = new_pc;
                    }
                    _ => panic!("RET: not a location"),
                }
            }
            Instruction::JMP(label) => {
                let loc = stack.pop().unwrap();
                match (loc) {
                    VMValue::Loc(new_pc) => {
                        pc = new_pc;
                    }
                    _ => panic!("JMP: not a loc"),
                }
            }
            Instruction::JMP_TRUE(label) => {
                let val = stack.pop().unwrap();
                let new_pc = stack.pop().unwrap();
                match (val, new_pc) {
                    (VMValue::Bool(b), VMValue::Loc(new_pc)) => {
                        if b {
                            pc = new_pc;
                        }
                    }
                    _ => panic!("JMP_TRUE: not a bool or loc"),
                }
            }
            Instruction::JMP_FALSE(label) => {
                let val = stack.pop().unwrap();
                let new_pc = stack.pop().unwrap();
                match (val, new_pc) {
                    (VMValue::Bool(b), VMValue::Loc(new_pc)) => {
                        if !b {
                            pc = new_pc;
                        }
                    }
                    _ => panic!("JMP_FALSE: not a bool or loc"),
                }
            }
            _ => panic!("unimplemented instruction"),
        }
        pc += 1;
    }

    Ok(stack.pop().unwrap())
}
