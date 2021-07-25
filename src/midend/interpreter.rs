use std::collections::HashMap;

use crate::corelib::BUILTINS;
use crate::data::Value;

use super::ir::Instruction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackItem {
    Value(Value),
    Ref(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterpreterError {
    pub type_: InterpreterErrorType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InterpreterErrorType {
    TypeError,
    IntegerUnderflow,
    IntegerOverflow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StepOk {
    Normal,
    CallFn { target: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallStackFrame {
    function: String,
    ip: usize,
}

pub struct State {
    pub ip: usize,
    pub current_function: String,
    pub stack: Vec<StackItem>,
    pub call_stack: Vec<CallStackFrame>,
}
impl State {
    pub fn new(current_function: String) -> Self {
        Self {
            ip: 0,
            current_function,
            stack: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    pub fn run(
        &mut self,
        functions: &HashMap<String, Vec<Instruction>>,
    ) -> Result<(), InterpreterError> {
        'outer: loop {
            match &functions.get(&self.current_function) {
                Some(code) => {
                    while self.ip < code.len() {
                        match self.step(code)? {
                            StepOk::Normal => {}
                            StepOk::CallFn { target } => {
                                self.call_stack.push(CallStackFrame {
                                    ip: self.ip,
                                    function: self.current_function.clone(),
                                });
                                self.ip = 0;
                                self.current_function = target;
                                continue 'outer;
                            }
                        }
                    }
                }
                None => {
                    let mut bfn = None;
                    for (name, _, f) in BUILTINS.iter() {
                        if *name == self.current_function {
                            bfn = Some(f);
                            break;
                        }
                    }
                    let bfn = bfn.expect("No interpreter impl for function");
                    bfn(self)?;
                }
            };

            if let Some(CallStackFrame { ip, function }) = self.call_stack.pop() {
                self.ip = ip;
                self.current_function = function;
            } else {
                println!("End {:?}", self.stack);
                return Ok(());
            }
        }
    }

    fn step(&mut self, code: &[Instruction]) -> Result<StepOk, InterpreterError> {
        println!("Step {:?} {:?}", &code[self.ip], self.stack);

        match &code[self.ip] {
            Instruction::Comment(comment) => {
                // println!("// {}", comment);
            }
            Instruction::Push { value } => {
                self.stack.push(StackItem::Value(value.clone()));
            }
            Instruction::PushRef { name } => {
                self.stack.push(StackItem::Ref(name.clone()));
            }
            Instruction::Load => {
                let target = self.stack.pop().expect("Stack underflow");
                todo!("Load")
            }
            Instruction::Store => {
                let target = self.stack.pop().expect("Stack underflow");
                let value = self.stack.pop().expect("Stack underflow");
                todo!("Store")
            }
            Instruction::Remove { depth } => {
                assert!(*depth < self.stack.len(), "Stack underflow");
                let index = self.stack.len() - depth - 1;
                self.stack.remove(index);
            }
            Instruction::Copy { depth } => {
                assert!(*depth < self.stack.len(), "Stack underflow");
                let index = self.stack.len() - depth - 1;
                let value = self.stack[index].clone();
                self.stack.push(value);
            }
            Instruction::Swap { depth } => {
                assert!(*depth < self.stack.len(), "Stack underflow");
                let index = self.stack.len() - depth - 1;
                let last_index = self.stack.len() - 1;
                self.stack.swap(index, last_index);
            }
            Instruction::CallFunction { argc: _ } => {
                let target = self.stack.pop().expect("Stack underflow");
                self.ip += 1;
                if let StackItem::Ref(r) = target {
                    return Ok(StepOk::CallFn { target: r });
                }
            }
            Instruction::Label { id } => {}
            Instruction::JumpToIf { condition, label } => {
                // TODO: this can be done more efficiently by caching/pre-computing
                let cond = self.stack.pop().expect("Stack underflow");
                if let StackItem::Value(Value::Boolean(c)) = cond {
                    if c == *condition {
                        self.ip = code
                            .iter()
                            .position(|instr| {
                                if let Instruction::Label { id } = instr {
                                    id == label
                                } else {
                                    false
                                }
                            })
                            .expect("Label not found");
                        return Ok(StepOk::Normal);
                    }
                } else {
                    panic!("Condition not boolean");
                }
            }
        }
        self.ip += 1;
        Ok(StepOk::Normal)
    }
}
