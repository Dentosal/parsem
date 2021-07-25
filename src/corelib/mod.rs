use crate::data::{DataType, Value};
use crate::midend::interpreter::{self, InterpreterError, InterpreterErrorType, StackItem};

type InterpFn = fn(&mut interpreter::State) -> Result<(), InterpreterError>;

lazy_static::lazy_static! {
    pub static ref BUILTINS: [(&'static str, DataType, InterpFn); 3] = [
        (
            "eq", DataType::Function {
                arguments: vec![DataType::Any, DataType::Any],
                returns: Box::new(DataType::Boolean),
            },
            f_eq,
        ), (
            "inc",
            DataType::Function {
                arguments: vec![DataType::Integer {bits: 64}],
                returns: Box::new(DataType::Integer {bits: 64}),
            },
            f_inc,
        ), (
            "dec",
            DataType::Function {
                arguments: vec![DataType::Integer {bits: 64}],
                returns: Box::new(DataType::Integer {bits: 64}),
            },
            f_dec,
        ),
    ];
}

fn f_eq(state: &mut interpreter::State) -> Result<(), InterpreterError> {
    let a = state.stack.pop().expect("Stack underflow");
    let b = state.stack.pop().expect("Stack underflow");
    state.stack.push(StackItem::Value(Value::Boolean(a == b)));
    Ok(())
}

fn f_inc(state: &mut interpreter::State) -> Result<(), InterpreterError> {
    let item = state.stack.pop().expect("Stack underflow");
    if let StackItem::Value(Value::Integer(v)) = item {
        let r = v.checked_add(1).ok_or(InterpreterError {
            type_: InterpreterErrorType::IntegerOverflow,
        })?;
        state.stack.push(StackItem::Value(Value::Integer(r)));
        Ok(())
    } else {
        Err(InterpreterError {
            type_: InterpreterErrorType::TypeError,
        })
    }
}

fn f_dec(state: &mut interpreter::State) -> Result<(), InterpreterError> {
    let item = state.stack.pop().expect("Stack underflow");
    if let StackItem::Value(Value::Integer(v)) = item {
        let r = v.checked_sub(1).ok_or(InterpreterError {
            type_: InterpreterErrorType::IntegerUnderflow,
        })?;
        state.stack.push(StackItem::Value(Value::Integer(r)));
        Ok(())
    } else {
        Err(InterpreterError {
            type_: InterpreterErrorType::TypeError,
        })
    }
}
