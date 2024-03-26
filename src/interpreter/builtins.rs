use super::{
    environment::Environment,
    errors::InterpretError,
    value::{FunctionParametersType, FunctionType, Type, Value},
};

pub trait Buildin {
    fn call(
        &self,
        args: Vec<Value>,
        environment: &mut Environment,
    ) -> Result<Value, InterpretError>;
}

pub fn lookup_function(name: &str) -> Option<Box<dyn Buildin>> {
    match name {
        "print" => Some(Box::new(Print)),
        _ => None,
    }
}

pub fn setup_builtins(environment: &mut Environment) {
    environment.insert(
        "print".to_string(),
        Value::String("print".to_string()),
        Type::Function(FunctionType {
            name: "print".to_string(),
            parameters: FunctionParametersType(Vec::new()),
            return_type: Box::new(Type::Int),
        }),
    );
}

pub struct Print;

impl Buildin for Print {
    fn call(&self, args: Vec<Value>, _: &mut Environment) -> Result<Value, InterpretError> {
        for arg in args {
            print!("{arg}");
        }
        println!();
        Ok(Value::Int(0))
    }
}
