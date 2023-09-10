use std::{io::Read, collections::HashMap, cell::RefCell, rc::Rc};

use ast::{Function, Str};
use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod parser;

lalrpop_mod! {
    #[allow(warnings)]
    pub rinha
}

pub struct Call {
    pub callee: Option<ast::Term>,
    pub arguments: Vec<ast::Term>,
    pub location: ast::Location,
    pub var_scope: HashMap<String, RuntimeValue>
}

type VoidResult = Result<(), Box<dyn std::error::Error>>;

#[derive(Clone)]
pub struct CallStack {
    inner: Rc<RefCell<Vec<Call>>>
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(Vec::new())),
        }
    }
    pub fn push(&self, value: Call) {
        self.inner.borrow_mut().push(value);
    }
    pub fn len(&self) -> usize {
        self.inner.borrow().len()
    }
    pub fn pop(&self) {
        self.inner.borrow_mut().pop();
    }
    pub fn get_var(&self, name: &String) -> RuntimeValue {
        let len = self.len() - 1;
        for i in (0..len).rev() {
            let var_scope = &self.inner.borrow()[i].var_scope;
            for (varname, runtime_value) in var_scope {
                if name.ne(varname) { continue }
                return match runtime_value {
                    RuntimeValue::Int(x) => RuntimeValue::Int(*x),
                    RuntimeValue::Str(x) => RuntimeValue::Str(x.to_string()),
                    RuntimeValue::Bool(x) => RuntimeValue::Bool(*x),
                    RuntimeValue::Tuple(_) => todo!(),
                    RuntimeValue::Function(_) => todo!(),
                    RuntimeValue::Void(_) => RuntimeValue::Void(()),
                }
            }
        }
        panic!("reference \"{name}\" not found")
    }
    pub fn set_var(&self, name: &String, value: RuntimeValue) {
        let len = self.len() - 1;
        let var_scope = &mut self.inner.borrow_mut()[len].var_scope;
        var_scope.insert(name.to_string(), value);
    }
}

pub enum RuntimeValue {
    Int(isize),
    Str(String),
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
    Function(ast::Function),
    Void(())
}

fn call_fn(callee: ast::Term, arguments: Vec<ast::Term>, call_stack: &CallStack) -> RuntimeValue {
    let name = match callee {
        ast::Term::Var(x) => x.text,
        _ => panic!("callee is not a var")
    };
    let result = match call_stack.get_var(&name) {
        RuntimeValue::Function(x) => {
            match x {
                Function { parameters, value, location } => {
                    if arguments.len() < parameters.len() {
                        panic!("wrong number of args passed to {name}");
                    }
                    call_stack.push(Call {
                        arguments,
                        callee: Some(ast::Term::Str(Str { ..Default::default() })),
                        location: location.clone(),
                        var_scope: HashMap::new()
                    });
                    eval(*value.clone(), &call_stack)
                }
            }
        }
        _ => panic!("error: \"{name}\" is not a function"),
    };
    call_stack.pop();
    result
}

fn eval_binary_op(op: ast::BinaryOp, l: RuntimeValue, r: RuntimeValue) -> isize {
    let l = match l {
        RuntimeValue::Int(x) => x,
        _ => panic!("operand is not an integer"),
    };
    let r = match r {
        RuntimeValue::Int(x) => x,
        _ => panic!("operand is not an integer"),
    };
    match op {
        ast::BinaryOp::Add => l + r,
        ast::BinaryOp::Sub => l - r,
        ast::BinaryOp::Mul => l * r,
        ast::BinaryOp::Div => l / r,
        ast::BinaryOp::Rem => l % r,
        ast::BinaryOp::Eq => if l == r {1} else {0},
        ast::BinaryOp::Neq => if l != r {1} else {0},
        ast::BinaryOp::Lt => if l < r {1} else {0},
        ast::BinaryOp::Gt => if l > r {1} else {0},
        ast::BinaryOp::Lte => if l <= r {1} else {0},
        ast::BinaryOp::Gte => if l >= r {1} else {0},
        ast::BinaryOp::And => if l != 0 && r != 0 {1} else {0},
        ast::BinaryOp::Or => if l != 0 || r != 0 {1} else {0},
    }
}

fn print_value(x: ast::Print, call_stack: &CallStack) -> RuntimeValue {
    match eval(*x.value, call_stack) {
        RuntimeValue::Int(x) => { print!("{x}") },
        RuntimeValue::Str(x) => { print!("{x}") },
        RuntimeValue::Bool(x) => { print!("{x}") },
        RuntimeValue::Tuple(_) => { print!("[tuple]") },
        RuntimeValue::Function(_) => { print!("[function]") },
        RuntimeValue::Void(_) => { print!("[void]") },
    };
    RuntimeValue::Void(())
}

fn eval(expr: ast::Term, call_stack: &CallStack) -> RuntimeValue {
    match expr {
        ast::Term::Error(x) => panic!("Panicked at {}:{} - {}", x.location.filename, x.location.start, x.full_text),
        ast::Term::Int(x) => RuntimeValue::Int(x.value),
        ast::Term::Str(x) => RuntimeValue::Str(x.value),
        ast::Term::Call(x) => call_fn(*x.callee, x.arguments, call_stack),
        ast::Term::Binary(x) =>
            RuntimeValue::Int(
                eval_binary_op(x.op, eval(*x.lhs, call_stack), eval(*x.rhs, call_stack))
            ),
        ast::Term::Function(x) => RuntimeValue::Function(x),
        ast::Term::Let(x) => {
            call_stack.set_var(&x.name.text, eval(*x.value, &call_stack));
            eval(*x.next, &call_stack)
        },
        ast::Term::If(_) => todo!(),
        ast::Term::Print(x) => print_value(x, &call_stack),
        ast::Term::First(_) => todo!(),
        ast::Term::Second(_) => todo!(),
        ast::Term::Bool(x) => RuntimeValue::Bool(x.value),
        ast::Term::Tuple(_) => todo!(),
        ast::Term::Var(x) => {
            call_stack.get_var(&x.text)
        },
    }
}

fn main() -> VoidResult {
    let mut json_bytes = std::fs::File::open("fib.json")?;
    let mut buf = vec![];
    json_bytes.read_to_end(&mut buf)?;
    let ast = serde_json::from_slice::<ast::File>(&buf)?;
    let call_stack: CallStack = CallStack::new();
    call_stack.push(Call {
        arguments: vec![],
        callee: None,
        location: ast::Location { start: 1, end: 1, filename: ast.name },
        var_scope: HashMap::new()
    });
    eval(ast.expression, &call_stack);

    Ok(())
}
