use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::ops::{Index, IndexMut};
use std::borrow::Borrow;
use std::str::FromStr;
use std::fmt;

use crate::parse::ParseError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: String,
    pub order: usize,  // Order of its derivative
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.name.as_str())?;
        for _ in 0..self.order {
            f.write_str("'")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticError {
    // Equation does not parse correctly
    InvalidEqns(ParseError),
    // When expression should be constant, but isn't
    ExprNonConst(String),
    // Variable `Var` is missing a value for derivative of some order
    MissingDeriv(Var),
    // Variable `Var` is repeated as an output of equations.
    RepeatOutputVar(Var),
    // Name `String` is repeated as output (potentially with a different order)
    RepeatOutputName(String),
    // The time variable `String` is differentiated with order `usize`.
    DerivOfTime(Var),
    // The order of an input variable `Var` is equal to or exceeds the output order `usize`.
    InputOrderTooHigh(Var, usize),
    // The non-time variable `String` has no equation defining it
    MissingEquation(String),
}

impl std::error::Error for SemanticError {}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            SemanticError::InvalidEqns(err) => write!(f, "Could not parse equation: {}", err),
            SemanticError::ExprNonConst(expr) => write!(f, "Expression did not evaluate to constant: {}", expr),
            SemanticError::MissingDeriv(var) => write!(f, "No value provided for {}", var),
            SemanticError::RepeatOutputVar(var) => write!(f, "Multiple equations for variable {}", var),
            SemanticError::RepeatOutputName(name) => write!(f, "Multiple equations (potentially with different derivatives) for variable name {}", name),
            SemanticError::DerivOfTime(var) => write!(f, "Time variable {} cannot have derivative: {}", var.name, var),
            SemanticError::InputOrderTooHigh(var, out_order) => if *out_order == 0 {
                write!(f, "Output variable {} should be a derivative", var.name)
            } else {
                write!(f, "Input variable {} has order exceeding the maximum order {}", var, out_order - 1)
            },
            SemanticError::MissingEquation(name) => write!(f, "Variable {} has no equation defining its behavior", name),
        }
    }
}



#[derive(Debug, Clone)]
pub struct State {
    // Store values of each derivative of a variable
    vars: HashMap<String, Vec<f64>>,
}

fn split_deriv<'a>(s: &'a str) -> (&'a str, usize) {
    let mut var = s.trim();
    let mut order = 0;
    while let Some(new_var) = var.strip_suffix("'") {
        var = new_var;
        order += 1;
    }
    (var, order)
}

impl<Q: Borrow<str> + ?Sized> Index<&Q> for State {
    type Output = f64;
    fn index(&self, idx: &Q) -> &Self::Output {
        let (name, order) = split_deriv(idx.borrow());
        &self.vars[name][order]
    }
}

impl<Q: Borrow<str> + ?Sized> IndexMut<&Q> for State {
    fn index_mut(&mut self, idx: &Q) -> &mut Self::Output {
        let (name, order) = split_deriv(idx.borrow());
        &mut self.vars.get_mut(name).unwrap()[order]
    }
}

impl FromStr for State {
    type Err = SemanticError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut vars = HashMap::<String, Vec<Option<f64>>>::new();
        let eqns = s.parse::<Eqns>().map_err(SemanticError::InvalidEqns)?;
        for (Var { name, order }, expr) in eqns.0.into_iter() {
            if let Expr::Const(value) = expr { 
                if let Some(derivs) = vars.get_mut(&name) {
                    if order >= derivs.len() {
                        derivs.resize_with(order + 1, || None);
                    } else if let Some(_) = derivs[order] {
                        return Err(SemanticError::RepeatOutputVar(Var { name, order }))
                    }
                    derivs[order] = Some(value);
                } else {
                    let mut derivs = Vec::new();
                    derivs.resize_with(order + 1, || None);
                    derivs[order] = Some(value);
                    vars.insert(name, derivs);
                }
            } else {
                return Err(SemanticError::ExprNonConst(s.into()))
            }
        }

        // Check that all derivatives have values
        let mut vars_unwrap = HashMap::with_capacity(vars.len());
        for (name, derivs) in vars.into_iter() {
            let mut derivs_unwrap = Vec::with_capacity(derivs.len());
            for (order, value_opt) in derivs.into_iter().enumerate() {
                if let Some(value) = value_opt {
                    derivs_unwrap.push(value);
                } else {
                    let name = name.clone();
                    return Err(SemanticError::MissingDeriv(Var { name, order }))
                }
            }
            vars_unwrap.insert(name, derivs_unwrap);
        }
        Ok(State { vars: vars_unwrap })
    }
}





pub type Unary = fn(f64) -> f64;
pub type Binary = fn(f64, f64) -> f64;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),  // Variable name and order of derivative
    Const(f64),
    Unary(Unary, Box<Expr>),
    Binary(Binary, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn eval(&self, state: &State) -> f64 {
        match self {
            Expr::Var(Var { name, order }) => state.vars[name][*order],
            Expr::Const(value) => *value,
            Expr::Unary(func, arg) => func(arg.eval(state)),
            Expr::Binary(func, arg1, arg2) => func(arg1.eval(state), arg2.eval(state)),
        }
    }

    fn for_each_var<'a>(&'a self, mut func: impl FnMut(&'a str, usize)) {
        self.for_each_var_raw(&mut func)
    }

    fn for_each_var_raw<'a>(&'a self, func: &mut impl FnMut(&'a str, usize)) {
        match self {
            Expr::Var(Var { name, order }) => func(name.as_str(), *order),
            Expr::Const(_) => {}
            Expr::Unary(_, arg) => arg.for_each_var_raw(func),
            Expr::Binary(_, arg1, arg2) => {
                arg1.for_each_var_raw(func);
                arg2.for_each_var_raw(func);
            }
        }
    }

    pub fn apply(self, func: Unary) -> Self {
        match self {
            Expr::Const(val) => Expr::Const(func(val)),
            arg => Expr::Unary(func, Box::new(arg)),
        }
    }

    pub fn apply2(self, func: Binary, other: Self) -> Self {
        match (self, other) {
            (Expr::Const(val1), Expr::Const(val2)) => Expr::Const(func(val1, val2)),
            (arg1, arg2) => Expr::Binary(func, Box::new(arg1), Box::new(arg2)),
        }
    }
}

impl Neg for Expr {
    type Output = Self;
    fn neg(self) -> Self {
        self.apply(|x| -x)
    }
}

impl Add for Expr {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        self.apply2(|x, y| x + y, rhs)
    }
}

impl Sub for Expr {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        self.apply2(|x, y| x - y, rhs)
    }
}

impl Mul for Expr {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        self.apply2(|x, y| x * y, rhs)
    }
}

impl Div for Expr {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        self.apply2(|x, y| x / y, rhs)
    }
}

impl Rem for Expr {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        self.apply2(|x, y| x % y, rhs)
    }
}



// Store equations keyed by the variable name
// with the value being the the order of the output derivative
// and the expression defining the derivative.
#[derive(Debug, Clone)]
pub struct Eqns(pub HashMap<Var, Expr>);



#[derive(Debug, Clone)]
pub struct System {
    time_var: String,
    eqns: HashMap<String, (usize, Expr)>,
}

impl System {
    pub fn new(time_var: String, eqns: Eqns) -> Result<Self, SemanticError> {
        // Re-key equations by the name of their output variable
        let mut eqns_by_name = HashMap::new();
        for (Var {name, order}, expr) in eqns.0.into_iter() {
            if eqns_by_name.contains_key(&name) {
                return Err(SemanticError::RepeatOutputName(name))
            }
            eqns_by_name.insert(name, (order, expr));
        }
        let eqns = eqns_by_name;

        // Collect input variables and the maximum order of the their derivatives
        let mut inputs = HashMap::<&str, usize>::new();

        for (_, (_, expr)) in eqns.iter() {
            expr.for_each_var(|name, order| {
                if let Some(max_order) = inputs.get_mut(&name) {
                    *max_order = (*max_order).max(order);
                } else {
                    inputs.insert(name, order);
                }
            });
        }

        // Every variable besides time should be represented by an equation
        for (&name, &order) in inputs.iter() {
            if name == time_var {
                if order > 0 {
                    return Err(SemanticError::DerivOfTime(Var { name: time_var, order }))
                }
            } else if let Some(&(out_order, _)) = eqns.get(name) {
                if order >= out_order {
                    return Err(SemanticError::InputOrderTooHigh(Var { name: name.into(), order }, out_order))
                }
            } else {
                return Err(SemanticError::MissingEquation(name.into()))
            }
        }

        Ok(System { time_var, eqns })
    }

    pub fn contains_var<Q: Borrow<str>>(&self, name: &Q) -> bool {
        self.eqns.contains_key(name.borrow())
    }

    // On error, returns the missing variable
    pub fn euler<'a>(&'a self, step_size: f64, mut state: State) -> Result<EulerIter<'a>, SemanticError> {
        // Check that all variables are present
        for (name, (order, _)) in self.eqns.iter() {
            if let Some(derivs) = state.vars.get(name) {
                if derivs.len() < *order {
                    return Err(SemanticError::MissingDeriv(Var {
                        name: name.clone(), order: derivs.len(),
                    }))
                }
            } else {
                return Err(SemanticError::MissingDeriv(Var {
                    name: name.clone(), order: 0,
                }))
            }
        }

        if !state.vars.contains_key(&self.time_var) {
            state.vars.insert(self.time_var.clone(), vec![0.0]);
        }
        Ok(EulerIter { system: self, step_size, state })
    }
}

pub struct EulerIter<'a> {
    system: &'a System,
    step_size: f64,
    state: State,
}

impl<'a> Iterator for EulerIter<'a> {
    type Item = State;
    fn next(&mut self) -> Option<Self::Item> {
        // Calculate new values for non-time variables
        let mut new_vars: HashMap<String, Vec<f64>> = self.system.eqns.iter().map(|(name, (out_order, expr))| {
            let max_order = out_order - 1;
            let vals = &self.state.vars[name];
            let mut new_vals = Vec::new();

            for order in 0..max_order {
                new_vals.push(vals[order] + self.step_size * vals[order + 1]);
            }
            new_vals.push(vals[max_order] + self.step_size * expr.eval(&self.state));
            (name.clone(), new_vals)
        }).collect();

        // Calculate new value of time variable
        let time_var = self.system.time_var.as_str();
        let time = self.state[time_var];
        new_vars.insert(time_var.into(), vec![time + self.step_size]);

        // Swap new state with old one and return the old one
        let mut return_state = State { vars: new_vars };
        std::mem::swap(&mut self.state, &mut return_state);
        Some(return_state)
    }
}
