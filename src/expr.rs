#[derive(Debug, PartialEq)]
pub enum Error {
    UndefinedVariable(String),
    UndefinedFunction(String),
    InvalidArgCount {
        op_name: String,
        got_args: usize,
        expected_args: usize,
    },

    MathError(String),
}

pub trait Expression {
    fn eval(&self, variables: &[(&str, &dyn Expression)]) -> Result<f32, Error>;
}

impl Expression for f32 {
    fn eval(&self, _: &[(&str, &dyn Expression)]) -> Result<f32, Error> {
        Ok(*self)
    }
}

pub struct Variable {
    name: String,
}

impl Variable {
    pub fn new_expression(name: String) -> Box<dyn Expression + 'static> {
        Box::new(Self { name })
    }
}

impl Expression for Variable {
    fn eval(&self, variables: &[(&str, &dyn Expression)]) -> Result<f32, Error> {
        variables.iter().find(|(v, _)| v.eq(&self.name)).map_or(
            Err(Error::UndefinedVariable(self.name.clone())),
            |(_, e)| e.eval(variables),
        )
    }
}

impl<'a> Expression for &'a str {
    fn eval(&self, variables: &[(&str, &dyn Expression)]) -> Result<f32, Error> {
        variables
            .iter()
            .find(|(v, _)| v.eq(self))
            .map_or(Err(Error::UndefinedVariable(self.to_string())), |(_, e)| {
                e.eval(variables)
            })
    }
}

pub enum BasicOp<'a> {
    Plus(Box<dyn Expression + 'a>, Box<dyn Expression + 'a>),
    Minus(Box<dyn Expression + 'a>, Box<dyn Expression + 'a>),
    Multiply(Box<dyn Expression + 'a>, Box<dyn Expression + 'a>),
    Divide(Box<dyn Expression + 'a>, Box<dyn Expression + 'a>),
    Negate(Box<dyn Expression + 'a>),
}

impl<'a> Expression for BasicOp<'a> {
    fn eval(&self, variables: &[(&str, &dyn Expression)]) -> Result<f32, Error> {
        match self {
            BasicOp::Plus(left, right) => left
                .eval(variables)
                .and_then(|l| right.eval(variables).map(|r| l + r)),
            BasicOp::Minus(left, right) => left
                .eval(variables)
                .and_then(|l| right.eval(variables).map(|r| l - r)),
            BasicOp::Multiply(left, right) => left
                .eval(variables)
                .and_then(|l| right.eval(variables).map(|r| l * r)),
            BasicOp::Divide(left, right) => left
                .eval(variables)
                .and_then(|l| right.eval(variables).map(|r| (l, r)))
                .map_or_else(Err, |(l, r)| {
                    if r == 0.0 {
                        Err(Error::MathError("Divide by zero".to_owned()))
                    } else {
                        Ok(l / r)
                    }
                }),
            BasicOp::Negate(r) => r.eval(variables).map(|res| -res),
        }
    }
}

pub trait Function {
    fn eval(&self, args: &[f32]) -> Result<f32, Error>;
    fn get_name(&self) -> &str;
}

pub struct ClosureFunction<F>
where
    F: Fn(&[f32]) -> Result<f32, Error>,
{
    name: String,
    func: F,
}

impl<F> Function for ClosureFunction<F>
where
    F: Fn(&[f32]) -> Result<f32, Error>,
{
    fn eval(&self, args: &[f32]) -> Result<f32, Error> {
        (self.func)(args)
    }

    fn get_name(&self) -> &str {
        &self.name
    }
}

impl<F> ClosureFunction<F>
where
    F: Fn(&[f32]) -> Result<f32, Error> + 'static,
{
    pub fn new_function(name: String, func: F) -> Box<dyn Function> {
        Box::new(Self { name, func })
    }
}

pub trait Language {
    fn find_func<'a>(&'a self, func_name: &str) -> Option<&'a dyn Function>;
}

pub struct FunctionExpression<'a> {
    language: &'a dyn Language,
    args: Vec<Box<dyn Expression + 'a>>,
    name: String,
}

impl<'a> FunctionExpression<'a> {
    pub fn new_expression(
        language: &'a dyn Language,
        args: Vec<Box<dyn Expression + 'a>>,
        name: String,
    ) -> Box<dyn Expression + 'a> {
        Box::new(Self {
            language,
            args,
            name,
        })
    }
}

impl<'a> Expression for FunctionExpression<'a> {
    fn eval(&self, variables: &[(&str, &dyn Expression)]) -> Result<f32, Error> {
        let func = self
            .language
            .find_func(&self.name)
            .ok_or_else(|| Error::UndefinedFunction(self.name.clone()))?;
        let calculated_args = self
            .args
            .iter()
            .map(|arg| arg.eval(variables))
            .collect::<Result<Vec<_>, _>>()?;
        func.eval(&calculated_args)
    }
}

pub struct DefaultLanguage {
    functions: Vec<Box<dyn Function>>,
}

impl DefaultLanguage {
    pub fn new(functions: Vec<Box<dyn Function>>) -> Self {
        Self { functions }
    }
}

impl Language for DefaultLanguage {
    fn find_func<'a>(&'a self, func_name: &str) -> Option<&'a dyn Function> {
        self.functions
            .iter()
            .find(|f| f.get_name().eq(func_name))
            .map(|f| f.as_ref())
    }
}

impl Default for DefaultLanguage {
    fn default() -> Self {
        Self {
            functions: vec![
                ClosureFunction::new_function("pow".to_string(), |args| {
                    if args.len() != 2 {
                        Err(Error::InvalidArgCount {
                            op_name: "pow".to_string(),
                            got_args: args.len(),
                            expected_args: 2,
                        })
                    } else {
                        Ok(args[0].powf(args[1]))
                    }
                }),
                ClosureFunction::new_function("sqrt".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "sqrt".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else if args[0] < 0.0 {
                        Err(Error::MathError("Sqrt of negative".to_owned()))
                    } else {
                        Ok(args[0].sqrt())
                    }
                }),
                ClosureFunction::new_function("sin".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "sin".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else {
                        Ok(args[0].sin())
                    }
                }),
                ClosureFunction::new_function("cos".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "cos".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else {
                        Ok(args[0].cos())
                    }
                }),
                ClosureFunction::new_function("tg".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "tg".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else {
                        Ok(args[0].tan())
                    }
                }),
                ClosureFunction::new_function("ctg".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "ctg".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else {
                        Ok((std::f32::consts::FRAC_PI_2 - args[0]).tan())
                    }
                }),
                ClosureFunction::new_function("exp".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "exp".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else {
                        Ok(args[0].exp())
                    }
                }),
                ClosureFunction::new_function("ln".to_string(), |args| {
                    if args.len() != 1 {
                        Err(Error::InvalidArgCount {
                            op_name: "ln".to_string(),
                            got_args: args.len(),
                            expected_args: 1,
                        })
                    } else if args[0] < 0.0 {
                        Err(Error::MathError("Log of negative".to_owned()))
                    } else {
                        Ok(args[0].ln())
                    }
                }),
                ClosureFunction::new_function("vec_len".to_string(), |args| {
                    Ok(args.iter().fold(0.0, |acc, x| acc + x * x).sqrt())
                }),
            ],
        }
    }
}

#[test]
fn expression_eval() {
    let lang = DefaultLanguage::default();
    let pow = FunctionExpression::new_expression(
        &lang,
        vec![Box::new(2.0), Box::new(10.0)],
        "pow".to_owned(),
    );
    let add: Box<dyn Expression> =
        Box::new(BasicOp::Plus(pow, Variable::new_expression("x".to_owned())));
    assert_eq!(add.eval(&[("x", &3.0)]), Ok(1027.0));
}

#[test]
fn variable_as_expr() {
    use super::*;

    let lang = DefaultLanguage::default();
    let expr = parse_expr(&tokenize("y+x").unwrap(), &lang).unwrap();
    assert_eq!(
        expr.eval(&[
            (
                "x",
                &BasicOp::Multiply(
                    Variable::new_expression("y".to_string()),
                    Variable::new_expression("z".to_string())
                )
            ),
            ("y", &1.0),
            ("z", &2.0)
        ]),
        Ok(3.0)
    );
}
