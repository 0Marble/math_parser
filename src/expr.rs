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
    fn eval(&self, variables: &[(&str, f32)]) -> Result<f32, Error>;
    fn compile<'a>(&'a self, variables: &[(&str, f32)]) -> Result<Box<dyn Expression + 'a>, Error>;
    fn to_number(&self) -> Option<f32>;
}

impl Expression for f32 {
    fn eval(&self, _: &[(&str, f32)]) -> Result<f32, Error> {
        Ok(*self)
    }

    fn compile<'a>(&'a self, _: &[(&str, f32)]) -> Result<Box<dyn Expression + 'a>, Error> {
        Ok(Box::new(*self))
    }

    fn to_number(&self) -> Option<f32> {
        Some(*self)
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
    fn eval(&self, variables: &[(&str, f32)]) -> Result<f32, Error> {
        variables.iter().find(|(v, _)| v.eq(&self.name)).map_or(
            Err(Error::UndefinedVariable(self.name.clone())),
            |(_, e)| e.eval(variables),
        )
    }

    fn compile<'a>(&'a self, variables: &[(&str, f32)]) -> Result<Box<dyn Expression + 'a>, Error> {
        Ok(variables
            .iter()
            .find(|(v, _)| v.eq(&self.name))
            .map_or_else(
                || Variable::new_expression(self.name.clone()),
                |(_, val)| Box::new(*val),
            ))
    }

    fn to_number(&self) -> Option<f32> {
        None
    }
}

impl<'a> Expression for &'a str {
    fn eval(&self, variables: &[(&str, f32)]) -> Result<f32, Error> {
        variables
            .iter()
            .find(|(v, _)| v.eq(self))
            .map_or(Err(Error::UndefinedVariable(self.to_string())), |(_, e)| {
                e.eval(variables)
            })
    }

    fn compile<'b>(&'b self, variables: &[(&str, f32)]) -> Result<Box<dyn Expression + 'b>, Error> {
        Ok(variables.iter().find(|(v, _)| v.eq(self)).map_or_else(
            || Variable::new_expression(self.to_string()),
            |(_, val)| Box::new(*val),
        ))
    }

    fn to_number(&self) -> Option<f32> {
        None
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
    fn eval(&self, variables: &[(&str, f32)]) -> Result<f32, Error> {
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

    fn compile<'b>(&'b self, variables: &[(&str, f32)]) -> Result<Box<dyn Expression + 'b>, Error> {
        match self {
            BasicOp::Plus(l, r) => {
                let l = l.compile(variables)?;
                let r = r.compile(variables)?;
                Ok(match (l.to_number(), r.to_number()) {
                    (None, None) => Box::new(BasicOp::Plus(l, r)),
                    (None, Some(r)) => Box::new(BasicOp::Plus(l, Box::new(r))),
                    (Some(l), None) => Box::new(BasicOp::Plus(Box::new(l), r)),
                    (Some(l), Some(r)) => Box::new(l + r),
                })
            }
            BasicOp::Minus(l, r) => {
                let l = l.compile(variables)?;
                let r = r.compile(variables)?;
                Ok(match (l.to_number(), r.to_number()) {
                    (None, None) => Box::new(BasicOp::Minus(l, r)),
                    (None, Some(r)) => Box::new(BasicOp::Minus(l, Box::new(r))),
                    (Some(l), None) => Box::new(BasicOp::Minus(Box::new(l), r)),
                    (Some(l), Some(r)) => Box::new(l - r),
                })
            }
            BasicOp::Multiply(l, r) => {
                let l = l.compile(variables)?;
                let r = r.compile(variables)?;
                Ok(match (l.to_number(), r.to_number()) {
                    (None, None) => Box::new(BasicOp::Multiply(l, r)),
                    (None, Some(r)) => Box::new(BasicOp::Multiply(l, Box::new(r))),
                    (Some(l), None) => Box::new(BasicOp::Multiply(Box::new(l), r)),
                    (Some(l), Some(r)) => Box::new(l * r),
                })
            }
            BasicOp::Divide(l, r) => {
                let l = l.compile(variables)?;
                let r = r.compile(variables)?;

                match (l.to_number(), r.to_number()) {
                    (None, None) => Ok(Box::new(BasicOp::Divide(l, r))),
                    (None, Some(r)) => Ok(Box::new(BasicOp::Divide(l, Box::new(r)))),
                    (Some(l), None) => Ok(Box::new(BasicOp::Divide(Box::new(l), r))),
                    (Some(l), Some(r)) if r != 0.0 => Ok(Box::new(l / r)),
                    _ => Err(Error::MathError("Divide by zero".to_string())),
                }
            }
            BasicOp::Negate(val) => {
                let val = val.compile(variables)?;

                if let Some(val) = val.to_number() {
                    Ok(Box::new(-val))
                } else {
                    Ok(Box::new(BasicOp::Negate(val)))
                }
            }
        }
    }

    fn to_number(&self) -> Option<f32> {
        None
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
    fn eval(&self, variables: &[(&str, f32)]) -> Result<f32, Error> {
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

    fn compile<'b>(&'b self, variables: &[(&str, f32)]) -> Result<Box<dyn Expression + 'b>, Error> {
        let func = self
            .language
            .find_func(&self.name)
            .ok_or_else(|| Error::UndefinedFunction(self.name.clone()))?;

        let compiled_args = self
            .args
            .iter()
            .map(|arg| arg.compile(variables))
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(num_args) = compiled_args
            .iter()
            .map(|a| a.to_number())
            .collect::<Option<Vec<_>>>()
        {
            Ok(Box::new(func.eval(&num_args)?))
        } else {
            Ok(FunctionExpression::new_expression(
                self.language,
                compiled_args,
                self.name.clone(),
            ))
        }
    }

    fn to_number(&self) -> Option<f32> {
        None
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
    assert_eq!(add.eval(&[("x", 3.0)]), Ok(1027.0));
}
