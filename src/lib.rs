mod expr;
mod parse;

pub use expr::*;
use parse::*;

pub fn parse<'a>(expr: &str, language: &'a dyn Language) -> Option<Box<dyn Expression + 'a>> {
    tokenize(expr).and_then(|tokens| parse_expr(&tokens, language))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_no_func() {
        let expr = "1+2";
        let lang = DefaultLanguage::new(vec![]);

        assert_eq!(parse(expr, &lang).map(|e| e.eval(&[])), Some(Ok(3.0)));

        let expr = "x-10";
        assert_eq!(
            parse(expr, &lang).map(|e| e.eval(&[("x", &10.0)])),
            Some(Ok(0.0))
        );

        let expr = "122+904-23.1*(72-x/4)";
        assert_eq!(
            parse(expr, &lang).map(|e| e.eval(&[("x", &8.0)])),
            Some(Ok(122.0 + 904.0 - 23.1 * (72.0 - 8.0 / 4.0)))
        );
    }

    #[test]
    fn language_test() {
        use super::expr::ClosureFunction;

        let expr = "pow(2,x)";
        let default_lang = DefaultLanguage::default();
        let empty_lang = DefaultLanguage::new(vec![]);
        let my_lang = DefaultLanguage::new(vec![ClosureFunction::new_function(
            "sum".to_string(),
            |args| Ok(args.iter().fold(0.0, |a, b| a + b)),
        )]);

        assert_eq!(
            parse(expr, &default_lang).map(|e| e.eval(&[("x", &10.0)])),
            Some(Ok(1024.0))
        );

        assert_eq!(
            parse(expr, &empty_lang).map(|e| e.eval(&[("x", &10.0)])),
            None
        );

        assert_eq!(
            parse("sum(x,y,z)", &my_lang).map(|e| e.eval(&[("x", &1.0), ("y", &2.0), ("z", &3.0)])),
            Some(Ok(6.0))
        );
    }

    #[test]
    fn complicated_expression() {
        fn vec_len(coords: &[f32]) -> f32 {
            coords.iter().fold(0.0, |a, b| a + b * b).sqrt()
        }
        let expr = "sin(cos(x-sqrt(3+2-x))+vec_len(1,2,3,4,y,6))-pow(1.1,(10-y)*x+y)";
        let actual = |x, y| {
            f32::sin(
                f32::cos(x - f32::sqrt(3.0 + 2.0 - x)) + vec_len(&[1.0, 2.0, 3.0, 4.0, y, 6.0]),
            ) - f32::powf(1.1, (10.0 - y) * x + y)
        };

        let x = 1.0;
        let y = 2.5;

        assert_eq!(
            parse(expr, &DefaultLanguage::default()).map(|e| e.eval(&[("x", &x), ("y", &y)])),
            Some(Ok(actual(x, y)))
        )
    }
}
