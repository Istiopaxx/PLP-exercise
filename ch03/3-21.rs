use std::ops;

#[derive(Debug)]
struct Rational {
    numer: i64,
    denom: i64,
}

impl Rational {
    fn gcd(a: &i64, b: &i64) -> i64 {
        if *b == 0 {
            *a
        } else {
            let r = *a % *b;
            Rational::gcd(b, &r)
        }
    }

    fn new(numer: i64, denom: i64) -> Rational {
        let g = Rational::gcd(&numer, &denom);
        Rational {
            numer: numer / g,
            denom: denom / g,
        }
    }

    fn value(&self) -> f64 {
        self.numer as f64 / self.denom as f64 
    }

    fn from(value: f64, error_bound: f64) -> Rational {
        let mut lower_bound = Rational::new(0, 1);
        let mut upper_bound = Rational::new(1, 0);

        loop {
            let m: Rational = Rational::new(
                lower_bound.numer + upper_bound.numer,
                lower_bound.denom + upper_bound.denom,
            );
            let m_value: f64 = m.value();
            if m_value < value - error_bound {
                lower_bound = Rational::new(m.numer, m.denom);
            } else if m_value > value + error_bound {
                upper_bound = Rational::new(m.numer, m.denom);
            } else {
                break Rational::new(m.numer, m.denom);
            }
        }
    }
}

impl ops::Add<Rational> for Rational {
    type Output = Rational;

    fn add(self, rhs: Rational) -> Rational {
        Rational::new(
            self.numer * rhs.denom + self.denom * rhs.numer,
            self.denom * rhs.denom,
        )
    }
}

impl ops::Sub<Rational> for Rational {
    type Output = Rational;

    fn sub(self, rhs: Rational) -> Rational {
        Rational::new(
            self.numer * rhs.denom - self.denom * rhs.numer,
            self.denom * rhs.denom,
        )
    }
}

impl ops::Mul<Rational> for Rational {
    type Output = Rational;

    fn mul(self, rhs: Rational) -> Rational {
        Rational::new(self.numer * rhs.numer, self.denom * rhs.denom)
    }
}

impl ops::Div<Rational> for Rational {
    type Output = Rational;

    fn div(self, rhs: Rational) -> Rational {
        Rational::new(self.numer * rhs.denom, self.denom * rhs.numer)
    }
}

impl ops::Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Rational::new(-self.numer, self.denom)
    }
}

fn main() {
    println!("{:?}", Rational::new(2, 5) / Rational::new(1, 2));
    println!("{:?}", Rational::new(2, 7) * Rational::new(1, 2));
    println!("{:?}", Rational::new(2, 3) + Rational::new(1, 2));
    println!("{:?}", Rational::new(8, 5) - Rational::new(1, 2));
    println!("{:?}", -Rational::new(2, 3));
    println!("{:?}", Rational::from(6.4285714285, 0.000000001));
}
