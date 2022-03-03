use std::ops;
use std::time::{Instant};

#[derive(Debug)]
struct Rational {
    numer: i32,
    denom: i32,
}

impl Rational {
    fn gcd(a: i32, b: i32) -> i32 {
        if b == 0 {
            a
        } else {
            let r = a % b;
            Rational::gcd(b, r)
        }
    }

    fn new(numer: i32, denom: i32) -> Rational {
        let g = Rational::gcd(numer, denom);
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

    fn from2(value: f64, error_bound: f64) -> Rational {

        let mut lower_bound = Rational::new(0, 1);
        let mut upper_bound = Rational::new(1, 0);

        loop {
            let m: Rational = Rational::new(
                lower_bound.numer + upper_bound.numer,
                lower_bound.denom + upper_bound.denom,
            );
            let m_value: f64 = m.value();
            if m_value < value - error_bound {
                let c = Rational::parametric_search(true, &m, &upper_bound, value, error_bound);
                lower_bound = Rational::new(m.numer + (c-1) * upper_bound.numer, m.denom + (c-1) * upper_bound.denom);
            } else if m_value > value + error_bound {
                let c = Rational::parametric_search(false, &m, &lower_bound, value, error_bound);
                upper_bound = Rational::new(m.numer + (c-1) * lower_bound.numer, m.denom + (c-1) * lower_bound.denom);
            } else {
                break Rational::new(m.numer, m.denom);
            }
        }
    }

    
    fn parametric_search(direction: bool, start: &Rational, end: &Rational, value: f64, error_bound: f64) -> i32 {
        let start_numer = start.numer;
        let start_denom = start.denom;
        let end_numer = end.numer;
        let end_denom = end.denom;

        let (end_bound, start_bound) = if end_numer > end_denom { 
            (end_numer, start_numer) 
        } else { 
            (end_denom, start_denom) 
        };
        let mut lower_bound: i32 = 1;
        let mut upper_bound: i32 = (i32::MAX - start_bound) / end_bound;
        loop {
            let mid: i32 = (lower_bound + upper_bound) / 2;
            let mid_value: f64 = Rational::new(start_numer + mid * end_numer, start_denom + mid * end_denom).value();
            
            if mid == upper_bound {
                break upper_bound;
            }
            if !direction && mid_value < value - error_bound {
                upper_bound = mid;
            } else if !direction && mid_value > value + error_bound {
                lower_bound = mid + 1;
            } else if direction && mid_value > value + error_bound {
                upper_bound = mid;
            } else if direction && mid_value < value - error_bound {
                lower_bound = mid + 1;
            }
            else {
                break mid;
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
    println!("{:?}\n", -Rational::new(2, 3));


    measure_time(Rational::from(6.4285714285, 0.000000001));
    measure_time(Rational::from2(6.4285714285, 0.000000001));

    measure_time(Rational::from(32_581_890.5384615384, 0.0000001));
    measure_time(Rational::from2(32_581_890.5384615384, 0.0000001));

    measure_time(Rational::from(0.00000680067, 0.000001));
    measure_time(Rational::from2(0.00000680067, 0.000001));
}


fn measure_time(r: Rational) {
    let start = Instant::now();
    println!("{:?}", r);
    let end = Instant::now();
    println!("{:?}", end.duration_since(start));
}


