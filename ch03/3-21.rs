use std::ops;

#[derive(Debug)]
struct Rational {
    numer: i32,
    denom: i32
}

fn gcd(a: &i32, b: &i32) -> i32 {
    if *b == 0 {
        *a
    } else {
        let r = *a % *b;
        gcd(b, &r)
    }
}

fn rational(numer: i32, denom: i32) -> Rational {
    let g = gcd(&numer, &denom);
    Rational { 
        numer : numer / g,
        denom : denom / g
    }
}

impl ops::Add<Rational> for Rational {
    type Output = Rational;

    fn add(self, rhs: Rational) -> Rational {
        rational(self.numer * rhs.denom + self.denom * rhs.numer, self.denom * rhs.denom)
    }
}

impl ops::Sub<Rational> for Rational {
    type Output = Rational;

    fn sub(self, rhs: Rational) -> Rational {
        rational(self.numer * rhs.denom - self.denom * rhs.numer, self.denom * rhs.denom)
    }
}

impl ops::Mul<Rational> for Rational {
    type Output = Rational;

    fn mul(self, rhs: Rational) -> Rational {
        rational(self.numer * rhs.numer, self.denom * rhs.denom)
    }
}

impl ops::Div<Rational> for Rational {
    type Output = Rational;

    fn div(self, rhs: Rational) -> Rational {
        rational(self.numer * rhs.denom, self.denom * rhs.numer)
    }
}


fn main() {
    println!("{:?}", rational(2, 4) + rational(1, 2));
}
