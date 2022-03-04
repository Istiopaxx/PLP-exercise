fn main() {
    let addone_and_multwo = compose(|x: i32| x + 1, |x: i32| x * 2);
    println!("{}", addone_and_multwo(2));

    let v = vec![1, 2, 3, 4, 5, 6];

    let ret = filter(&v, |x| x % 2 == 0);
    println!("{:?}", ret);

    let ret = map(|x| x * 2, &v);
    println!("{:?}", ret);

}

fn filter<A: Clone>(list: &Vec<A>, predicate: impl Fn(&A) -> bool) -> Vec<A> {
    let mut result = Vec::new();
    for e in list.iter() {
        if predicate(e) {            
            result.push((*e).clone());
        }
    }
    result
}

fn compose<A, B, C>(f: impl Fn(A) -> B, g: impl Fn(B) -> C) -> impl Fn(A) -> C {
    move |x| g(f(x))
}

fn map<A, B>(f: impl Fn(&A) -> B, list: &Vec<A>) -> Vec<B> {
    let mut result = Vec::new();
    for e in list.iter() {
        result.push(f(e));
    }
    result
}
