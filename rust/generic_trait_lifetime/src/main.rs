fn main() {
    fn main() {
        let s: &'static str = "I have a static lifetime.";
        struct Bar<'a> {
            part: &'a str,
        }

        {
            {
                let r;
                let x = 5;
                r = &x;
                println!("r: {}", r);
            }
        }
        let string1 = String::from("abcd");
        let string2 = "xyz";
        let s = string1.as_str();
        let s = &string1;

        fn longest1<'a>(x: &'a String, y: &'a String) -> &'a String {
            if x.len() > y.len() {
                x
            } else {
                y
            }
        }

        fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
            if x.len() > y.len() {
                x
            } else {
                y
            }
        }

        let s1 = "123".to_string();
        let res;
        {
            let s2 = "asdfasd".to_string();
            res = longest(s1.as_str(), s2.as_str());
            println!("{}", res);
        }

        let foo;
        {
            let bar = "123";
            foo = bar;
        }

        println!("{}", foo);

        fn longest2<'a>(x: &'a str, y: &str) -> &'a str {
            x
            /* y */
        }

        // let foo;
        // {
        //     let _bar = "123".to_string();
        //     let bar = _bar.as_str();
        //     foo = bar;
        // }
        //
        // println!("{}", foo);
    }

    #[derive(Debug)]
    struct Point<X, Y> {
        x: X,
        y: Y,
    }

    impl Point<f32, f32> {
        pub fn add(&self) -> f32 {
            self.x + self.y
        }
    }

    impl<X1, Y1> Point<X1, Y1> {
        pub fn mixup<X2, Y2>(self, p: Point<X2, Y2>) -> Point<X1, Y2> {
            Point { x: self.x, y: p.y }
        }
    }

    pub trait Bar {
        fn bar(&self, name: String) -> String;
    }

    impl<X, Y> Bar for Point<X, Y> {
        fn bar(&self, name: String) -> String {
            format!("{}", name)
        }
    }

    struct Pair<T> {
        x: T,
        y: T,
    }

    // impl Bar for Pair<i32> {
    //     fn bar(&self, name: String) -> String {
    //         format!("{}", name)
    //     }
    // }
    //
    impl<T> Bar for Pair<T> {
        fn bar(&self, name: String) -> String {
            format!("{}", name.to_string())
        }
    }

    let pair = Pair { x: 1, y: 2 };
    pair.bar("s".to_string());
    let pair = Pair { x: "1", y: "2" };
    // pair.bar("s".to_string());

    let p = Point { x: 1, y: 2 };
    p.bar(" a ".to_string());

    fn dada() -> impl Bar {
        Point { x: 1, y: 2 }
    }

    fn papa(x: impl Bar) -> String {
        x.bar("asdf".to_string())
    }

    assert!(is_valid("()".to_string()), "()");
}

fn is_valid(s: String) -> bool {
    s.chars()
        .fold(Vec::new(), |mut acc, cur| {
            match (cur, acc.last()) {
                (')', Some('(')) | ('}', Some('{')) | (']', Some('[')) => {
                    acc.pop();
                }
                _ => acc.push(cur),
            };
            acc

        })
        .is_empty()
}

struct Solution;
impl Solution {
    pub fn is_valid(s: String) -> bool {
        fn is_match(s: &char, v: Option<&char>) -> bool {
            match (s, v) {
                ('(', Some(')')) | ('{', Some('}')) | ('[', Some(']')) => true,
                _ => false,
            }
        }

        s.chars()
            .fold(Vec::new(), |mut acc, cur| {
                if cur == '(' || cur == '{' || cur == '[' {
                    acc.push(cur);
                } else if is_match(&cur, acc.last()) {
                    acc.pop();
                } else {
                    acc.push(cur);
                }
                acc
            })
            .len()
            == 0
    }
}
