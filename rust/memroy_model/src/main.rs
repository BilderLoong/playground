use core::borrow;
mod reborrowing;
mod dyn_trait;

struct Foo {
    name: String,
}
fn main() {
    let foo = Foo {
        name: "haha".to_string(),
    };

    let name = foo.name;

    let foo = Foo {
        name: "haha".to_string(),
    };
    let name = &foo.name;

    let foo2 = foo;

    struct Foo {
        name: String,
    }

    // let foo = Foo {
    //     name: "haha".to_string(),
    // };

    let name = &foo.name;
    let name = &(&foo).name;
    let name = foo.name;
    let name = foo.name;
    let name: String = (&foo).name;

    let s1 = "hello";
    // let s2 = s1[1..3];
    let s3 = &s1[1..3];
    // let s4 = String::
    let s4 = String::from("hello");
    let s5 = &s4[1..3];

    let bytes = [b'h', b'e', b'l', b'l', b'o'];
    
}
