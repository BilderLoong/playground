fn main() {
    let x = five();
    println!("The value of x is: {}", x); // xの値は{}です
    let user = User {
        email: String::from("someone@example.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_count: 1,
    };

    let mut user1 = User {
        email: String::from("someone@example.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_count: 1,
    };

    user1.email = String::from("bar");

    let user3 = build_user(String::from("birudo"), String::from("123"));

    let color = Color(0, 0, 0);
    let point = Point(0, 0, 0);
    print!("{}", color.0);
}

fn build_user(username: String, email: String) -> User {
    User {
        username,
        email,
        active: true,
        sign_in_count: 1,
    }
}

fn five() -> String {
    let mut s = String::from("hello");
    let r1 = &mut s;
    print!("{}", r1);
    change(&mut s);
    s
}

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}

struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

struct Color(i32, i32, i32);
struct Point(i32, i32, i32);
