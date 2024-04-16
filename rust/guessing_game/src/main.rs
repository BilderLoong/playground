use rand::Rng;
use std::cmp::Ordering;
use std::io;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1..101);

    println!("The secret number is: {}", secret_number);

    println!("Guess the number!");

    guessing(&secret_number);
}

fn guessing(secret_number: &u32) {
    println!("Please input your guess."); // ほら、予想を入力してね

    let mut guess = String::new();

    io::stdin().read_line(&mut guess).expect("Failed");

    let guess: u32 = match guess.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            guessing(&secret_number);
            return;
        }
    };

    println!("You guessed: {}", guess);

    match guess.cmp(&secret_number) {
        Ordering::Less => {
            println!("Too small!");
            guessing(&secret_number);
        }

        Ordering::Greater => {
            println!("Too big!");
            guessing(&secret_number);
        }
        Ordering::Equal => {
            println!("You win!");
        }
    }
}
