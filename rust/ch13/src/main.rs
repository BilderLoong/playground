fn main() {
}

struct Counter {
    counter: i32,
}

impl Counter {
    fn new() -> Counter {
        Counter { counter: 0 }
    }
}

impl Iterator for Counter {
    type Item = i32;
    fn next(&mut self) -> Option<Self::Item> {
        self.counter += 1;
        if self.counter < 6 {
            Some(self.counter)
        } else {
            None
        }
    }
}
