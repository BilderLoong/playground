pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn larger_can_hold_smaller() {
        let larger = Rectangle {
            width: 8,
            height: 7,
        };
        let smaller = Rectangle {
            width: 5,
            height: 1,
        };

        assert!(larger.can_hold(&smaller));
    }

    #[test]
    fn explosion() {
        let result = add(2, 2);
        assert_ne!(2, 3);
        // assert!(2 == 3);
    }

    #[test]
    #[should_panic]
    fn ba() {
        panic!("Boom");
    }
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

struct Solution;

impl Solution {
    // I             1
    // V             5
    // X             10
    // L             50
    // C             100
    // D             500
    // M             1000
    // I can be placed before V (5) and X (10) to make 4 and 9.
    // X can be placed before L (50) and C (100) to make 40 and 90.
    // C can be placed before D (500) and M (1000) to make 400 and 900.

    pub fn roman_to_int(s: String) -> i32 {
        s.chars().enumerate().fold(0, |acc, (index, c)| {
            acc + match (c, s.chars().nth(index + 1)) {
                ('I', Some('V') | Some('X')) => -1,
                ('X', Some('L') | Some('C')) => -10,
                ('C', Some('D') | Some('M')) => -100,
                ('I', _) => 1,
                ('V', _) => 5,
                ('X', _) => 10,
                ('L', _) => 50,
                ('C', _) => 100,
                ('D', _) => 500,
                ('M', _) => 1000,
                _ => 0,
            }
        })
    }
}
