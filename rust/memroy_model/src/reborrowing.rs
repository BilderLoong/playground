fn main() {
    let mut x = 42;
    let r = &mut x;

    // Case A: Moving the reference
    // let moved_r = r;
    // println!("{}", r); // COMPILE ERROR: use of moved value `r`

    // Case B: Explicit reborrow
    // let reborrowed_r = &mut *r;
    
    // let b = &mut x;
 1   
    // Once `reborrowed_r` goes out of scope/stops being used, `r` is active again
    // *r += 1;

    // println!("{}", r); // prints 44
}
