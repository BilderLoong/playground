mod common;

#[test]
fn it_adds_two() {
    common::setup();
    common::mod2::bar();
    assert_eq!(4, adder::add(2, 2));
}
