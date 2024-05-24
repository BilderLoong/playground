#[cfg(test)]
mod test {
    #[test]
    fn iterator_test() {
        let v1 = vec![1, 2, 3, 4];
        let mut iter = v1.iter();
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
    }
}
