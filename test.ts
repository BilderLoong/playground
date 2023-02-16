const a = {
  bar: 12,
  // foo(this: { bar: number; foo: () => void }) {
  foo() {
    this.foo();
    console.log(this.bar);
    // return '';
    return this;
  },
};

class Bar {
    return this;
  }
}
