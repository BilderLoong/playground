namespace Birudo {
  export interface Foo {
    name: string;
  }

  export const a = 1;
}

const bar: Birudo.Foo = {
  name: 'bar',
};

const b = Birudo.a;
