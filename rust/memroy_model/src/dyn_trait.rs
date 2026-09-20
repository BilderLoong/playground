use std::future::Future;

trait AnimalAsync {
    async fn speak(&self) -> String;
}

trait AnimalAsyncCorrect {
    fn speak(&self) -> Box<dyn Future<Output = String>>;
}

trait Animal {
    fn speak(&self) -> String;
}

struct Dog;

impl AnimalAsync for Dog {
    async fn speak(&self) -> String {
        "".to_owned()
    }
}
struct Cat;

impl AnimalAsync for Cat {
    async fn speak(&self) -> String {
        "".to_owned()
    }
}

struct Duck;
impl Animal for Duck {
    fn speak(&self) -> String {
        "".to_string()
    }
}

struct Chicken;
impl Animal for Chicken {
    fn speak(&self) -> String {
        "".to_string()
    }
}

impl AnimalAsyncCorrect for Dog {
    fn speak(&self) -> Box<dyn Future<Output = String>> {
        Box::new(async { "".to_string() })
    }
}

fn run() {
    let cat = Cat;
    let future = cat.speak();

    let cat = Cat;
    let animal = Box::new(cat);
    let future = animal.speak();

    // This is allowed
    let animal: Box<dyn Animal> = if 1 != 0 {
        Box::new(Duck)
    } else {
        Box::new(Chicken)
    };

    let speaking = animal.speak();

    let cat = Cat;
    let animal: Box<dyn AnimalAsync> = Box::new(cat);
    let animal: &dyn AnimalAsync = &cat;
    // Cause the speak() may return difference complier generated future type, it not possible to
    // determine the future varibale type here.
    let future = animal.speak();

    let dog = Dog;
    let animal: Box<dyn AnimalAsyncCorrect> = Box::new(dog);
    let animal: &dyn AnimalAsyncCorrect = &dog;
    // Correct cause now we use a uniform type to describe the future.
    let future = animal.speak();
}

fn boo() {
    trait AudioAdapter {
        // Rust can't know how big the future is, so this trait
        // cannot be used as `dyn AudioAdapter`. Compile error:
        // "the trait is not dyn compatible"
        async fn obtain(&self) -> Vec<u8>;
    }

    struct Jisho;
    struct Imported;
    impl AudioAdapter for Jisho {
        async fn obtain(&self) -> Vec<u8> {
            vec![]
        }
    }

    let adapter = Box::new(Jisho);

    let adapter: Box<dyn AudioAdapter> = Box::new(Jisho);

    impl Imported {
        async fn obtain(&self) -> Vec<u8> {
            vec![]
        }
    }
    let jisho = Jisho;
    let imported = Imported;

    let f1 = jisho.obtain();
    let f2 = imported.obtain();
}
