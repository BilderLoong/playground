fn main() {
    generate_workout(26, 2)
}

fn cache_result(calculate: impl Fn(u32) -> u32) -> impl FnMut(u32) -> u32 {
    let mut cached_res: Option<u32> = None;
    move |x| match cached_res {
        Some(v) => v,
        None => {
            let res = calculate(x);
            cached_res = Some(res);
            res
        }
    }
}

fn generate_workout(intensity: u32, random_number: u32) {
    let mut expensive_result = cache_result(|num| {
        println!("calculating slowly...");
        num + 1
    });

    if intensity < 25 {
        println!("Today, do {} pushups!", expensive_result(intensity));
        println!("Next, do {} situps!", expensive_result(intensity));
        return;
    }

    if random_number == 3 {
        println!("Take a break today! Remember to stay hydrated!");
        return;
    }

    println!("Today, run for {} minutes!", expensive_result(intensity));
}
