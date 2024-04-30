fn main() {
    #[derive(Debug)]
    struct Point<X,Y> {
        x:X,
        y:Y
    }

    impl Point<f32,f32> {
        pub fn add(&self)-> f32{
           self.x+self.y 
        }
    }

    impl<X1,Y1> Point<X1,Y1>{
        pub fn mixup<X2,Y2>(self, p:Point<X2,Y2>)-> Point<X1,Y2>{
               Point{
                   x:self.x,
                   y:p.y
               } 
        }
    }
}
