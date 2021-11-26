#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }

    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_fit(&self, rect: &Rectangle) -> bool {
        self.width >= rect.width && self.height >= rect.height
    }
}

fn main() {
    let scale = 2;
    let rect = Rectangle {
        width: dbg!(30 * scale),
        height: 50,
    };
    let square = Rectangle::square(40);

    println!(
        "The area of the {:?} is {} square pixels",
        rect,
        rect.area()
    );

    println!(
        "{:?} fits inside {:?}: {}",
        square,
        rect,
        rect.can_fit(&square)
    )
}
