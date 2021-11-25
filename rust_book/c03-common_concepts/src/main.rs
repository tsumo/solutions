fn main() {
    mutability();
    println!("===");
    shadowing();
    println!("===");
    tuples();
    println!("===");
    arrays();
    println!("===");
    println!("10 fahr = {} cels", fahr2cels(10.0));
    println!("-12.2 cels = {} cels", cels2fahr(-12.2));
    println!("10 cels = {} fahr", cels2fahr(10.0));
    println!("50 fahr = {} cels", fahr2cels(50.0));
    println!("===");
    print!("fib: ");
    for i in 0..10 {
        print!("{} ", fib(i))
    }
    print!("\n");
}

fn mutability() {
    let mut x = 5;
    println!("The value of x is: {}", x);
    x = 6;
    println!("The value of x is: {}", x);
}

fn shadowing() {
    let x = 5;
    let x = x + 1;
    {
        let x = x * 2;
        println!("The value of x in the inner scope is: {}", x);
    }
    println!("The value of x is: {}", x);
}

fn tuples() {
    let tup: (i32, f64, u8) = (500, 6.4, 1);
    let (_x, y, _z) = tup;
    println!("The value of x is: {}", tup.0);
    println!("The value of y is: {}", y);
}

fn arrays() {
    let a1: [i32; 5] = [1, 2, 3, 4, 5];
    let a2 = [0; 5];
    println!("The value of a1 is: {:?}", a1);
    println!("The value of a2 is: {:?}", a2);
}

fn fahr2cels(fahr: f32) -> f32 {
    (fahr - 32.0) * (5.0 / 9.0)
}

fn cels2fahr(cels: f32) -> f32 {
    cels * (9.0 / 5.0) + 32.0
}

fn fib(n: i32) -> i32 {
    if n == 0 || n == 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
