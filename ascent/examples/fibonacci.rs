//! Conditional `if` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(isize);

    // Rules:

    relation fib(isize, isize);
    relation fib_table(isize, isize, isize, isize, isize);

    fib(0, 1) <-- number(0);
    fib(1, 1) <-- number(1);
    fib_table(x, x - 1, y, x - 2, z), fib(x, y + z) <-- number(x), if *x >= 2, fib(x - 1, y), fib(x - 2, z);
    // basically collect the queries on the RHS into a tuple, so
    // - number(x) -> x
    // - fib(x - 1, y) -> x - 1, y
    // - fib(x - 2, z) -> x - 2, z
    // and in total we get x, x-1, y, x-2, z
    // fib_table(x, x - 1, y, x - 2, z) <-- number(x), if *x >= 2, fib(x - 1, y), fib(x - 2, z);
}

fn main() {
    let mut prog = AscentProgram::default();

    prog.number = (0..6).map(|n| (n,)).collect();

    prog.run();

    let AscentProgram { mut fib, mut fib_table, .. } = prog;

    fib.sort_by_key(|(key, _)| *key);
    fib_table.sort_by_key(|(key, _, _, _, _)| *key);

    assert_eq!(fib, vec![(0, 1), (1, 1), (2, 2), (3, 3), (4, 5), (5, 8),]);
    println!("{:?}", fib_table);
}
