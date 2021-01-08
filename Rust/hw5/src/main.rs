mod hw5;

use std::collections::HashMap;

fn main() {
    // TODO: Review the provided methods of the Iterator trait:
    //  https://doc.rust-lang.org/std/iter/trait.Iterator.html
    //
    // We can access an iterator via `.iter()` and `.iter_mut()`
    // for immutable and mutable iterators, respectively.
    //
    // Option<T> is Rust's equivalent of Haskell's Maybe type.
    // Option can be either Some(x) or None, where x is the
    // value stored in the Option. In Rust, most operations that
    // can fail (such as indexing) will return an Option type
    // (similar to Haskell), so it is important to become
    // comfortable working with Options.

    // The following example shows you several ways to index
    // vectors and access elements in a hash map.

    //
    // Vectors
    //
    let v = vec![1, 2, 3];

    // Get element at index 1 -- this will succeed
    if let Some(x) = v.get(1) {
        println!("v[1] = {}", x);
    }

    // Get element at index 100 - this will fail
    // Prefixing the variable x with an underscore tells the compiler
    // I acknowledge that x is unused. Without it, the compiler will give
    // a warning that x is unused.
    if let Some(_x) = v.get(100) {
        println!("You will never see this message!");
    }

    // Alternatively, we can use pattern matching
    let x1 = match v.get(1) {
        // Here, the x in Some(x) is actually a _reference_ to the
        // corresponding element in v, so we need to _dereference_ it
        // using *. This results in us returning a copy of x.
        Some(x) => *x,

        // In the prior examples, we conditionally bound x (when we got Some(x)),
        // but in this match statement, we are unconditionally binding to x1,
        // which means we have to provide a defaut value.
        None => 0,
    };

    // Alternatively, we can use unwrap_or
    let x1p = v.get(1).unwrap_or(&0);

    let x2 = match v.get(100) {
        Some(x) => *x, // Dereference x
        None => 0, // Provide a default value if we get None
    };

    let x2p = v.get(100).unwrap_or(&0);

    println!("Got x1 = {} and x2 = {}", x1, x2);
    println!("Also got x1p = {} and x2p = {}", x1p, x2p);

    // We can also match on tuples
    match (v.get(0), v.get(10)) {
        (Some(a), Some(b)) => println!("Got a = {} and b = {}", a, b),
        (Some(a), None) => println!("Got a = {} but b was out of index", a),
        _ => println!("All other cases!"),
    }

    //
    // Hash Maps
    //

    // First, note that we had to import std::collections::HashMap on line 3
    // Unlike Vec, HashMap needs to be imported.

    // Create a mutable hash map
    let mut m1 = HashMap::new();
    m1.insert("one".to_string(), 1);
    m1.insert("two".to_string(), 2);
    m1.insert("three".to_string(), 3);

    // Or create an immutable hash map from a list of tuples
    let m2: HashMap<String, i32> = [
        ("one".to_string(), 1),
        ("two".to_string(), 2),
        ("three".to_string(), 3),
    ]
    .iter()
    .cloned()
    .collect();

    if let Some(x) = m1.get("one") {
        println!("m1[one] = {}", x);
    }

    match m2.get("four") {
        Some(x) => println!("Somehow we got {}", x),
        None => println!("m2 does not contain a key of four."),
    };


    // One last handy trick with HashMaps:
    println!("m1 = {:?}", m1);
    {
        // Note: I have to create this scope so that entryRef goes out
        // of scope before I create entryRef2. Both of these are mutable
        // references to the internals of m1.
        let entry_ref = m1.entry("two".to_string()).or_insert(2);
        println!("m1[two] = {}", entry_ref);
    }
    let entry_ref2 = m1.entry("four".to_string()).or_insert(3);
    println!("entry_ref2 before incrementing: {}", entry_ref2);

    // We can increment the value of the key-value pair if we want
    *entry_ref2 += 1;
    println!("entryRef2 after incrementing: {}", entry_ref2);
    println!("m1 = {:?}", m1);
}
