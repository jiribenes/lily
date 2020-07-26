int* foo(int* x) {
    // Lily does not understand this code properly
    // if we had borrowing implemented, this kind of assignment would
    // be a single-user mutable borrow, which would make this OK!
    *x = *x + 2;
    return x;
}
