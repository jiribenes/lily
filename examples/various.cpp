int doubleIt(int num, int voidy) {
    return num * num + 2;
}

int usesDoubleIt(int othernum) {
    return doubleIt(othernum, 2);
}

int deref(int* ptr) {
    return *ptr;
}

int* unsafeReference(int index) {
    auto local = new int[3];
    // This is _not_ detected by Lily!
    return &local[index];
}

bool compare(int x, int y) {
    return x == y;
}

using IntToInt = int(*)(int);
int compose(IntToInt f, IntToInt g, int x) {
    return f(g(x));
}

// the K combinator
int konst(int a, int b) {
    return a;
}
