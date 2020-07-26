#include <utility>

// ----------
// EXAMPLE 1
int moo(int&& x) {
    int x1 = std::move(x);
    int x2 = std::move(x); // moving a resource which has been previously moved!
    return x2;
}

// ----------
// EXAMPLE 2
// This function is fine!
int mootwo(int&& x) {
    int x1 = std::move(x);
    int x2 = std::move(x1);
    return x2;
}

// ---------
// EXAMPLE 3

int use(int x) {
    return x;
}

void useaftermove(int&& x) {
    int moved = std::move(x);
    use(x); // this should be detected by Lily (use after move)
}
