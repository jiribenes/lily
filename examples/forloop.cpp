// This is a representation of a for-loop
using TIntoVoid = void(*)(int);
void forloop(int i, int j, TIntoVoid f) {
    if (i == j) {
        return;
    } else {
        f(i);
        forloop(i + 1, j, f);
    }
}

void fakeprint(int i) {
    // pretend that this function actually prints
    // its argument to the standard output
    return;
}

// This code is equivalent to:
// 
// ```
// void printrange(int upper) {
//     for (int i = 0; i < upper; ++i) {
//         fakeprint(i);
//     }   
// }
// ```
void printrange(int upper) {
    forloop(0, upper, fakeprint);
}

