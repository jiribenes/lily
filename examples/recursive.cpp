int rec(int x) {
    if (x == 1) {
        return x;
    }
    return rec(x);
}

int factorial(int n) {
    if (n == 0) {
        return 1;
    }

    return n * factorial(n-1);
}

int even(int n);
int odd(int n);

int odd(int n) {
    if (n == 0) return 0;
    else return even(n - 1);
}

int even(int n) {
    if (n == 0) return 1;
    else return odd(n - 1);
}

int square(int n) {
    return n * n;
}

int uses_even(int n) {
    return even(n) * 2;
}
