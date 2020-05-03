int factorial(int n) {
    if (n == 0) {
        return 1;
    }
    else {
        return n * factorial(n-1);
    }
}

int even(unsigned int n);
int odd(unsigned int n);

int odd(unsigned int n) {
    if (n == 0) return 0;
    else return even(n - 1);
}

int even(unsigned int n) {
    if (n == 0) return 1;
    else if (n == 100000) return odd(1);
    else return odd(n - 1);
}

int square(unsigned int n) {
    return n * n;
}
