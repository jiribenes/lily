int even(int n, int* importantptr);
int odd(int n, int* importantptr);

int odd(int n, int* importantptr) {
    if (n == 0) {
        delete importantptr;
        return *importantptr;
    }
    else return even(n - 1, importantptr);
}

int even(int n, int* importantptr) {
    if (n == 0) return *importantptr;
    else return odd(n - 1, importantptr);
}

