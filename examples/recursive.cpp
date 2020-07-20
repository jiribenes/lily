int rec(int x) {
    if (x == 1) {
        return x;
    }
    return rec(x);
}

int main() {
    return 1;
}

int add(int x, int y) {
    int result = x + y;
    return result;
}

void test(int x) {
    int result = x * x;
}

int factorial2(int n) {
    if (n == 0) {
        return 1;
    }
    else {
        return n * factorial2(n-1);
    }
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
    //else if (n == 100000) return odd(1);
    else return odd(n - 1);
}

int square(int n) {
    return n * n;
}

int uses_even(int n) {
    return even(n) * 2;
}

int fibbonacci(int n) {
   if (n == 0){
      return 0;
   } else if (n == 1) {
      return 1;
   } else {
      return fibbonacci(n-1) + fibbonacci(n-2);
   }
}
