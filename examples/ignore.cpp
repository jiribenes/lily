template <typename T>
T ignored(T x) {
    return x;
}


int main() {
    // we can infer that 'ignored' _must_ have the 'Un' constraint on its return type!
    ignored("some strings");
}
