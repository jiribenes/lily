template <typename T>
T ignored(T x) {
    return x;
}

struct Box {
    int x;
    int y;
    explicit Box(int _x, int _y) {
        this->x = _x;
        this->y = _y;
    }
};

int main() {
    // we can infer that 'ignored' _must_ have the 'Un' constraint on its return type!
    ignored("some strings");

    ignored(2);
    
    // TODO: this case should fail in the checking phase (which is TODO)
    // because 'Box' is not semantically 'Un'restricted.
    // Therefore this could potentially leak resources!
    //
    // For now, we correctly infer that 'main' needs @Un Box@.
    // We just have to throw the error!
    ignored(Box(6, 6));
}

int retFirst(Box& box) {
    return box.x;
}

int retSecond(Box& box) {
    return box.y;
}

int boxey(Box& box) {
    int a = retFirst(box);
    int b = retSecond(box);
    return a + b;
}
