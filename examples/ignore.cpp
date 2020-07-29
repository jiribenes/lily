struct Box {
    int x;
    int y;
    explicit Box(int _x, int _y) {
        this->x = _x;
        this->y = _y;
    }
};

void ignore(int x) {
    return;
}

void ignoreBox(Box x) {
    return;
}

int main() {
    // We can infer that 'ignored' _must_ have the 'Un' constraint on its return type!
    ignore(2);
    
    // For now, we correctly infer that 'main' needs @Un Box@.
    ignoreBox(Box(6, 6));

    return 0;
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
