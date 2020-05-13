int* ptrey() {
    return new int[7];
}

struct Point {
    int x;
    int y;
};

Point* pointee() {
    Point* pt = new Point;
    return pt;
}
