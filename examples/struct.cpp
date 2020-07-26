#include <utility>

struct Point {
    int x;
    int y;

    Point(int _x, int _y) {
        this->x = _x;
        this->y = _y;
    }
};

Point makePoint(int a, int b) {
    Point pt = Point(a, b);
    return pt;
}

// // Uncomment to produce a error of type '(Un (RRef Point))'
//
//void forgetPoint(Point&& x) {
//    return;
//}

void copyPoint(const Point& x) {
    return;
}

int main() {
    Point p = makePoint(1, 2);
    copyPoint(p);
    // forgetPoint(std::move(p)); uncomment to produce an error
    //
    // We "forget" to deal with Point here, producing a 'Un Point' error.
    // This might be fine, since point is a small type, but if it was 2 MB big, it might be a problem!
    return 1;
}
