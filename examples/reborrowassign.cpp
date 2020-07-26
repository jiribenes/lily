#include <utility>

struct Box {
    int _x;
    Box(int x) {
        this->_x = x;
    }
};

Box reborrowandassign(Box b, int&& newX) {
    Box local = b;
    // This causes a false positive since Lily does not understand
    // exclusive mutable borrows :(
    local._x = newX;
    return local;
}
