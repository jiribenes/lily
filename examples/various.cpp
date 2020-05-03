template <typename T>
class Vec {
    public:
        Vec() : data(nullptr), length(0), capacity(0) {}

        void add(T something) {
            if (length >= capacity) {
                capacity *= 2;
                T* bigger_data = new T[capacity];
                for (int i = 0; i < length; ++i) {
                    bigger_data[i] = data[i];
                }
                delete data;
                data = bigger_data;
            }
        }

        T* ref(int index) const {
            return &data[index];
        }

    private:
        T* data;
        int length;
        int capacity;
};
;

int* unsugref(Vec<int> v, int index) {
    return v.ref(index);
}

int* justReadin(Vec<int> v) {
    return unsugref(v, 1);
}

int doubleIt(int num, char voidy) {
    return num * num + 2;
}

int usesDoubleIt(int othernum) {
    return doubleIt(othernum, 'o');
}

int woo(int* moo) {
    return *moo;
}

int* unsafeThingy(int booyah) {
    auto local = new int[3];
    return &local[booyah];
}

int compare(int x, int y) {
    return x == y;
}

using IntToInt = int(*)(int);

int compose(IntToInt f, IntToInt g, int x) {
    return f(g(x));
}

template <typename T, typename U, typename Z>
T konst(T a, U b) {
    return a;
}
