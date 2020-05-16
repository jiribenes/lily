struct Vec {
        Vec() {
            this->data = nullptr;
            this->length = 0;
            this->capacity = 0;
        }    

        void add(int something) {
            if (length >= capacity) {
                this->capacity *= 2;
                int* bigger_data = new int[this->capacity];
                for (int i = 0; i < length; ++i) {
                    bigger_data[i] = this->data[i];
                }
                delete this->data;
                this->data = bigger_data;
            }
        }

        int* ref(int index) const {
            return &this->data[index];
        }

        int* data;
        int length;
        int capacity;
};

int* unsugref(Vec v, int index) {
    return v.ref(index);
}

int* justReadin(Vec v) {
    return unsugref(v, 1);
}

int doubleIt(int num, int voidy) {
    return num * num + 2;
}

int usesDoubleIt(int othernum) {
    return doubleIt(othernum, 2);
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
