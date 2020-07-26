int* makeuseafterfree() {
    int* newly = new int;
    delete newly;
    return newly; // returns (uses) after deleting!
}

int* useafterfree(int* inp) {
    delete inp;
    return inp; // same as above
}

