bool ordinato(int[] a) {
    bool result = true;
    int i = 0;
    while (result && i < a.length - 1) {
        if (a[i] > a[i + 1]) {
            result := false;
        }
        i := i + 1;
    }
    return result;
}
bool r = ordinato([1, 2, 3, 4, 5]);