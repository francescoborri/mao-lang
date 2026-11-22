int azzera_negativi(int[] a) {
    int counter = 0;
    int i = 0;
    while (i < a.length) {
        if (a[i] < 0) {
            a[i] := 0;
            counter := counter + 1;
        }
        i := i + 1;
    }
    return counter;
}
int r = azzera_negativi([1, -1, 2, -2, 3, -3]);
