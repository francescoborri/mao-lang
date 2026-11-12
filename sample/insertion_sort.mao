insertion_sort(int[] a) {
    int i = 1;
    while (i < a.length) {
        int item = a[i];
        int j = i - 1;
        while (j >= 0 && a[j] > item) {
            a[j + 1] := a[j];
            j := j - 1;
        }
        a[j + 1] := item;
        i := i + 1;
    }
}
insertion_sort([5, 2, 4, 6, 1, 3]);