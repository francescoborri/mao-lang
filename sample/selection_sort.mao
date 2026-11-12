selection_sort(int[] a) {
    int index_min(int[] a, int from) {
        int min = from;
        int j = from + 1;
        while (j < a.length) {
            if (a[j] < a[min]) {
                min := j;
            } else {
                skip;
            }
            j := j + 1;
        }
        return min;
    }
    int i = 0;
    while (i < a.length) {
        int min = index_min(a, i);
        int temp = a[i];
        a[i] := a[min];
        a[min] := temp;
        i := i + 1;
    }
}
selection_sort([5, 2, 4, 6, 1, 3]);