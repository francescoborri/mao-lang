ordina_binaria(int[] a) {          
    int i, j;
    while (i < a.length) {
        if (a[i] == 0) {
            int temp = a[j];
            a[j] := a[i];
            a[i] := temp;
            j := j + 1;
        }
        i := i + 1;
    }
}
ordina_binaria([0, 1, 0, 1, 1, 0]);