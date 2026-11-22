int trova_mancante(int[] a) {
    int n = a.length;
    int totale = ((n + 1) * (n + 2)) / 2;
    int i, somma;
    while (i < n) {
        somma, i := somma + a[i], i + 1;
    }
    return (totale - somma);
}
int r = trova_mancante([2, 3, 1, 5]);
