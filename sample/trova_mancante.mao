int trova_mancante(int[] a) {      
    int n = a.length;
    int somma = ((n + 1) * (n + 2)) / 2;
    int i, s;
    while (i < n) {
        s, i := s + a[i], i + 1;
    }
    return (somma - s);
}
int result = trova_mancante([2, 3, 1, 5]);