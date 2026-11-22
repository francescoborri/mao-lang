int tail_conta_maggiori(int[] a, int k) {
    int aux(int[] a, int k, int i, int acc) {
        int r = acc;
        if (i < a.length) {
            if (a[i] > k) {
                r := aux(a, k, i + 1, acc + 1);
            } else {
                r := aux(a, k, i + 1, acc);
            }
        }
        return r;
    }
    return aux(a, k, 0, 0);
}
int r = tail_conta_maggiori([3, 2, 5, 1, 6, 4], 3);
