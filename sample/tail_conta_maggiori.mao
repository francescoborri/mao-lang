int tail_conta_maggiori(int[] a, int k) {
    int aux_tail_conta_maggiori(int[] a, int k, int i, int acc) {
        int result = acc;
        if (i < a.length) {
            if (a[i] > k) {
                result := aux_tail_conta_maggiori(a, k, i + 1, acc + 1);
            } else {
                result := aux_tail_conta_maggiori(a, k, i + 1, acc);
            }
        }
        return result;
    }
    return aux_tail_conta_maggiori(a, k, 0, 0);
}
int result = tail_conta_maggiori([3, 2, 5, 1, 6, 4], 3);