int conta_maggiori(int[] a, int k) {
    int aux_conta_maggiori(int[] a, int k, int i) {
        int result = 0;
        if (i < a.length) {
            if (a[i] > k) {
                result := 1 + aux_conta_maggiori(a, k, i + 1);
            } else {
                result := aux_conta_maggiori(a, k, i + 1);
            }
        }
        return result;
    }
    return aux_conta_maggiori(a, k, 0);
}
int r = conta_maggiori([3, 2, 5, 1, 6, 4], 3);
