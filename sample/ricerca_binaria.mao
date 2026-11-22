int ricerca_binaria(int[] a, int k) {
    int aux(int[] a, int k, int from, int to) {
        int r = -1;
        if (from <= to) {
            int mid = (from + to) / 2;
            if (a[mid] == k) {
                r := mid;
            }
            if (a[mid] < k) {
                r := aux(a, k, mid + 1, to);
            }
            if (a[mid] > k) {
                r := aux(a, k, from, mid - 1);
            }
        }
        return r;
    }
    return aux(a, k, 0, a.length - 1);
}
int i = ricerca_binaria([-3, -1, 0, 2, 4, 5, 7], 4);