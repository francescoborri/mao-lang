int fib_rec(int n) {
    int r;
    if (n <= 1) {
        r := n;
    } else {
        r := fib_rec(n - 1) + fib_rec(n - 2);
    }
    return r;
}
fib_iter(int n, int[] ptr) {
    int f0, f1 = 0, 1;
    while (n > 0) {
        n, f0, f1 := n - 1, f1, f0 + f1;
    }
    ptr[0] := f0;
}
int n, int[] ptr = 10, new int[1];
fib_iter(n, ptr);
int x, y = fib_rec(n), ptr[0];
bool ok = x == y;
