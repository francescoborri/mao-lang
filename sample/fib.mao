int fib1(int n) {
  int r;
  if (n <= 1) {
    r := n;
  } else {
    r := fib1(n - 1) + fib1(n - 2);
  }
  return r;
}

fib2(int n, int[] ptr) {
  int f0, f1 = 0, 1;
  while (n > 0) {
    n, f0, f1 := n - 1, f1, f0 + f1;
  }
  ptr[0] := f0;
}

int n = 10;
int[] r = new int[1];
fib2(n, r);
bool ok = fib1(n) == r[0];