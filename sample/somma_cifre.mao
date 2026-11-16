int somma_cifre(int n) {           
    int sum = n;
    if (n >= 10) {
        sum := (n % 10) + somma_cifre(n / 10);
    }
    return sum;
}
int result = somma_cifre(1234);