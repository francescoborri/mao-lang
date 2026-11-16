bool contiene_cifra(int n, int c) { 
    bool result = (n % 10) == c;
    if (n >= 10) {
        result := result || contiene_cifra(n / 10, c);
    }
    return result;
}
bool result = contiene_cifra(1234, 3);