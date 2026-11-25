merge(int[] a, int from, int mid, int to) { 
    int n_l, n_r = mid - from + 1, to - mid;
    int[] left = new int[n_l];
    int[] right = new int[n_r];
    int i, j;
    while(i < n_l) {
        left[i] := a[from+i];
        i := i + 1;
    }
    while (j < n_r) {
        right[j] := a[mid+1+j];
        j := j + 1;    
    }
    i, j := 0, 0;
    int k = from;
    while(i < n_l && j < n_r) {
        if(left[i] <= right[j]) {
            a[k] := left[i];
            i := i + 1;
        } else {
            a[k] := right[j];
            j := j + 1;
        }
        k := k + 1;
    }
    while(i < n_l) {
        a[k] := left[i];
        i, k := i + 1, k + 1;
    }
    while (j < n_r) {
        a[k] := right[j];
        j, k := j + 1, k + 1;    
    }
}

merge_sort_aux(int[] a, int from, int to) {
    if (from < to) {
        int mid = (from+to) / 2;
        merge_sort_aux(a, from, mid);
        merge_sort_aux(a,mid + 1, to);
        merge(a, from, mid, to);    
    }    
}

merge_sort(int[] a) {
    merge_sort_aux(a, 0, a.length-1);
}

merge_sort([2,3,5,4,1,6]);