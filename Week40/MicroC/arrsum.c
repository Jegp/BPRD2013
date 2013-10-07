void main(int n) {
	int aa[4];
	int i; i=0;
	aa[0]=7;
	aa[1]=13;
	aa[2]=9;
	aa[3]=8;

	int aa[20];
	nsquares(4, aa);
	i=0;
	arrsum(4,aa, &i);
	print i;

	int aa[7];
	aa[0]=1;
	aa[1]=2;
	aa[2]=1;
	aa[3]=1;
	aa[4]=1;
	aa[5]=2;
	aa[6]=0;
	int freq[4];
	freq[0]=0;
	freq[1]=0;
	freq[2]=0;
	freq[3]=0;
	histogram(7, aa, 3, freq);
	print freq[0];
	print freq[1];
	print freq[2];
	print freq[3];
}

//ex. 7.2 i
void arrsum(int n, int arr[], int *sump){
	int i;
	for(i=0;i<n;i=i+1){
		*sump = *sump + arr[i];
	}
}

//ex. 7.2 ii
void nsquares(int n, int arr[]){
	int i;
	for (i=0; i<n; i=i+1){
		arr[i]=i*i;
	}
}

//ex. 7.2 iii
void histogram(int n, int ns[], int max, int freq[]){
	int i;
	for(i=0;i<n;i=i+1){
		freq[ns[i]] = freq[ns[i]]+1;
	}
}

