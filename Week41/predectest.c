void main(int n){
	int a[4];
	a[0]=0;
	a[1]=1;
	a[2]=2;
	a[3]=3;
	++a;
	int i; i=2;
	int b;
	b = --a[--i];
	print b;
	print i;
}
