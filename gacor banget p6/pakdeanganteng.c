#include <stdio.h>

int main()
{
	int t;
	long long a, b, c;
	scanf("%d", &t);
	for (int i = 0; i < t; i++)
	{
		scanf("%lld %lld %lld", &a, &b, &c);

    long long hasil = 1;
		a = a % c;

		while (b > 0)
		{
			if (b % 2 == 1)
			{
				hasil = (hasil * a) % c;
			}
			a = (a * a) % c;
			b /= 2;
		}

		printf("%lld\n", hasil);
	}

	return 0;
}