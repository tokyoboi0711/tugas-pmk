#include <stdio.h>
int main()
{
    int uts,uas,quis;
    float nilaiAkhir;
    scanf("%d %d", &uts, &uas);
    nilaiAkhir=(40/100)*uts+(40/100)*uas+(20/100)*quis;
    printf("%.2f\n",nilaiAkhir);
    return 0;
}