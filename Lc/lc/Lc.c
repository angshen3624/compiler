#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
	int i;
	FILE *f;
	if (!system(NULL)) 
		exit(EXIT_FAILURE);
	//char *str1 = "./L1_rkt ";
	//char *str2 = argv[1];
	//char *str = malloc(strlen(str1) + strlen(str2) + 1);
	//strcat(str, str1);
	//strcat(str, str2);
	
	f = fopen("filename.in", "w");
	fprintf(f, "\"%s\"", argv[1]);
	fclose(f);
	//system("raco exe -o L1_rkt L1.rkt");
	//system(str);
	system("racket Lc.rkt");
	system("as -o prog.o prog.S");
	//system("gcc -O2 -c -g -o runtime.o runtime.c");
	system("gcc -o a.out prog.o runtime.o");
	system("./a.out");
	
	return 0;
}
