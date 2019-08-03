#include "Ex6.h"
#include <stdio.h>
int main() {
	//Change to valid pid
	size_t id = 16732;
	//Ex3

	printf("Ex 3\n");
	PrintMemInfo(id);

	//Ex4
	printf("%\nEx 4\n");
	LPVOID addr = AllocBoundedBlock(9444);
	FreeBoundedBlock(addr);
	

	//Ex5
	printf("%\nEx 5\n");
	printf("Number of Modules in Process %d = %d\n", id, GetTotalModules(OpenProcess(PROCESS_ALL_ACCESS, TRUE, id)));
	getchar();
}