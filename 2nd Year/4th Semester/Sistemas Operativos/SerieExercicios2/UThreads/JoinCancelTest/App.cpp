#include <stdio.h>
#include <Windows.h>
#include "../Include/UThread.h"

HANDLE T1, T2, T3, T4;

VOID F1(UT_ARGUMENT arg) {
	UCHAR Char = (UCHAR)arg;
	printf("Thread %c start\n", Char);
	UtJoin(T2);
	UtYield();
	printf("Thread %c end\n", Char);
}
VOID F2(UT_ARGUMENT arg) {
	UCHAR Char = (UCHAR)arg;
	printf("Thread %c start\n", Char);
	UtYield();
	printf("Thread %c end\n", Char);
}
VOID F3(UT_ARGUMENT arg) {
	UCHAR Char = (UCHAR)arg;
	printf("Thread %c start\n", Char);
	UtJoin(T2);
	UtJoin(T2);
	printf("Thread %c end\n", Char);
}
VOID F4(UT_ARGUMENT arg) {
	UCHAR Char = (UCHAR)arg;
	printf("Thread %c start\n", Char);
	UtJoinCancel(T3);
	UtYield();
	printf("Thread %c end\n", Char);
}



int main() {
	UtInit();

	T1 = UtCreate(F1, (UT_ARGUMENT)'1');
	T2 = UtCreate(F2, (UT_ARGUMENT)'2');
	T3 = UtCreate(F3, (UT_ARGUMENT)'3');
	T4 = UtCreate(F4, (UT_ARGUMENT)'4');

	printf("\nTest start\n");
	UtRun();
	printf("Test end\n");

	UtEnd();

	printf("Press any key to finish");
	getchar();

}
