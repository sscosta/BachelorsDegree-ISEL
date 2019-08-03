#include <stdio.h>
#include "List.h"
#include "..\Include\Uthread.h"


///////////////////////////////////////////////////////////////
//															 //
// Test 1: N threads, each one printing its number M times //
//															 //
///////////////////////////////////////////////////////////////


HANDLE tA, tB, tC;

VOID ThreadA(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tC);
	printf("Thread C is Alive? Expected - false | Actual - %s\n", UtAlive(tC) ? "true" : "false");
	printf("Thread %c end\n", Char);
}
VOID ThreadB(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtYield();
	printf("Thread %c end\n", Char);
}
VOID ThreadC(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tB);
	printf("Thread A is Alive? Expected - true | Actual - %s\n", UtAlive(tA) ? "true" : "false");
	printf("Thread B is Alive? Expected - false | Actual - %s\n", UtAlive(tB) ? "true" : "false");
	printf("Thread %c end\n", Char);
}

VOID Test1() {

	tA = UtCreate(ThreadA, (UT_ARGUMENT)'A');
	tB = UtCreate(ThreadB, (UT_ARGUMENT)'B');
	tC = UtCreate(ThreadC, (UT_ARGUMENT)'C');

	printf("\n :: Test 1 - BEGIN :: \n\n");

	UtRun();

	printf("\n\n :: Test 1 - END :: \n");
}

int main() {
	UtInit();

	Test1();

	printf("Press any key to finish");
	getchar();


	UtEnd();
	return 0;
}