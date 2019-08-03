#include <stdio.h>
#include "List.h"
#include "..\Include\Uthread.h"

#include <iostream>       // std::cout, std::endl
#include <thread>         // std::this_thread::sleep_for
#include <chrono>

///////////////////////////////////////////////////////////////
//															 //
// Test 1: N threads, each one printing its number M times //
//															 //
///////////////////////////////////////////////////////////////


HANDLE tA, tB, tC, tD, tE, tF, tG, tH, tI, tJ, tK, tL, tM, tN, tO;

VOID ThreadA(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tC);
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
	printf("Thread %c end\n", Char);
}
VOID Test1()  {

	ULONG start = GetTickCount();

	tA = UtCreate(ThreadA, (UT_ARGUMENT)'A');
	tB = UtCreate(ThreadB, (UT_ARGUMENT)'B');
	tC = UtCreate(ThreadC, (UT_ARGUMENT)'C');

	printf("\n :: Test 1 (Join) - BEGIN :: \n\n");
	ULONG elapsed = GetTickCount() - start;
	UtRun();

	printf("\n\n :: Test 1 (Join) - END :: \n");
}

VOID ThreadD(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tF);
	printf("Thread %c end\n", Char);
}
VOID ThreadE(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtYield();
	printf("Thread %c end\n", Char);
}
VOID ThreadF(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	printf("Thread D is in State: Expected - Blocked | Actual  - %s\n", UtStateToString(UtGetState(tD)));
	printf("Thread E is in State: Expected - Ready | Actual - %s\n", UtStateToString(UtGetState(tE)));
	printf("Thread F is in State: Expected - Running | Actual - %s\n", UtStateToString(UtGetState(tF)));
	UtJoin(tE);
	printf("Thread %c end\n", Char);
}

VOID Test2() {

	tD = UtCreate(ThreadD, (UT_ARGUMENT)'D');
	tE = UtCreate(ThreadE, (UT_ARGUMENT)'E');
	tF = UtCreate(ThreadF, (UT_ARGUMENT)'F');

	printf("\n :: Test 2 (State) - BEGIN :: \n\n");
	UtRun();

	printf("\n\n :: Test 2 (State) - END :: \n");
}

VOID ThreadG(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tI);
	printf("Thread I is Alive? Expected - false | Actual - %s\n", UtAlive(tI) ? "true" : "false");
	printf("Thread %c end\n", Char);
}
VOID ThreadH(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtYield();
	printf("Thread %c end\n", Char);
}

VOID ThreadI(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tH);
	printf("Thread G is Alive? Expected - true | Actual - %s\n", UtAlive(tG) ? "true" : "false");
	printf("Thread H is Alive? Expected - false | Actual - %s\n", UtAlive(tH) ? "true" : "false");
	printf("Thread %c end\n", Char);
}

VOID Test3() {
	ULONG start = GetTickCount();

	tG = UtCreate(ThreadG, (UT_ARGUMENT)'G');
	tH = UtCreate(ThreadH, (UT_ARGUMENT)'H');
	tI = UtCreate(ThreadI, (UT_ARGUMENT)'I');

	printf("\n :: Test 3 (UtAlive) - BEGIN :: \n\n");

	UtRun();

	printf("\n\n :: Test 3 (UtAlive) - END :: \n");
}

VOID ThreadJ(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	std::this_thread::sleep_for(std::chrono::seconds(1));
	UtJoin(tL);
	std::this_thread::sleep_for(std::chrono::seconds(1));
	printf("Thread %c end\n", Char);
}
VOID ThreadK(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	std::this_thread::sleep_for(std::chrono::seconds(5));
	UtYield();
	printf("Thread %c end\n", Char);
}
VOID ThreadL(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tK);
	std::this_thread::sleep_for(std::chrono::seconds(3));
	printf("Thread %c end\n", Char);
}

VOID Test4() {
	ULONG start = GetTickCount();

	tJ = UtCreate(ThreadJ, (UT_ARGUMENT)'J');
	tK = UtCreate(ThreadK, (UT_ARGUMENT)'K');
	tL = UtCreate(ThreadL, (UT_ARGUMENT)'L');

	printf("\n :: Test 4 (times) - BEGIN :: \n\n");
	UtRun();

	ULONG elapsed = GetTickCount() - start;
	printf("\n\n :: Test 4 (times) - END :: \n");
}
VOID ThreadM(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtJoin(tO);
	printf("Thread %c end\n", Char);
}
VOID ThreadN(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtYield();
	printf("Thread %c end\n", Char);
}
VOID ThreadO(UT_ARGUMENT Argument) {
	UCHAR Char = (UCHAR)Argument;

	printf("Thread %c start\n", Char);
	UtFinish();
	printf("Thread %c end\n", Char);
}
VOID Test5() {
	tM = UtCreate(ThreadM, (UT_ARGUMENT)'M');
	tN = UtCreate(ThreadN, (UT_ARGUMENT)'N');
	tO = UtCreate(ThreadO, (UT_ARGUMENT)'O');
	printf("\n :: Test 5 (finish) - BEGIN :: \n\n");
	UtRun();
	printf("\n\n :: Test 5 (finish) - END :: \n");
}

int main() {
	UtInit();

	Test1();

	printf("Press any key to finish");
	getchar();

	Test2();

	printf("Press any key to finish");
	getchar();

	Test3();

	printf("Press any key to finish");
	getchar();

	//Test4();

	printf("Press any key to finish");
	getchar();

	Test5();

	printf("Press any key to finish");
	getchar();

	UtEnd();

	return 0;
}