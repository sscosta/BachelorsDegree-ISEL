#include <Windows.h>
#include <stdio.h>
#include <process.h>
static ULONGLONG NUMBER_OF_SWITCHES = 1000000;

int main() {
	for (volatile ULONGLONG i = 0;i < NUMBER_OF_SWITCHES / 2;i++) {
		Yield();
	}
	return 0;
}