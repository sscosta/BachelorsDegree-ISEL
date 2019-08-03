#include <Windows.h>
#include <stdio.h>
#include <process.h>
#include <Tchar.h>

static ULONGLONG NUMBER_OF_SWITCHES = 1000000;

unsigned __stdcall tA(LPVOID lpParam) {
	for (volatile ULONG i = 0;i < NUMBER_OF_SWITCHES/2;i++) {
		Yield();
	}
	return 0;
}
unsigned __stdcall tB(LPVOID lpParam) {
	for (volatile ULONG i = 0;i < NUMBER_OF_SWITCHES/2;i++) {
		Yield();
	}
	return 0;
}
VOID TestSameProcess() {
	//testar tempo de comutação entre:
	//threads do mesmo processo

	DWORD dwThreadIdArray[2];
	HANDLE  hThread[2];

	hThread[0] = (HANDLE)_beginthreadex(
		NULL,                   // default security attributes
		0,                      // use default stack size  
		tA,       // thread function name
		NULL,          // argument to thread function 
		CREATE_SUSPENDED,                      // use default creation flags 
		(unsigned int*)&dwThreadIdArray[0]);   // returns the thread identifier



	hThread[1] = (HANDLE) _beginthreadex(
		NULL,                   // default security attributes
		0,                      // use default stack size  
		tB,       // thread function name
		NULL,          // argument to thread function 
		CREATE_SUSPENDED,                      // use default creation flags 
		(unsigned int*)&dwThreadIdArray[1]);   // returns the thread identifier

	if (!SetProcessPriorityBoost(GetCurrentProcess(), TRUE))
		printf("Error on SetPriority Boost : %d\n",GetLastError());

	if(!SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS))
		printf("Error on SetPriorityClass : %d\n", GetLastError());

	if(!SetThreadPriority(hThread[0], THREAD_PRIORITY_TIME_CRITICAL))
		printf("Error on SetThreadPriority for thread 0 : %d\n", GetLastError());

	if(!SetThreadPriority(hThread[1], THREAD_PRIORITY_TIME_CRITICAL))
		printf("Error on SetThreadPriority for thread 1 : %d\n", GetLastError());

	if(!SetThreadAffinityMask(hThread[0], 1))
		printf("Error on SetProcessAffinityMask for thread 0 : %d\n", GetLastError());
	if(!SetThreadAffinityMask(hThread[1], 1))
		printf("Error on SetProcessAffinityMask for thread 1 : %d\n", GetLastError());

	DWORD start = GetTickCount();
	ResumeThread(hThread[0]);
	ResumeThread(hThread[1]);

	WaitForMultipleObjects(2, hThread, TRUE, INFINITE);
	DWORD end = GetTickCount();
	DOUBLE elapsed = end - start;
	DOUBLE contextSwitchTime = (elapsed / NUMBER_OF_SWITCHES);
	//DOUBLE  = aux * 1000000;

	CloseHandle(hThread[0]);
	CloseHandle(hThread[1]);
	printf("Switching between threads of the same process took %lf micros\n", contextSwitchTime);
}

VOID TestDifferentProcesses(){
	//Criar uma thread no processo corrente c/ afinidade a um processador e prioridade alta
//	HANDLE  hThread;
	HANDLE  hThread[2];

	hThread[0] = (HANDLE)_beginthreadex(
		NULL,                   // default security attributes
		0,                      // use default stack size  
		tA,       // thread function name
		NULL,          // argument to thread function 
		CREATE_SUSPENDED,                      // use default creation flags 
		NULL);   // returns the thread identifier

	if (!SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS))
		printf("error on SetPriorityClass for current process : %d\n", GetLastError());
	if (!SetProcessPriorityBoost(GetCurrentProcess(), TRUE))
		printf("error on SetProcessPriorityBoost for current process : %d\n", GetLastError());
	if (!SetThreadPriority(hThread[0], THREAD_PRIORITY_TIME_CRITICAL))
		printf("error on SetThreadPriority for process 0 : %d\n", GetLastError());
	if(!SetThreadAffinityMask(hThread[0], 1))
		printf("error on SetThreadAffinityMask for process 0 : %d\n", GetLastError());

	//Criar processo 1
	PROCESS_INFORMATION pi1;
	SECURITY_ATTRIBUTES sa = {sizeof(sa)};
	STARTUPINFO si = { sizeof(si) };
	const LPCSTR path = TEXT("..\\x64\\Debug\\AnotherProcessApp.exe");

	if (!CreateProcess(path,   // No module name (use command line)
		NULL,        // Command line
		&sa,           // Process handle  inheritable
		NULL,           // Thread handle not inheritable
		TRUE,          // Set handle inheritance to FALSE
		CREATE_SUSPENDED,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&pi1))
	{
		printf("error creating process: LastError = %d\n", GetLastError());
		
		return;
	}
	hThread[1] = pi1.hThread;
	if (!SetProcessPriorityBoost(pi1.hProcess, TRUE))
		printf("Error on SetProcessPriorityBoost for process 1 : %d\n",GetLastError());
	if(!SetPriorityClass(pi1.hProcess, REALTIME_PRIORITY_CLASS))
		printf("Error on SetPriorityClass for process 1 : %d\n", GetLastError());
	if (!SetThreadPriority(hThread[1], THREAD_PRIORITY_TIME_CRITICAL))
		printf("error on SetThreadPriority for process 1 : %d\n", GetLastError());
	if (!SetThreadAffinityMask(hThread[1], 1))
		printf("error on SetThreadAffinityMask for process 1 : %d\n", GetLastError());

	DWORD start = GetTickCount();
	ResumeThread(hThread[0]);
	ResumeThread(pi1.hThread);
	
	//WaitForSingleObject(pi1.hThread, INFINITE);
	//WaitForSingleObject(hThread, INFINITE);
	WaitForMultipleObjects(2, hThread, TRUE, INFINITE);
	DWORD end = GetTickCount();

	DOUBLE elapsed = end - start;
	DOUBLE contextSwitchTime = (elapsed / NUMBER_OF_SWITCHES);

	CloseHandle(pi1.hThread);
	CloseHandle(pi1.hProcess);
	CloseHandle(hThread[0]);
	printf("Switching between threads of different processes took %lf micros\n", contextSwitchTime);
}

static int N_ROUNDS = 10;
VOID WarmUpMeasurements() {
	printf("executing 10 warm-up rounds of TestSameProcess and TestDifferentProcesses\n");
	for (int i = 0; i < N_ROUNDS; i++) {
		printf("---------WarmUp Round %d-----------\n", i);
		TestSameProcess();
		TestDifferentProcesses();
	}
	printf("Warm-Up ended\n");
}
int main() {
	
	//WarmUpMeasurements();

	TestSameProcess();
	getchar();

	TestDifferentProcesses();
	getchar();
}