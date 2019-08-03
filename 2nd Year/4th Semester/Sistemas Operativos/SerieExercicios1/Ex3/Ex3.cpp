#include <Windows.h>
#include <psapi.h>
#include <stdio.h>
#include <cstdlib>
using namespace std;

void PrintPerformanceInfo() {
	PERFORMACE_INFORMATION pi;
	GetPerformanceInfo(&pi, sizeof(pi));
	printf("Total Commited = %u\nPhysical Total = %u\nPage Size = %u\n", pi.CommitTotal, pi.PhysicalTotal, pi.PageSize);
}

void PrintProcessMemoryInfo(DWORD id, HANDLE hproc) {
	HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS, TRUE, id);
	PROCESS_MEMORY_COUNTERS_EX info;
	info.cb = sizeof(info);
	GetProcessMemoryInfo(hProc, (PROCESS_MEMORY_COUNTERS*)&info, info.cb);
	printf("Size of the Working Set = %u\nNumber of page Faults = %u\n", info.WorkingSetSize, info.PageFaultCount);
}

void PrintMemoryStatus() {
	MEMORYSTATUSEX statex;
	statex.dwLength = sizeof(statex);
	GlobalMemoryStatusEx(&statex);
	printf("Memory in use = %I64u\nTotal Physical memory = %I64u\nFree Physical Memory = %I64u\nTotal Virtual Memory = %I64u\nFree VirtualMemory = %I64u\n",
		statex.dwMemoryLoad,
		statex.ullTotalPhys,
		statex.ullAvailPhys,
		statex.ullTotalVirtual,
		statex.ullAvailVirtual);
}

void PrintWorkingSet(DWORD id, HANDLE hProc) {
	size_t nSharedWorkingSetPages = 0, size, factorPonderacao = 100;
	PSAPI_WORKING_SET_INFORMATION wsi;
	PPSAPI_WORKING_SET_INFORMATION pwsi = NULL;

	QueryWorkingSet(hProc, (PVOID)&wsi, sizeof(wsi));

	size = sizeof(PSAPI_WORKING_SET_INFORMATION)
		+ sizeof(PSAPI_WORKING_SET_BLOCK) * wsi.NumberOfEntries + factorPonderacao;

	pwsi = (PPSAPI_WORKING_SET_INFORMATION)malloc(size);

	if (!QueryWorkingSet(hProc, (LPVOID)pwsi, size)) {
		printf("# Second QueryWorkingSet failed: %lu\n"
			, GetLastError());
	}

	for (size_t i = 0; i < pwsi->NumberOfEntries; i++) {
		if (!pwsi->WorkingSetInfo[i].Shared)
			nSharedWorkingSetPages++;
	}
	printf("Number of Shared WorkingSet Pages = %u\n", nSharedWorkingSetPages);
	free(pwsi);
}

void PrintMemInfo(DWORD id) {
	HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS, TRUE, id);
	PrintPerformanceInfo();
	PrintProcessMemoryInfo(id, hProc);
	PrintMemoryStatus();
	PrintWorkingSet(id, hProc);
}


int main() {
	PrintMemInfo(31848);
	getchar();
}