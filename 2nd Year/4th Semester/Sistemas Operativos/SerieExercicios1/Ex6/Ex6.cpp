#include <Windows.h>
#include <psapi.h>
#include <stdio.h>
#include <cstdlib>
#include "Ex6.h"
using namespace std;

void PrintPerformanceInfo() {
	PERFORMACE_INFORMATION pi;
	GetPerformanceInfo(&pi, sizeof(pi));
	printf("Total Commited = %llu\nPhysical Total = %llu\nPage Size = %llu\n", pi.CommitTotal, pi.PhysicalTotal, pi.PageSize);
}

void PrintProcessMemoryInfo(DWORD id, HANDLE hproc) {
	HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS, TRUE, id);
	PROCESS_MEMORY_COUNTERS_EX info;
	info.cb = sizeof(info);
	GetProcessMemoryInfo(hProc, (PROCESS_MEMORY_COUNTERS*)&info, info.cb);
	printf("Size of the Working Set = %llu\nNumber of page Faults = %llu\n", info.WorkingSetSize, info.PageFaultCount);
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
		printf("# Second QueryWorkingSet failed: %llu\n"
			, GetLastError());
	}

	for (size_t i = 0; i < pwsi->NumberOfEntries; i++) {
		if (!pwsi->WorkingSetInfo[i].Shared)
			nSharedWorkingSetPages++;
	}
	printf("Number of Shared WorkingSet Pages = %llu\n", nSharedWorkingSetPages);
	free(pwsi);
}

void PrintMemInfo(DWORD id) {
	HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS, TRUE, id);
	PrintPerformanceInfo();
	PrintProcessMemoryInfo(id, hProc);
	PrintMemoryStatus();
	PrintWorkingSet(id, hProc);
}


size_t GetPageSize() {
	PERFORMACE_INFORMATION pi;
	GetPerformanceInfo(&pi, sizeof(pi));
	return pi.PageSize;
}

size_t PAGE_SIZE = GetPageSize();

LPVOID AllocBoundedBlock(size_t dwSize) {
	size_t sizeToCommit;
	size_t nPages = (dwSize / PAGE_SIZE);
	LPVOID addrCommited;

	//check if theres one more page to add
	if (dwSize % PAGE_SIZE != 0) {
		nPages = nPages++;
	}
	printf("Initial Reserved pages for dwSize %llu is %llu\n", dwSize, nPages);

	sizeToCommit = nPages * PAGE_SIZE;
	//Reserve two more pages than is necessary
	LPVOID addrReserved = VirtualAlloc(0, sizeToCommit + (2 * PAGE_SIZE), MEM_RESERVE, PAGE_NOACCESS);
	printf("Alocated two more pages than initial with MEM_RESERVE and PAGE_NOACCESS\n");
	//Commit block beggining in the 2nd committed page
	printf("Move pointer to second page and change status of the %llu pages to MEM_COMMIT, PAGE_READWRITE\n", nPages);
	printf("leaving the first and last with MEM_RESERVE, PAGE_NOACCESS\n");
	return VirtualAlloc(LPVOID(((size_t)addrReserved) + PAGE_SIZE), sizeToCommit, MEM_COMMIT, PAGE_READWRITE);

}
VOID FreeBoundedBlock(LPVOID lpAddress) {
	if (!VirtualFree(LPVOID((size_t)lpAddress - PAGE_SIZE), 0, MEM_RELEASE)) {
		printf("# VirtualFree failed: %llu\n", GetLastError());
	}
}

WORD GetTotalModules(HANDLE process) {
	WORD nModules = 0;
	MEMORY_BASIC_INFORMATION mbi;
	char* addr = 0;
	char* alBase = 0;
	size_t regions = 0;


	while (VirtualQueryEx(process, addr, &mbi, sizeof(MEMORY_BASIC_INFORMATION)) > 0) {
		alBase = (char*)mbi.AllocationBase;
		if (mbi.Type == MEM_IMAGE && (mbi.State == MEM_COMMIT || mbi.State == MEM_RESERVE)
			&& mbi.AllocationBase == addr) {
			regions++;
		}
		addr = (char*)mbi.BaseAddress + mbi.RegionSize;
	}
	return regions;
}