#include <Windows.h>
#include <stdio.h>
#include <psapi.h>

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
	printf("Initial Reserved pages for dwSize %lu is %lu\n",dwSize, nPages);

	sizeToCommit = nPages * PAGE_SIZE;
	//Reserve two more pages than is necessary
	LPVOID addrReserved = VirtualAlloc(0, sizeToCommit + (2 * PAGE_SIZE), MEM_RESERVE, PAGE_NOACCESS);
	printf("Alocated two more pages than initial with MEM_RESERVE and PAGE_NOACCESS\n");
	//Commit block beggining in the 2nd committed page
	printf("Move pointer to second page and change status of the %lu pages to MEM_COMMIT, PAGE_READWRITE\n", nPages);
	printf("leaving the first and last with MEM_RESERVE, PAGE_NOACCESS\n");
	return VirtualAlloc(LPVOID(((size_t)addrReserved) + PAGE_SIZE), sizeToCommit, MEM_COMMIT, PAGE_READWRITE);
	
}
VOID FreeBoundedBlock(LPVOID lpAddress) {
	if (!VirtualFree(LPVOID((size_t)lpAddress - PAGE_SIZE), 0, MEM_RELEASE)) {
		printf("# VirtualFree failed: %lu\n", GetLastError());
	}
}

int main() {
	LPVOID addr = AllocBoundedBlock(9444);
	FreeBoundedBlock(addr);
	getchar();
}