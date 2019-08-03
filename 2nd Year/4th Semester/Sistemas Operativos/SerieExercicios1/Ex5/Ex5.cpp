#include<Windows.h>
#include <stdio.h>

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

int main() {
	int id = 42084;
	printf("Number of Modules in Process %d = %d\n", id, GetTotalModules(OpenProcess(PROCESS_ALL_ACCESS, TRUE, id)));
	getchar();
}