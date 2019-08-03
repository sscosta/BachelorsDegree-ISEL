#include<Windows.h>

LPVOID AllocBoundedBlock(size_t dwSize);
VOID FreeBoundedBlock(LPVOID lpAddress);