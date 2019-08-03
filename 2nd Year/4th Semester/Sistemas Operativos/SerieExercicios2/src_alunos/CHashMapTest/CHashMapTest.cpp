#include "../CHashMap/ConcurrentHashMap.h"
#include <stdio.h>
#include <crtdbg.h>

VOID TestCreate() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	_ASSERTE(map->Capacity == 5);
	printf("Assertion Successful\n");
	free(map);
}

VOID TestCapacity() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	CHashMapDestroy(map);
	_ASSERTE(map->Capacity != 5);
	printf("Assertion Successful\n");
}

VOID TestGetNonExisting() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	DOUBLE * pval = (DOUBLE *)malloc(sizeof(DOUBLE *));
	BOOL present = CHashMapGet(map, 3, pval);
	_ASSERTE(present == 0);
	printf("Assertion Successful\n");
	free(map);
	free(pval);
}

VOID TestGet() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	CHashMapPut(map, 3, 5);
	DOUBLE * pval = (DOUBLE *)malloc(sizeof(DOUBLE *));
	BOOL present = CHashMapGet(map, 3, pval);
	_ASSERTE(present == 1 && *pval == 5);
	printf("Assertion Successful\n");
	free(map);
	free(pval);
}

VOID TestRemove() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	CHashMapPut(map, 3, 5);
	BOOL removed = CHashMapRemove(map, 3);
	_ASSERTE(removed == TRUE);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}

VOID TestRemoveColliding() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	CHashMapPut(map, 3, 5);
	CHashMapPut(map, 8, 12);
	BOOL removed = CHashMapRemove(map, 3);
	_ASSERTE(removed == TRUE);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}

VOID TestRemoveNonExisting() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	CHashMapPut(map, 3, 5);
	BOOL removed = CHashMapRemove(map, 8);
	_ASSERTE(removed == FALSE);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}


VOID TestSize() {
	ConcurrentHashMap * map = CHashMapCreate(5);
	CHashMapPut(map, 3, 5);
	CHashMapPut(map, 8, 12);
	CHashMapPut(map, 1, 8.5);
	DWORD NumberOfNodes = CHashMapSize(map);
	_ASSERTE(NumberOfNodes == 3);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}

ULONG ListSize(PLIST_ENTRY plist) {
	int sz = 0;
	PLIST_ENTRY head = plist;
	PLIST_ENTRY node = head->Flink;
	while (head != node) {
		sz++;
		node = node->Flink;
	}
	return sz;
}
VOID TestToList() {
	ConcurrentHashMap * map = CHashMapCreate(4);
	CHashMapPut(map, 3, 5);
	CHashMapPut(map, 8, 12);
	CHashMapPut(map, 1, 8.5);
	CHashMapPut(map, 2, 2.2);

	CHashMapPut(map, 4, 1);
	CHashMapPut(map, 6, 22);
	CHashMapPut(map, 7, 2.5);
	CHashMapPut(map, 9, 3.2);

	CHashMapPut(map, 10, 1);
	CHashMapPut(map, 11, 22);
	CHashMapPut(map, 12, 2.5);
	CHashMapPut(map, 13, 3.2);

	LIST_ENTRY list = CHashMapToList(map);
	list.Flink->Blink = &list; list.Blink->Flink = &list;
	DWORD size = ListSize(&list);
	_ASSERTE(size == 12);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}

VOID TestToListBig() {
	ConcurrentHashMap * map = CHashMapCreate(225);
	CHashMapPut(map, 3, 5);
	CHashMapPut(map, 8, 12);
	CHashMapPut(map, 1, 8.5);
	CHashMapPut(map, 2, 2.2);

	CHashMapPut(map, 4, 1);
	CHashMapPut(map, 6, 22);
	CHashMapPut(map, 7, 2.5);
	CHashMapPut(map, 9, 3.2);

	CHashMapPut(map, 10, 1);
	CHashMapPut(map, 11, 22);
	CHashMapPut(map, 12, 2.5);
	CHashMapPut(map, 13, 3.2);

	//last bucket of first unit of work (225/8) +1
	CHashMapPut(map, 28, 2.8);
	//last bucket 
	CHashMapPut(map, 224, 1.6);

	LIST_ENTRY list = CHashMapToList(map);
	list.Flink->Blink = &list; list.Blink->Flink = &list;
	DWORD size = ListSize(&list);

	_ASSERTE(size == map->Size);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}

VOID TestMoreThanOnePutWithSameKey() {
	ConcurrentHashMap * map = CHashMapCreate(4);
	CHashMapPut(map, 1, 5);
	CHashMapPut(map, 5, 3);
	CHashMapPut(map, 9, 7);
	CHashMapPut(map, 5, 9);
	DOUBLE doub, doub1;
	CHashMapGet(map, 9, &doub);
	CHashMapGet(map, 5, &doub1);
	_ASSERTE(map->Capacity == 4);
	_ASSERTE(doub == 7);
	_ASSERTE(doub1 == 9);
	printf("Assertion Successful\n");
	CHashMapDestroy(map);
}
int main() {
	printf("ConcurrentHashMap - Test Create start\n");

	TestCreate();

	printf("ConcurrentHashMap - Test Create end\n");

	getchar();

	printf("ConcurrentHashMap - Test Destroy start\n");

	TestCapacity();

	printf("ConcurrentHashMap - Test Destroy end\n");

	getchar();

	printf("ConcurrentHashMap - Test Not Existing Node start\n");

	TestGetNonExisting();

	printf("ConcurrentHashMap - Test Not Existing Node end\n");

	getchar();

	printf("ConcurrentHashMap - Test  Existing Node start\n");

	TestGet();

	printf("ConcurrentHashMap - Test  Existing Node end\n");

	getchar();

	printf("ConcurrentHashMap - Test Remove Existing start\n");

	TestRemove();

	printf("ConcurrentHashMap - Test  Remove Existing end\n");

	getchar();

	printf("ConcurrentHashMap - Test Remove Colliding start\n");

	TestRemoveColliding();

	printf("ConcurrentHashMap - Test  Remove Colliding end\n");

	getchar();

	printf("ConcurrentHashMap - Test Remove Non-Existing start\n");

	TestRemoveNonExisting();

	printf("ConcurrentHashMap - Test  Remove Non-Existing end\n");

	getchar();

	printf("ConcurrentHashMap - Test Size start\n");

	TestSize();

	printf("ConcurrentHashMap - Test  Size end\n");

	getchar();

	printf("ConcurrentHashMap - Test ToList start\n");

	TestToList();

	printf("ConcurrentHashMap - Test ToList end\n");

	getchar();

	printf("ConcurrentHashMap - Test Conflict start\n");

	TestMoreThanOnePutWithSameKey();

	printf("ConcurrentHashMap - Test Conflict end\n");

	getchar();

	//printf("ConcurrentHashMap - Test ToList Big Map start\n");

	//TestToListBig();

	//printf("ConcurrentHashMap - Test ToList Big Map end\n");

	//getchar();

}