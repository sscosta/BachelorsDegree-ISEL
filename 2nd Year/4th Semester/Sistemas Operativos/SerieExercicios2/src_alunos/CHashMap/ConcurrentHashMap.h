#include <Windows.h>

#ifdef CHM_EXPORTS
#define CHM_API _declspec(dllexport)
#else
#define CHM_API _declspec(dllimport)
#endif

typedef struct ConcurrentHashMap {
	LONG Size;
	DWORD Capacity;
	PLIST_ENTRY * DispersionList;
	LPCRITICAL_SECTION * cs;
	DWORD NSections;
} ConcurrentHashmap, *PConcurrentHashMap;

typedef struct PairNode {
	ULONG Key;
	DOUBLE Val;
	LIST_ENTRY link;
}PairNode, *PPairNode;

typedef struct ThreadArg{
	ConcurrentHashMap * Map;
	ULONG InitIdx;
	ULONG NBuckets;
	PLIST_ENTRY ListHead;
	HANDLE evt;
}THREAD_ARG, *PTHREAD_ARG;

CHM_API ConcurrentHashMap * CHashMapCreate(DWORD capacity);
CHM_API VOID CHashMapDestroy(ConcurrentHashMap * map);
CHM_API BOOL CHashMapGet(ConcurrentHashMap * map, ULONG key, DOUBLE * pval);
CHM_API BOOL CHashMapPut(ConcurrentHashMap * map, ULONG key, DOUBLE val);
CHM_API BOOL CHashMapRemove(ConcurrentHashMap * map, ULONG key);
CHM_API ULONG CHashMapSize(ConcurrentHashMap * map);
CHM_API LIST_ENTRY CHashMapToList(ConcurrentHashMap * map);