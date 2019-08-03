#include <Windows.h>
#include "ConcurrentHashMap.h"
#include<stdio.h>
#include <process.h>
#include "List.h"

static int NUMBER_OF_SECTIONS = 6;
// Cria e inicia um novo HashMap concorrente com capacidade capacity
ConcurrentHashMap * CHashMapCreate(DWORD capacity) {

	if (capacity < 1) return NULL;
	ConcurrentHashMap * map;
	/* Allocate the table itself. */
	if ((map = (ConcurrentHashMap*) malloc(sizeof(ConcurrentHashMap) + (capacity * sizeof(LIST_ENTRY)))) == NULL) {
		return NULL;
	}
	if ((map->DispersionList = (PLIST_ENTRY*)malloc(sizeof(PLIST_ENTRY) * capacity)) == NULL)
		return NULL;

	for (DWORD i = 0; i < capacity; i++) {
		if ((map->DispersionList[i] = (PLIST_ENTRY) malloc(sizeof(LIST_ENTRY))) == NULL) {
			return NULL;
		}
	}
	for (DWORD i = 0; i < capacity; i++) {
		InitializeListHead(map->DispersionList[i]);
	}

	map->Capacity = capacity;
	map->Size = 0;

	map->NSections = capacity < NUMBER_OF_SECTIONS ? capacity / 2 : NUMBER_OF_SECTIONS;//rever
	map->cs = (LPCRITICAL_SECTION *)malloc(sizeof(CRITICAL_SECTION) * map->NSections);

	for (DWORD i = 0; i < map->NSections; i++) {
		if ((map->cs[i] = (LPCRITICAL_SECTION)malloc(sizeof(CRITICAL_SECTION))) == NULL)
			return NULL;
		InitializeCriticalSection(map->cs[i]);
	}

	return map;
}

// Elimina os pares associados à coleção map e o próprio map
VOID CHashMapDestroy(ConcurrentHashMap * map) {
	for (DWORD i = 0;i < map->Capacity;i++) {
		PLIST_ENTRY list = map->DispersionList[i];
		while (!IsListEmpty(list)) {
			//free(RemoveTailList(list));
			RemoveTailList(list);
			InterlockedDecrement(&map->Size);
		}
		free(map->DispersionList[i]);
	}
	free(map->DispersionList);
	for (DWORD i = 0; i < map->NSections; i++) {
		DeleteCriticalSection(map->cs[i]);
		free(map->cs[i]);
	}
	free(map->cs);
	free(map);
	return;
}
ULONG HashCode(ConcurrentHashMap * map, ULONG key) {
	return key % map->Capacity;
}
// Retorna o valor em pval correspondente à chave key ou FALSE se não existir
BOOL CHashMapGet(ConcurrentHashMap * map, ULONG key, DOUBLE * pval) {
	ULONG hc = HashCode(map,key);
	
	DWORD SegmentSelector = (hc / (map->Capacity / map->NSections)) >= map->NSections - 1 ? map->NSections - 1 : (hc / (map->Capacity / map->NSections));
	LPCRITICAL_SECTION Segment = map->cs[SegmentSelector];
	EnterCriticalSection(Segment);

	PLIST_ENTRY head = map->DispersionList[hc];
	PLIST_ENTRY node = head->Flink;
	while (head != node) {
		PPairNode ppair = CONTAINING_RECORD(node, PairNode, link);
		if(ppair->Key == key){
			*pval = ppair->Val;
			LeaveCriticalSection(Segment);
			return TRUE;
		}
		node = node->Flink;
	}
	LeaveCriticalSection(Segment);
	return FALSE;
}
// Acrescenta ou substitui o par chave/valor. Retorna TRUE em caso de adição,
// FALSE em caso de substituição
BOOL CHashMapPut(ConcurrentHashMap * map, ULONG key, DOUBLE val) {

	ULONG hc = HashCode(map, key);

	DWORD SegmentSelector = (hc / (map->Capacity / map->NSections)) >= map->NSections - 1 ? map->NSections - 1 : (hc / (map->Capacity / map->NSections));
	LPCRITICAL_SECTION Segment = map->cs[SegmentSelector];
	EnterCriticalSection(Segment);

	PPairNode newNode = (PPairNode)malloc(sizeof(PairNode));
	newNode->Key = key;
	newNode->Val = val;

	PLIST_ENTRY head = map->DispersionList[hc];
	PLIST_ENTRY node = head->Flink;
	while (head != node) {
		PPairNode ppair = CONTAINING_RECORD(node, PairNode, link);
		if (ppair->Key == key) {
			ppair->Key = key;
			ppair->Val = val;
			LeaveCriticalSection(Segment);
			return FALSE;
		}
		node = node->Flink;
	}
	InsertHeadList(head, &newNode->link);
	InterlockedIncrement(&map->Size);

	LeaveCriticalSection(Segment);
	return TRUE;
}
// Remove o par chave/valor retornando FALSE no caso da chave não existir
BOOL CHashMapRemove(ConcurrentHashMap * map, ULONG key) {
	int hc = HashCode(map, key);

	PLIST_ENTRY head = map->DispersionList[hc];
	PLIST_ENTRY node = head->Flink;
	while (head != node) {
		if (CONTAINING_RECORD(node, PairNode, link)->Key == key) {
			DWORD SegmentSelector = (hc / (map->Capacity / map->NSections)) >= map->NSections - 1 ? map->NSections - 1 : (hc / (map->Capacity / map->NSections));
			LPCRITICAL_SECTION Segment = map->cs[SegmentSelector];
			EnterCriticalSection(Segment);

			RemoveEntryList(node);
			InterlockedDecrement(&map->Size);
			LeaveCriticalSection(Segment);
			return TRUE;
		}
		node = node->Flink;
	}
	return FALSE;
}

// Retorna o número de pares chave/valor da coleção
ULONG CHashMapSize(ConcurrentHashMap * map) {
	return map->Size;
}

DWORD WorkerThread(void* data) {
	THREAD_ARG arg = *(PTHREAD_ARG)data;

	ConcurrentHashMap * map = (ConcurrentHashMap *)arg.Map;
	PLIST_ENTRY temp = (PLIST_ENTRY)malloc(sizeof(LIST_ENTRY));
	InitializeListHead(temp);

	for (DWORD i = arg.InitIdx;i < arg.InitIdx + arg.NBuckets;i++) {
		if (IsListEmpty(map->DispersionList[i])) {
			continue;
		}
		PLIST_ENTRY head = map->DispersionList[i];
		PLIST_ENTRY node = head->Flink;
		while (head != node) {
			PPairNode ppair = CONTAINING_RECORD(node, PairNode, link);
			PPairNode newNode = (PPairNode)malloc(sizeof(PairNode));
			memcpy(newNode, ppair, sizeof(PairNode));
			InsertHeadList(temp, &newNode->link);
			node = node->Flink;
		}
	}
	*arg.ListHead = *temp;
	SetEvent(arg.evt);
	return 0;
}

LIST_ENTRY CombinePartialLists(THREAD_ARG * args, DWORD size) {
	PLIST_ENTRY res = (PLIST_ENTRY)malloc(sizeof(LIST_ENTRY));
	InitializeListHead(res);

	//como nao há concorrencia não é necessario usar nenhum mecanismo de sincronização
	for (DWORD i = 0; i < size; i++) {
		//processar uma lista parcial
		LIST_ENTRY list = *args[i].ListHead;
		list.Flink->Blink = &list; list.Blink->Flink = &list;
		PLIST_ENTRY head = &list;
		PLIST_ENTRY node = head->Flink;
		if (IsListEmpty(node->Flink))
			continue;
		while (head != node) {
			PPairNode ppair = CONTAINING_RECORD(node, PairNode, link);
			PPairNode newNode = (PPairNode)malloc(sizeof(PairNode));
			memcpy(newNode, ppair, sizeof(PairNode));
			InsertHeadList(res, &newNode->link);
			node = node->Flink;
		}
	}
	return *res;
}
// Retorna uma lista formada por todos os pares chave/valor presentes na coleção map.
// Na implementação deverá tirar partido da multiplicidade de processadores do sistema.
LIST_ENTRY CHashMapToList(ConcurrentHashMap * map) {
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	ULONG NUMBER_OF_THREADS = sysinfo.dwNumberOfProcessors > map->Capacity ? map->Capacity : sysinfo.dwNumberOfProcessors;

	ULONG entriesPerThread = map->Capacity / NUMBER_OF_THREADS;
	ULONG remainingEntries = map->Capacity % NUMBER_OF_THREADS;

	HANDLE * evts = (HANDLE *)malloc(sizeof(HANDLE) * NUMBER_OF_THREADS);
	HANDLE * handles = (HANDLE *)malloc(sizeof(HANDLE) * NUMBER_OF_THREADS);
	THREAD_ARG * args = (THREAD_ARG *) malloc(sizeof(THREAD_ARG) * NUMBER_OF_THREADS);
	for (ULONG i = 0; i < NUMBER_OF_THREADS; i++) {
		
		args[i].Map = map;
		args[i].InitIdx = i * entriesPerThread + (i < remainingEntries ? i : remainingEntries);
		args[i].NBuckets = (entriesPerThread + (ULONG)(i < remainingEntries ? 1 : 0));
		args[i].ListHead = (PLIST_ENTRY)malloc(sizeof(LIST_ENTRY));
		InitializeListHead(args[i].ListHead);
		evts[i] = CreateEvent(NULL, true, false, NULL);
		args[i].evt = evts[i];
		QueueUserWorkItem(WorkerThread, &args[i], 0);
	}
	WaitForMultipleObjects(NUMBER_OF_THREADS, evts, TRUE, INFINITE);
	//WaitForMultipleObjects(NUMBER_OF_THREADS,handles,true,INFINITE);
	return CombinePartialLists(args, NUMBER_OF_THREADS);
}


LIST_ENTRY CHashMapToList1(ConcurrentHashMap * map) {

	PLIST_ENTRY temp = (PLIST_ENTRY)malloc(sizeof(LIST_ENTRY));
	InitializeListHead(temp);

	for (DWORD i = 0;i < map->Capacity;i++) {
		if (IsListEmpty(map->DispersionList[i])) {
			continue;
		}
		PLIST_ENTRY head = map->DispersionList[i];
		PLIST_ENTRY node = head->Flink;
		while (head != node) {
			PPairNode ppair = CONTAINING_RECORD(node, PairNode, link);
			PPairNode newNode = (PPairNode)malloc(sizeof(PairNode));
			memcpy(newNode, ppair, sizeof(PairNode));
			InsertHeadList(temp, &newNode->link);
			node = node->Flink;
		}
	}
	return *temp;
}
