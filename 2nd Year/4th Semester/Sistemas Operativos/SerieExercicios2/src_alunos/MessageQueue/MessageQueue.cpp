// MessageQueue.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include <stdlib.h>
#include <stdio.h>
#include "MessageQueue.h"
#include "MessageQueueImpl.h"

// local queue auxiliary functions

static VOID QueueInit(PMSG_QUEUE q, DWORD elemSize, DWORD capacity) {
	q->Capacity = capacity;
	q->ElemSize = elemSize;
	q->PutIdx = q->GetIdx = -1;
}

static VOID QueueGet(PMSG_QUEUE q, PVOID msg) {
	DWORD idx = InterlockedIncrement(&q->GetIdx);
	PBYTE msgStart = q->Msgs + (idx % q->Capacity)*q->ElemSize;
	memcpy(msg, msgStart, q->ElemSize);
}

static VOID QueuePut(PMSG_QUEUE q, PVOID msg) {
	DWORD idx = InterlockedIncrement(&q->PutIdx);
	PBYTE msgStart = q->Msgs + (idx % q->Capacity)*q->ElemSize;
	memcpy(msgStart, msg, q->ElemSize);
}
static VOID SysQueueGet(PMSG_QUEUE q, PVOID msg) {
	DWORD idx = InterlockedIncrement(&q->GetIdx);
	PBYTE msgStart = q->Msgs + (idx % q->Capacity)*q->ElemSize;
	memcpy(msg, msgStart, q->ElemSize);
}
static VOID SysQueuePut(PMSG_QUEUE q, PVOID msg) {
	DWORD idx = InterlockedIncrement(&q->PutIdx);
	PBYTE msgStart = q->Msgs + (idx % q->Capacity)*q->ElemSize;
	memcpy(msgStart, msg, q->ElemSize);
}

// local blocking queue auxiliary functions

static VOID MqInit( MQ_HANDLE mq, 
					PMSG_QUEUE queue, 
					DWORD elemSize, 
					DWORD capacity,
					PCSTR mqName
				   ) {
	
	QueueInit(queue, elemSize, capacity);
	mq->queue = queue;

	CHAR hasSpaceSemName[128], hasItemsSemName[128];

	if (mqName != NULL) {
		// in case the message queue has a name,
		// we create corresponding names for the mq semaphores
		sprintf_s(hasSpaceSemName, "SemHasSpace_%s", mqName);
		sprintf_s(hasItemsSemName, "SemHsItems_%s", mqName);
	}

	mq->hasSpace = CreateSemaphoreA(NULL, capacity, capacity,
		(mqName == NULL) ? NULL : hasSpaceSemName);
	mq->hasItems = CreateSemaphoreA(NULL, 0, capacity,
		(mqName == NULL) ? NULL : hasItemsSemName);

}

// public queue functions

MQ_HANDLE MqCreate(DWORD elemSize, DWORD capacity) {
	MQ_HANDLE mq = (MQ_HANDLE) malloc(sizeof(MQ));
	
	MqInit(mq,
		(PMSG_QUEUE)malloc(sizeof(MSG_QUEUE) + elemSize * capacity),
		elemSize,
		capacity,
		NULL /* no name in the local version */);
	return mq;
}

VOID MqGet(MQ_HANDLE mq, PVOID msg) {
	WaitForSingleObject(mq->hasItems, INFINITE);
	QueueGet((PMSG_QUEUE)mq->queue, msg);
	ReleaseSemaphore(mq->hasSpace, 1, NULL);
}

VOID MqPut(MQ_HANDLE mq, PVOID msg) {
	WaitForSingleObject(mq->hasSpace, INFINITE);
	QueuePut((PMSG_QUEUE)mq->queue, msg);
	ReleaseSemaphore(mq->hasItems, 1, NULL);
}

VOID MqDestroy(MQ_HANDLE mq) {	
	CloseHandle(mq->hasItems);
	CloseHandle(mq->hasSpace);
	free(mq->queue);
	free(mq);
}
 
VOID CreateSynchronizers(SYSMQ_HANDLE handle, DWORD capacity, PCSTR mqName) {

	CHAR hasSpaceSemName[128], hasItemsSemName[128];

	if (mqName != NULL) {
		// in case the message queue has a name,
		// we create corresponding names for the mq semaphores
		sprintf_s(hasSpaceSemName, "SemHasSpace_%s", mqName);
		sprintf_s(hasItemsSemName, "SemHsItems_%s", mqName);
	}

	handle->hasSpace = CreateSemaphoreA(NULL, capacity, capacity,
		(mqName == NULL) ? NULL : hasSpaceSemName);
	handle->hasItems = CreateSemaphoreA(NULL, 0, capacity,
		(mqName == NULL) ? NULL : hasItemsSemName);


	handle->hasSpaceCnt = capacity;
	handle->hasItemsCnt = 0;
}
SYSMQ_HANDLE SysMqCreate(PCSTR name, DWORD elemSize, DWORD capacity) {
	HANDLE hfm = CreateFileMappingA(
		INVALID_HANDLE_VALUE,
		NULL,
		PAGE_READWRITE,
		0,
		sizeof(MSG_QUEUE) + elemSize * capacity, name);

	if (hfm == NULL) {
		printf("Cannot create section object. Error #%d\n", GetLastError());
		return 0;
	}
	PVOID hmvof = MapViewOfFile(hfm, FILE_MAP_ALL_ACCESS, 0, 0, 0);
	if (hmvof == NULL) {
		printf("Cannot map view of file. Error #%d\n", GetLastError());
	}
	QueueInit((PMSG_QUEUE)hmvof, elemSize, capacity);
	SYSMQ_HANDLE handle = (SYSMQ_HANDLE)malloc(sizeof(SYSMQ));
	handle->queue = hmvof;
	CreateSynchronizers(handle, capacity, name);
	return handle;
}
VOID OpenSynchronizers(SYSMQ_HANDLE handle, PCSTR name) {
	CHAR hasSpaceSemName[128], hasItemsSemName[128];

	if (name != NULL) {
		// in case the message queue has a name,
		// we create corresponding names for the mq semaphores
		sprintf_s(hasSpaceSemName, "SemHasSpace_%s", name);
		sprintf_s(hasItemsSemName, "SemHsItems_%s", name);
	}
	handle->hasSpace = OpenSemaphoreA(SEMAPHORE_MODIFY_STATE, 0, hasSpaceSemName);
	handle->hasItems = OpenSemaphoreA(SEMAPHORE_MODIFY_STATE, 0, hasItemsSemName);
}
SYSMQ_HANDLE SysMqOpen(PCSTR name) {
	HANDLE hofm = OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, name);
	if (hofm == NULL) {
		printf("Cannot open section object. Error #%d\n", GetLastError());
		return 0;
	}
	PVOID hmvof = MapViewOfFile(hofm, FILE_MAP_WRITE, 0, 0, 0);
	if (hmvof == NULL) {
		printf("Cannot map view of file. Error #%d\n", GetLastError());
	}
	SYSMQ_HANDLE handle = (SYSMQ_HANDLE)malloc(sizeof(SYSMQ));
	handle->queue = hmvof;
	OpenSynchronizers(handle, name);

	return handle;
}

VOID SysMqGet(SYSMQ_HANDLE mq, PVOID msg) {
	WaitForSingleObject(mq->hasItems, INFINITE);
	SysQueueGet((PMSG_QUEUE)mq->queue, msg);
	ReleaseSemaphore(mq->hasSpace, 1, &mq->hasSpaceCnt);
}

VOID SysMqPut(SYSMQ_HANDLE mq, PVOID msg) {
	WaitForSingleObject(mq->hasSpace, INFINITE);
	SysQueuePut((PMSG_QUEUE)mq->queue, msg);
	bool success = ReleaseSemaphore(mq->hasItems, 1, &mq->hasItemsCnt);
	if (!success) {
		printf("Cannot map view of file. Error #%d\n", GetLastError());
	}
}

VOID SysMqDestroy(SYSMQ_HANDLE mq) {
	UnmapViewOfFile(mq->queue);
	CloseHandle(mq->hasItems);
	CloseHandle(mq->hasSpace);
	free(mq);
}
