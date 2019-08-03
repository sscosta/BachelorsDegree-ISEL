#pragma once

typedef struct {
	DWORD Capacity;	// Queue capacity
	DWORD ElemSize; // Size of each element/message in queue
	DWORD GetIdx;   // Index to get element/message
	DWORD PutIdx;   // Index to put element/message
	BYTE  Msgs[1];  // Array of elements/messages
} MSG_QUEUE, *PMSG_QUEUE;


