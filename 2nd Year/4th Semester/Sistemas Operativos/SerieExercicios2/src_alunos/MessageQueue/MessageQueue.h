#pragma once

#ifdef MESSAGEQUEUE_EXPORTS
#define MQ_API _declspec(dllexport)
#else
#define MQ_API _declspec(dllimport)
#endif

typedef struct {
	HANDLE hasSpace;
	HANDLE hasItems;
	PVOID queue;
} MQ, *MQ_HANDLE;

typedef struct {
	HANDLE hasSpace;
	HANDLE hasItems;
	PVOID queue;
	LONG hasItemsCnt;
	LONG hasSpaceCnt;
} SYSMQ, *SYSMQ_HANDLE;

MQ_API MQ_HANDLE MqCreate(DWORD elemSize, DWORD capacity);
	 
MQ_API VOID MqGet(MQ_HANDLE mq, PVOID msg);

MQ_API VOID MqPut(MQ_HANDLE mq, PVOID msg);
	 
MQ_API VOID MqDestroy(MQ_HANDLE mq);
	 

MQ_API SYSMQ_HANDLE SysMqCreate(PCSTR name, DWORD elemSize, DWORD capacity);
	 
MQ_API VOID SysMqGet(SYSMQ_HANDLE mq, PVOID msg);
	 

MQ_API VOID SysMqPut(SYSMQ_HANDLE mq, PVOID msg);
	 

MQ_API VOID SysMqDestroy(SYSMQ_HANDLE mq);


MQ_API SYSMQ_HANDLE SysMqOpen(PCSTR name);
	 

