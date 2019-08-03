#include "stdafx.h"
#include "CppUnitTest.h"
#include "../CHashMap/ConcurrentHashMap.h"
#include "List.h"
using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace ConcurrentHashMapTestUnits
{		

#define NUMBER_OF_PAIRS	1000
#define NUMBER_OF_WTHREADS	2

	typedef struct {
		PConcurrentHashMap map;
		DWORD startKey;
		DWORD * workCnt;
		DWORD * barrier1Cnt;
		DWORD * barrier2Cnt;
		HANDLE barrier1Evt;
		HANDLE barrier2Evt;
		HANDLE finishEvt;
	} ConcurrentTestCtx, *PConcurrentTestCtx;

#define BARRIER1(pctx, localCnt)									\
			if ((localCnt) == NUMBER_OF_WTHREADS)					\
				/* Barrier 1 striked. */							\
				SetEvent((pctx)->barrier1Evt);						\
			else													\
				WaitForSingleObject((pctx)->barrier1Evt, INFINITE)
#define BARRIER2(pctx, localCnt)									\
			if ((localCnt) == NUMBER_OF_WTHREADS * 2)				\
				/* Barrier 2 striked. */							\
				SetEvent((pctx)->barrier2Evt);						\
			else													\
				WaitForSingleObject((pctx)->barrier2Evt, INFINITE)

	DWORD WINAPI WorkerThreadPut(LPVOID arg) {
		PConcurrentTestCtx pctx = (PConcurrentTestCtx)arg;
		DWORD localWorkCnt = InterlockedIncrement(pctx->barrier1Cnt);
		// Barrier 1. All threads begin at the same time to put the pairs interval.
		BARRIER1(pctx, localWorkCnt);

		for (DWORD i = pctx->startKey; i < pctx->startKey + NUMBER_OF_PAIRS; i++)
			CHashMapPut(pctx->map, i, i);

		// Barrier 2. All threads begin at the same time to put and remove the pairs interval.
		localWorkCnt = InterlockedIncrement(pctx->barrier2Cnt);
		BARRIER2(pctx, localWorkCnt);

		for (DWORD i = NUMBER_OF_PAIRS * NUMBER_OF_WTHREADS + pctx->startKey;
			i < NUMBER_OF_PAIRS * NUMBER_OF_WTHREADS + pctx->startKey + NUMBER_OF_PAIRS; i++)
			CHashMapPut(pctx->map, i, i);

		localWorkCnt = InterlockedDecrement(pctx->workCnt);
		if (localWorkCnt == 0)
			SetEvent(pctx->finishEvt);

		return 0;
	}
	DWORD WINAPI WorkerThreadRemove(LPVOID arg) {
		PConcurrentTestCtx pctx = (PConcurrentTestCtx)arg;
		// Barrier 2. All threads begin at the same time to put and remove the pairs interval.
		DWORD localWorkCnt = InterlockedIncrement(pctx->barrier2Cnt);
		BARRIER2(pctx, localWorkCnt);

		for (DWORD i = pctx->startKey; i < pctx->startKey + NUMBER_OF_PAIRS; i++)
			CHashMapRemove(pctx->map, i);

		localWorkCnt = InterlockedDecrement(pctx->workCnt);
		if (localWorkCnt == 0)
			SetEvent(pctx->finishEvt);

		return 0;
	}

	TEST_CLASS(UnitTest1)
	{
	public:
		
		TEST_METHOD(BasicPutGetTest)
		{
			const DWORD numberOfPairs = 100;
			Logger::WriteMessage("Basic put/get test");
			ConcurrentHashMap * map = CHashMapCreate(10);
			BOOL res;
			for (DWORD i = 0; i < numberOfPairs; i++) 
				CHashMapPut(map, i, i);

			DOUBLE val;
			for (DWORD i = 0; i < numberOfPairs; i++) {
				res = CHashMapGet(map, /*i * 10*/i, &val);
				Assert::IsTrue(res, NULL, LINE_INFO());
				Assert::AreEqual((double)i, val, 0, LINE_INFO());
			}
			CHashMapDestroy(map);
		}
		TEST_METHOD(BasicPutRepetitionsTest)
		{
			Logger::WriteMessage("Basic put repetitions test");
			ConcurrentHashMap * map = CHashMapCreate(10);
			BOOL res;
			for (DWORD i = 0; i < 20; i++) {
				res = CHashMapPut(map, i, i);
				Assert::IsTrue(res, NULL, LINE_INFO());
			}
			Logger::WriteMessage("Basic put repetitions test: put with return TRUE ok");
			for (DWORD i = 0; i < 20; i++) {
				res = CHashMapPut(map, i, i);
				Assert::IsFalse(res, NULL, LINE_INFO());
			}
			Logger::WriteMessage("Basic put repetitions test: put with return FALSE ok");

			CHashMapDestroy(map);
		}
		TEST_METHOD(BasicGetOutTest)
		{
			Logger::WriteMessage("Basic get out test");
			ConcurrentHashMap * map = CHashMapCreate(10);

			DOUBLE val;
			BOOL res;
			for (DWORD i = 0; i < 10; i++) {
				res = CHashMapGet(map, i, &val);
				Assert::IsFalse(res, NULL, LINE_INFO());
			}
			CHashMapDestroy(map);
		}
		TEST_METHOD(BasicRemoveTest)
		{
			Logger::WriteMessage("Basic remove test");
			ConcurrentHashMap * map = CHashMapCreate(10);
			for (DWORD i = 0; i < 100; i++)
				CHashMapPut(map, /*10 * i*/i, i);

			BOOL res;
			for (DWORD i = 0; i < 50; i++) {
				res = CHashMapRemove(map, /*i * 10*/i);
				Assert::IsTrue(res, NULL, LINE_INFO());
			}
			Assert::AreEqual((double)50, (double)map->Size, 0, LINE_INFO());
			Logger::WriteMessage("Basic remove test: remove with return TRUE ok");

			for (DWORD i = 0; i < 50; i++) {
				res = CHashMapRemove(map, /*i * 10*/i);
				Assert::IsFalse(res, NULL, LINE_INFO());
			}
			Logger::WriteMessage("Basic remove test: remove with return FALSE ok");

			for (DWORD i = 50; i < 100; i++) {
				res = CHashMapRemove(map, /*i * 10*/i);
				Assert::IsTrue(res, NULL, LINE_INFO());
			}
			Assert::AreEqual((double)0, (double)map->Size, 0, LINE_INFO());
			CHashMapDestroy(map);
		}
		TEST_METHOD(BasicToListTest)
		{
			const DWORD numberOfPairs = 100;
			Logger::WriteMessage("Basic to list test");
			ConcurrentHashMap * map = CHashMapCreate(10);
			for (DWORD i = 0; i < numberOfPairs; i++)
				CHashMapPut(map, /*10 * i*/i, i);

			LIST_ENTRY list = CHashMapToList(map); 
			// Must update references to new Sentinel
			list.Flink->Blink = &list; list.Blink->Flink = &list;
			
			DWORD cnt = 0;
			while (! IsListEmpty(&list)) {
				PPairNode pnode = CONTAINING_RECORD(RemoveHeadList(&list), PairNode, link);
				CHashMapRemove(map, pnode->Key);
				free(pnode);
				cnt += 1;
			}
			Assert::AreEqual((double)numberOfPairs, (double)cnt, 0, LINE_INFO());
			Assert::AreEqual((double)0, (double)map->Size, 0, LINE_INFO());
			CHashMapDestroy(map);
		}

		/*
		* Barrier 1 -> Concurrentely 2 threads put 1000 pairs each.
		* Barrier 2 -> Concurrentely 2 threads put more 1000 new pairs each and 2 threads remove first 1000 pairs.
		* At finish -> Validates the presence of last 2000 pairs in collection.
		*/
		TEST_METHOD(ConcurrentTest)
		{
			Logger::WriteMessage("Concurrent test");
			const DWORD numberOfWThreads = NUMBER_OF_WTHREADS;
			HANDLE finishEvt = CreateEvent(NULL, TRUE, FALSE, NULL);
			HANDLE barrier1Evt = CreateEvent(NULL, TRUE, FALSE, NULL);
			HANDLE barrier2Evt = CreateEvent(NULL, TRUE, FALSE, NULL);
			DWORD barrier1Cnt = 0;
			DWORD barrier2Cnt = 0;
			DWORD workCnt = numberOfWThreads * 2 + 1; // NUMBER_OF_WTHREADS Put + NUMBER_OF_WTHREADS Remove + self
			ConcurrentHashMap * map = CHashMapCreate(10);
			ConcurrentTestCtx ctx[numberOfWThreads * 2];
			for (DWORD i = 0; i < numberOfWThreads; i++) {
				ctx[i].finishEvt = finishEvt;
				ctx[i].barrier1Evt = barrier1Evt;
				ctx[i].barrier2Evt = barrier2Evt;
				ctx[i].barrier1Cnt = &barrier1Cnt;
				ctx[i].barrier2Cnt = &barrier2Cnt;
				ctx[i].workCnt = &workCnt;
				ctx[i].map = map; 
				ctx[i].startKey = i * NUMBER_OF_PAIRS;
				BOOL res = QueueUserWorkItem(WorkerThreadPut, &ctx[i], 0);
				Assert::IsTrue(res, 0, LINE_INFO());
			}
			for (DWORD i = 0; i < numberOfWThreads; i++) {
				ctx[numberOfWThreads + i].finishEvt = finishEvt;
				ctx[numberOfWThreads + i].barrier2Evt = barrier2Evt;
				ctx[numberOfWThreads + i].barrier2Cnt = &barrier2Cnt;
				ctx[numberOfWThreads + i].workCnt = &workCnt;
				ctx[numberOfWThreads + i].map = map;
				ctx[numberOfWThreads + i].startKey = i * NUMBER_OF_PAIRS;
				BOOL res = QueueUserWorkItem(WorkerThreadRemove, &ctx[i], 0);
				Assert::IsTrue(res, 0, LINE_INFO());
			}
			
			// Wait for work finish
			if (InterlockedDecrement(&workCnt) > 0)
				WaitForSingleObject(finishEvt, INFINITE);
			
			// Validates the presence of last 2000 pairs in collection.
			Assert::AreEqual((float)(NUMBER_OF_PAIRS * 2), (float)map->Size, 0, LINE_INFO());

			DOUBLE val;
			BOOL res;
			for (DWORD i = 0; i < map->Capacity; i++) {
				DWORD key_base = NUMBER_OF_PAIRS * NUMBER_OF_WTHREADS;
				res = CHashMapGet(map, key_base + i, &val);
				Assert::IsTrue(res, 0, LINE_INFO());
			}

			CHashMapDestroy(map);
		}
	};
}