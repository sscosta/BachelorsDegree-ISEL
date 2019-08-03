// MQTest.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <Windows.h>
#include "../MessageQueue/MessageQueue.h"

int main()
{
	SYSMQ_HANDLE mq = SysMqCreate("teste", sizeof(int), 100);

	for (int i = 1; i <= 100; ++i)
		SysMqPut(mq, &i);

	for (int i = 1; i <= 100; ++i) {
		int val;
		SysMqGet(mq, &val);
		printf("%d\n", val);
	}
	SysMqDestroy(mq);
	return 0;

}

