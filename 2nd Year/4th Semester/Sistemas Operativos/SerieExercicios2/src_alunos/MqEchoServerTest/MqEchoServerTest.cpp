// MqEchoServerTest.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <stdio.h>
#include <stdlib.h> 
#include "EchoServer.h"

int main()
{
	SYSMQ_HANDLE srv_mq = SysMqCreate("echo", sizeof(ECHO_CMD), 128);

	printf("Starting echo server!\n\n");

	while (true) {
		ECHO_CMD cmd;
		SysMqGet(srv_mq, &cmd);

		if (cmd.clientId == 0) break;

		printf("Cmd received: %s\n", cmd.msg);
		// prepare the response
		 
		CHAR clientMqName[128];
		sprintf_s(clientMqName, "%d", cmd.clientId);
		SYSMQ_HANDLE client_mq = SysMqOpen(clientMqName);

		cmd.clientId = 0;
		SysMqPut(client_mq, &cmd);

		SysMqDestroy(client_mq);
	}

	SysMqDestroy(srv_mq);
	return 0;
}

