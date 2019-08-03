// MqClientTest.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>

#include <stdio.h>
#include <stdlib.h> 
#include "../MqEchoServerTest/EchoServer.h"


void fillMessage(PECHO_CMD cmd, int i) {
	cmd->clientId = GetCurrentThreadId();
	sprintf_s(cmd->msg, "Hello %d from %d", i, cmd->clientId);
}

int main()
{
 
	CHAR clientName[128];
	sprintf_s(clientName, "%d", GetCurrentThreadId());

	SYSMQ_HANDLE client_mq = SysMqCreate(clientName, sizeof(ECHO_CMD), 1);
	SYSMQ_HANDLE srv_mq = SysMqOpen("echo");


	printf("Starting echo client!\n\n");
	for(int i=0; i < 10000; ++i) {
		
		ECHO_CMD cmd;

		fillMessage(&cmd, i);
		SysMqPut(srv_mq, &cmd);

		 
		// get the response


		SysMqGet(client_mq, &cmd);

		printf("Echo received: %s\n", cmd.msg);

	}

	printf("End echo client, press return to continue...");
	getchar();

	SysMqDestroy(srv_mq);
	SysMqDestroy(client_mq);
	return 0;
}


 