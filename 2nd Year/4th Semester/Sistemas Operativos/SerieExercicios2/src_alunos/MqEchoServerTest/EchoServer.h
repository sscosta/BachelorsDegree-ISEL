#pragma once

#include "../MessageQueue/MessageQueue.h"

#define MAX_MSG 128

typedef struct {
	DWORD clientId;
	CHAR msg[MAX_MSG];
} ECHO_CMD, *PECHO_CMD;
