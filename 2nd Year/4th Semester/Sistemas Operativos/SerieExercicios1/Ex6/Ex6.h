/////////////////////////////////////////////////////////////////
//
// ISEL 
// 2019
//
// System Information library:
//   Obtain process and global information on Windows environment.
//
// Authors:
//   André Ferreira, André Mendes, Samuel Costa
// 
#pragma once

#include <Windows.h>
#ifndef EX6_DLL
#define EX6_API __declspec(dllimport)
#else
#define EX6_API __declspec(dllexport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

EX6_API void PrintPerformanceInfo();
EX6_API void PrintProcessMemoryInfo(DWORD id, HANDLE hproc);
EX6_API void PrintMemoryStatus();
EX6_API void PrintWorkingSet(DWORD id, HANDLE hProc);
EX6_API void PrintMemInfo(DWORD id);


EX6_API LPVOID AllocBoundedBlock(size_t dwSize);
EX6_API VOID FreeBoundedBlock(LPVOID lpAddress);


EX6_API WORD GetTotalModules(HANDLE process);

#ifdef __cplusplus
} // extern "C"
#endif