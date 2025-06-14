/* **********************************************************
 * Copyright (C) 1998-2001 VMware, Inc.
 * All Rights Reserved
 * **********************************************************/

/*
 *
 * vm_basic_types.h -- 
 *
 *    basic data types.
 */

 
#ifndef _VM_BASIC_TYPES_H_
#define _VM_BASIC_TYPES_H_

#define INCLUDE_ALLOW_USERLEVEL
#define INCLUDE_ALLOW_MONITOR
#define INCLUDE_ALLOW_MODULE
#define INCLUDE_ALLOW_VMKERNEL
#include "includeCheck.h"

/* STRICT ANSI means the Xserver build and X defines Bool differently. */
#if 0
typedef char           Bool;
#endif

#ifndef FALSE
#define FALSE          0
#endif

#ifndef TRUE
#define TRUE           1
#endif

#if defined(__GNUC__)
/* The Xserver source compiles with -ansi -pendantic */
#ifndef __STRICT_ANSI__
typedef unsigned long long uint64;
typedef long long int64;
#endif
#else
/* int64/uint64 aren't actually used in the vmware driver. */
#if 0
#error - Need compiler define for int64/uint64
#endif
#endif

typedef unsigned int       uint32;
typedef unsigned short     uint16;
typedef unsigned char      uint8;

typedef int       int32;
typedef short     int16;
typedef char      int8;

typedef uint32 VA;
typedef uint32 VPN;

typedef uint32 PA;
typedef uint32 PPN;

typedef uint32 MA;
typedef uint32 MPN;

#define INVALID_MPN ((MPN)-1)

#define EXTERN        extern
/*
 * Right now our use of CONST is broken enough that it only works
 * with GCC. XXX Need to fix this.
 */
#ifdef __GNUC__
#define CONST         const
#else
#ifndef CONST
#define CONST
#endif
#endif

#ifndef INLINE
#define INLINE        inline
#endif

#if defined(WIN32) && !defined(VMX86_NO_THREADS)
#define THREADSPECIFIC _declspec(thread) 
#else
#define THREADSPECIFIC
#endif

/* 
 * Like "INLINE" but use this token to mark functions that are inline
 * because they have only a single call site. In other words, if a second
 * call site is introduced, the "INLINE_SINGLE_CALLER" token should be 
 * removed. 
 */
#define INLINE_SINGLE_CALLER INLINE

/*
 * Attributes placed on function declarations to tell the compiler
 * that the function never returns.
 */
#if defined(__GNUC__) && __GNUC__ >= 2 && __GNUC_MINOR__ >= 5
#define NORETURN_DECL(_fndecl)    _fndecl __attribute__((__noreturn__))
#else
#define NORETURN_DECL(_fndecl)    _fndecl
#endif


/*
 * GCC's argument checking for printf-like functions
 * This is conditional until we have replaced all `"%x", void *'
 * with `"0x%08x", (uint32) void *'. Note that %p prints different things
 * on different platforms.
 *
 * fmtPos is the position of the format string argument, beginning at 1
 * varPos is the position of the variable argument, beginning at 1
 */
#if defined(__GNUC__) && defined(notdef)
# define PRINTF_DECL(fmtPos, varPos) __attribute__((__format__(__printf__, fmtPos, varPos)))
#else
# define PRINTF_DECL(fmtPos, varPos)
#endif

/*
 * Used to silence compiler warnings that get generated when the
 * compiler thinks that a function returns when it is marked noreturn.
 */
#define INFINITE_LOOP()           do { } while (1)

#endif
