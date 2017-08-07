#ifndef _COMMON_COMPAT_H_
#define _COMMOM_COMPAT_H_

#if (GET_ABI_MAJOR(ABI_VIDEODRV_VERSION) >= 18)
#define CONST_ABI_18_0 const
#else
#define CONST_ABI_18_0
#endif

#endif

