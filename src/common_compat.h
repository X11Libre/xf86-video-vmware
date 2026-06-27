#ifndef _COMMON_COMPAT_H_
#define _COMMON_COMPAT_H_

#ifndef fbGetRotatedPixmap
#define fbGetRotatedPixmap(_pGC) NULL
#endif

#ifndef DRM_MODE_FB_DIRTY_MAX_CLIPS
#define DRM_MODE_FB_DIRTY_MAX_CLIPS 256
#endif

#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif

#endif
