#  Copyright 2005 Adam Jackson.
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files (the "Software"),
#  to deal in the Software without restriction, including without limitation
#  on the rights to use, copy, modify, merge, publish, distribute, sub
#  license, and/or sell copies of the Software, and to permit persons to whom
#  the Software is furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice (including the next
#  paragraph) shall be included in all copies or substantial portions of the
#  Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  IN NO EVENT SHALL
#  ADAM JACKSON BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
#  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# Process this file with autoconf to produce a configure script

# Initialize Autoconf
AC_PREREQ([2.60])
AC_INIT([xlibre-xf86-video-vmware],
        [25.0.0],
        [https://github.com/X11Libre/xf86-video-vmware/issues],
        [xlibre-xf86-video-vmware])
AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR(.)

# Initialize Automake
AM_INIT_AUTOMAKE([foreign dist-xz])

# Require X.Org macros 1.8 or later for MAN_SUBSTS set by XORG_MANPAGE_SECTIONS
m4_ifndef([XORG_MACROS_VERSION],
          [m4_fatal([must install xorg-macros 1.8 or later before running autoconf/autogen])])
XORG_MACROS_VERSION(1.8)
XORG_DEFAULT_OPTIONS

# Initialize libtool
AC_DISABLE_STATIC
AC_PROG_LIBTOOL

# Checks for programs.
AM_PROG_CC_C_O

AH_TOP([#include "xorg-server.h"])

# Define a configure option for an alternate module directory
PKG_PROG_PKG_CONFIG([0.25])
AC_ARG_WITH(xorg-module-dir,
            AS_HELP_STRING([--with-xorg-module-dir=DIR],
                           [Default xorg module directory]),
            [moduledir="$withval"],
            [moduledir=`$PKG_CONFIG --variable=moduledir xorg-server`])

# Define a configure option to build the vmwarectrl client tool
AC_ARG_ENABLE(vmwarectrl-client,
              AS_HELP_STRING([--enable-vmwarectrl-client],
                             [Enable vmwarectrl client (default: disabled)]),
                             [VMWARECTRL=$enableval], [VMWARECTRL=no])
AM_CONDITIONAL(BUILD_VMWARECTRL, [test "x$VMWARECTRL" = xyes])

# Store the list of server defined optional extensions in REQUIRED_MODULES
XORG_DRIVER_CHECK_EXT(RANDR, randrproto)
XORG_DRIVER_CHECK_EXT(RENDER, renderproto)
XORG_DRIVER_CHECK_EXT(DPMSExtension, xextproto)
XORG_DRIVER_CHECK_EXT(XINERAMA, xineramaproto)
XORG_DRIVER_CHECK_EXT(XV, videoproto)

# Obtain compiler/linker options for the driver dependencies
PKG_CHECK_MODULES(XORG, [xorg-server >= 25.0.0 xproto fontsproto $REQUIRED_MODULES])

# Obtain compiler/linker options for the vmwarectrl client tool
PKG_CHECK_MODULES(X11, x11 xext)
PKG_CHECK_MODULES([PCIACCESS], [pciaccess >= 0.8.0])

AC_SUBST([moduledir])

if test x$BUILD_VMWGFX = xyes; then
	PKG_CHECK_MODULES([LIBDRM], [libdrm],[],[BUILD_VMWGFX=no])
fi
if test x$BUILD_VMWGFX = xyes; then
#
# Early versions of mesa 10 forgot to bump the XA major version number in
# the xa_tracker.h header
#
	PKG_CHECK_MODULES([XATRACKER], [xatracker >= 0.4.0],
			  [PKG_CHECK_EXISTS([xatracker = 2.0.0],
	                  [AC_DEFINE([HAVE_XA_2], 1,
               		  [Has version 2 of XA])])],
			  [BUILD_VMWGFX=no])
#
# Check for prime.
#
	PKG_CHECK_EXISTS([libdrm >= 2.4.38],
			 [AC_DEFINE([HAVE_LIBDRM_2_4_38], 1,
			 [Has version 2.4.38 or greater of libdrm])])
#
# Check for drmGetDeviceNameFromFd2. On linux we need
# 2.4.96 for compatibility with the standalone vmwgfx driver
#
	PKG_CHECK_EXISTS([libdrm >= 2.4.74],
			 [AC_DEFINE([HAVE_LIBDRM_2_4_74], 1,
			 [Has version 2.4.74 or greater of libdrm])])
	PKG_CHECK_EXISTS([libdrm >= 2.4.96],
			 [AC_DEFINE([HAVE_LIBDRM_2_4_96], 1,
			 [Has version 2.4.96 or greater of libdrm])])
fi

AC_MSG_CHECKING([whether to build Kernel Mode Setting and 3D])
if test x$BUILD_VMWGFX = xyes; then
	AC_MSG_RESULT([yes])
        AC_DEFINE([BUILD_VMWGFX], 1, [Building the vmwgfx driver path])
	libudev_check=yes
	AC_ARG_WITH([libudev],
		[AS_HELP_STRING([--without-libudev],
			[Use to build without libudev on linux])],
			[if test x$withval = xno; then libudev_check=no; fi]
			[])
	if test $libudev_check != no; then
		PKG_CHECK_MODULES(LIBUDEV, [libudev],
					   [AC_DEFINE([HAVE_LIBUDEV], 1,
					   [Has libudev installed])],
					   []);
        fi
else
	AC_MSG_RESULT([no])
fi

AM_CONDITIONAL(BUILD_VMWGFX, test "x$BUILD_VMWGFX" = xyes)

AC_CONFIG_FILES([
                Makefile
                man/Makefile
                saa/Makefile
                vmwgfx/Makefile
                src/Makefile
                vmwarectrl/Makefile
])

AC_OUTPUT
