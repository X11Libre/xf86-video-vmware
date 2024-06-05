#!/usr/bin/env bash

set -e

PLATFORM="$1"
XSERVER_REF="$2"

if [ ! "$PLATFORM" ]; then
    echo "missing PLATFORM" >&2
    exit 1
fi

if [ ! "$XSERVER_REF" ]; then
    echo "missing XSERVER_REF" >&2
    exit 1
fi

.gitlab-ci/common/build-xserver.sh "$PLATFORM" "$XSERVER_REF"

MACH=`gcc -dumpmachine`
echo "Building on machine $MACH"

case "$PLATFORM" in
    freebsd)
        export PKG_CONFIG_PATH="/usr/lib/pkgconfig:/usr/libdata/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/libdata/pkgconfig"
        export ACLOCAL_PATH="/usr/share/aclocal:/usr/local/share/aclocal"
        export CFLAGS="$CFLAGS -I/usr/local/include"
        export UDEV_CFLAGS=" "
        export UDEV_LIBS=" "
    ;;
    debian)
        export PKG_CONFIG_PATH="/usr/lib/pkgconfig:/usr/share/pkgconfig"
    ;;
    *)
        echo "unknown platform $PLATFORM" >&2
    ;;
esac

if [ -f autogen.sh ]; then
    (
        echo "building driver via autotools"
        rm -Rf _builddir
        mkdir -p _builddir
        cd _builddir
        ../autogen.sh --disable-silent-rules
        make
        make check
        make distcheck
    )
elif [ -f meson.build ]; then
    (
        echo "building driver via meson"
        meson setup _build
        cd _build
        meson compile
        meson install
    )
else
    echo "failed detecting build system"
    exit 1
fi
