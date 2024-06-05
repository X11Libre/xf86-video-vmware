#!/usr/bin/env bash

set -e

# note: really wanna install to /usr/local, since that's explicitly searched first,
# so we always catch the locally installed before any system/ports provided one
# otherwise we might run into trouble like trying to use outdated xorgproto
build_autoconf() {
    local subdir="$1"
    shift
    (
        cd $subdir
        ./autogen.sh --prefix=/usr/local "$@"
        make -j${FDO_CI_CONCURRENT:-4}
        make -j${FDO_CI_CONCURRENT:-4} install
    )
}

build_meson() {
    local subdir="$1"
    shift
    (
        cd $subdir
        meson _build -Dprefix=/usr/local "$@"
        ninja -C _build -j${FDO_CI_CONCURRENT:-4} install
    )
}

do_clone() {
    git clone "$1" --depth 1 --branch="$2"
}

cp .gitlab-ci/common/freebsd/FreeBSD.conf /etc/pkg

pkg upgrade -f -y

pkg install -y \
    git gcc pkgconf autoconf automake libtool xorg-macros xorgproto meson \
    ninja pixman xtrans libXau libXdmcp libXfont libXfont2 libxkbfile libxcvt \
    libpciaccess font-util libepoll-shim libdrm mesa-libs libdrm libglu mesa-dri \
    libepoxy nettle xkbcomp libXvMC xcb-util valgrind libXcursor libXScrnSaver \
    libXinerama libXtst evdev-proto libevdev libmtdev libinput spice-protocol \
    libspice-server

[ -f /bin/bash ] || ln -sf /usr/local/bin/bash /bin/bash

# Xwayland requires drm 2.4.116 for drmSyncobjEventfd
# xf86-video-freedreno and xf86-video-omap need extra features
echo "Installing libdrm"
do_clone https://gitlab.freedesktop.org/mesa/drm libdrm-2.4.116
(
    cd drm
    git config user.email "buildbot@freebsd"
    git config user.name "FreeBSD build bot"
    git am ../.gitlab-ci/common/freebsd/libdrm-2.4.116.patch
)
build_meson drm -Dfreedreno=enabled -Dnouveau=enabled -Domap=enabled

echo "=== post-install script END"
