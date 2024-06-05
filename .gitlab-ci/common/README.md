common CI stuff - supposed to be synced across all drivers

moving this to a dedicated CI component is left for a later exercise.

Some scripts can also be used locally, eg. in a VM:

Prepare the image:

    .gitlab-ci/common/debian/image-install.sh
    .gitlab-ci/common/freebsd/image-install.sh

Build the xserver + driver:

    .gitlab-ci/common/build-driver.sh debian <xserver git ref>
    .gitlab-ci/common/build-driver.sh freebsd <xserver git ref>

Build just the xserver:

    .gitlab-ci/common/build-xserver.sh debian <xserver git ref>
    .gitlab-ci/common/build-xserver.sh freebsd <xserver git ref>
