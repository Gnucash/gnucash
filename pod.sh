#!/bin/bash
# Build and run GnuCash in a Fedora podman container, for development.
#
# Usage: ./pod.sh [run|rebuild|valgrind|shell]
#   run       configure/build/install, then launch GnuCash (default)
#   rebuild   wipe the build directory first, then run
#   valgrind  configure/build/install, then launch GnuCash under valgrind
#   shell     configure/build/install, then drop into a shell in the container
set -euo pipefail

IMAGE_NAME=${IMAGE_NAME:-gnucash-dev}
FEDORA_VERSION=${FEDORA_VERSION:-41}
ACTION=${1:-run}

case "$ACTION" in
  run|check|rebuild|valgrind|shell) ;;
  *)
    echo "usage: $0 [run|check|rebuild|valgrind|shell]" >&2
    exit 1
    ;;
esac

command -v podman >/dev/null 2>&1 || { echo "podman is required but not found in PATH" >&2; exit 1; }

# ---- Build the image (podman caches layers, so this is a no-op when unchanged) ----
podman build -t "$IMAGE_NAME" --build-arg "FEDORA_VERSION=$FEDORA_VERSION" -f - <<'EOF'
ARG FEDORA_VERSION
FROM fedora:${FEDORA_VERSION}
RUN dnf builddep -y gnucash
RUN dnf -y install perl-podlators ninja-build git webkit2gtk4.1{,-devel} \
    glibc-langpack-en glibc-langpack-fr
RUN dnf -y install gdk-pixbuf2-modules-extra # for Gdk to generate image from XPM
RUN dnf clean all
WORKDIR /app
EOF

# ---- Ensure persistent build volume ----
podman volume inspect gnucash-build >/dev/null 2>&1 || podman volume create gnucash-build

# ---- Build, install, and run inside the container ----
podman run -it --rm \
  -v "$PWD:/app:Z" \
  -v gnucash-build:/app/build:Z \
  -e DISPLAY="$DISPLAY" \
  -e DBUS_SESSION_BUS_ADDRESS="$DBUS_SESSION_BUS_ADDRESS" \
  -e XDG_RUNTIME_DIR="/run/user/$UID" \
  -e NO_AT_BRIDGE=1 \
  -e ACTION="$ACTION" \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  -v "/run/user/$UID:/run/user/$UID" \
  -v /etc/machine-id:/etc/machine-id:ro \
  "$IMAGE_NAME" bash -c '
    set -euo pipefail
    mkdir -p /app/build
    cd /app/build
    if [ "$ACTION" = rebuild ]; then
        echo "Cleaning build directory..."
        rm -rf ./*
    fi
    echo "Configuring CMake with install prefix..."
    cmake -GNinja .. -DCMAKE_BUILD_TYPE=Debug -DWITH_PYTHON=ON \
          -DCMAKE_INSTALL_PREFIX=/usr/local
    echo "Building with Ninja..."
    ninja
    echo "Installing to /usr/local..."
    ninja install > /dev/null
    case "$ACTION" in
      check)
        echo "Checking installed GnuCash..."
        ninja check
        ;;
      run|rebuild)
        echo "Running installed GnuCash..."
        /usr/local/bin/gnucash
        ;;
      valgrind)
        echo "Running installed GnuCash under Valgrind..."
        valgrind /usr/local/bin/gnucash
        ;;
      shell)
        exec bash
        ;;
    esac
  '
