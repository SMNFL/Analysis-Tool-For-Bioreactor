#!/bin/bash
set -euo pipefail

VERSION="${1:-1.0.0}"
APP_NAME="Multicultivator-macos-x64"
APP_PATH="releases/${APP_NAME}.app"
PKGROOT="pkgroot"
PKG_OUT="releases/${APP_NAME}.pkg"

if [ ! -d "$APP_PATH" ]; then
  echo "Error: app bundle not found at $APP_PATH"
  exit 1
fi

rm -rf "$PKGROOT"
mkdir -p "$PKGROOT/Applications"
cp -R "$APP_PATH" "$PKGROOT/Applications/"

pkgbuild \
  --root "$PKGROOT" \
  --install-location / \
  --identifier com.smnfl.multicultivator \
  --version ${VERSION} \
  "$PKG_OUT"

rm -rf "$PKGROOT"

echo "Created package: $PKG_OUT"