#!/bin/bash
set -euo pipefail

VERSION="${1:-0.1.0-local}"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

APP_PROJECT="src/App/App.fsproj"
RELEASES_DIR="releases"

mkdir -p "$RELEASES_DIR"

echo "Cleaning old releases..."
rm -rf "$RELEASES_DIR"/*

echo "Building solution..."
dotnet clean "$APP_PROJECT"
dotnet build "$APP_PROJECT" -c Release

echo "Publishing macOS arm64..."
dotnet publish "$APP_PROJECT" -c Release -r osx-arm64 --self-contained true \
  -p:Version="$VERSION" \
  -p:InformationalVersion="$VERSION" \
  -p:AssemblyVersion=0.1.0.0 \
  -p:FileVersion=0.1.0.0

echo "Publishing macOS x64..."
dotnet publish "$APP_PROJECT" -c Release -r osx-x64 --self-contained true \
  -p:Version="$VERSION" \
  -p:InformationalVersion="$VERSION" \
  -p:AssemblyVersion=0.1.0.0 \
  -p:FileVersion=0.1.0.0

echo "Publishing Windows x64..."
dotnet publish "$APP_PROJECT" -c Release -r win-x64 --self-contained true \
  -p:Version="$VERSION" \
  -p:InformationalVersion="$VERSION" \
  -p:AssemblyVersion=0.1.0.0 \
  -p:FileVersion=0.1.0.0

echo "Publishing Linux x64..."
dotnet publish "$APP_PROJECT" -c Release -r linux-x64 --self-contained true \
  -p:Version="$VERSION" \
  -p:InformationalVersion="$VERSION" \
  -p:AssemblyVersion=0.1.0.0 \
  -p:FileVersion=0.1.0.0

echo "Building macOS .app bundles..."
chmod +x packaging/macOS_arm64/build_app.sh
chmod +x packaging/macOS_x64/build_app.sh

packaging/macOS_arm64/build_app.sh "$VERSION"
packaging/macOS_x64/build_app.sh "$VERSION"

echo "Building macOS DMGs..."
chmod +x packaging/macOS_arm64/build_dmg.sh
chmod +x packaging/macOS_x64/build_dmg.sh

packaging/macOS_arm64/build_dmg.sh
packaging/macOS_x64/build_dmg.sh

echo "Building macOS PKGs..."
chmod +x packaging/macOS_arm64/build_pkg.sh
chmod +x packaging/macOS_x64/build_pkg.sh

packaging/macOS_arm64/build_pkg.sh "$VERSION"
packaging/macOS_x64/build_pkg.sh "$VERSION"

echo "Creating Windows ZIP..."
cd src/App/bin/Release/net8.0/win-x64/publish
zip -r "$ROOT_DIR/$RELEASES_DIR/Multicultivator-win-x64.zip" .
cd "$ROOT_DIR"

echo "Building Windows setup EXE with Inno Setup via Wine..."
INNO_COMPILER="$HOME/.wine/drive_c/users/$USER/AppData/Local/Programs/Inno Setup 6/ISCC.exe"

if [ -f "$INNO_COMPILER" ]; then
  wine "$INNO_COMPILER" packaging/windows_x64/multicultivator.iss
else
  echo "Warning: Inno Setup compiler not found:"
  echo "$INNO_COMPILER"
  echo "Skipping Windows setup EXE."
fi

echo "Creating Linux tar.gz..."
tar -czvf "$RELEASES_DIR/Multicultivator-linux-x64.tar.gz" \
  -C src/App/bin/Release/net8.0/linux-x64/publish .

echo "Done."
echo ""
echo "Created artifacts:"
ls -lh "$RELEASES_DIR"