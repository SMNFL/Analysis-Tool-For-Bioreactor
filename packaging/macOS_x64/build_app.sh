#!/bin/bash
set -euo pipefail

VERSION="${1:-1.0.0}"
APP_BUNDLE_NAME="Multicultivator-macos-x64"
APP_DISPLAY_NAME="Multicultivator"
PROJECT="src/App/App.fsproj"
RID="osx-x64"
PUBLISH_DIR="src/App/bin/Release/net8.0/${RID}/publish"
APP_DIR="releases/${APP_BUNDLE_NAME}.app"

rm -rf "$APP_DIR"
mkdir -p "$APP_DIR/Contents/MacOS"
mkdir -p "$APP_DIR/Contents/Resources"

cp -R "$PUBLISH_DIR"/. "$APP_DIR/Contents/MacOS/"
cp src/App/Assets/app_icon.icns "$APP_DIR/Contents/Resources/app_icon.icns"

EXECUTABLE_PATH=$(find "$PUBLISH_DIR" -maxdepth 1 -type f -perm -111 ! -name "*.dll" ! -name "*.json" ! -name "*.dylib" ! -name "*.so" ! -name "*.pdb" | head -n1)

if [ -z "$EXECUTABLE_PATH" ]; then
  echo "Error: no executable found in $PUBLISH_DIR"
  exit 1
fi

EXECUTABLE_NAME=$(basename "$EXECUTABLE_PATH")

cat > "$APP_DIR/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>${APP_DISPLAY_NAME}</string>
    <key>CFBundleDisplayName</key>
    <string>${APP_DISPLAY_NAME}</string>
    <key>CFBundleIdentifier</key>
    <string>com.smnfl.multicultivator</string>
    <key>CFBundleVersion</key>
    <string>${VERSION}</string>
    <key>CFBundleShortVersionString</key>
    <string>${VERSION}</string>
    <key>CFBundleExecutable</key>
    <string>${EXECUTABLE_NAME}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleIconFile</key>
    <string>app_icon.icns</string>
    <key>LSMinimumSystemVersion</key>
    <string>11.0</string>
    <key>NSHighResolutionCapable</key>
    <true/>
</dict>
</plist>
EOF

chmod +x "$APP_DIR/Contents/MacOS/${EXECUTABLE_NAME}"
xattr -dr com.apple.quarantine "$APP_DIR" 2>/dev/null || true
codesign --force --deep --sign - "$APP_DIR"

echo "Executable inside app: $EXECUTABLE_NAME"
echo "Created app bundle: $APP_DIR"