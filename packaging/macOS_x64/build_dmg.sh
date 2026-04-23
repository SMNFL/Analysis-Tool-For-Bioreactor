#!/bin/bash
set -euo pipefail

APP_BUNDLE_NAME="Multicultivator-macos-x64"
APP_DISPLAY_NAME="Multicultivator"
BACKGROUND_IMAGE="packaging/assets/dmg_background_blank_resized.png"
APP_DIR="releases/${APP_BUNDLE_NAME}.app"
DMG_NAME="releases/${APP_BUNDLE_NAME}.dmg"
DMG_CONTENTS="dmg_contents_x64"

cleanup() {
  rm -rf "$DMG_CONTENTS"
  rm -f releases/rw.*.dmg 2>/dev/null || true
}

if [ ! -d "$APP_DIR" ]; then
  echo "Error: app bundle not found at $APP_DIR"
  exit 1
fi

rm -rf "$DMG_CONTENTS"
mkdir -p "$DMG_CONTENTS"

cp -R "$APP_DIR" "$DMG_CONTENTS/${APP_DISPLAY_NAME}.app"
# ln -s /Applications "$DMG_CONTENTS/Applications"

rm -f "$DMG_NAME"

# hdiutil create \
#   -volname "$APP_DISPLAY_NAME" \
#   -srcfolder "$DMG_CONTENTS" \
#   -ov \
#   -format UDZO \
#   "$DMG_NAME"
create-dmg \
  --volname "${APP_DISPLAY_NAME}" \
  --window-size 800 400 \
  --background "${BACKGROUND_IMAGE}" \
  --icon-size 100 \
  --icon "${APP_DISPLAY_NAME}.app" 250 230 \
  --hide-extension "${APP_DISPLAY_NAME}.app" \
  --app-drop-link 550 230 \
  "$DMG_NAME" \
  "$DMG_CONTENTS"

rm -rf "$DMG_CONTENTS"

echo "Created DMG: $DMG_NAME"