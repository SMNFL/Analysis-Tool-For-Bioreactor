<p align="center">
  <img src="../deps/src/App/Assets/app_icon_readme.png" alt="Multicultivator Logo" width="680"/>
</p>

<p align="center">
Desktop application for optical density based growth phase analysis in multicultivator systems<br><br>
<a href="../README.md">Main Overview</a> | <a href="../docs/analysis_summary.md">Analysis Summary</a> | <a href="../deps/README.md">App and Development</a> | <a href="README.md">Packaging and Installation</a>
</p>

## Download

Download the latest release from:

https://github.com/SMNFL/Analysis-Tool-For-Bioreactor/releases

---

## Overview

Multicultivator is a cross platform desktop application written in F# for analyzing optical density (OD) measurements.  
It detects growth phases, computes growth rates using robust regression, and exports structured results and interactive plots.

The application provides a graphical interface and does not require command line interaction for normal usage.

---

## Supported Platforms

- macOS (Apple Silicon and Intel)
- Windows (x64)
- Linux (x64)

---

## Requirements

Note: Prebuilt releases are self contained and do not require .NET to be installed.

### macOS
  - .NET 8 SDK
  - For `.icns` generation:
    - `sips`
    - `iconutil`

### Windows
  - .NET 8 SDK

### Linux
  - .NET 8 SDK
  - Required native libraries for Avalonia depending on distribution

---

## FSharp Project

### Build and Development

###### Build the project

From repository root:

```bash
dotnet build deps/src/App/App.fsproj
```

###### Run the app in development mode

```bash
dotnet run --project deps/src/App/App.fsproj
```

###### Clean build artifacts

```bash
dotnet clean deps/src/App/App.fsproj
```

### Publish

#### Publish self contained executables

###### macOS Apple Silicon
```bash
dotnet publish deps/src/App/App.fsproj -c Release -r osx-arm64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

###### macOS Intel
```bash
dotnet publish deps/src/App/App.fsproj -c Release -r osx-x64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

###### Windows x64
```bash
dotnet publish deps/src/App/App.fsproj -c Release -r win-x64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

###### Linux x64
```bash
dotnet publish deps/src/App/App.fsproj -c Release -r linux-x64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

#### Publish output folders

###### macOS Apple Silicon
```text
deps/src/App/bin/Release/net8.0/osx-arm64/publish/
```

###### macOS Intel
```text
deps/src/App/bin/Release/net8.0/osx-x64/publish/
```

###### Windows x64
```text
deps/src/App/bin/Release/net8.0/win-x64/publish/
```

###### Linux x64
```text
deps/src/App/bin/Release/net8.0/linux-x64/publish/
```

## App Icon

### Create Windows `.ico` icon

Source image should ideally be 1024 x 1024 PNG.

```bash
magick deps/src/App/Assets/app_icon.png -define icon:auto-resize=256,128,64,48,32,16 deps/src/App/Assets/app_icon.ico
```

### Create macOS `.icns` icon

```bash
rm -rf deps/src/App/Assets/app_icon.iconset
mkdir deps/src/App/Assets/app_icon.iconset

sips -z 16 16 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_16x16.png
sips -z 32 32 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_16x16@2x.png
sips -z 32 32 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_32x32.png
sips -z 64 64 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_32x32@2x.png
sips -z 128 128 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_128x128.png
sips -z 256 256 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_128x128@2x.png
sips -z 256 256 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_256x256.png
sips -z 512 512 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_256x256@2x.png
sips -z 512 512 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_512x512.png
sips -z 1024 1024 deps/src/App/Assets/app_icon.png --out deps/src/App/Assets/app_icon.iconset/icon_512x512@2x.png

iconutil -c icns deps/src/App/Assets/app_icon.iconset -o deps/src/App/Assets/app_icon.icns
```

### App project icon configuration

In `deps/src/App/App.fsproj` use:

```xml
<<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <Version>0.1.0-local</Version>
    <AssemblyVersion>0.1.0.0</AssemblyVersion>
    <FileVersion>0.1.0.0</FileVersion>
    <InformationalVersion>0.1.0-local</InformationalVersion>
    <AssemblyName>Multicultivator</AssemblyName>
    <RootNamespace>AnalysisTool</RootNamespace>
    <OutputType>WinExe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <UseAppHost>true</UseAppHost>
    <ApplicationIcon>Assets\app_icon.ico</ApplicationIcon>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="Icon.fs" />
    <Compile Include="UI.fs" />
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="..\Library\Library.fsproj" />
    <ProjectReference Include="..\Function\Function.fsproj" />
  </ItemGroup>

  <ItemGroup>
    <PackageReference Include="Avalonia" Version="11.1.3" />
    <PackageReference Include="Avalonia.Desktop" Version="11.1.3" />
    <PackageReference Include="Avalonia.FuncUI" Version="1.5.1" />
    <PackageReference Include="Avalonia.FuncUI.Elmish" Version="1.5.1" />
    <PackageReference Include="Avalonia.Themes.Fluent" Version="11.1.3" />
    <PackageReference Include="Elmish" Version="4.2.0" />
  </ItemGroup>

  <ItemGroup>
    <AvaloniaResource Include="Assets\**" />
    <None Include="Assets\app_icon.ico">
      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
    <None Include="Assets\app_icon.png">
      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
    <None Include="Assets\app_icon.icns">
      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
  </ItemGroup>

</Project>
```

### Runtime window icon helper

Use this in `icon.fs`:

```fsharp
namespace AnalysisTool

module WindowIconHelper =

    open System
    open System.IO
    open Avalonia.Controls

    let trySetWindowIcon (window: Window) =
        try
            let iconPath = Path.Combine(AppContext.BaseDirectory, "Assets", "app_icon.ico")
            if File.Exists iconPath then
                window.Icon <- WindowIcon(iconPath)
        with _ ->
            ()
```

Then call it in `MainWindow()`:

```fsharp
type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Multicultivator Analysis"
        base.Width <- 700.0
        base.Height <- 800.0
        WindowIconHelper.trySetWindowIcon this

        UI.createApp ()
        |> Program.withHost this
        |> Program.runWithAvaloniaSyncDispatch ()
```


## Build a macOS `.app` bundle

First publish:

```bash
dotnet publish deps/src/App/App.fsproj -c Release -r osx-arm64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

Then create the bundle using the provided script (recommended):

```bash
# for arm64
chmod +x packaging/macOS_arm64/build_app.sh
packaging/macOS_arm64/build_app.sh "0.1.0-local"
```

```bash
#for x64
chmod +x packaging/macOS_x64/build_app.sh
packaging/macOS_x64/build_app.sh "0.1.0-local"
```

Alternatively, you can create the `.app` bundle manually as shown below.

### Manual `.app` bundle creation (alternative)

- Copy all files from the publish folder into `Multicultivator-macos-arm64.app/Contents/MacOS/` or `Multicultivator-macos-x64.app/Contents/MacOS/`
- Add `Info.plist` and `app_icon.icns`
- Ensure executable permissions are set

```bash
# for arm64
rm -rf releases/Multicultivator-macos-arm64.app
mkdir -p releases/Multicultivator-macos-arm64.app/Contents/MacOS
mkdir -p releases/Multicultivator-macos-arm64.app/Contents/Resources
```

Copy all published files:


```bash
# for arm64
cp -R deps/src/App/bin/Release/net8.0/osx-arm64/publish/* Multicultivator-macos-arm64.app/Contents/MacOS/
```

Copy the icon:

```bash
# for arm64
cp deps/src/App/Assets/app_icon.icns Multicultivator-macos-arm64.app/Contents/Resources/app_icon.icns
```

Create `Info.plist`:

```bash
cat > Multicultivator-macos-arm64.app/Contents/Info.plist <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>Multicultivator</string>
    <key>CFBundleDisplayName</key>
    <string>Multicultivator</string>
    <key>CFBundleIdentifier</key>
    <string>com.simon.multicultivator</string>
    <key>CFBundleVersion</key>
    <string>0.1.0-local</string>
    <key>CFBundleShortVersionString</key>
    <string>0.1.0-local</string>
    <key>CFBundleExecutable</key>
    <string>App</string>
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
```

Make sure the apphost is executable:

```bash
chmod +x releases/Multicultivator-macos-arm64.app/Contents/MacOS/App
```

Test directly:

```bash
./releases/Multicultivator-macos-arm64.app/Contents/MacOS/App
```

Then open normally:

```bash
open releases/Multicultivator-macos-arm64.app
```

### Create `.dmg` or `.pkg` installer

Once the `.app` bundle works correctly, create a distributable disk image:

```bash
# for arm64 .dmg
# wrapped in sh script: hdiutil create -volname "Multicultivator" -srcfolder releases/Multicultivator-macos-arm64.app -ov -format UDZO releases/Multicultivator-macos-arm64.dmg
chmod +x packaging/macOS_arm64/build_dmg.sh
packaging/macOS_arm64/build_dmg.sh
```

```bash
# for x64 .dmg
# wrapped in sh script: hdiutil create -volname "Multicultivator" -srcfolder releases/Multicultivator-macos-x64.app -ov -format UDZO releases/Multicultivator-macos-x64.dmg
chmod +x packaging/macOS_x64/build_dmg.sh
packaging/macOS_x64/build_dmg.sh
```

```bash
# for arm64 .pkg 
chmod +x packaging/macOS_arm64/build_pkg.sh
packaging/macOS_arm64/build_pkg.sh "0.1.0-local"
```

```bash
# for x64 .pkg
chmod +x packaging/macOS_x64/build_pkg.sh
packaging/macOS_x64/build_pkg.sh "0.1.0-local"
```

If macOS blocks it once:

```bash
# for arm64
xattr -dr com.apple.quarantine releases/Multicultivator-macos-arm64.app
open releases/Multicultivator-macos-arm64.app
```

```bash
# for x64
xattr -dr com.apple.quarantine releases/Multicultivator-macos-x64.app
open releases/Multicultivator-macos-x64.app
```

## Windows packaging

First publish:

```bash
dotnet publish deps/src/App/App.fsproj -c Release -r win-x64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

Output:

```text
deps/src/App/bin/Release/net8.0/win-x64/publish/
```

Inside that folder there will be the Windows executable.

For first testing it can be easier to temporarily use console mode.

In `App.fsproj` set:

```xml
<OutputType>Exe</OutputType>
```

This way startup errors are visible in a console window.

For the final desktop app set it back to:

```xml
<OutputType>WinExe</OutputType>
```

### Windows `.exe` Installer

```pwsh
# (alternative windows.zip)
Compress-Archive -Path deps/src/App/bin/Release/net8.0/win-x64/publish/* -DestinationPath releases/Multicultivator-win-x64.zip
```

```bash
# (alternative windows.zip)
cd deps/src/App/bin/Release/net8.0/win-x64/publish
zip -r "$(pwd | sed 's#/deps/src/App/bin/Release/net8.0/win-x64/publish##')/releases/Multicultivator-win-x64.zip" .
```

1. Publish for Windows  
2. Install Inno Setup (https://jrsoftware.org/isinfo.php)

```bash
## use inno setup on macOS
brew install --cask wine-stable
wine "$HOME/Downloads/innosetup-6.7.1.exe"
```

3. Create a new installer script in the `packaging/` folder named `multicultivator.iss` with the following content:

```ini
[Setup]
AppName=Multicultivator
AppVersion=0.1.0-local
DefaultDirName={autopf}\Multicultivator
DefaultGroupName=Multicultivator
OutputDir=..\..\releases
OutputBaseFilename=MulticultivatorSetup
Compression=lzma
SolidCompression=yes
WizardStyle=modern
SetupIconFile=..\..\deps\src\App\Assets\app_icon.ico
UninstallDisplayIcon={app}\Multicultivator.exe

[Files]
Source: "..\..\deps\src\App\bin\Release\net8.0\win-x64\publish\*"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; Flags: checkedonce

[Icons]
Name: "{group}\Multicultivator"; Filename: "{app}\Multicultivator.exe"
Name: "{autodesktop}\Multicultivator"; Filename: "{app}\Multicultivator.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\Multicultivator.exe"; Description: "Launch Multicultivator"; Flags: nowait postinstall skipifsilent
```

4. Open the `.iss` file in Inno Setup and click **Build**

```bash
## use inno setup on macOS
cd /path/to/Analysis-Tool-For-Bioreactor
wine "$HOME/.wine/drive_c/users/$USER/AppData/Local/Programs/Inno Setup 6/ISCC.exe" packaging/windows_x64/multicultivator.iss
```

This will generate an installer:

```
releases/MulticultivatorSetup.exe
```

---

## Linux packaging

Publish:

```bash
dotnet publish deps/src/App/App.fsproj -c Release -r linux-x64 --self-contained true -p:Version=0.1.0-local -p:InformationalVersion=0.1.0-local -p:AssemblyVersion=0.1.0.0 -p:FileVersion=0.1.0.0
```

Output:

```text
deps/src/App/bin/Release/net8.0/linux-x64/publish/
```

Run on Linux:

```bash
./App
```

Depending on the Linux distribution, required system packages for GUI rendering may need to be installed.

### Linux `.tar.gz`

```bash
tar -czvf releases/Multicultivator-linux.tar.gz -C deps/src/App/bin/Release/net8.0/linux-x64/publish .
```

---

## Project Structure

```
deps/
  src/
    App/        UI (Avalonia)
    Function/   analysis pipeline
    Library/    shared types and helpers
```

---

## Support

For support or inquiries, please contact foelling@rptu.de. I will do my best to assist you.
