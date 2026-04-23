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
SetupIconFile=..\..\src\App\Assets\app_icon.ico
UninstallDisplayIcon={app}\Multicultivator.exe

[Files]
Source: "..\..\src\App\bin\Release\net8.0\win-x64\publish\*"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; Flags: checkedonce

[Icons]
Name: "{group}\Multicultivator"; Filename: "{app}\Multicultivator.exe"
Name: "{autodesktop}\Multicultivator"; Filename: "{app}\Multicultivator.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\Multicultivator.exe"; Description: "Launch Multicultivator"; Flags: nowait postinstall skipifsilent