<p align="center">
  <img src="deps/src/App/Assets/app_icon_readme.png" alt="Multicultivator Logo" width="680"/>
</p>
<p align="center">
Multicultivator Analysis — Cross platform desktop application for optical density based growth phase analysis<br><br>
<a href="README.md">Main Overview</a> | <a href="docs/analysis_summary.md">Analysis Summary</a> | <a href="deps/README.md">App and Development</a> | <a href="packaging/README.md">Packaging and Installation</a>
</p>

## Download and Run

Prebuilt releases are available for:

- macOS (Apple Silicon and Intel)
- Windows (x64)
- Linux (x64)

Download the latest version from:

https://github.com/SMNFL/Analysis-Tool-For-Bioreactor/releases

---

## Installation

### macOS

1. Download the `.dmg`
2. Open it
3. Drag `Multicultivator.app` into `Applications`
4. Start the application

If macOS blocks the app:

- Open System Settings
- Go to Privacy & Security
- Allow the application manually

### Windows

1. Download the installer `.exe`
2. Run the installer
3. Start from desktop or start menu

### Linux

1. Download the `.tar.gz`
2. Extract it
3. Run the executable inside the folder

---

## Usage

1. Launch the application
2. Select a tab separated `.txt` file
3. Configure parameters:
   - Upper OD threshold
   - Lower OD threshold
   - Cylinders to analyze (1–8 or all)
4. Run analysis

---

## Input File Requirements

The input must be a tab separated `.txt` file.

Required structure:

- Column `time`
- Columns containing `.od` (optical density)
- Columns containing `.pump`
- Columns containing `.light`

Each cylinder must include all three data types.

Cylinder assignment is inferred from naming patterns:

- `-1-`, `-2-`, …, `-8-`

---

## Output

Results are written automatically next to the selected input file.

A new folder is created:

```
results_multicultivator_<input_filename>/
```

Example:

```
experiment.txt
results_multicultivator_experiment/
```

Generated content:

- CSV tables with growth phase statistics
- Interactive HTML plots
- Debug log (only if errors occur)

---

## Features

- Growth phase detection from OD data
- Robust Theil–Sen regression
- Duplication time calculation
- Multi cylinder support (1–8 or all)
- Interactive Plotly HTML visualization
- Automatic result export

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

## Development

Run locally:

```
dotnet run --project deps/src/App/App.fsproj
```

Build:

```
dotnet build deps/src/App/App.fsproj
```

---

## Development and Packaging

Packaging scripts and installer definitions are located in:

```
packaging/
```

This includes:

- macOS app bundle and DMG creation
- macOS PKG installer creation
- Windows installer via Inno Setup

Build outputs are written to:

```
releases/
```

---

## Notes

- Built with Avalonia for cross platform UI
- Charts are exported as HTML for browser based interaction
- Released builds are self contained and require no additional dependencies

---

## Contact for Support

For support or inquiries, please contact foelling@rptu.de. I will do my best to assist you.