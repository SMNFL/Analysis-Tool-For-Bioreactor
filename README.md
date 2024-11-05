# AnalysisTool-for-Bioreactor

## Introduction

Welcome to the **AnalysisTool-for-Bioreactor** console application! This guide will walk you through the setup and usage of the tool.

### Prerequisites

1. **Update your username:** Replace `<currentUser>` with your actual username in the steps below Here’s the corrected sentence: (Example for saving the repo on Desktop).
2. **Install .NET 8.0:** If not installed, download it from [here](https://dotnet.microsoft.com/en-us/download/dotnet/8.0).
3. **Prepare your data:** Copy and paste your data table into the `InsertTableHere` folder (`/AnalysisTool-for-bioreactor/InsertTableHere`).
4. **Check the output:** After analysis, find the results in the `Output` folder (including data analysis tables and plot HTML files).

---

## Step-by-Step Guide (New Version)

### Windows:
1. Open `RunAnalysisTool_Windows.bat` using right-click with PowerShell.

### MacOS:
1. Open Terminal and run:
   	```bash
   	xattr -d com.apple.quarantine /path/to/Analysis-Tool-For-Bioreactor/RunAnalysisTool_MacOS.app
2. Open RunAnalysisTool_MacOS.app by double-clicking it.
(Note: If blocked due to an unidentified developer, go to Settings > Privacy & Security, scroll down, and press “Allow Anyway.”)

#### Alternative MacOS: 
1. Open Terminal, navigate to the folder:
   	```bash
	cd /Users/<currentUser>/Desktop/AnalysisTool-for-bioreactor/Analysis_Tool_dependencies
3. Run the application:
   	```bash
	pwsh ./run_Analysis_Tool.ps1
 (You may need to install PowerShell. If not installed, download it from [here](https://github.com/PowerShell/PowerShell)

## Step-by-Step Guide (Old Version for Windows or MacOS):
1. Open the console and navigate to the app data path.
   	```bash
   	cd /Users/<currentUser>/Desktop/AnalysisTool-for-bioreactor/Analysis_Tool_dependencies/Analysis/src/App
3. Start the application:
	```bash
 	dotnet run

## Input Format
Provide the input in the following format:
	
	202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 3
 or
 	
 	202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 all


### Parameters:
- **Filename:** The `.txt` data table must contain all 8 columns (Light, OD, and pump data).
- **Upper OD Threshold:** A number with a decimal point, e.g., `3.0`.
- **Lower OD Threshold:** A number with a decimal point, e.g., `3.0`.
- **Cylinder:** Choose a number between `1` and `8`, or use `all`.


## Troubleshooting

If the application fails and you cannot identify the issue, please contact foelling@rptu.de.












