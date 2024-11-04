# AnalysisTool-for-bioreactor

Introduction for console application of AnalysisTool-for-bioreactor (Save the hole folder "AnalysisTool-for-bioreactor")


Do!
	- change <currentUser> to your username below

	- install .NET 8.0 if you have not done before		(https://dotnet.microsoft.com/en-us/download/dotnet/8.0)

	- copy/paste your data table to "InsertTableHere" folder (/AnalysisTool-for-bioreactor/Analysis_Tool_dependencies/InsertTableHere) 

	- after analysis look into folder "Output"	(for the data analysis tables and plot html files)


Step by Step introduction (New Update):
	- Windows: 
		open the "run_Analysis_Tool.ps1" with the powershell
	- MacOS: 
		open the Terminal and navigate to the folder by copy/paste "cd /Users/<currentUser>/Desktop/AnalysisTool-for-bioreactor"
			run the application with "pwsh ./run_Analysis_Tool.ps1" (maybe you have to install PowerShell)

Step by Step introduction (Old Version):
	- open console and locate to the App data path

	e.g. if it saved on Desktop 
		copy/paste "cd /Users/<currentUser>/Desktop/AnalysisTool-for-bioreactor/Analysis_Tool_dependencies/Analysis/src/App" and press enter

	- start application
		copy/paste "dotnet run" and press enter

==> Now the application runs and you can read the introduction to use it!

TIPS!:
Give the Input like:
	"202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 3" 
Or 
	"202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 all"


[
	filename (filename of the .txt data table needs all 8 columns for Light- ,OD- and pumpdata),
	upper OD Threshold (needs to be a number with dot e.g. 3.0),
	lower OD Threshold (needs to be a number with dot e.g. 3.0),
	cylinder (choose "1" - "8" as single number or "all" as word)
]


If it fails and you don't know why please contact foelling@rptu.de
