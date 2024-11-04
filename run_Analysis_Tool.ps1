# Cross-platform navigation and execution
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition

# Navigate to the project directory relative to the script location
$projectPath = Join-Path -Path $scriptDir -ChildPath "Analysis_Tool_dependencies/Analysis/src/App" 
Set-Location -Path $projectPath

# Execute the Analysis-Tool
dotnet run