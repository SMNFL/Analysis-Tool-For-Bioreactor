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