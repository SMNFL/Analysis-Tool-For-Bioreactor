namespace AnalysisTool

open System
open System.Globalization
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Platform
open Avalonia.Platform.Storage
open AnalysisTool

module UI =

    type Model =
        { FilePath       : string
          UpperThreshold : string
          LowerThreshold : string
          Cylinder       : string
          Status         : string option
          IsRunning      : bool
          ProgressText   : string
          Logs           : string list }

    type Msg =
        | UpdateFilePath of string
        | BrowseFile
        | FilePicked of string
        | UpdateUpperThreshold of string
        | UpdateLowerThreshold of string
        | UpdateCylinder of string
        | RunAnalysis
        | AddLog of string
        | SetProgressText of string
        | AnalysisFinished of Result<unit, exn>

    let init : Model =
        { FilePath = ""
          UpperThreshold = ""
          LowerThreshold = ""
          Cylinder = ""
          Status = None
          IsRunning = false
          ProgressText = ""
          Logs = [] }

    let private tryParseFloat (s: string) =
        match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

    let private parseCylinders (input: string) : int list =
        if String.Equals(input.Trim(), "all", StringComparison.OrdinalIgnoreCase) then
            [ 0 .. 7 ]
        else
            match Int32.TryParse(input.Trim()) with
            | true, n when n >= 1 && n <= 8 -> [ n - 1 ]
            | _ -> [ 0 ]

    let update (msg: Msg) (state: Model) : Model * Cmd<Msg> =
        match msg with
        | UpdateFilePath path ->
            { state with FilePath = path }, Cmd.none

        | BrowseFile ->
            let runEffect (dispatch: Msg -> unit) : unit =
                Async.StartImmediate (async {
                    try
                        let lifetime =
                            Application.Current.ApplicationLifetime
                            :?> Avalonia.Controls.ApplicationLifetimes.IClassicDesktopStyleApplicationLifetime

                        let mainWindow = lifetime.MainWindow

                        let txtType = FilePickerFileType("Text files")
                        txtType.Patterns <- System.Collections.Generic.List<string>([ "*.txt" ])
                        txtType.MimeTypes <- System.Collections.Generic.List<string>([ "text/plain" ])

                        let options = FilePickerOpenOptions()
                        options.Title <- "Select input table (.txt)"
                        options.AllowMultiple <- false
                        options.FileTypeFilter <- System.Collections.Generic.List<FilePickerFileType>([ txtType ])

                        let! files =
                            mainWindow.StorageProvider.OpenFilePickerAsync(options)
                            |> Async.AwaitTask

                        if not (isNull files) && files.Count > 0 then
                            let path = files.[0].TryGetLocalPath()
                            match path with
                            | null -> dispatch (AddLog "Selected file has no local path.")
                            | p -> dispatch (FilePicked p)

                    with ex ->
                        dispatch (AddLog $"File dialog error: {ex.Message}")
                })

            state, Cmd.ofEffect runEffect

        | FilePicked path ->
            { state with FilePath = path }, Cmd.none

        | UpdateUpperThreshold u ->
            { state with UpperThreshold = u }, Cmd.none

        | UpdateLowerThreshold l ->
            { state with LowerThreshold = l }, Cmd.none

        | UpdateCylinder c ->
            { state with Cylinder = c }, Cmd.none

        | AddLog logMsg ->
            { state with Logs = state.Logs @ [ logMsg ] }, Cmd.none

        | SetProgressText txt ->
            { state with ProgressText = txt }, Cmd.none

        | RunAnalysis ->
            let maybeUpper = tryParseFloat state.UpperThreshold
            let maybeLower = tryParseFloat state.LowerThreshold

            match maybeUpper, maybeLower with
            | Some upper, Some lower ->
                let cylIndices = parseCylinders state.Cylinder

                let runEffect (dispatch: Msg -> unit) : unit =
                    Async.Start (async {
                        try
                            dispatch (SetProgressText "Running analysis...")
                            
                            do! Async.SwitchToThreadPool()

                            let! result =
                                async {
                                    return
                                        AnalysisFunction.analysis
                                            state.FilePath
                                            upper
                                            lower
                                            cylIndices
                                            (fun message -> 
                                                dispatch (AddLog message)
                                                if not (String.IsNullOrWhiteSpace message) then
                                                    System.Threading.Thread.Sleep(50)
                                            )
                                }

                            dispatch (AnalysisFinished result)
                        with ex ->
                            dispatch (AnalysisFinished (Error ex))
                    })

                { state with
                    IsRunning = true
                    Status = Some "Running analysis..."
                    ProgressText = "Starting analysis..."
                    Logs = [] },
                Cmd.ofEffect runEffect

            | _ ->
                { state with Status = Some "Error: invalid numeric input for thresholds" }, Cmd.none

        | AnalysisFinished result ->
            let newStatus, finalLogs =
                match result with
                | Ok _ ->
                    Some "Analysis completed successfully. Charts saved to the Output folder.",
                    [""]
                    // [ "Analysis successful"
                    //   "Charts saved to Output" ]
                | Error e ->
                    Some (sprintf "Analysis failed: %s" e.Message),
                    [""]
                    // [ sprintf "Error: %s" e.Message
                    //   sprintf "Exception type: %s" (e.GetType().FullName)
                    //   "Analysis failed!"
                    //   e.ToString() ]

            { state with
                IsRunning = false
                Status = newStatus
                ProgressText = ""
                Logs = state.Logs @ finalLogs },
            Cmd.none

    let view (state: Model) (dispatch: Msg -> unit) =
        DockPanel.create [
            DockPanel.background (SolidColorBrush(Color.Parse("#434549")))
            DockPanel.lastChildFill true
            DockPanel.children [
                ScrollViewer.create [
                    ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.margin 20.0
                            StackPanel.spacing 10.0
                            StackPanel.children [
                                // New header section with logo and title
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                                    StackPanel.children [
                                        Image.create [
                                            Image.source (new Bitmap(AssetLoader.Open(new Uri("avares://Multicultivator/Assets/app_icon_readme_transparent.png"))))
                                            Image.width 580.0
                                            Image.height 120.0
                                        ]
                                        TextBlock.create [
                                            TextBlock.text "Multicultivator Analysis"
                                            TextBlock.fontSize 24.0
                                            TextBlock.fontWeight FontWeight.Bold
                                            TextBlock.foreground Brushes.LightSteelBlue
                                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                                            TextBlock.margin (Thickness(0.0, 8.0, 0.0, 8.0))
                                        ]
                                        TextBlock.create [
                                            TextBlock.text "Configure parameters and run the analysis"
                                            TextBlock.fontStyle FontStyle.Italic
                                            TextBlock.foreground Brushes.Gray
                                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        ]
                                    ]
                                ]

                                TextBlock.create [
                                    TextBlock.text "Select input table (.txt):"
                                ]

                                StackPanel.create [
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 8.0
                                    StackPanel.children [
                                        TextBox.create [
                                            TextBox.watermark "/full/path/to/your/file.txt"
                                            TextBox.text state.FilePath
                                            TextBox.onTextChanged (UpdateFilePath >> dispatch)
                                            TextBox.width 460.0
                                        ]

                                        Button.create [
                                            Button.content "Select file..."
                                            Button.onClick (fun _ -> dispatch BrowseFile)
                                        ]
                                    ]
                                ]

                                TextBlock.create [
                                    TextBlock.text "Upper OD threshold:"
                                ]

                                TextBox.create [
                                    TextBox.watermark "e.g. 0.44"
                                    TextBox.text state.UpperThreshold
                                    TextBox.onTextChanged (UpdateUpperThreshold >> dispatch)
                                ]

                                TextBlock.create [
                                    TextBlock.text "Lower OD threshold:"
                                ]

                                TextBox.create [
                                    TextBox.watermark "e.g. 0.36"
                                    TextBox.text state.LowerThreshold
                                    TextBox.onTextChanged (UpdateLowerThreshold >> dispatch)
                                ]

                                TextBlock.create [
                                    TextBlock.text "Cylinder (1-8 or all):"
                                ]

                                TextBox.create [
                                    TextBox.watermark "e.g. 3 or all"
                                    TextBox.text state.Cylinder
                                    TextBox.onTextChanged (UpdateCylinder >> dispatch)
                                ]

                                Button.create [
                                    Button.content (if state.IsRunning then "Running..." else "Run Analysis")
                                    Button.isEnabled (not state.IsRunning)
                                    Button.onClick (fun _ -> dispatch RunAnalysis)
                                ]

                                if state.IsRunning then
                                    ProgressBar.create [
                                        ProgressBar.isIndeterminate true
                                        ProgressBar.height 18.0
                                    ]
                                else
                                    ProgressBar.create [
                                        ProgressBar.isVisible false
                                    ]

                                if not (String.IsNullOrWhiteSpace state.ProgressText) then
                                    TextBlock.create [
                                        TextBlock.text state.ProgressText
                                        TextBlock.fontStyle FontStyle.Italic
                                        TextBlock.foreground Brushes.DodgerBlue
                                    ]
                                else
                                    TextBlock.create []

                                Border.create [
                                    Border.background (SolidColorBrush(Color.Parse("#111827")))
                                    Border.cornerRadius 8.0
                                    Border.padding 12.0
                                    Border.child (
                                        StackPanel.create [
                                            StackPanel.spacing 4.0
                                            StackPanel.children (
                                                if List.isEmpty state.Logs then
                                                    [
                                                        TextBlock.create [
                                                            TextBlock.text "Logs will appear here..."
                                                            TextBlock.foreground Brushes.Gray
                                                        ]
                                                    ]
                                                else
                                                    state.Logs
                                                    |> List.map (fun logMsg ->
                                                        TextBlock.create [
                                                            TextBlock.text logMsg
                                                            TextBlock.foreground Brushes.LightGreen
                                                            TextBlock.fontFamily "Consolas"
                                                            TextBlock.textWrapping TextWrapping.Wrap
                                                        ])
                                            )
                                        ]
                                    )
                                ]

                                match state.Status with
                                | Some msg ->
                                    TextBlock.create [
                                        TextBlock.text msg
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        TextBlock.fontWeight FontWeight.SemiBold
                                        TextBlock.foreground (
                                            if msg.StartsWith("Analysis completed") then
                                                Brushes.ForestGreen
                                            else
                                                Brushes.OrangeRed
                                        )
                                    ]
                                | None ->
                                    TextBlock.create []

                                if state.Status
                                   |> Option.map (fun s -> s.StartsWith("Analysis completed"))
                                   |> Option.defaultValue false then
                                    TextBlock.create [
                                        TextBlock.text "Charts have been saved as HTML files to the Output folder. Open them in a browser to view."
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        TextBlock.fontStyle FontStyle.Italic
                                        TextBlock.foreground Brushes.Gray
                                    ]
                                else
                                    TextBlock.create []

                                if state.IsRunning then
                                    Border.create [
                                        Border.background (SolidColorBrush(Color.Parse("#88000000")))
                                        Border.cornerRadius 8.0
                                        Border.padding 20.0
                                        Border.horizontalAlignment HorizontalAlignment.Center
                                        Border.verticalAlignment VerticalAlignment.Center
                                        Border.child (
                                            StackPanel.create [
                                                StackPanel.spacing 10.0
                                                StackPanel.horizontalAlignment HorizontalAlignment.Center
                                                StackPanel.children [
                                                    ProgressBar.create [
                                                        ProgressBar.isIndeterminate true
                                                        ProgressBar.width 250.0
                                                        ProgressBar.height 14.0
                                                    ]
                                                    TextBlock.create [
                                                        TextBlock.text (
                                                            if String.IsNullOrWhiteSpace state.ProgressText then
                                                                "Processing..."
                                                            else
                                                                state.ProgressText
                                                        )
                                                        TextBlock.foreground Brushes.White
                                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                        TextBlock.fontWeight FontWeight.SemiBold
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                                else
                                    Border.create [
                                        Border.isVisible false
                                    ]
                            ]
                        ]
                    )
                ]
            ]
        ]


    let createApp () =
        Program.mkProgram (fun _ -> init, Cmd.none) update view

module Program =

    open Avalonia
    open Avalonia.Controls
    open Avalonia.Controls.ApplicationLifetimes
    open Avalonia.FuncUI.Hosts
    open Avalonia.FuncUI.Elmish
    open Avalonia.Themes.Fluent

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

    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add(FluentTheme())

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktop ->
                desktop.MainWindow <- MainWindow()
            | _ -> ()

            base.OnFrameworkInitializationCompleted()

    [<EntryPoint>]
    let main argv =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .LogToTrace()
            .StartWithClassicDesktopLifetime(argv)
        |> ignore

        0