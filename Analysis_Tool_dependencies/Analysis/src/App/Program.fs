module Program

open System
open System.Threading
open Spectre.Console
open System.IO

let runAnalysis fileName upperThreshold lowerThreshold cylinders result =
    async {
        try
            Thread.Sleep(200) 
            let analysis = (AnalysisFunction.analysis fileName (float upperThreshold) (float lowerThreshold) cylinders result)
            return Ok analysis
        with ex ->
            return Error ex.Message
    }

let displayProgress () =
    async {
        AnsiConsole.Status()
            .Start("\n\n[green bold][slowblink]Loading[/][/]\n", fun ctx ->
                ctx.Spinner <- Spinner.Known.SimpleDotsScrolling
                ctx.SpinnerStyle <- Style.Parse("green bold")
                Thread.Sleep(1500)
            )

        AnsiConsole.Status()
            .Start("[green bold]Read input[/]", fun ctx ->
                AnsiConsole.MarkupLine("\n\nLOG: Read data ...")
                ctx.Spinner <- Spinner.Known.SimpleDotsScrolling
                ctx.SpinnerStyle <- Style.Parse("green bold")
                Thread.Sleep(700)
            )

        AnsiConsole.Status()
            .Start("[green bold]Read input[/]", fun ctx ->
                ctx.Spinner <- Spinner.Known.SimpleDotsScrolling
                ctx.SpinnerStyle <- Style.Parse("green bold")
                AnsiConsole.MarkupLine("LOG: Setup OD thresholds ...")
                Thread.Sleep(700)

                AnsiConsole.MarkupLine("LOG: Choose cylinder(s) ...")
                Thread.Sleep(700)
            )

        AnsiConsole.Status()
            .Start("[green bold]Read input[/]", fun ctx ->
                ctx.Spinner <- Spinner.Known.SimpleDotsScrolling
                ctx.SpinnerStyle <- Style.Parse("green bold")
                AnsiConsole.MarkupLine("LOG: Loading light treatment data ...")
                Thread.Sleep(700)

                AnsiConsole.MarkupLine("LOG: Loading OD measurement data ...")
                Thread.Sleep(700)

                AnsiConsole.MarkupLine("LOG: Loading medium pump volume data ...")
                Thread.Sleep(700)
            )
        return ()
    }

let showBeforeResultLogs analysisResult fileName upperThreshold lowerThreshold cylinder =
    match analysisResult with
    | Ok _ ->
        AnsiConsole.MarkupLine "\n [bold green]Valid Input![/]\n"
        Thread.Sleep(700)
        AnsiConsole.MarkupLine $"[bold lightsteelblue3]File:[/] {fileName}"
        Thread.Sleep(750)
        AnsiConsole.MarkupLine $"[bold lightsteelblue3]Upper OD Threshold:[/] {upperThreshold}"
        Thread.Sleep(750)
        AnsiConsole.MarkupLine $"[bold lightsteelblue3]Lower OD Threshold:[/] {lowerThreshold}"
        Thread.Sleep(150)
        AnsiConsole.MarkupLine $"[bold lightsteelblue3]Cylinder:[/] {cylinder}"
        Thread.Sleep(150)
        AnsiConsole.Progress()
                .Start(fun ctx ->
                    let task1 = ctx.AddTask("[green3]Calculating Log Data[/]")
                    let task2 = ctx.AddTask("[green3]Analyzing Linear Regression[/]")
                    let task3 = ctx.AddTask("[green3]Plotting Graphs[/]")
                    let task4 = ctx.AddTask("[green3]Saving Analysis Data[/]")

                    while not ctx.IsFinished do
                        task1.Increment(2.0)
                        Thread.Sleep(20)
                        task2.Increment(1.75)
                        Thread.Sleep(20)
                        task3.Increment(1.5)
                        Thread.Sleep(20)
                        task4.Increment(1.25)
                )
    | Error errorMsg ->
        AnsiConsole.MarkupLine "\n\n [red bold]An error occurred:[/] \n  [red]Oh something went wrong, maybe [underline]you mistyped[/] or the data [underline]table layout is not correct[/].[/]\n  [red]Analysis failed.[/]\n\n"
        AnsiConsole.MarkupLine $"[red bold]Issue: [/][gray]{errorMsg}[/] \n\n"

let showAnalysisResult analysisResult =
    match analysisResult with
    | Ok _ ->
        AnsiConsole.MarkupLine "\n[green bold]Analysis successful![/]\n"
    | Error errorMsg ->
        AnsiConsole.MarkupLine "\n\n [red bold]An error occurred:[/] \n  [red]Oh something went wrong, maybe [underline]you mistyped[/] or the data [underline]table layout is not correct[/].[/]\n  [red]Analysis failed.[/]\n\n"
        AnsiConsole.MarkupLine $"[red bold]Issue: [/][gray]{errorMsg}[/] \n\n"

[<EntryPoint>]
let rec main argv =
    Console.ForegroundColor <- ConsoleColor.DarkGray
    AnsiConsole.Clear()
    Console.SetWindowSize(90, 50)
    AnsiConsole.Write(new FigletText(Color = Color.LightSteelBlue1, text = "AnalysisTool"))
    AnsiConsole.Markup 
        "\n Please [underline]enter the filename[/] of the data table\n [gray]('TAB' separated textfile, filename without '.txt')[/]\n [gray](column names have to contain '.pump', '.light', and '.od',[/]\n [gray]-> for cylinder indetification have to contain also '-NUMBER-' e.g. '-1-' .. '-8-')[/]\n also give an [underline]upper and lower OD threshold[/] for the calculation of the growphase,\n additionaly [underline]add the cylinder you want to analyse[/] [gray](choose 1-8 or all)\n (press SPACE between filename and the values)[/]\n\n [lightsteelblue3][bold]Example:[/] 202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 3[/]\n\n [gray]Write [underline]exit[/] as input to quit the application [/]\n\n"
    AnsiConsole.Write(new Rule("[blue]Analysis[/]"))
    AnsiConsole.MarkupLine$"\n[bold]Input: [/]"

    let input = Console.ReadLine().Split([|' '|])

    match input with
    | [| "exit" |] -> 
        AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        0
    | [| fileName; upperThreshold; lowerThreshold; cylinder |] ->
        let cylinders = if cylinder = "all" then [0..7] else [int cylinder - 1]

        Async.RunSynchronously(displayProgress ())
        let analysisResult = Async.RunSynchronously(runAnalysis fileName (float upperThreshold) (float lowerThreshold) cylinders true)
        showBeforeResultLogs analysisResult fileName (float upperThreshold) (float lowerThreshold) cylinder
        Thread.Sleep(500)
        match analysisResult with
        | Ok _ ->
            let analysisResult = Async.RunSynchronously(runAnalysis fileName (float upperThreshold) (float lowerThreshold) cylinders false)
            showAnalysisResult analysisResult
            let restart = AnsiConsole.Confirm(prompt = "\n[yellow]Do you want to restart the application?[/]\n")
            if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        | Error errorMsg ->
            let restart = AnsiConsole.Confirm(prompt = "\n[yellow]Do you want to restart the application?[/]\n")
            if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        0
    | _ -> 
        AnsiConsole.MarkupLine "\n\n [red bold]Invalid input.[/] Please check the format and try again. Look into the introduction or see Example above.\n\n"
        let restart = AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to try again?[/]\n\n")
        if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        0