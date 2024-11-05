// For more information see https://aka.ms/fsharp-console-apps
module Program
open System
open System.Threading
open Spectre.Console

[<EntryPoint>]
let rec main argv =
    let systemPathSeparator = System.IO.Path.DirectorySeparatorChar
    let seperatorAsString = systemPathSeparator.ToString()
    let stringToReplace = $"{seperatorAsString}Analysis{seperatorAsString}src{seperatorAsString}App"
    let currentProjectPathByApp = 
        (__SOURCE_DIRECTORY__)
            .Replace(stringToReplace, "")
    Console.ForegroundColor <- ConsoleColor.DarkGray
    AnsiConsole.Clear()
    Console.SetWindowSize(90 ,50)
    AnsiConsole.Write(new FigletText(Color = Color.LightSteelBlue1,text = "AnalysisTool"))
    AnsiConsole.Markup 
        "\n Please [underline]enter the filename[/] of the data table\n [gray]('TAB' separated textfile, filename without '.txt')[/]\n [gray](column names have to contain '.pump', '.light', and '.od',[/]\n [gray]-> for cylinder indetification have to contain also '-NUMBER-' e.g. '-1-' .. '-8-')[/]\n also give an [underline]upper and lower OD threshold[/] for the calculation of the growphase,\n additionaly [underline]add the cylinder you want to analyse[/] [gray](choose 1-8 or all)\n (press SPACE between filename and the values)[/]\n\n [lightsteelblue3][bold]Example:[/] 202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 3[/]\n\n [gray]Write [underline]exit[/] as input to quit the application [/]\n\n"
    AnsiConsole.Write(new Rule("[blue]Analysis[/]"))
    AnsiConsole.MarkupLine$"\n[bold]Input: [/]"

    let input = 
        let lineInput = Console.ReadLine()
        lineInput.Split([|' '|])

    try
        match input with
        | [|"exit"|] when input.Length = 1 ->
            AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        | _ when input.Length <> 4 -> 
            AnsiConsole.MarkupLine "\n\n [red bold]Invalid input.[/] Look into the introduction or see Example above.\n\n"

            let restart = 
                AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to try again?[/]\n\n")

            if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"

        | [| fileName; upperThreshold; lowerThreshold; cylinder |] when input.Length = 4 ->

            AnsiConsole.Status()
                .Start("\n\n[green bold][slowblink]Loading[/][/]\n", fun ctx ->
                    ctx.Spinner <- Spinner.Known.SimpleDotsScrolling;
                    ctx.SpinnerStyle <- Style.Parse("green bold");
                    Thread.Sleep(1600);
                )

            AnsiConsole.Status()
                .Start("[green bold]read input[/]", fun ctx ->
                    AnsiConsole.MarkupLine("\n\nLOG: Read data ...");
                    ctx.Spinner <- Spinner.Known.SimpleDotsScrolling;
                    ctx.SpinnerStyle <- Style.Parse("green bold");
                    Thread.Sleep(800);
                )

            let isFailed result =
                match result with
                | Ok _ -> false
                | Error _ -> true
            
            let resultForFailTest = snd (AnalysisFunction.analysis fileName (float upperThreshold) (float lowerThreshold) (if cylinder = "all" then [0..7] else [int cylinder - 1 .. int cylinder - 1]) true)

            AnsiConsole.Status()
                .Start("[green bold]read input[/]", fun ctx ->
                    ctx.Spinner <- Spinner.Known.SimpleDotsScrolling;
                    ctx.SpinnerStyle <- Style.Parse("green bold");
                    AnsiConsole.MarkupLine("LOG: Setup OD thresholds ...");
                    Thread.Sleep(800);
                    //Update the status and spinner

                    AnsiConsole.MarkupLine("LOG: Choose cylinder(s) ...");
                    Thread.Sleep(800);
                )

            if isFailed resultForFailTest then
                Console.WriteLine(Failure "")
            else
                AnsiConsole.Status()
                    .Start("[green bold]read input[/]", fun ctx ->
                        ctx.Spinner <- Spinner.Known.SimpleDotsScrolling;
                        ctx.SpinnerStyle <- Style.Parse("green bold");
                        AnsiConsole.MarkupLine("LOG: Loading light treatment data ...");
                        Thread.Sleep(800);

                        AnsiConsole.MarkupLine("LOG: Loading OD messurement data ...");
                        Thread.Sleep(800);

                        AnsiConsole.MarkupLine("LOG: Loading medium pump volume data ...");
                        Thread.Sleep(800);
                    );
                AnsiConsole.MarkupLine "\n [bold green]Valid Input![/]\n"     
                Thread.Sleep(800)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]File:[/] {fileName}"
                Thread.Sleep(250)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]Upper OD Threshold:[/] {upperThreshold}"
                Thread.Sleep(250)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]Lower OD Threshold:[/] {lowerThreshold}"        
                Thread.Sleep(250)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]Cylinder:[/] {cylinder}"

                let progress =
                    AnsiConsole.Progress()
                        .Start(fun ctx ->
                            // Define tasks
                            let task1 = ctx.AddTask("[green3]Calculating Log Data[/]")
                            let task2 = ctx.AddTask("[green3]Analyse linear Regression[/]")                                    
                            let task3 = ctx.AddTask("[green3]Plotting graphes[/]")
                            let task4 = ctx.AddTask("[green3]Saving analysis data table[/]")

                            while not ctx.IsFinished do 
                                task1.Increment(2.0)
                                Thread.Sleep(25)
                                task2.Increment(1.75)
                                Thread.Sleep(25)
                                task3.Increment(1.5)
                                Thread.Sleep(25)
                                task4.Increment(1.25)
                            )
                progress

                let analysis = fst (AnalysisFunction.analysis fileName (float upperThreshold) (float lowerThreshold) (if cylinder = "all" then [0..7] else [int cylinder - 1 .. int cylinder - 1]) (false))

                AnsiConsole.MarkupLine "\n [bold green]Analysis successful![/]\n\n"

                AnsiConsole.Status()
                    .Start("\n\n[bold][slowblink]Loading...[/][/]\n", fun ctx ->
                        Thread.Sleep(200);
                        analysis |> ignore
                    )
                
                let restart = 
                    AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to restart the application?[/]\n\n")
                if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        | _ -> 
            AnsiConsole.MarkupLine "\n\n [red bold]Invalid input.[/] Look into the introduction or see Example above.\n\n"

            let restart = 
                AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to try again?[/]\n\n")

            if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
    with
    | ex -> 
        Thread.Sleep(2500)
        AnsiConsole.MarkupLine "\n\n [red bold]An error occurred:[/] \n  [red]Oh something went wrong, maybe [underline]you mistyped[/] or the data [underline]table layout is not correct[/].[/]\n  [red]Analysis failed.[/]\n\n"
        AnsiConsole.MarkupLine $"[red bold]Issue: [/][gray]{ex.Message}[/] \n\n"

        let restart = 
            AnsiConsole.Confirm(prompt = "\n\n \t[yellow]Do you want to try again?[/]\n\n")

        if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
    0