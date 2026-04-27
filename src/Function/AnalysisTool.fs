namespace AnalysisTool

open System
open System.IO
open FSharp.Stats
open FSharp.Stats.Fitting
open Deedle
open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.TraceObjects
open Plotly.NET.StyleParam
open FSharpAux.IO
open Library

/// <summary>
/// This module contains the full analysis pipeline for the bioreactor tool.
/// It supports live progress logging via a callback and generates charts.
/// The public entry point is the <c>analysis</c> function.
/// </summary>
module AnalysisFunction =

    /// Helper to safely invoke a logger callback.  If the callback throws it will be ignored.
    let private logging (log : string -> unit) (message : string) (time : bool) =
        try
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
            if time then
                log (timestamp + "  " + message)
            else
                log message
        with _ -> ()

    let private safeLog (log : string -> unit) (message : string) =
        logging log message false

    let private safeLogTime (log : string -> unit) (message : string) =
        logging log message true

    /// Safe natural logarithm for OD values. F# `log` is the natural logarithm, i.e. ln(x).
    /// Non-positive OD values cannot be log-transformed and are converted to NaN,
    /// then removed later during per-cylinder extraction.
    let private safeLn (x : float) =
        if x > 0.0 then log x else Double.NaN

    /// Read the raw data from a tab‑separated file.
    let private readRawData (path : string) : Frame<float,string> =
        Frame.ReadCsv(
            location   = path,
            separators = "\t",
            hasHeaders = true,
            inferRows  = 10000
        )
        |> Frame.indexRows "time"

    /// Extract a column subset from a Deedle frame based on a substring match.
    let private getColumnsWithSubstring (frame : Frame<float,string>) (substring : string) =
        frame.ColumnKeys |> Seq.filter (fun x -> x.Contains(substring))

    /// Allocate a zero array for empty cylinders.  Many functions in the
    /// analysis pipeline require arrays of time/value tuples; this provides a
    /// sensible default when data is missing.
    let private zeroArray : (float * float) array = Array.init 250 (fun idx -> (float idx, 0.0))

    /// Normalize a pump data array by subtracting its initial value.  Pump
    /// measurements are recorded as an absolute volume; subtracting the first
    /// measurement produces a more interpretable displacement series.
    let private normalizePump (arr : (float * float) array) =
        if Array.isEmpty arr then
            arr
        else
            arr |> Array.map (fun (t, v) -> (t, v - snd arr.[0]))

    let private safeArrayMinBySndOr (fallback: float) (arr: (float * float) array) =
        if Array.isEmpty arr then fallback else snd (arr |> Array.minBy snd)

    let private safeArrayMaxBySndOr (fallback: float) (arr: (float * float) array) =
        if Array.isEmpty arr then fallback else snd (arr |> Array.maxBy snd)


    let private safeMinMaxForOdAxis (odData: (float * float) array) =
        let yAxisMinODData = safeArrayMinBySndOr 0.0 odData
        let yAxisMaxODData = safeArrayMaxBySndOr 0.0 odData
        let yAxisMinODDataAdded =
            if yAxisMinODData > 0.0 then yAxisMinODData * 0.95
            elif yAxisMinODData < 0.0 then yAxisMinODData * 1.05
            else yAxisMinODData - 0.1
        let yAxisMaxODDataAdded =
            if yAxisMaxODData > 0.0 then yAxisMaxODData * 1.05
            elif yAxisMaxODData < 0.0 then yAxisMaxODData * 0.95
            else yAxisMaxODData + 0.1
        yAxisMinODDataAdded, yAxisMaxODDataAdded

    let private safeMinMaxForPumpLightAxis (pumpData: (float * float) array) (lightData: (float * float) array) =
        let pumpMin = safeArrayMinBySndOr 0.0 pumpData
        let pumpMax = safeArrayMaxBySndOr 0.0 pumpData
        let lightMax = safeArrayMaxBySndOr 0.0 lightData
        let maxCombined = max pumpMax lightMax
        pumpMin, (if maxCombined = 0.0 then 1.0 else maxCombined * 1.05)
    
    let private safeMinMaxForPumpAxis (pumpData: (float * float) array) =
        let pumpMin = safeArrayMinBySndOr 0.0 pumpData
        let pumpMax = safeArrayMaxBySndOr 0.0 pumpData
        pumpMin, (if pumpMax = 0.0 then 1.0 else pumpMax * 1.05)

    let private safeArrayLastFstOr (fallback: float) (arr: (float * float) array) =
        if Array.isEmpty arr then fallback else fst arr.[arr.Length - 1]

    /// Compute contiguous regions of constant values within a series of
    /// measurements.  Returns an array of arrays, each representing a contiguous
    /// segment.  Segments are only kept if they either have a non‑zero value
    /// or exceed two measurements; this prevents tiny background noise segments
    /// from being interpreted as a light phase.
    let private groupByConsecutive (data : (float * float) array) : (float * float) array array =
        let folded =
            data
            |> Array.fold (
                fun (acc, current) (time, intensity) ->
                    match current with
                    | [] -> acc, [ (time, intensity) ]
                    | (_, prev) :: _ when prev = intensity -> acc, (time, intensity) :: current
                    | _ -> (current |> List.rev |> Array.ofList) :: acc, [ (time, intensity) ]
            ) ([], [])
        let acc, lastGroup = folded
        (lastGroup |> List.rev |> Array.ofList) :: acc |> List.rev |> Array.ofList
        |> Array.filter (fun segment -> (snd segment.[0]) <> 0.0 || segment.Length > 2)

    /// Compute light phases for a given light intensity series.  Light phases
    /// correspond to contiguous segments of non‑zero intensity (or prolonged
    /// constant readings).  Each returned tuple is (startTime, endTime).
    let private computeLightPhases (lightData : (float * float) array) : (float * float) array =
        groupByConsecutive lightData
        |> Array.map (fun segment -> (fst segment.[0], fst segment.[segment.Length - 1]))

    let private computeGrowPhases (pumpData : (float * float) array) (odData : (float * float) array) (upperODCut : float) (lowerODCut : float) : (float * float) array =
        let arrayOfConstantPumpValue =
            pumpData
            |> Array.groupBy (fun (_timePump, pumpVolume) -> pumpVolume)
            |> Array.filter (fun (_constantVolume, arrayTimePumpVolume) -> arrayTimePumpVolume.Length > 250)

        let startEndOfConstantPumpValue : (float * float) array =
            arrayOfConstantPumpValue
            |> Array.map (fun ((_constantVolume : float), (arrayTimePumpVolume : (float * float) array)) ->
                if Array.isEmpty arrayTimePumpVolume then
                    (0.0, 0.0)
                else
                    (float (fst arrayTimePumpVolume.[0]), float (fst (arrayTimePumpVolume |> Array.last)))
            )

        startEndOfConstantPumpValue
        |> Array.map (fun (startOfGrowphase, endOfGrowphase) ->
            let filteredOd =
                odData
                |> Array.filter (fun (time, od680) ->
                    time >= startOfGrowphase
                    && time <= endOfGrowphase
                    && od680 >= (log lowerODCut)
                    && od680 <= (log upperODCut))

            if Array.isEmpty filteredOd then
                (0.0, 0.0)
            else
                (fst filteredOd.[0], fst (filteredOd |> Array.last))
        )

    /// Compute the slope of a growth phase using Theil–Sen robust regression.
    /// Returns 0.0 if fewer than six observations are available.
    let private computeSlope (growthphase : (float * float) array) (odData : (float * float) array) (gpIndex : int) (upperODCut : float) (lowerODCut : float) : float =
        let (start, finish) = growthphase.[gpIndex]
        let filtered =
            odData
            |> Array.filter (fun (t, od) -> t >= start && t <= finish && od >= log lowerODCut && od <= log upperODCut)
        let xs = filtered |> Array.map fst |> vector
        let ys = filtered |> Array.map snd |> vector
        if Vector.length xs > 5 && Vector.length ys > 5 then
            let fit = Fitting.LinearRegression.fit(xs, ys, FittingMethod = Fitting.Method.Robust RobustEstimator.TheilSen)
            fit.Coefficients.[1]
        else
            0.0

    /// Convert a growth phase into a list of table rows capturing phase
    /// characteristics (ID, start/end times, slope and duplication time).
    let private tableRows (growthphase : (float * float) array) (odData : (float * float) array) (upperODCut : float) (lowerODCut : float) : Library.tableRow list =
        [ for i in 0 .. growthphase.Length - 1 do
            let (start, finish) = growthphase.[i]
            let slope = computeSlope growthphase odData i upperODCut lowerODCut
            let duplicationTime = if slope = 0.0 then Double.PositiveInfinity else log 2.0 / slope
            yield { PhaseID = i + 1
                    startTimeGrowthphase = start
                    endTimeGrowthphase = finish
                    slopeOrGrowthrateOfLinearRegressionOrGrowthphase = slope
                    duplicationTimeOfGrowthphase = duplicationTime } ]

    // Save a table of growth phase characteristics to a CSV file.
    // Rows are semicolon-separated with columns for phase ID, start time, end time, growth rate, and duplication time.
    let private saveTable (rows : Library.tableRow list) (outputDir : string) (cylinderId : string) =
        let header =
            seq [ "Growth phase ID;Start time of growth phase (h);End time of growth phase (h);Growth rate (h^-1);Duplication time, Td (h)" ]

        let dataLines =
            rows
            |> Seq.map (fun row ->
                sprintf "%d;%.2f;%.2f;%.4f;%.4f"
                    row.PhaseID
                    row.startTimeGrowthphase
                    row.endTimeGrowthphase
                    row.slopeOrGrowthrateOfLinearRegressionOrGrowthphase
                    row.duplicationTimeOfGrowthphase
            )

        let csvLines =
            Seq.append header dataLines
        let path = Path.Combine(outputDir, sprintf "tableOfCylinder_%s.csv" cylinderId)
        FileIO.writeToFile true path csvLines

    let private chartGrowphaseLinearFitOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (growphaseIndex : int)
        (upperODCut : float)
        (lowerODCut : float) =

        let arrayGrowphaseTimeOD =
            growthphase
            |> Array.map (fun (minT, maxT) ->
                odData
                |> Array.filter (fun (time, od680) ->
                    time >= minT && time <= maxT && od680 >= log lowerODCut && od680 <= log upperODCut))

        let growphaseTimeOD = arrayGrowphaseTimeOD.[growphaseIndex]

        let xs =
            growphaseTimeOD
            |> Array.map fst
            |> vector

        let ys =
            growphaseTimeOD
            |> Array.map snd
            |> vector

        let fit =
            if Vector.length xs > 5 && Vector.length ys > 5 then
                Fitting.LinearRegression.fit(xs, ys, FittingMethod = Fitting.Method.Robust RobustEstimator.TheilSen)
            else
                Fitting.LinearRegression.Coefficients(vector [0.0; 0.0])

        let e =
            if Array.isEmpty growphaseTimeOD then 0.0
            else growphaseTimeOD |> Array.map fst |> Array.last

        let s =
            if Array.isEmpty growphaseTimeOD then 0.0
            else growphaseTimeOD |> Array.map fst |> Array.sortDescending |> Array.last

        [ s .. 0.1 .. e ]
        |> List.map (fun x -> x, LinearRegression.predict fit x)
        |> Chart.Line

    let private shapeGrowOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float) =

        growthphase
        |> Array.indexed
        |> Array.choose (fun (i, (start, finish)) ->
            let filteredODData =
                odData
                |> Array.filter (fun (time, od680) ->
                    time >= start && time <= finish && od680 >= log lowerODCut && od680 <= log upperODCut)

            let range = [ start .. 0.1 .. finish ]

            if Array.isEmpty filteredODData || List.isEmpty range then
                None
            else
                let start' = range.[0]
                let finish' = range |> List.last
                let maxODValue = snd (filteredODData |> Array.sortBy snd |> Array.last)
                let minODValue = snd (filteredODData |> Array.sortByDescending snd |> Array.last)

                Some (
                    Shape.init(
                        ShapeType = StyleParam.ShapeType.Rectangle,
                        X0 = start',
                        X1 = finish',
                        Y0 = maxODValue,
                        Y1 = minODValue,
                        Opacity = 0.2,
                        FillColor = Color.fromHex "#a3e77f",
                        Label = ShapeLabel.init(TextTemplate = $"{i + 1}", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.BottomCenter)
                    )
                )
        )
        |> Array.toList

    let private shapeLightOriginal
        (odData : (float * float) array)
        (lightData : (float * float) array) =

        let treatment1and2Tuple =
            let groupedByTreatment =
                (groupByConsecutive lightData)
                |> Array.map (fun timeLightDataArray ->
                    snd timeLightDataArray.[0], (fst timeLightDataArray.[0], fst (timeLightDataArray |> Array.last)))
            groupedByTreatment

        let yAxisMinODDataAdded, yAxisMaxODDataAdded = safeMinMaxForOdAxis odData

        let maxYValue =
            match Array.isEmpty lightData with
            | true -> 0.0
            | false -> yAxisMaxODDataAdded

        let minYValue =
            match Array.isEmpty lightData with
            | true -> 0.0
            | false -> yAxisMinODDataAdded

        treatment1and2Tuple
        |> Array.mapi (fun i (_light, (start, finish)) ->
            Shape.init(
                ShapeType = StyleParam.ShapeType.Rectangle,
                X0 = start,
                X1 = finish,
                Y0 = minYValue,
                Y1 = maxYValue,
                Opacity = 0.1,
                FillColor = Color.fromHex "#ffffba",
                Label = ShapeLabel.init(TextTemplate = $"Highlight {fst treatment1and2Tuple.[i]}", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.BottomCenter)
            )
        )
        |> Array.toList

    let private endGraphOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (pumpData : (float * float) array)
        (lightData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float)
        (sampleId : string)
        : GenericChart.GenericChart =

        let yAxisMinPumpLightData, yAxisMaxPumpLightData = safeMinMaxForPumpLightAxis pumpData lightData
        let yAxisMinODDataAdded, yAxisMaxODDataAdded = safeMinMaxForOdAxis odData

        let linearRegressionAllChartLine =
            [
                for i in 0 .. growthphase.Length - 1 do
                    chartGrowphaseLinearFitOriginal growthphase odData i upperODCut lowerODCut
                    |> Chart.withTraceInfo $"Robust Theil-Sen fit {i + 1}"
                    |> Chart.withLineStyle(Width = 2.25, Color = Color.fromHex "#fb2e01", Dash = StyleParam.DrawingStyle.Dot)
                    |> Chart.withAxisAnchor(X = 1, Y = 1)
                    |> Chart.withTraceInfo(Name = $"Growth phase ID: {i + 1}")
            ]
            |> Chart.combine

        let odDataAllChartPoint =
            Chart.Point(xy = odData)
            |> Chart.withLineStyle(Color = Color.fromHex "#7bc043")
            |> Chart.withTraceInfo(Name = "ln(OD680)")
            |> Chart.withAxisAnchor(X = 1, Y = 1)

        let pumpDataAllChartPoint =
            Chart.Point(xy = pumpData)
            |> Chart.withLineStyle(Color = Color.fromHex "#005b96")
            |> Chart.withTraceInfo(Name = "Pump volume (mL)")
            |> Chart.withAxisAnchor(X = 1, Y = 2)

        let lightDataAllChartPoint =
            Chart.Point(xy = lightData)
            |> Chart.withLineStyle(Color = Color.fromHex "#ffffba")
            |> Chart.withTraceInfo(Name = "Light treatment (µE)")
            |> Chart.withAxisAnchor(X = 1, Y = 2)

        [
            odDataAllChartPoint
            linearRegressionAllChartLine
            lightDataAllChartPoint
            pumpDataAllChartPoint
        ]
        |> Chart.combine
        |> Chart.withXAxisStyle(
            "Time (h)",
            Id = StyleParam.SubPlotId.XAxis 1,
            MinMax = (0.0, safeArrayLastFstOr 0.0 odData),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            $"ln(OD680), cylinder {sampleId}",
            Side = StyleParam.Side.Left,
            MinMax = (yAxisMinODDataAdded, yAxisMaxODDataAdded),
            Id = StyleParam.SubPlotId.YAxis 1,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            $"Pump volume (mL) and light treatment (µE), cylinder {sampleId}",
            Side = StyleParam.Side.Right,
            MinMax = (yAxisMinPumpLightData, yAxisMaxPumpLightData),
            Id = StyleParam.SubPlotId.YAxis 2,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )

    let private endGraphSingleOriginal
        (growthphase : (float * float) array)
        (lightphase : (float * float) array)
        (odData : (float * float) array)
        (pumpData : (float * float) array)
        (lightData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float)
        (sampleId : string) =

        let yAxisMinPumpLightData, yAxisMaxPumpLightData = safeMinMaxForPumpLightAxis pumpData lightData
        let yAxisMinODDataAdded, yAxisMaxODDataAdded = safeMinMaxForOdAxis odData

        let linearRegressionAllChartLine =
            [
                for i in 0 .. growthphase.Length - 1 do
                    chartGrowphaseLinearFitOriginal growthphase odData i upperODCut lowerODCut
                    |> Chart.withTraceInfo $"Robust Theil-Sen fit {i + 1}"
                    |> Chart.withLineStyle(Width = 2.25, Color = Color.fromHex "#fb2e01", Dash = StyleParam.DrawingStyle.Dot)
                    |> Chart.withAxisAnchor(Y = 1)
                    |> Chart.withTraceInfo(Name = $"Growth phase ID: {i + 1}")
            ]
            |> Chart.combine

        let odDataAllChartPoint =
            Chart.Point(xy = odData)
            |> Chart.withLineStyle(Color = Color.fromHex "#7bc043")
            |> Chart.withTraceInfo(Name = "ln(OD680)")
            |> Chart.withAxisAnchor(Y = 1)

        let pumpDataAllChartPoint =
            Chart.Point(xy = pumpData)
            |> Chart.withLineStyle(Color = Color.fromHex "#005b96")
            |> Chart.withTraceInfo(Name = "Pump volume (mL)")
            |> Chart.withAxisAnchor(Y = 2)

        let lightDataAllChartPoint =
            Chart.Point(xy = lightData)
            |> Chart.withLineStyle(Color = Color.fromHex "#ffffba")
            |> Chart.withTraceInfo(Name = "Light treatment (µE)")
            |> Chart.withAxisAnchor(Y = 2)

        [
            odDataAllChartPoint
            linearRegressionAllChartLine
            lightDataAllChartPoint
            pumpDataAllChartPoint
        ]
        |> Chart.combine
        |> Chart.withShapes(shapes = shapeGrowOriginal growthphase odData upperODCut lowerODCut, Append = true)
        |> Chart.withShapes(shapes = shapeLightOriginal odData lightData, Append = true)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withLegendStyle(Orientation = StyleParam.Orientation.Horizontal)
        |> Chart.withSize(Width = 1600, Height = 800)
        |> Chart.withTitle($"Robust linear regression of the growth phases for cylinder {sampleId}")
        |> Chart.withXAxisStyle(
            "Time (h)",
            Id = StyleParam.SubPlotId.XAxis 1,
            MinMax = (0.0, safeArrayLastFstOr 0.0 odData),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            $"ln(OD680), cylinder {sampleId}",
            Side = StyleParam.Side.Left,
            MinMax = (yAxisMinODDataAdded, yAxisMaxODDataAdded),
            Id = StyleParam.SubPlotId.YAxis 1,
            Overlaying = StyleParam.LinearAxisId.Y 2,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            $"Pump volume (mL) and light treatment (µE), cylinder {sampleId}",
            Side = StyleParam.Side.Right,
            MinMax = (yAxisMinPumpLightData, yAxisMaxPumpLightData),
            Id = StyleParam.SubPlotId.YAxis 2,
            ShowLine = false,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )

    let private pointchartSlopeOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float) =

        let xy =
            tableRows growthphase odData upperODCut lowerODCut
            |> Seq.map (fun x -> float x.PhaseID, float x.slopeOrGrowthrateOfLinearRegressionOrGrowthphase)

        Chart.Point(xy = xy, Name = "Growth rate (h<sup>-1</sup>)")
        |> Chart.withLineStyle(Color = Color.fromHex "#005b96")

    let private boxplotSlopeOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float) =

        let y =
            tableRows growthphase odData upperODCut lowerODCut
            |> List.map (fun x -> float x.slopeOrGrowthrateOfLinearRegressionOrGrowthphase)

        Chart.BoxPlot(
            Y = y,
            Name = "",
            Jitter = 0.1,
            BoxPoints = StyleParam.BoxPoints.SuspectedOutliers,
            BoxMean = StyleParam.BoxMean.True,
            OutlineWidth = 1.5
        )
        |> Chart.withLineStyle(Color = Color.fromHex "#005b96")

    let private sortSlopeListOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float) =

        tableRows growthphase odData upperODCut lowerODCut
        |> List.map (fun rowItem -> rowItem.slopeOrGrowthrateOfLinearRegressionOrGrowthphase)
        |> List.sort

    let private safeSlopeMinMax (growthphase: (float * float) array) (odData: (float * float) array) (upperODCut: float) (lowerODCut: float) =
        let sorted = sortSlopeListOriginal growthphase odData upperODCut lowerODCut
        match sorted with
        | [] -> (-0.005, 0.005)
        | xs -> (xs.Head - 0.005, xs |> List.last |> fun v -> v + 0.005)

    let private tableChartOriginal
        (growthphase : (float * float) array)
        (odData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float) =

        let header : seq<string> =
            seq [ "<b>Growth phase ID</b>"; "Start time of growth phase (h)"; "End time of growth phase (h)"; "Growth rate (h<sup>-1</sup>)"; "Duplication time, Td (h)" ]

        let rows : seq<seq<float>> =
            tableRows growthphase odData upperODCut lowerODCut
            |> Seq.map (fun rowItem ->
                seq [
                    float rowItem.PhaseID
                    round 2 rowItem.startTimeGrowthphase
                    round 2 rowItem.endTimeGrowthphase
                    round 3 rowItem.slopeOrGrowthrateOfLinearRegressionOrGrowthphase
                    round 3 rowItem.duplicationTimeOfGrowthphase
                ])

        Chart.Table(
            header,
            rows,
            HeaderAlign = StyleParam.HorizontalAlign.Left,
            CellsAlign = StyleParam.HorizontalAlign.Left,
            MultiColumnWidth = [ 1.5; 2.5; 2.5; 4.0 ],
            HeaderFillColor = Color.fromString "DarkGray",
            HeaderHeight = 2,
            HeaderOutlineColor = Color.fromString "Grey",
            HeaderOutlineWidth = 2.0,
            CellsFillColor = Color.fromString "LightGray",
            CellsOutlineColor = Color.fromString "Grey",
            CellsOutlineWidth = 1.5,
            CellsHeight = 25
        )
        |> Chart.withSize(Width = 1600, Height = 800)

    let private buildAdvancedChart
        (growthphase : (float * float) array)
        (lightphase : (float * float) array)
        (odData : (float * float) array)
        (pumpData : (float * float) array)
        (lightData : (float * float) array)
        (upperODCut : float)
        (lowerODCut : float)
        (sampleId : string)
        =

        let yAxisMinPumpData, yAxisMaxPumpData = safeMinMaxForPumpAxis pumpData
        let yAxisMinODDataAdded, yAxisMaxODDataAdded = safeMinMaxForOdAxis odData
        let slopeMin, slopeMax = safeSlopeMinMax growthphase odData upperODCut lowerODCut

        let subplotGrid =
            [|
                [| StyleParam.LinearAxisId.X 1, StyleParam.LinearAxisId.Y 1; StyleParam.LinearAxisId.X 2, StyleParam.LinearAxisId.Y 2 |]
                [| StyleParam.LinearAxisId.X 3, StyleParam.LinearAxisId.Y 3; StyleParam.LinearAxisId.X 4, StyleParam.LinearAxisId.Y 4 |]
            |]

        let pumpDataAllChartPoint =
            Chart.Point(xy = pumpData)
            |> Chart.withLineStyle(Color = Color.fromHex "#005b96")
            |> Chart.withTraceInfo(Name = "Pump volume (mL)")

        [
            (endGraphOriginal growthphase odData pumpData lightData upperODCut lowerODCut sampleId)
            |> Chart.withShapes(shapes = shapeGrowOriginal growthphase odData upperODCut lowerODCut, Append = true)
            |> Chart.withShapes(shapes = shapeLightOriginal odData lightData, Append = true)
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X = 1, Y = 1)

            pumpDataAllChartPoint
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X = 1, Y = 2)

            (pointchartSlopeOriginal growthphase odData upperODCut lowerODCut)
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X = 3, Y = 3)

            (boxplotSlopeOriginal growthphase odData upperODCut lowerODCut)
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X = 4, Y = 4)

            tableChartOriginal growthphase odData upperODCut lowerODCut
            |> Chart.withTemplate ChartTemplates.lightMirrored
        ]
        |> Chart.Grid(nRows = 3, nCols = 2, Pattern = StyleParam.LayoutGridPattern.Independent)
        |> Chart.withShapes(shapes = shapeGrowOriginal growthphase odData upperODCut lowerODCut, Append = true)
        |> Chart.withShapes(shapes = shapeLightOriginal odData lightData, Append = true)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withLegendStyle(Orientation = StyleParam.Orientation.Horizontal)
        |> Chart.withXAxisStyle(
            "",
            Id = StyleParam.SubPlotId.XAxis 1,
            MinMax = (0.0, safeArrayLastFstOr 0.0 odData),
            Domain = (0.00, 1.00),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            $"ln(OD680), cylinder {sampleId}",
            Side = StyleParam.Side.TopLeft,
            MinMax = (yAxisMinODDataAdded, yAxisMaxODDataAdded),
            Id = StyleParam.SubPlotId.YAxis 1,
            Domain = (0.725, 1.00),
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            $"Pump volume (mL), cylinder {sampleId}",
            Side = StyleParam.Side.Right,
            MinMax = (yAxisMinPumpData, yAxisMaxPumpData),
            Id = StyleParam.SubPlotId.YAxis 2,
            Domain = (0.625, 0.725),
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withXAxisStyle(
            "Time (h)",
            Id = StyleParam.SubPlotId.XAxis 2,
            MinMax = (0.0, safeArrayLastFstOr 0.0 odData),
            Domain = (0.00, 1.00),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withXAxisStyle(
            $"Growth phases in cylinder {sampleId}",
            Id = StyleParam.SubPlotId.XAxis 3,
            MinMax = (0.0, float growthphase.Length + 1.0),
            Domain = (0.00, 0.49),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            "Growth rate (h<sup>-1</sup>)",
            Id = StyleParam.SubPlotId.YAxis 3,
            MinMax = (slopeMin, slopeMax),
            Domain = (0.325, 0.55),
            Side = StyleParam.Side.Left,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withXAxisStyle(
            $"Cylinder {sampleId}",
            Id = StyleParam.SubPlotId.XAxis 4,
            Domain = (0.51, 1.00),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            "Growth rate (h<sup>-1</sup>)",
            Id = StyleParam.SubPlotId.YAxis 4,
            MinMax = (slopeMin, slopeMax),
            Domain = (0.325, 0.55),
            Side = StyleParam.Side.Right,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withXAxisStyle(
            "",
            Id = StyleParam.SubPlotId.XAxis 5,
            Domain = (0.00, 0.33),
            Side = StyleParam.Side.Bottom,
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withYAxisStyle(
            "",
            Side = StyleParam.Side.TopLeft,
            Id = StyleParam.SubPlotId.YAxis 5,
            Domain = (0.00, 1.00),
            ShowLine = true,
            LineColor = Color.fromKeyword Black,
            ZeroLine = false
        )
        |> Chart.withLayoutGridStyle(YGap = 0.3, XGap = 0.1, SubPlots = subplotGrid)
        |> Chart.withLayoutStyle(ShowLegend = false)
        |> Chart.withLayoutStyle(Font = (Font.init(Family = StyleParam.FontFamily.Arial, Size = 14)))
        |> Chart.withSize(1500, 1600)
        |> Chart.withConfig(Config.init(ToImageButtonOptions = ConfigObjects.ToImageButtonOptions.init(Format = StyleParam.ImageFormat.SVG)))
        |> Chart.withTitle($"Growth phase analysis for cylinder {sampleId}")

    let analysis
        (filePath : string)
        (upperODCut : float)
        (lowerODCut : float)
        (cylinder : int list)
        (logger : string -> unit)
        : Result<unit, Exception> =
        try
            safeLog logger "════════════════════════════════════════"
            safeLogTime logger "START ANALYSIS"
            safeLog logger "════════════════════════════════════════"
            safeLogTime logger (sprintf "File Path: %s" filePath)
            safeLogTime logger (sprintf "Upper OD cut: %.2f" upperODCut)
            safeLogTime logger (sprintf "Lower OD cut: %.2f" lowerODCut)

            if lowerODCut <= 0.0 || upperODCut <= 0.0 then
                invalidArg "OD cut values" "OD cut values must be greater than 0 because ln(x) is only defined for positive values."
            if lowerODCut >= upperODCut then
                invalidArg "OD cut values" "Lower OD cut must be smaller than upper OD cut."

            safeLogTime logger (sprintf "Cylinders: %s" (String.Join(", ", cylinder |> List.map (fun c -> string (c + 1)))))
            safeLogTime logger ""
            safeLogTime logger "Reading input data..."
            let rawData = readRawData filePath
            safeLogTime logger "✓ Loaded input data."

            // extract pump, light and OD data frames
            safeLogTime logger "Extracting pump data..."
            let pumpFrame =
                let keys = getColumnsWithSubstring rawData ".pump"
                rawData
                |> Frame.sliceCols (keys |> Seq.sort)
                |> Frame.mapColKeys (fun x ->
                    if x.Contains("-1-") then "pump_data_1"
                    elif x.Contains("-2-") then "pump_data_2"
                    elif x.Contains("-3-") then "pump_data_3"
                    elif x.Contains("-4-") then "pump_data_4"
                    elif x.Contains("-5-") then "pump_data_5"
                    elif x.Contains("-6-") then "pump_data_6"
                    elif x.Contains("-7-") then "pump_data_7"
                    elif x.Contains("-8-") then "pump_data_8"
                    else x + "_Not_related_with_AnyCylinder"
                )
                |> Frame.fillMissing Direction.Forward
            safeLogTime logger "✓ Pump data extracted."

            safeLogTime logger "Extracting light data..."
            let lightFrame =
                let keys = getColumnsWithSubstring rawData ".light"
                rawData
                |> Frame.sliceCols (keys |> Seq.sort)
                |> Frame.mapColKeys (fun x ->
                    if x.Contains("-1-") then "light_treatment_1"
                    elif x.Contains("-2-") then "light_treatment_2"
                    elif x.Contains("-3-") then "light_treatment_3"
                    elif x.Contains("-4-") then "light_treatment_4"
                    elif x.Contains("-5-") then "light_treatment_5"
                    elif x.Contains("-6-") then "light_treatment_6"
                    elif x.Contains("-7-") then "light_treatment_7"
                    elif x.Contains("-8-") then "light_treatment_8"
                    else x
                )
                |> Frame.fillMissing Direction.Forward
            safeLogTime logger "✓ Light data extracted."

            safeLogTime logger "Extracting OD data and applying ln transform..."
            let odFrame =
                let keys = getColumnsWithSubstring rawData ".od"
                rawData
                |> Frame.sliceCols (keys |> Seq.sort)
                |> Frame.mapColKeys (fun x ->
                    if x.Contains("-1-") then "od_data_1"
                    elif x.Contains("-2-") then "od_data_2"
                    elif x.Contains("-3-") then "od_data_3"
                    elif x.Contains("-4-") then "od_data_4"
                    elif x.Contains("-5-") then "od_data_5"
                    elif x.Contains("-6-") then "od_data_6"
                    elif x.Contains("-7-") then "od_data_7"
                    elif x.Contains("-8-") then "od_data_8"
                    else x
                )
                |> Frame.mapValues safeLn
            safeLogTime logger "✓ OD data extracted."
            safeLogTime logger "OD values were transformed using the natural logarithm ln(x); non-positive OD values were ignored."

            // Helper to convert columns to arrays with defaults.
            let toArray (frame : Frame<float,string>) (colName : string) : (float * float) array =
                let col = frame.TryGetColumnObservation<float>(colName, Lookup.Exact)
                match col.HasValue with
                | true  -> col.Value.Value |> Series.observations |> Seq.toArray
                | false -> zeroArray

            let toArrayDropMissing (frame : Frame<float,string>) (colName : string) : (float * float) array =
                let col = frame.TryGetColumnObservation<float>(colName, Lookup.Exact)
                match col.HasValue with
                | true  ->
                    col.Value.Value
                    |> Series.dropMissing
                    |> Series.observations
                    |> Seq.filter (fun (_t, v) -> not (Double.IsNaN v) && not (Double.IsInfinity v))
                    |> Seq.toArray
                | false -> zeroArray

            safeLogTime logger "Preparing per cylinder arrays..."
            let pumpDataAll =
                [| toArray pumpFrame "pump_data_1";
                   toArray pumpFrame "pump_data_2";
                   toArray pumpFrame "pump_data_3";
                   toArray pumpFrame "pump_data_4";
                   toArray pumpFrame "pump_data_5";
                   toArray pumpFrame "pump_data_6";
                   toArray pumpFrame "pump_data_7";
                   toArray pumpFrame "pump_data_8" |]
                |> Array.map normalizePump

            let odDataAll =
                [| toArrayDropMissing odFrame "od_data_1";
                   toArrayDropMissing odFrame "od_data_2";
                   toArrayDropMissing odFrame "od_data_3";
                   toArrayDropMissing odFrame "od_data_4";
                   toArrayDropMissing odFrame "od_data_5";
                   toArrayDropMissing odFrame "od_data_6";
                   toArrayDropMissing odFrame "od_data_7";
                   toArrayDropMissing odFrame "od_data_8" |]

            let lightDataAll =
                [| toArray lightFrame "light_treatment_1";
                   toArray lightFrame "light_treatment_2";
                   toArray lightFrame "light_treatment_3";
                   toArray lightFrame "light_treatment_4";
                   toArray lightFrame "light_treatment_5";
                   toArray lightFrame "light_treatment_6";
                   toArray lightFrame "light_treatment_7";
                   toArray lightFrame "light_treatment_8" |]
            safeLogTime logger "✓ Prepared per cylinder arrays."

            // for i in 0 .. 7 do
            //     safeLogTime log (sprintf "Cylinder %d first 5 pump values: %A" (i + 1) (pumpDataAll.[i] |> Array.truncate 5))
            //     safeLogTime log (sprintf "Cylinder %d first 5 od values: %A" (i + 1) (odDataAll.[i] |> Array.truncate 5))

            // for i in 0 .. 7 do
            //     safeLogTime logger (sprintf "Cylinder %d lengths: pump=%d od=%d light=%d"
            //         (i + 1)
            //         pumpDataAll.[i].Length
            //         odDataAll.[i].Length
            //         lightDataAll.[i].Length)

            safeLogTime logger "Computing light phases..."
            let lightPhasesAll =
                lightDataAll |> Array.map computeLightPhases
            safeLogTime logger "✓ Light phases computed."

            // for i in 0 .. 7 do
            //     safeLogTime logger (sprintf "Cylinder %d light phases: %d" (i + 1) lightPhasesAll.[i].Length)

            safeLogTime logger "Computing growth phases..."
            let growPhasesAll =
                Array.init 8 (fun i ->
                    computeGrowPhases pumpDataAll.[i] odDataAll.[i] upperODCut lowerODCut
                )
            safeLogTime logger "✓ Growth phases computed."

            // for i in 0 .. 7 do
            //     safeLogTime logger (sprintf "Cylinder %d growth phases: %d" (i + 1) growPhasesAll.[i].Length)
            //     safeLogTime logger (sprintf "Cylinder %d growth phase windows: %A" (i + 1) growPhasesAll.[i])

            // Create output directory
            let fileDir = Path.GetDirectoryName(filePath)
            let outputDir = Path.Combine(fileDir, ("results_multicultivator_" + Path.GetFileNameWithoutExtension(filePath)))
            Directory.CreateDirectory(outputDir) |> ignore

            // For each requested cylinder, generate CSV and charts
            for idx in cylinder do
                let cylIndex = idx
                let cylId = sprintf "%d" (cylIndex + 1)
                safeLog logger "────────────────────────────────────────"
                safeLogTime logger (sprintf "CYLINDER %s" cylId)
                safeLogTime logger (sprintf "Before export: OD data=%d, pump data=%d, light data=%d, growth phases=%d, light phases=%d"
                    odDataAll.[cylIndex].Length
                    pumpDataAll.[cylIndex].Length
                    lightDataAll.[cylIndex].Length
                    growPhasesAll.[cylIndex].Length
                    lightPhasesAll.[cylIndex].Length)
                safeLog logger "────────────────────────────────────────"

                if Array.isEmpty odDataAll.[cylIndex] then
                    safeLogTime logger (sprintf "⚠ Skipping cylinder %s because OD data is empty." cylId)
                else
                    safeLogTime logger (sprintf "Exporting table for cylinder %s..." cylId)
                    let rows = tableRows growPhasesAll.[cylIndex] odDataAll.[cylIndex] upperODCut lowerODCut
                    safeLogTime logger (sprintf "   • detected growth phases: %d" growPhasesAll.[cylIndex].Length)
                    safeLogTime logger (sprintf "   • detected light phases: %d" lightPhasesAll.[cylIndex].Length)
                    saveTable rows outputDir cylId
                    safeLogTime logger (sprintf "✓ Table exported for cylinder %s." cylId)

                    // safeLogTime logger (sprintf "   • growth phases raw: %A" growPhasesAll.[cylIndex])
                    // safeLogTime logger (sprintf "   • first 10 OD values: %A" (odDataAll.[cylIndex] |> Array.truncate 10))
                    // safeLogTime logger (sprintf "   • first 10 pump values: %A" (pumpDataAll.[cylIndex] |> Array.truncate 10))
                    // let simpleShapeCount = shapeGrowOriginal growPhasesAll.[cylIndex] odDataAll.[cylIndex] upperODCut lowerODCut |> List.length
                    // let lightShapeCount = shapeLightOriginal odDataAll.[cylIndex] lightDataAll.[cylIndex] |> List.length
                    // safeLogTime logger (sprintf "   • growth shapes used: %d" simpleShapeCount)
                    // safeLogTime logger (sprintf "   • light shapes used: %d" lightShapeCount)

                    safeLogTime logger (sprintf "Building simple chart for cylinder %s..." cylId)
                    let simpleChart =
                        endGraphSingleOriginal
                            growPhasesAll.[cylIndex]
                            lightPhasesAll.[cylIndex]
                            odDataAll.[cylIndex]
                            pumpDataAll.[cylIndex]
                            lightDataAll.[cylIndex]
                            upperODCut
                            lowerODCut
                            cylId
                    safeLogTime logger (sprintf "✓ Simple chart built for cylinder %s." cylId)

                    safeLogTime logger (sprintf "Building advanced chart for cylinder %s..." cylId)
                    let advancedChart =
                        buildAdvancedChart
                            growPhasesAll.[cylIndex]
                            lightPhasesAll.[cylIndex]
                            odDataAll.[cylIndex]
                            pumpDataAll.[cylIndex]
                            lightDataAll.[cylIndex]
                            upperODCut
                            lowerODCut
                            cylId
                    safeLogTime logger (sprintf "✓ Advanced chart built for cylinder %s." cylId)

                    let simpleHtml = Path.Combine(outputDir, sprintf "simpleAnalysisOfCylinder_%s.html" cylId)
                    let advHtml = Path.Combine(outputDir, sprintf "advancedAnalysisOfCylinder_%s.html" cylId)

                    simpleChart |> Chart.saveHtml(path = simpleHtml, OpenInBrowser = false) |> ignore
                    advancedChart |> Chart.saveHtml(path = advHtml, OpenInBrowser = false) |> ignore

                    safeLogTime logger (sprintf "Charts saved for cylinder %s." cylId)

            safeLog logger "════════════════════════════════════════"
            safeLogTime logger "✓ ANALYSIS COMPLETED"
            safeLog logger "════════════════════════════════════════"
            Result.Ok()
        with ex ->
            try
                let fileDir = Path.GetDirectoryName(filePath)
                let outputDir = Path.Combine(fileDir, ("results_multicultivator_" + Path.GetFileNameWithoutExtension(filePath)))
                Directory.CreateDirectory(outputDir) |> ignore
                let debugLogPath = Path.Combine(outputDir, "debug.log")
                File.AppendAllText(debugLogPath, sprintf "[%s] Analysis error: %s%s%s%s" (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")) ex.Message Environment.NewLine (ex.ToString()) Environment.NewLine)
            with _ -> ()
            safeLog logger "════════════════════════════════════════"
            safeLogTime logger "✗ ANALYSIS FAILED"
            safeLog logger "════════════════════════════════════════"
            safeLogTime logger (sprintf "Analysis error: %s" ex.Message)
            safeLogTime logger (ex.ToString())
            Result.Error ex
