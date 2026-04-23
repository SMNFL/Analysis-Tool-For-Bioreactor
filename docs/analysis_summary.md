
<p align="center">
  <img src="../deps/src/App/Assets/app_icon_readme.png" alt="Multicultivator Logo" width="680"/>
</p>
<p align="center">
Multicultivator Analysis — Cross platform desktop application for optical density based growth phase analysis<br><br>
<a href="../README.md">Main Overview</a> | <a href="analysis_summary.md">Analysis Summary</a> | <a href="../deps/README.md">App and Development</a> | <a href="../packaging/README.md">Packaging and Installation</a>
</p>

# Analysis Summary

## Purpose

This document explains, in non-code form, how the application processes a selected multicultivator input table and generates the exported result files. It summarizes the full analysis pipeline, the transformations applied to the raw measurements, the mathematical calculations used, and the meaning of the output.

---

## Input Data

The analysis expects a tab separated `.txt` file containing at least the following column types:

- `time`
- columns containing `.pump`
- columns containing `.light`
- columns containing `.od`

Cylinder assignment is inferred from patterns such as `-1-`, `-2-`, ..., `-8-` in the column names.

For each cylinder, the program tries to collect three signal types:

- pump volume
- light intensity
- optical density

If a required cylinder-specific column is missing, a fallback zero array is used internally so the pipeline can continue.

---

## Step 1: Read and index the table

The selected file is read as a tab separated table. The `time` column is used as the row index.

Mathematically, each measured signal becomes a time series of ordered pairs:

\[
(t_i, x_i)
\]

where:

- \(t_i\) is the measurement time
- \(x_i\) is the measured value at that time

---

## Step 2: Map columns to cylinders

The program renames columns internally so that each signal can be handled uniformly. For example:

- `pump_data_1`, ..., `pump_data_8`
- `light_treatment_1`, ..., `light_treatment_8`
- `od_data_1`, ..., `od_data_8`

Missing values in pump and light data are forward-filled.

This means that if a value is missing at time \(t_i\), the previous valid value is reused.

---

## Step 3: Log transform optical density

All OD values are transformed using the natural logarithm:

\[
y_i = \ln(\mathrm{OD}_i)
\]

This is done because exponential growth becomes approximately linear on the log scale.

If biomass follows:

\[
\mathrm{OD}(t) = \mathrm{OD}_0 e^{\mu t}
\]

then after log transformation:

\[
\ln(\mathrm{OD}(t)) = \ln(\mathrm{OD}_0) + \mu t
\]

where:

- \(\mu\) is the growth rate
- the slope of the line in log space is the growth rate

---

## Step 4: Normalize pump data

Pump data are converted from absolute volume to relative displacement by subtracting the first recorded value:

\[
p_{\mathrm{norm}}(t) = p(t) - p(t_0)
\]

This makes the pump signal easier to interpret visually and analytically, because the curve starts at zero.

---

## Step 5: Detect light phases

Light phases are identified by grouping consecutive time points with the same light intensity.

A phase is defined as a contiguous segment of constant light value:

\[
L_k = \{(t_i, l_i), (t_{i+1}, l_{i+1}), ..., (t_j, l_j)\}
\]

with:

\[
l_i = l_{i+1} = \cdots = l_j
\]

For each such segment, the reported light phase is:

\[
(t_{\mathrm{start}}, t_{\mathrm{end}})
\]

Short or zero-only noise segments are filtered out unless they are sufficiently long.

---

## Step 6: Detect grow phases from pump plateaus and OD thresholds

Grow phases are derived in two stages.

### 6.1 Constant pump regions

The program first identifies pump regions where the pump value remains constant for a sufficiently long interval. Internally, only plateaus with more than 250 time points are considered.

Each plateau defines a candidate time interval:

\[
[t_a, t_b]
\]

### 6.2 Restrict by OD window

Within each candidate interval, only time points are kept whose log-transformed OD lies inside the user-defined OD bounds:

\[
\ln(\mathrm{lowerODCut}) \le \ln(\mathrm{OD}(t)) \le \ln(\mathrm{upperODCut})
\]

Equivalently in original OD scale:

\[
\mathrm{lowerODCut} \le \mathrm{OD}(t) \le \mathrm{upperODCut}
\]

The final grow phase is the first and last time point of the filtered interval:

\[
(t_{\mathrm{grow,start}}, t_{\mathrm{grow,end}})
\]

If no valid OD points are found inside a candidate interval, the phase is reported internally as:

\[
(0,0)
\]

---

## Step 7: Estimate growth rate with robust linear regression

For each grow phase, the program extracts all points:

\[
(t_i, y_i)
\]

where:

- \(t_i\) lies inside the grow phase
- \(y_i = \ln(\mathrm{OD}_i)\)
- the OD is within the selected OD window

A robust Theil–Sen linear regression is then fitted:

\[
y = a + \mu t
\]

where:

- \(a\) is the intercept
- \(\mu\) is the slope

The slope \(\mu\) is taken as the growth rate:

\[
\mu = \frac{d\ln(\mathrm{OD})}{dt}
\]

### Theil–Sen estimator

The Theil–Sen slope is a robust estimate of the regression slope. Conceptually, it is based on the median of pairwise slopes:

\[
\mu = \mathrm{median}\left(\frac{y_j-y_i}{t_j-t_i}\right), \quad i < j
\]

This makes the fit less sensitive to outliers than ordinary least squares.

If fewer than 6 points are available in a grow phase, the program sets:

\[
\mu = 0
\]

---

## Step 8: Calculate duplication time

For every grow phase, the duplication time is computed from the growth rate:

\[
T_d = \frac{\ln 2}{\mu}
\]

where:

- \(T_d\) is the duplication time
- \(\mu\) is the growth rate

If \(\mu = 0\), the duplication time is reported as positive infinity.

---

## Step 9: Build exported tables

For each analyzed cylinder, a CSV table is written. Each row corresponds to one grow phase and contains:

- Phase ID
- start time of grow phase
- end time of grow phase
- growth rate
- duplication time

Formally, each table row is:

\[
(\mathrm{PhaseID}, t_{\mathrm{start}}, t_{\mathrm{end}}, \mu, T_d)
\]

---

## Step 10: Build exported charts

For each analyzed cylinder, two HTML plots are written.

### Simple chart

This chart contains:

- log-transformed OD values
- robust linear fits for the detected grow phases
- normalized pump data
- light treatment data
- highlighted grow phase and light phase regions

### Advanced chart

This chart extends the simple view and also includes:

- point plot of grow rates by phase
- box plot of grow rates
- embedded phase summary table

---

## Axis calculations

The plotting code computes safe axis ranges automatically.

### OD axis

If the minimum and maximum log-OD values are \(y_{\min}\) and \(y_{\max}\), the plotted OD axis is slightly expanded:

For the lower bound:

\[
y_{\min}^{*} =
\begin{cases}
0.95 y_{\min}, & y_{\min} > 0 \\
1.05 y_{\min}, & y_{\min} < 0 \\
y_{\min} - 0.1, & y_{\min} = 0
\end{cases}
\]

For the upper bound:

\[
y_{\max}^{*} =
\begin{cases}
1.05 y_{\max}, & y_{\max} > 0 \\
0.95 y_{\max}, & y_{\max} < 0 \\
y_{\max} + 0.1, & y_{\max} = 0
\end{cases}
\]

### Pump/light axis

The pump/light axis lower bound is the pump minimum. The upper bound is based on the larger of the pump maximum and light maximum:

\[
y_{\max}^{\mathrm{pump/light}} = 1.05 \cdot \max(p_{\max}, l_{\max})
\]

If both are zero, a default upper bound of 1 is used.

---

## Output location

All results are written next to the selected input file in a folder named:

```text
results_multicultivator_<input_filename_without_extension>
```

Example:

```text
experiment.txt
results_multicultivator_experiment/
```

This folder contains:

- per-cylinder CSV summaries
- per-cylinder simple HTML analysis plots
- per-cylinder advanced HTML analysis plots
- `debug.log` if an error occurs

---

## Summary of mathematical functions used

### Natural logarithm of OD

\[
y = \ln(\mathrm{OD})
\]

### Pump normalization

\[
p_{\mathrm{norm}}(t) = p(t) - p(t_0)
\]

### OD filter window

\[
\mathrm{lowerODCut} \le \mathrm{OD}(t) \le \mathrm{upperODCut}
\]

or in transformed space:

\[
\ln(\mathrm{lowerODCut}) \le y(t) \le \ln(\mathrm{upperODCut})
\]

### Growth model in log space

\[
y = a + \mu t
\]

### Theil–Sen growth rate estimate

\[
\mu = \mathrm{median}\left(\frac{y_j-y_i}{t_j-t_i}\right)
\]

### Duplication time

\[
T_d = \frac{\ln 2}{\mu}
\]

---

## Practical interpretation

In practical terms, the application does the following:

1. Reads the selected multicultivator table
2. Extracts time, pump, light, and OD signals
3. Converts OD to log space
4. Normalizes pump volume
5. Detects constant-light intervals
6. Detects growth windows from long constant-pump regions constrained by the OD range selected by the user
7. Fits robust linear regressions to those growth windows
8. Computes growth rate and duplication time
9. Exports tables and interactive charts for each selected cylinder

---

## Notes on robustness and edge cases

- Missing cylinder columns are replaced internally by zero arrays
- Missing pump/light values are forward-filled
- OD missing values are dropped before OD analysis
- Grow phases with too few points produce slope \(= 0\)
- If no valid OD points exist inside a candidate pump plateau, the grow phase collapses to `(0,0)` internally
- If slope \(= 0\), duplication time becomes infinite

---

## Output files generated per selected cylinder

For each selected cylinder \(k\), the program writes:

- `tableOfCylinder_k.csv`
- `simpleAnalysisOfCylinder_k.html`
- `advancedAnalysisOfCylinder_k.html`

These files summarize the detected grow phases and their quantitative characteristics.
