# EvalTest 1.0.6

## Changes in this version

- Updated package title better reflect the existence of a function to compute indicators not (just the Shiny app).
- Updated description to clarify that the package provides both a function for computing diagnostic indicators and a Shiny application for evaluating diagnostic test performance.
- Refined script for calculating confidence intervals of PPV and NPV to improve statistical accuracy.
- Renamed outputs returned by `compute_indicators()` for improved consistency and readability.
- Updated the `compute_indicators()` example using the same data used in the vignette to ensure alignment across documentation.
- Minor wording and clarity improvements in the EvalTest vignette.

# EvalTest 1.0.5

## Changes in this version

-   Modified the `EvalTest`app script to align with the latest versions of the packages used in its build process.

# EvalTest 1.0.4

## Changes in this version

-   Updated the `EvalTest` Shiny app to ensure compatibility with the latest version of **ggplot2**.
-   Replaced the deprecated argument `size` in **ggplot2** with `linewidth` in the app script to remove warnings.
-   Fixed minor typos in the vignette and README files.

# EvalTest 1.0.3

## Changes in this version

-   Fixed a minor typo in the application script: removed duplicated word displayed in the app layout.
-   Updated vignette (`introduction.Rmd`) and README for clarity.

# EvalTest 1.0.2

## Initial CRAN release

-   First public version of the **EvalTest** package.
-   Provides an R function for computing indicators and a Shiny application for evaluating diagnostic tests performance.
-   Added unit tests and detailed documentation for key functions.
