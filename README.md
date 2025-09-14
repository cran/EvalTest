# EvalTest

## Overview

**EvalTest** is an R Shiny application designed for evaluating diagnostic test performance. It aims to facilitate the application of statistical methods in diagnostic test evaluation by healthcare professionals.

## Description

The 'EvalTest' package provides a 'Shiny' application for evaluating diagnostic test performance using data from laboratory or diagnostic research. It supports both binary and continuous test variables. It allows users to compute and visualize:

-   **Confusion matrix**: for binary test results (or threshold categorized quantitative test results) and disease status.

-   **Key performance indicators**: sensitivity, specificity, positive predictive value (PPV), negative predictive value (NPV), likelihood ratios (LR+ and LR-), accuracy, and Youden index.

-   **Receiver Operating Characteristic (ROC) curve**: determine optimal cut-off thresholds of quantitative tests, display ROC plot and area under curve (AUC) with confidence intervals.

-   **Outputs and plot**: The application provides interactive tables and plots that can be exported for reporting purposes.

## Installation

You can install the development version of 'EvalTest' from GitHub like so:

``` r
devtools::install_github("NassimAyad87/EvalTest", dependencies = TRUE)
```

Or from CRAN (after the package is published there):

``` r
install.packages(EvalTest)
```

## Using the Application

Launch the Shiny application using:

``` r
library(EvalTest)
run_app()
```

This will open the application in your default web browser (or RStudio viewer pane) and follow these steps:

-   Before uploading your data, you should ensure that: the test variable is in one column (either qualitative or quantitative) and the reference variable (disease status) is in another column (binary: 1/0). There are no missing values in the selected columns.

-   Upload your data in Excel format (.xlsx).

-   Choose your variable test type (Qualitative or Quantitative).

-   Select the appropriate columns for test variable and reference variable (disease status).

-   Input disease prevalence value (between 0 and 1) of the study population.

-   Run the analysis and explore the results in the different tabs.

-   You can download the ROC plot and the results table for your report.

## License

This package is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

To cite the 'EvalTest' package in publications, use this script:

``` r
citation("EvalTest")
```

Or just cite as (after the package is published there):

-   Ayad N (2025). EvalTest: A Shiny App to Evaluate Diagnostic Tests Performance. R package version 1.0.2. CRAN. Available from: <https://CRAN.R-project.org/package=EvalTest>
