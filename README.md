
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2023

<!-- badges: start -->
[![R-CMD-check](https://github.com/tttiiinnnaaa417/bis620.2023/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tttiiinnnaaa417/bis620.2023/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/tttiiinnnaaa417/bis620.2023/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/tttiiinnnaaa417/bis620.2023/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/tttiiinnnaaa417/bis620.2023/actions/workflows/lint.yaml/badge.svg)](https://github.com/tttiiinnnaaa417/bis620.2023/actions/workflows/lint.yaml)
<!-- badges: end -->

The goal of bis620.2023 is to get the spectral signature of accelerometry data, plot UKBiobank accelerometry data, and perform some Shiny App tasks designed for clinical trial data exploration. This package captures functions, data, and documentation for BIS620 class in Fall 2023.

Details of each function:
- `spectual_signiture()`: This function can be found in the `R` folder. This function obtains the spectral signature of UKBiobank data, which is calculated by taking the modulus of the Fourier coefficients of the signal. 
- `accel_plot()`: This function can be found in the `R` folder. This function plots UKBiobank accelerometry data. 
- `launchShinyApp()`: This function can be found in the `inst` folder, and it starts the Shiny App.
- `plot_status_histogram()`: This function can be found in the `inst` folder. This function takes in one parameter data, where data is the studies to be filtered for plotting the trial status histogram.
- `get_date_data()`: This function can be found in the `inst` folder. This function takes in three parameters fromDate, toDate, and data. fromDate is the lower limit (first day) of the desired date range, toDate is the upper limit (last day) of the desired date range, and data is the studies to subset according to the selected date range.
- ` get_gender_data() `: This function can be found in the `inst` folder. This function takes in two parameters selected_gender and data, where selected_gender is the gender selected by the user and data is the studies to be filtered based on the gender preference.
- `plot_country_histogram()`: This function can be found in the `inst` folder.  This function takes in one parameter data, where data is the studies to be filtered for plotting the trial location (country) histogram.

## Installation

You can install the development version of bis620.2023 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tttiiinnnaaa417/bis620.2023")
