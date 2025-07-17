
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfi

<!-- badges: start -->
<!-- badges: end -->

The SFI R package provides tools to compute the Signal Fidelity Index
(SFI), which quantifies diagnostic data quality at the patient level. It
enables simulation of clinical data, calculation of SFI components, and
evaluation of SFI-aware calibration methods to improve model performance
across heterogeneous datasets without requiring outcome labels in target
domains.

## Installation

You can install the development version of sfi like so:

``` r
install.packages("devtools")  # if not already installed
#> Installing package into '/private/var/folders/tk/fzb3c9wj2zn6bztrz6kd_mxr0000gq/T/RtmpQDdxOg/temp_libpath227e2cc6acaa'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/tk/fzb3c9wj2zn6bztrz6kd_mxr0000gq/T//Rtmpq8cRSY/downloaded_packages
devtools::install_github("clai-group/sfi")
#> Downloading GitHub repo clai-group/sfi@HEAD
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/tk/fzb3c9wj2zn6bztrz6kd_mxr0000gq/T/Rtmpq8cRSY/remotes28691268517/clai-group-SFI-a3ba8ed/DESCRIPTION’ ... OK
#> * preparing ‘sfi’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘sfi_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/tk/fzb3c9wj2zn6bztrz6kd_mxr0000gq/T/RtmpQDdxOg/temp_libpath227e2cc6acaa'
#> (as 'lib' is unspecified)
```
