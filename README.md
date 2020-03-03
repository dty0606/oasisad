
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OASISAD

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/oasisad.svg?branch=master)](https://travis-ci.com/muschellij2/oasisad)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/oasisad?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/oasisad)
<!-- badges: end -->

R package for oasisad, a white matter segmentation tool described in
\[<https://www.sciencedirect.com/science/article/pii/S2213158219304978>\]

Please cite at

Ding, T., et al. “An improved algorithm of white matter hyperintensity
detection in elderly adults.” NeuroImage: Clinical 25 (2020): 102151.

# Notes

1.  Please install fsl before using this package.
2.  ‘oasisad’ needs to be run under Linux/Unix system. If you are using
    windows, please find solution here:
    \[<https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation/Windows>\].
3.  In oasisad\_testcode.R script, a brief guide to use ‘oasisad’
    package is written.
4.  If you have any question regarding this package, feel free to send
    email to tid16atpittdotedu

# Installation

``` r
library(devtools)
devtools::install_github("dty0606/oasisad")
```
