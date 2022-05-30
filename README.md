
<!-- README.md is generated from README.Rmd. Please edit that file -->

# engate

<!-- badges: start -->

[![R-CMD-check](https://github.com/BernardoCGdaSilva/engate/workflows/R-CMD-check/badge.svg)](https://github.com/BernardoCGdaSilva/engate/actions)
<!-- badges: end -->

engate is a package which aims to help data manipulation of brazilian
public data. It consists of two sets of functions - nomes and codigos -
the former matchs an index column with its description and the latter, a
related index.

## Installation

You can install the development version of engate from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BernardoCGdaSilva/engate")
```

## Examples

This is an example of the nomes set of functions:

``` r
library(engate)
df1 <- data.frame(codigos_cbo = c("010105", "515220", "992115"))
df2 <- nomes_cbo(df1,"codigos_cbo")
df2
#>   codigos_cbo                                  nomes_cbo
#> 1      010105             Oficial General da Aeronautica
#> 2      515220 Auxiliar de Laboratorio de Imunobiologicos
#> 3      992115                                Borracheiro
```

And this is an example of the codigos set:

``` r
library(engate)
df1 <- data.frame(codigos_ncm = c("01012100", "35052000", "84459030"))
df2 <- codigos_ncm_cnae(df1, "codigos_ncm")
df2
#>   codigos_ncm cnae_classe
#> 1    01012100       01521
#> 2    35052000       20916
#> 3    84459030       28631
```
