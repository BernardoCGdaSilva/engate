
<!-- README.md is generated from README.Rmd. Please edit that file -->

# engate

<!-- badges: start -->
<!-- badges: end -->

Engate is a package which aims to help data manipulation of brazilian
public data. It consists of two sets of functions - nomes and codigos -
that match a code column with its description (in the first case) or a
related code (in the second).

## Installation

You can install the development version of engate from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BernardoCGdaSilva/engate")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(engate)
df1 <- data.frame(col1 = c("010105", "515220", "992115"))
df2 <- nomes_cbo(df1,"col1")
df2
#>     col1                                       nome
#> 1 010105             Oficial General da Aeronautica
#> 2 515220 Auxiliar de Laboratorio de Imunobiologicos
#> 3 992115                                Borracheiro
```
