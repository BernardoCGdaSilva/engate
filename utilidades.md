Utilidades para o pacote Engate
================

# Contribuições

As funções do pacote Engate devem atender apenas a duas funções:
traduzir séries de dados equivalentes através dos seus códigos e inserir
os nomes dos dados baseado nos seus códigos associados.

Para isso, três critérios devem ser atendidos:

1.  As funções devem aceitar códigos em formato de caractere com ou sem
    zeros à esquerda e número;
2.  Devem retornar, para o caso de outros códigos, uma coluna com o
    formato igual ao dos códigos de input;
3.  A coluna do output deve estar imediatamente à direita da coluna do
    input.

# Códigos úteis

## Novas seções

``` r
library(devtools)
library(testthat)
devtools::check()
```

## Para atualizar o README

``` r
build_readme()
```
