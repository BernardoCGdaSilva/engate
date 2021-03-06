% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nomeia_municipio.R
\encoding{UTF-8}
\name{nomeia_municipio}
\alias{nomeia_municipio}
\title{Nomes Municípios Brasileiros}
\usage{
nomeia_municipio(tabela, campo)
}
\arguments{
\item{tabela}{Dataframe: a tabela para adicionar os nomes dos municípios brasileiros}

\item{campo}{Caractere: a coluna com os códigos dos municípios brasileiros}
}
\value{
Adiciona uma coluna com os nomes dos municípios brasileiros em uma tabela que possui apenas os códigos.
}
\description{
Procura em dataframe por coluna com códigos dos municípios brasileiros estabelecidos pelo IBGE
e adiciona uma coluna com os nomes relacionados.
}
\examples{
df1 <- data.frame(codigos_municipios = c("5219704", "3123858", "2111532"))
df2 <- nomeia_municipio(df1, "codigos_municipios")
df2

df3 <- data.frame(codigos_municipios = c(521970, 312385, 211153))
df4 <- nomeia_municipio(df3, "codigos_municipios")
df4
}
