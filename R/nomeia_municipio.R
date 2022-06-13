#' Nomes Municípios Brasileiros
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos dos municípios brasileiros estabelecidos pelo IBGE
#'    e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes dos municípios brasileiros
#' @param campo Caractere: a coluna com os códigos dos municípios brasileiros
#'
#' @return Adiciona uma coluna com os nomes dos municípios brasileiros em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_municipios = c("5219704", "3123858", "2111532"))
#' df2 <- nomeia_municipio(df1, "codigos_municipios")
#' df2
#'
#' df3 <- data.frame(codigos_municipios = c(521970, 312385, 211153))
#' df4 <- nomeia_municipio(df3, "codigos_municipios")
#' df4
nomeia_municipio <- function(tabela, campo) {
  col_campo <- tabela[[campo]]

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%s", col_campo))
  x$cod_caracter <- strtrim(x$cod_caracter, 6)
  x <- x %>%
    dplyr::left_join(suporte_municipios, by = rlang::set_names("cod", "cod_caracter")) %>%
    subset(select = -cod_caracter)

  # Realoca a coluna nova para depois do código

    x <- dplyr::relocate(x, "nome_municipio", .after = campo)

  return(x)
}
