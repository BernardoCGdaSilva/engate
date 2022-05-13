#' Códigos Municípios - Unidades Federativas Brasileiras
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos dos municípios brasileiros estabelecidos pelo IBGE
#'    e adiciona uma coluna com os códigos das unidades federativas relacionadas.
#'
#' @param tabela Dataframe: a tabela para adicionar os códigos das unidades federativas
#' @param campo Caractere: a coluna com os códigos dos municípios brasileiros
#' @param add_nomes Logical: deseja adicionar uma coluna com os nomes das unidades federativas?
#'
#' @return Adiciona uma coluna com os códigos das unidades federativas brasileiras em uma tabela que possui apenas os códigos dos municípios.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_municipios = c("5219704", "3123858", "2111532"))
#' df2 <- codigos_municipio_uf(df1, "codigos_municipios")
#' df2
#'
#' df3 <- data.frame(codigos_municipios = c(521970, 312385, 211153))
#' df4 <- codigos_municipio_uf(df3, "codigos_municipios", add_nomes = TRUE)
#' df4
codigos_municipio_uf <- function(tabela, campo, add_nomes = FALSE) {
  col_campo <- subset(tabela, select = campo) %>% unlist()

  x <- dplyr::mutate(tabela, codigo_uf = sprintf("%s", col_campo))
  x$codigo_uf <- strtrim(x$codigo_uf, 2)

  # Adiciona nomes

  if (add_nomes == TRUE) {
    x <- nomes_uf(x, "codigo_uf")
  }

  # Transforma coluna output no formato do input

  if (is.numeric(col_campo)) {
    x$codigo_uf <- as.numeric(x$codigo_uf)
  }

  return(x)
}
