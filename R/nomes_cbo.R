#' Nomes Classificação Brasileira de Ocupação
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CBO e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das CBOs
#' @param campo Caractere: a coluna com os códigos CBO
#'
#' @return Adiciona uma coluna com as descrições das CBOs em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cbo = c("010105", "515220", "992115"))
#' df2 <- nomes_cbo(df1, "codigos_cbo")
#' df2
#'
#' df3 <- data.frame(codigos_cbo = c(010105, 515220, 992115))
#' df4 <- nomes_cbo(df3, "codigos_cbo")
#' df4
nomes_cbo <- function(tabela, campo) {
  col_campo <- tabela[[campo]]

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%06d", as.numeric(col_campo))) %>%
    dplyr::left_join(suporte_cbo, by = rlang::set_names("cod", "cod_caracter")) %>%
    subset(select = -cod_caracter)

  # Realoca a coluna nova para depois do código

  x <- dplyr::relocate(x, "nomes_cbo", .after = campo)

  return(x)
}
