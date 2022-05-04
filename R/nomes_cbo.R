#' Nomes Classificação Brasileira de Ocupação
#' @encoding UTF-8
#' @param tabela Data.frame: a tabela para adicionar os nomes das CBOs
#' @param campo Caracter: a coluna com os códigos CBO
#'
#' @return Adiciona uma coluna com as descrições das CBOs em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("010105", "515220", "992115"))
#' df2 <- nomes_cbo(df1, "col1")
#' df2
#' df3 <- data.frame(col1 = c(010105, 515220, 992115))
#' df4 <- nomes_cbo(df3, "col1")
#' df4
nomes_cbo <- function(tabela, campo) {
  cod_caracter <- NULL
  teste <- subset(tabela, select = campo) %>% unlist()

  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%06d", teste))) %>%
      merge(suporte_cbo, by.x = "cod_caracter", by.y = "cod", all.x = T) %>%
      subset(select = -cod_caracter)
  } else {
    x <- merge(tabela, suporte_cbo, by.x = campo, by.y = "cod", all.x = T)
  }
  return(x)
}
