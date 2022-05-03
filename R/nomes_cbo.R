#' Nomes Classificação Brasileira de Ocupação
#' @encoding UTF-8
#' @param tabela A tabela para adicionar os nomes das CBOs
#' @param campo Caracter: a coluna com os códigos CBO
#'
#' @return Adiciona uma coluna com as descrições das CBOs em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("010105", "515220", "992115"))
#' df2 <- nomes_cbo(df1,"col1")
#' df2
nomes_cbo <- function(tabela,campo){
  x <- merge(tabela, suporte_cbo, by.x = campo, by.y = "cod", all.x = T)
  return(x)
}
