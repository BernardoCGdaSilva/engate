#' Códigos Classificação Nacional de Atividades Econômicas
#' @encoding UTF-8
#' @param tabela Data.frame: a tabela para adicionar outros níveis CNAE
#' @param campo Caracter: a coluna com os códigos subclasse
#' @param nivel Caracter: o nível CNAE desejado. Deve ser "Seção", "Divisão", "Grupo" ou "Classe"
#' @param add_nomes Logical: deseja adicionar uma coluna com os nomes?
#'
#' @return Uma nova coluna com os códigos CNAE em algum nível mais agrupados.
#' @export
#'
#' @examples
codigos_CNAE <- function(tabela,campo,nivel,add_nomes=F) {
#x <- dplyr::mutate(tabela, cod = sprintf("%07d",tabela$campo))
#return(x)
}

df1 <- data.frame(col1 = c(9609201, 111302))
df2 <- data.frame(col1 = c("9609201", "111302"))
codigos_CNAE(df1, "col1")


a <- dplyr::mutate(df1, cod = sprintf("%07d",df1$col1))
b <- dplyr::mutate(df2, cod = sprintf("%07s",df2$col1))
