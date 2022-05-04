#' Nomes Classificação Nacional de Atividades Econômicas - Seção
#'
#' @encoding UTF-8
#'
#' @param tabela A tabela para adicionar os nomes das seções da CNAE
#' @param campo A coluna com os códigos das seções da CNAE
#'
#' @return Adiciona uma coluna com as descrições das seções da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("A", "G", "U"))
#' df2 <- nomes_cnae_secao(df1, col1)
#' df2
nomes_cnae_secao <- function(tabela, campo) {
  campo <- deparse(substitute(campo))
  x <- merge(tabela, suporte_cnae_secao, by.x = campo, by.y = "cod", all.x = T)

  return(x)
}
