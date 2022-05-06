#' Nomes Classificação Nacional de Atividades Econômicas - Seção
#'
#' @encoding UTF-8
#'
#' @param tabela Data.frame: a tabela para adicionar os nomes das seções da CNAE
#' @param campo Caracter: a coluna com os códigos das seções da CNAE
#'
#' @return Adiciona uma coluna com as descrições das seções da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cnae_secao = c("A", "G", "U"))
#' df2 <- nomes_cnae_secao(df1, "codigos_cnae_secao")
#' df2
nomes_cnae_secao <- function(tabela, campo) {
  x <- dplyr::left_join(tabela, suporte_cnae_secao, by = rlang::set_names("cod", rlang::quo_name(campo)))
  return(x)
}
