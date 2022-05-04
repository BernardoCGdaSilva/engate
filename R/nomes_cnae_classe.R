#' Nomes Classificação Nacional de Atividades Econômicas - Classe
#'
#' @encoding UTF-8
#'
#' @param tabela A tabela para adicionar os nomes das classes da CNAE
#' @param campo A coluna com os códigos das classes da CNAE
#'
#' @return Adiciona uma coluna com as descrições das classes da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("01113", "45200", "99008"))
#' df2 <- nomes_cnae_classe(df1, col1)
#' df2
#'
#' df3 <- data.frame(col1 = c(1113, 45200, 99008))
#' df4 <- nomes_cnae_classe(df3, col1)
#' df4
nomes_cnae_classe <- function(tabela, campo) {
  cod_caracter <- NULL
  campo <- deparse(substitute(campo))
  teste <- subset(tabela, select = campo) %>% unlist()

  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%05d", teste))) %>%
      merge(suporte_cnae_classe, by.x = "cod_caracter", by.y = "cod", all.x = T) %>%
      subset(select = -cod_caracter)
  } else {
    x <- merge(tabela, suporte_cnae_classe, by.x = campo, by.y = "cod", all.x = T)
  }
  return(x)
}
