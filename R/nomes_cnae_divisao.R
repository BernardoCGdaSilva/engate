#' Nomes Classificação Nacional de Atividades Econômicas - Divisão
#'
#' @encoding UTF-8
#'
#' @param tabela Data.frame: a tabela para adicionar os nomes das divisões da CNAE
#' @param campo Caracter: a coluna com os códigos das divisões da CNAE
#'
#' @return Adiciona uma coluna com as descrições das divisões da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("01","45","99"))
#' df2 <- nomes_cnae_divisao(df1, "col1")
#' df2
#'
#' df3 <- data.frame(col1 = c(1,45,99))
#' df4 <- nomes_cnae_divisao(df3, "col1")
#' df4
nomes_cnae_divisao <- function(tabela, campo) {
  cod_caracter <- NULL
  teste <- subset(tabela, select = campo) %>% unlist()

  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%02d", teste))) %>%
      merge(suporte_cnae_divisao, by.x = "cod_caracter", by.y = "cod", all.x = T) %>%
      subset(select = -cod_caracter)
  } else {
    x <- merge(tabela, suporte_cnae_divisao, by.x = campo, by.y = "cod", all.x = T)
  }
  return(x)
}
