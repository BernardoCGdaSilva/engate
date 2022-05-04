#' Nomes Classificação Nacional de Atividades Econômicas - Subclasse
#'
#' @encoding UTF-8
#'
#' @param tabela Data.frame: a tabela para adicionar os nomes das subclasses da CNAE
#' @param campo Caracter: a coluna com os códigos das subclasses da CNAE
#'
#' @return Adiciona uma coluna com as descrições das subclasses da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("0111301","4520008","9900800"))
#' df2 <- nomes_cnae_subclasse(df1, "col1")
#' df2
#'
#' df3 <- data.frame(col1 = c(111301,4520008,9900800))
#' df4 <- nomes_cnae_subclasse(df3, "col1")
#' df4
nomes_cnae_subclasse <- function(tabela, campo) {
  cod_caracter <- NULL
  teste <- subset(tabela, select = campo) %>% unlist()

  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%07d", teste))) %>%
      merge(suporte_cnae_subclasse, by.x = "cod_caracter", by.y = "cod", all.x = T) %>%
      subset(select = -cod_caracter)
  } else {
    x <- merge(tabela, suporte_cnae_subclasse, by.x = campo, by.y = "cod", all.x = T)
  }
  return(x)
}
