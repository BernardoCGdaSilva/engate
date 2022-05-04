#' Nomes Classificação Nacional de Atividades Econômicas - Grupo
#'
#' @encoding UTF-8
#'
#' @param tabela Data.frame: a tabela para adicionar os nomes das grupos da CNAE
#' @param campo Caracter: a coluna com os códigos das grupos da CNAE
#'
#' @return Adiciona uma coluna com as descrições das grupos da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c("011","452","990"))
#' df2 <- nomes_cnae_grupo(df1, "col1")
#' df2
#'
#' df3 <- data.frame(col1 = c(11,452,990))
#' df4 <- nomes_cnae_grupo(df3, "col1")
#' df4
nomes_cnae_grupo <- function(tabela, campo) {
  cod_caracter <- NULL
  teste <- subset(tabela, select = campo) %>% unlist()

  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%03d", teste))) %>%
      merge(suporte_cnae_grupo, by.x = "cod_caracter", by.y = "cod", all.x = T) %>%
      subset(select = -cod_caracter)
  } else {
    x <- merge(tabela, suporte_cnae_grupo, by.x = campo, by.y = "cod", all.x = T)
  }
  return(x)
}
