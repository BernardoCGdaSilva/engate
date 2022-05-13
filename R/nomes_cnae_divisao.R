#' Nomes Classificação Nacional de Atividades Econômicas - Divisão
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CNAE divisão e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das divisões da CNAE
#' @param campo Caractere: a coluna com os códigos das divisões da CNAE
#'
#' @return Adiciona uma coluna com as descrições das divisões da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cnae_divisao = c("01", "45", "99"))
#' df2 <- nomes_cnae_divisao(df1, "codigos_cnae_divisao")
#' df2
#'
#' df3 <- data.frame(codigos_cnae_divisao = c(1, 45, 99))
#' df4 <- nomes_cnae_divisao(df3, "codigos_cnae_divisao")
#' df4
nomes_cnae_divisao <- function(tabela, campo) {
  col_campo <- subset(tabela, select = campo) %>% unlist()

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%02d", as.numeric(col_campo))) %>%
    dplyr::left_join(suporte_cnae_divisao, by = rlang::set_names("cod", "cod_caracter")) %>%
    subset(select = -cod_caracter)

  return(x)
}
