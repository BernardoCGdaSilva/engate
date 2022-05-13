#' Nomes Classificação Nacional de Atividades Econômicas - Grupo
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CNAE grupo e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das grupos da CNAE
#' @param campo Caractere: a coluna com os códigos das grupos da CNAE
#'
#' @return Adiciona uma coluna com as descrições das grupos da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cnae_grupo = c("011", "452", "990"))
#' df2 <- nomes_cnae_grupo(df1, "codigos_cnae_grupo")
#' df2
#'
#' df3 <- data.frame(codigos_cnae_grupo = c(11, 452, 990))
#' df4 <- nomes_cnae_grupo(df3, "codigos_cnae_grupo")
#' df4
nomes_cnae_grupo <- function(tabela, campo) {
  col_campo <- subset(tabela, select = campo) %>% unlist()

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%03d", as.numeric(col_campo))) %>%
    dplyr::left_join(suporte_cnae_grupo, by = rlang::set_names("cod", "cod_caracter")) %>%
    subset(select = -cod_caracter)

  return(x)
}
