#' Nomes Classificação Nacional de Atividades Econômicas - Subclasse
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CNAE subclasse e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das subclasses da CNAE
#' @param campo Caractere: a coluna com os códigos das subclasses da CNAE
#'
#' @return Adiciona uma coluna com as descrições das subclasses da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cnae_subclasse = c("0111301", "4520008", "9900800"))
#' df2 <- nomes_cnae_subclasse(df1, "codigos_cnae_subclasse")
#' df2
#'
#' df3 <- data.frame(codigos_cnae_subclasse = c(111301, 4520008, 9900800))
#' df4 <- nomes_cnae_subclasse(df3, "codigos_cnae_subclasse")
#' df4
nomes_cnae_subclasse <- function(tabela, campo) {
  col_campo <- subset(tabela, select = campo) %>% unlist()

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%07d", as.numeric(col_campo))) %>%
    dplyr::left_join(suporte_cnae_subclasse, by = rlang::set_names("cod", "cod_caracter")) %>%
    subset(select = -cod_caracter)

  return(x)
}
