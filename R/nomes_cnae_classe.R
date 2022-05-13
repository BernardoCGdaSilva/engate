#' Nomes Classificação Nacional de Atividades Econômicas - Classe
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CNAE classe e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das classes da CNAE
#' @param campo Caractere: a coluna com os códigos das classes da CNAE
#'
#' @return Adiciona uma coluna com as descrições das classes da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cnae_classes = c("01113", "45200", "99008"))
#' df2 <- nomes_cnae_classe(df1, "codigos_cnae_classes")
#' df2
#'
#' df3 <- data.frame(codigos_cnae_classes = c(1113, 45200, 99008))
#' df4 <- nomes_cnae_classe(df3, "codigos_cnae_classes")
#' df4
nomes_cnae_classe <- function(tabela, campo) {
  col_campo <- subset(tabela, select = campo) %>% unlist()

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%05d", as.numeric(col_campo))) %>%
    dplyr::left_join(suporte_cnae_classe, by = rlang::set_names("cod", "cod_caracter")) %>%
    subset(select = -cod_caracter)

  return(x)
}
