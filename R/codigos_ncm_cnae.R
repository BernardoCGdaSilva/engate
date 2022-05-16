#' Códigos Nomeclatura Comum do Mercosul - Classificação Brasileira de Atividades Econômicas (classe)
#'
#' @description Em um dataframe que possui uma coluna com os códigos NCM, adiciona uma coluna extra com os códigos
#'     das classes CNAE equivalentes. Adiciona ainda os nomes destas, se desejar. A tradução é realizada de acordo com
#'     a correspondência NCM 2012 x CNAE 2.0 divulgada pelo
#'     \href{https://concla.ibge.gov.br/classificacoes/correspondencias/atividades-economicas.html}{IBGE}.
#'
#' @param tabela Dataframe: a tabela para adicionar os códigos das classes CNAE.
#' @param campo Caractere: a coluna com os códigos NCM.
#' @param add_nomes Logical: deseja adicionar uma coluna com os nomes das classes CNAE?
#'
#' @return Uma nova coluna com os códigos das classes CNAE equivalentes aos códigos NCM.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_ncm = c("01012100", "35052000", "84459030"))
#' df2 <- codigos_ncm_cnae(df1, "codigos_ncm")
#' df2
#'
#' df3 <- data.frame(codigos_ncm = c(1012100, 35052000, 84459030))
#' df4 <- codigos_ncm_cnae(df3, "codigos_ncm", add_nomes = TRUE)
#' df4
codigos_ncm_cnae <- function(tabela, campo, add_nomes = FALSE) {
  col_campo <- subset(tabela, select = campo) %>% unlist()

  x <- dplyr::mutate(tabela, cod_caracter = sprintf("%08d", as.numeric(col_campo))) %>%
    dplyr::left_join(tradutor_ncm_cnae_classe, by = c("cod_caracter" = "ncm")) %>%
    subset(select = -cod_caracter)

  # Adiciona nomes

  if (add_nomes == TRUE) {
    x <- do.call(nomes_cnae, args = list(tabela = x, campo = "cnae_classe", nivel = "classe"))
  }

  # Transforma coluna output no formato do input

  if (is.numeric(col_campo)) {
    x$cnae_classe <- as.numeric(x$cnae_classe)
  }

  return(x)
}
