#' Nomes Classificação Brasileira de Ocupação
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CBO e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das CBOs
#' @param campo Caractere: a coluna com os códigos CBO
#'
#' @return Adiciona uma coluna com as descrições das CBOs em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cbo = c("010105", "515220", "992115"))
#' df2 <- nomes_cbo(df1, "codigos_cbo")
#' df2
#'
#' df3 <- data.frame(codigos_cbo = c(010105, 515220, 992115))
#' df4 <- nomes_cbo(df3, "codigos_cbo")
#' df4
nomes_cbo <- function(tabela, campo) {

  # transforma o a coluna do campo em vetor para verificar se é número ou caracter
  teste <- subset(tabela, select = campo) %>% unlist()

  # caso em que as cbos são número
  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf("%06d", teste)) %>%
      merge(suporte_cbo, by.x = "cod_caracter", by.y = "cod", all.x = T) %>%
      subset(select = -cod_caracter)
    # caso em que são caracteres
  } else {
    x <- merge(tabela, suporte_cbo, by.x = campo, by.y = "cod", all.x = T)
  }
  return(x)
}
