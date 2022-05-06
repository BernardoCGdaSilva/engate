#' Nomes Municípios Brasileiros
#'
#' @encoding UTF-8
#'
#'@description Procura em dataframe por coluna com códigos dos municípios brasileiros e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes dos municípios brasileiros
#' @param campo Caractere: a coluna com os códigos dos municípios brasileiros
#'
#' @return Adiciona uma coluna com nomes dos municípios brasileiros em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_municipios = c("5219704", "3123858", "2111532"))
#' df2 <- nomes_cnae_grupo(df1, "codigos_municipios")
#' df2
#'
#' df3 <- data.frame(codigos_municipios = c(5219704, 3123858, 2111532))
#' df4 <- nomes_cnae_grupo(df3, "codigos_municipios")
#' df4
nomes_municipio <- function(tabela, campo) {

  # transforma o a coluna do campo em vetor para verificar se é número ou caracter
  teste <- subset(tabela, select = campo) %>% unlist()

  # caso em que os grupos são número
  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf("%06d", teste)) %>%
      dplyr::left_join(suporte_municipios, by = rlang::set_names("cod", "cod_caracter")) %>%
      subset(select = -cod_caracter)
    # caso em que são caracteres
  } else {
    x <- dplyr::left_join(tabela, suporte_municipios, by = rlang::set_names("cod", rlang::quo_name(campo)))
  }
  return(x)
}
