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
#' df1 <- data.frame(codigos_cnae_divisao = c("01", "45", "99"))
#' df2 <- nomes_cnae_divisao(df1, "codigos_cnae_divisao")
#' df2
#'
#' df3 <- data.frame(codigos_cnae_divisao = c(1, 45, 99))
#' df4 <- nomes_cnae_divisao(df3, "codigos_cnae_divisao")
#' df4
nomes_cnae_divisao <- function(tabela, campo) {

  # transforma o a coluna do campo em vetor para verificar se é número ou caracter
  teste <- subset(tabela, select = campo) %>% unlist()

  # caso em que as divisões são número
  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%02d", teste))) %>%
      dplyr::left_join(suporte_cnae_divisao, by = rlang::set_names("cod", "cod_caracter")) %>%
      subset(select = -cod_caracter)
    # caso em que são caracteres
  } else {
    x <- dplyr::left_join(tabela, suporte_cnae_divisao, by = rlang::set_names("cod", rlang::quo_name(campo)))
  }
  return(x)
}
