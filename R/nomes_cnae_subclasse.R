#' Nomes Classificação Nacional de Atividades Econômicas - Subclasse
#'
#' @encoding UTF-8
#'
#' @param tabela A tabela para adicionar os nomes das subclasses da CNAE
#' @param campo A coluna com os códigos das subclasses da CNAE
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

  # transforma o a coluna do campo em vetor para verificar se é número ou caracter
  teste <- subset(tabela, select = campo) %>% unlist()

  # caso em que as subclasses são número
  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf(sprintf("%07d", teste))) %>%
      dplyr::left_join(suporte_cnae_subclasse, by = rlang::set_names("cod", "cod_caracter")) %>%
      subset(select = -cod_caracter)
    # caso em que são caracteres
  } else {
    x <- dplyr::left_join(tabela, suporte_cnae_subclasse, by = rlang::set_names("cod", rlang::quo_name(campo)))
  }
  return(x)
}
