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

  # transforma o a coluna do campo em vetor para verificar se é número ou caracter
  teste <- subset(tabela, select = campo) %>% unlist()

  # caso em que as classes são número
  if (is.numeric(teste)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf("%05d", teste)) %>%
      dplyr::left_join(suporte_cnae_classe, by = rlang::set_names("cod", "cod_caracter")) %>%
      subset(select = -cod_caracter)

    # caso em que são caracteres
  } else {
    x <- dplyr::left_join(tabela, suporte_cnae_classe, by = rlang::set_names("cod", rlang::quo_name(campo)))
  }
  return(x)
}
