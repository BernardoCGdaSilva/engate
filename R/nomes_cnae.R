#' Nomes Classificação Nacional de Atividades Econômicas - Subclasse
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos CNAE subclasse e adiciona uma coluna com os nomes relacionados.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes das subclasses da CNAE
#' @param campo Caractere: a coluna com os códigos das subclasses da CNAE
#' @param nivel Caractere: o nível CNAE. Deve ser "subclasse", "classe", "grupo", "divisão" ou "seção".
#'
#' @return Adiciona uma coluna com as descrições das subclasses da CNAE em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_cnae_subclasse = c("0111301", "4520008", "9900800"))
#' df2 <- nomes_cnae(df1, "codigos_cnae_subclasse", "subclasse")
#' df2
#'
#' df3 <- data.frame(codigos_cnae_grupo = c(11, 452, 990))
#' df4 <- nomes_cnae(df3, "codigos_cnae_grupo", "grupo")
#' df4
nomes_cnae <- function(tabela, campo, nivel = "subclasse") {

  # Erros
  if (!(nivel %in% list("subclasse", "classe", "grupo", "divis\u00e3o", "se\u00e7\u00e3o"))) {
    stop("Por favor, escolha um n\u00edvel CNAE v\u00e1lido: subclasse, classe, grupo, divis\u00e3o ou se\u00e7\u00e3o", call. = F)
  }

  # casos em que o nivel nao seja secao
  if (nivel != "se\u00e7\u00e3o") {

    # Condicionais do nivel
    if (nivel == "subclasse") {
      tamanho <- 7
      suporte <- suporte_cnae_subclasse
      nome <- "nomes_cnae_subclasse"
    } else if (nivel == "classe") {
      tamanho <- 5
      suporte <- suporte_cnae_classe
      nome <- "nomes_cnae_classe"
    } else if (nivel == "grupo") {
      tamanho <- 3
      suporte <- suporte_cnae_grupo
      nome <- "nomes_cnae_grupo"
    } else if (nivel == "divis\u00e3o") {
      tamanho <- 2
      suporte <- suporte_cnae_divisao
      nome <- "nomes_cnae_divisao"
    }
    vetor_tamanho <- paste0("%0", tamanho, "d")

    col_campo <- tabela[[campo]]

    x <- dplyr::mutate(tabela, cod_caracter = sprintf(vetor_tamanho, as.numeric(col_campo))) %>%
      dplyr::left_join(suporte, by = rlang::set_names("cod", "cod_caracter")) %>%
      subset(select = -cod_caracter)

    # caso em que o nivel seja secao
  } else {
    x <- dplyr::left_join(tabela, suporte_cnae_secao, by = rlang::set_names("cod", rlang::quo_name(campo)))
    nome <- "nomes_cnae_secao"
  }

  # Realoca a coluna nova para depois do código

  x <- dplyr::relocate(x, nome, .after = campo)

  return(x)
}
