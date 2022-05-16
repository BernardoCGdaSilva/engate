#' Códigos Classificação Nacional de Atividades Econômicas - Níveis
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com os códigos da CNAE subclasse e adiciona coluna extra contendo
#'     os códigos CNAE equivalentes em nível superior (classe, grupo, divisão ou seção), conforme escolha.
#'
#' @param tabela Dataframe: a tabela para adicionar outros níveis CNAE.
#' @param campo Caractere: a coluna com os códigos da CNAE atuais para input.
#' @param nivel_input Caractere: o nível CNAE atual. Deve ser "subclasse", "classe", "grupo" ou "divisão".
#' @param nivel_output Caractere: o nível CNAE desejado. Deve ser "classe", "grupo", "divisão" ou "seção".
#' @param add_nomes Logical: deseja adicionar uma coluna com os nomes?
#'
#' @return Uma nova coluna com os códigos CNAE em nível mais agrupados. Se desejar, uma outra coluna com os nomes.
#' @export
#'
#' @examples
#' df1 <- data.frame(cnae_subclasse = c(111302, 9609201))
#' df2 <- codigos_cnae_nivel(df1, "cnae_subclasse")
#' df2
#'
#' df3 <- data.frame(cnae_grupo = c("011", "960"))
#' df4 <- codigos_cnae_nivel(df3, "cnae_grupo", "grupo", "divisão", TRUE)
#' df4
codigos_cnae_nivel <- function(tabela, campo, nivel_input = "subclasse", nivel_output = "se\u00e7\u00e3o", add_nomes = FALSE) {

  # Erros
  if (!(nivel_output %in% list("classe", "grupo", "divis\u00e3o", "se\u00e7\u00e3o"))) {
    stop("Por favor, escolha um n\u00edvel CNAE acima de subclasse para output.", call. = F)
  }
  if (!(nivel_input %in% list("subclasse", "classe", "grupo", "divis\u00e3o"))) {
    stop("Por favor, escolha um n\u00edvel CNAE abaixo de se\u00e7\u00e3o para input", call. = F)
  }
  if (nivel_input == "classe" & nivel_output %in% list("classe")) {
    stop("Por favor, para input classe, escolha como output grupo, divis\u00e3o ou se\u00e7\u00e3o", call. = F)
  }
  if (nivel_input == "grupo" & nivel_output %in% list("classe", "grupo")) {
    stop("Por favor, para input grupo, escolha como output divis\u00e3o ou se\u00e7\u00e3o", call. = F)
  }
  if (nivel_input == "divis\u00e3o" & nivel_output %in% list("classe", "grupo", "divis\u00e3o")) {
    stop("Por favor, para input divis\u00e3o, escolha como output se\u00e7\u00e3o", call. = F)
  }

  # Condicionais do input
  if (nivel_input == "subclasse") {
    tamanho <- 7
  } else if (nivel_input == "classe") {
    tamanho <- 5
  } else if (nivel_input == "grupo") {
    tamanho <- 3
  } else if (nivel_input == "divis\u00e3o") {
    tamanho <- 2
  }

  # Condicionais do output
  if (nivel_output == "classe") {
    limite <- 5
    vetor_nivel_output_ascii <- paste0(nivel_output)
  } else if (nivel_output == "grupo") {
    limite <- 3
    vetor_nivel_output_ascii <- paste0(nivel_output)
  } else if (nivel_output == "divis\u00e3o") {
    limite <- 2
    vetor_nivel_output_ascii <- "divisao"
  } else if (nivel_output == "se\u00e7\u00e3o") {
    limite <- 2
    vetor_nivel_output_ascii <- "secao"
  }

  # Resutlados dos condicionais e argumentos
  vetor_nome <- paste0("codigo_cnae_", vetor_nivel_output_ascii)
  vetor_tamanho <- paste0("%0", tamanho, "d")
  col_campo <- subset(tabela, select = campo) %>% unlist()

  # dataframe basico
  x <- dplyr::mutate(tabela, cod_caracter = sprintf(vetor_tamanho, as.numeric(col_campo))) %>%
    dplyr::mutate(vetor_temp = substr(cod_caracter, 1, limite)) %>%
    subset(select = -cod_caracter)

  # se secao
  if (nivel_output != "se\u00e7\u00e3o") {
    # Transforma coluna output no formato do input
    if (is.numeric(col_campo)) {
      x <- x %>% dplyr::mutate(across(vetor_temp, as.numeric))
    }
    x <- x %>% dplyr::rename(!!vetor_nome := vetor_temp)
  } else {
    x <- x %>%
      dplyr::left_join(tradutor_cnae_secao_divisao, by = c("vetor_temp" = "cnae_divisao")) %>%
      dplyr::rename("codigo_cnae_secao" = "cnae_secao") %>%
      subset(select = -c(vetor_temp))
  }

  # Adiciona nomes

  if (add_nomes == TRUE) {
    x <- do.call(nomes_cnae, args = list(tabela = x, campo = vetor_nome, nivel = nivel_output))
  }

  return(x)
}
