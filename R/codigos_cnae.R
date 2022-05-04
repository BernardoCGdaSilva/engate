#' Códigos Classificação Nacional de Atividades Econômicas
#'
#' @encoding UTF-8
#'
#' @param tabela A tabela para adicionar outros níveis CNAE.
#' @param campo A coluna com os códigos subclasse da CNAE.
#' @param nivel O nível CNAE desejado. Deve ser "seção", "divisão", "grupo" ou "classe".
#' @param add_nomes Logical: deseja adicionar uma coluna com os nomes?
#'
#' @return Uma nova coluna com os códigos CNAE em nível mais agrupados a partir de subclasse.
#' @export
#'
#' @examples
#' df1 <- data.frame(col1 = c(9609201, 111302))
#' df2 <- codigos_cnae(df1, col1, "seção")
#' df2
#'
#' df3 <- data.frame(col1 = c("9609201", "0111302"))
#' df4 <- codigos_cnae(df3, col1, "divisão")
#' df4
codigos_cnae <- function(tabela, campo, nivel, add_nomes = F) {

  cod_caracter <- cnae_divisao <- var <- `:=` <-  NULL

  # verifica o nivel escolhido
  try(if(!(nivel %in% list("classe", "grupo", "divis\u00e3o","se\u00e7\u00e3o"))){
         stop("Por favor escolha um nivel CNAE acima de subclasse.")})

  # nome a partir do nivel escolhido
  vetor_nome <- paste0("codigo_cnae_", nivel)
  # tamanho do codigo do nivel desejado
  vetor_amplitude <- ifelse(nivel == "classe", 5, ifelse(nivel == "grupo", 3, 2))
  # quote o campo
  campo <- deparse(substitute(campo))
  # vetor com os valores do campo e sua classe
  teste <- subset(tabela, select = campo) %>% unlist()

  # Calculos

  if (nivel == "se\u00e7\u00e3o") {
    if (is.numeric(teste)) {
      x <- dplyr::mutate(tabela, cod_caracter = sprintf("%07d", teste)) %>%
        dplyr::mutate(cnae_divisao = substr(cod_caracter, 1, 2)) %>%
        dplyr::left_join(tradutor_cnae_secao_divisao, by = "cnae_divisao") %>%
        dplyr::rename("codigo_cnae_secao" = "cnae_secao") %>%
        subset(select = -c(cnae_divisao,cod_caracter))
    } else {
      x <- tabela %>%
        dplyr::mutate(cnae_divisao = substr(teste, 1, 2)) %>%
        dplyr::left_join(tradutor_cnae_secao_divisao, by = "cnae_divisao") %>%
        dplyr::rename("codigo_cnae_secao" = "cnae_secao") %>%
        subset(select = -cnae_divisao)
    }
  } else {
    if (is.numeric(teste)) {
      x <- dplyr::mutate(tabela, cod_caracter = sprintf("%07d", teste)) %>%
        dplyr::mutate(var = substr(cod_caracter, 1, vetor_amplitude)) %>%
        dplyr::rename(!!vetor_nome := var) %>%
        subset(select = -cod_caracter) %>%
        dplyr::mutate(across(!!vetor_nome, as.numeric))
    } else {
      x <- tabela %>%
        dplyr::mutate(var = substr(teste, 1, vetor_amplitude)) %>%
        dplyr::rename(!!vetor_nome := var)
    }
  }

  # Adiciona nomes

  #if (add_nomes == T) {
  #  func_nomes <- paste0("nomes_cnae_", nivel)
  #  x <- do.call(func_nomes, args = c(x,vetor_nome))
  #}

  return(x)
}



df3 <- data.frame(col1 = c("9609201", "0111302"))
df4 <- codigos_cnae(df3, col1, "seção", add_nomes = T)

nivel <- "classe"

