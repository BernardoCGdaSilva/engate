#' Nomes Unidades Federativas Brasileiras
#'
#' @encoding UTF-8
#'
#' @description Procura em dataframe por coluna com códigos das unidades federativas brasileiras estabelecidos pelo IBGE
#'    e adiciona uma coluna com os nomes ou siglas relacionadas.
#'
#' @param tabela Dataframe: a tabela para adicionar os nomes os siglas das unidades federativas brasileiras
#' @param campo Caractere: a coluna com os códigos das unidades federativas brasileiras
#' @param nome Logical: deseja que seja adiconado os nomes das UFs?
#' @param sigla Logical: deseja que seja adiconado as siglas das UFs?
#'
#' @return Adiciona uma coluna com os nomes ou as siglas das unidades federativas brasileiras em uma tabela que possui apenas os códigos.
#' @export
#'
#' @examples
#' df1 <- data.frame(codigos_uf = c("11", "22", "43"))
#' df2 <- nomes_uf(df1, "codigos_uf")
#' df2
#'
#' df3 <- data.frame(codigos_uf = c(11, 22, 43))
#' df4 <- nomes_uf(df3, "codigos_uf", nome = FALSE, sigla = TRUE)
#' df4
nomes_uf <- function(tabela, campo, nome = TRUE, sigla = FALSE) {
  if (nome == FALSE & sigla == FALSE) {
    stop("Por favor, defina TRUE em ao menos um argumento nome ou sigla", call. = F)
  }

  # transforma o a coluna do campo em vetor para verificar se é número ou caracter
  col_campo <- tabela[[campo]]

  # Verifica se deseja nome e/ou sigla

  vetor_escolha <- c("cod")
  if (nome == TRUE) {
    vetor_escolha <- append(vetor_escolha, c("nome_uf"))
  }
  if (sigla == TRUE) {
    vetor_escolha <- append(vetor_escolha, c("sigla_uf"))
  }
  suporte_uf_escolhido <- suporte_uf %>% dplyr::select(tidyselect::all_of(vetor_escolha))

  # caso em que as classes são número
  if (is.numeric(col_campo)) {
    x <- dplyr::mutate(tabela, cod_caracter = sprintf("%02d", col_campo)) %>%
      dplyr::left_join(suporte_uf_escolhido, by = rlang::set_names("cod", "cod_caracter")) %>%
      subset(select = -cod_caracter)

    # caso em que são caracteres
  } else {
    x <- dplyr::left_join(tabela, suporte_uf_escolhido, by = rlang::set_names("cod", rlang::quo_name(campo)))
  }

  # Realoca a coluna nova para depois do código

  if (sigla == TRUE) {
    x <- dplyr::relocate(x, "sigla_uf", .after = campo)
  }
  if (nome == TRUE) {
    x <- dplyr::relocate(x, "nome_uf", .after = campo)
  }

  return(x)
}
