
test_that("Input caractere", {
  df_carac <- data.frame(col1 = c("A", "G", "U"))

  expect_equal(
    nomes_cnae_secao(df_carac, "col1"),
    data.frame(
      col1 = c("A", "G", "U"),
      nomes_cnae_secao = c(
        "Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura",
        "Comércio, Reparação de Veículos Automotores e Motocicletas",
        "Organismos Internacionais e Outras Instituições Extraterritoriais"
      )
    )
  )
})
