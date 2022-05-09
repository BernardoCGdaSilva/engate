

test_that("nomes_cnae aceitam caracteres",{

  #subclasse
  df_caractere_1 <- data.frame(col1 = c("0111301", "4520008", "9900800"))
  expect_equal(nomes_cnae_subclasse(df_caractere_1,"col1"),
               data.frame(col1 = c("0111301", "4520008", "9900800"),
                          nomes_cnae_subclasse = c("Cultivo de Arroz",
                                                   "Serviços de Capotaria",
                                                   "Organismos Internacionais e Outras Instituições Extraterritoriais")))

})


test_that("nomes_cnae aceitam numeros",{

  #subclasse
  df_numero_1 <- data.frame(col1 = c(111301, 4520008, 9900800))
  expect_equal(nomes_cnae_subclasse(df_numero_1,"col1"),
               data.frame(col1 = c(111301, 4520008, 9900800),
                          nomes_cnae_subclasse = c("Cultivo de Arroz",
                                                   "Serviços de Capotaria",
                                                   "Organismos Internacionais e Outras Instituições Extraterritoriais")))

})
