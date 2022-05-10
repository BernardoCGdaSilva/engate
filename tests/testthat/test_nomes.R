
#### Testes para ver se as funções nomes aceitam coluna de caracteres

test_that("nomes aceitam caracteres",{

  # nomes_cnae_subclasse
  df_caractere_1 <- data.frame(col1 = c("0111301", "4520008", "9900800"))
  expect_equal(nomes_cnae_subclasse(df_caractere_1,"col1"),
               data.frame(col1 = c("0111301", "4520008", "9900800"),
                          nomes_cnae_subclasse = c("Cultivo de Arroz",
                                                   "Serviços de Capotaria",
                                                   "Organismos Internacionais e Outras Instituições Extraterritoriais")))

  # nomes_cnae_classe
  df_caractere_2 <- data.frame(col1 = c("01113", "45200", "99008"))
  expect_equal(nomes_cnae_classe(df_caractere_2,"col1"),
               data.frame(col1 = c("01113", "45200", "99008"),
                          nomes_cnae_classe = c("Cultivo De Cereais",
                                                   "Manutenção E Reparação De Veículos Automotores",
                                                   "Organismos Internacionais E Outras Instituições Extraterritoriais")))

  # nomes_cnae_grupo
  df_caractere_3 <- data.frame(col1 = c("011", "452", "990"))
  expect_equal(nomes_cnae_grupo(df_caractere_3,"col1"),
               data.frame(col1 = c("011", "452", "990"),
                          nomes_cnae_grupo = c("Produção De Lavouras Temporárias",
                                               "Manutenção E Reparação De Veículos Automotores",
                                               "Organismos Internacionais E Outras Instituições Extraterritoriais")))

  # nomes_cnae_divisao
  df_caractere_4 <- data.frame(col1 = c("01", "45", "99"))
  expect_equal(nomes_cnae_divisao(df_caractere_4,"col1"),
               data.frame(col1 = c("01", "45", "99"),
                          nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados",
                                                 "Comércio E Reparação De Veículos Automotores E Motocicletas",
                                                 "Organismos Internacionais E Outras Instituições Extraterritoriais")))

  # nomes_cnae_secao
  df_caractere_5 <- data.frame(col1 = c("A", "G", "U"))
  expect_equal(nomes_cnae_secao(df_caractere_5,"col1"),
               data.frame(col1 = c("A", "G", "U"),
                          nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura",
                                                 "Comércio, Reparação de Veículos Automotores e Motocicletas",
                                                 "Organismos Internacionais e Outras Instituições Extraterritoriais")))

  # nomes_cbo
  df_caractere_6 <- data.frame(col1 = c("010105", "515220", "992115"))
  expect_equal(nomes_cbo(df_caractere_6,"col1"),
               data.frame(col1 = c("010105", "515220", "992115"),
                          nomes_cbo = c("Oficial General da Aeronautica",
                                        "Auxiliar de Laboratorio de Imunobiologicos",
                                        "Borracheiro")))

  # nomes_municipio 7 caracteres
  df_caractere_7 <- data.frame(col1 = c("5219704", "3123858", "2111532"))
  expect_equal(nomes_municipio(df_caractere_7,"col1"),
               data.frame(col1 = c("5219704", "3123858", "2111532"),
                          nome_municipio = c("Santa Terezinha de Goiás",
                                        "Entre Folhas",
                                        "São Pedro da Água Branca")))

  # nomes_municipio 6 caracteres
  df_caractere_8 <- data.frame(col1 = c("521970", "312385", "211153"))
  expect_equal(nomes_municipio(df_caractere_8,"col1"),
               data.frame(col1 = c("521970", "312385", "211153"),
                          nome_municipio = c("Santa Terezinha de Goiás",
                                             "Entre Folhas",
                                             "São Pedro da Água Branca")))

  # nomes_uf
  df_caractere_9 <- data.frame(col1 = c("11", "22", "43"))
  expect_equal(nomes_uf(df_caractere_9,"col1"),
               data.frame(col1 = c("11", "22", "43"),
                          nome_uf = c("Rondônia",
                                             "Piauí",
                                             "Rio Grande do Sul")))


})


#### Testes para ver se as funções nomes aceitam coluna de números

test_that("nomes_cnae aceitam numeros",{

  #subclasse
  df_numero_1 <- data.frame(col1 = c(111301, 4520008, 9900800))
  expect_equal(nomes_cnae_subclasse(df_numero_1,"col1"),
               data.frame(col1 = c(111301, 4520008, 9900800),
                          nomes_cnae_subclasse = c("Cultivo de Arroz",
                                                   "Serviços de Capotaria",
                                                   "Organismos Internacionais e Outras Instituições Extraterritoriais")))

  # nomes_cnae_classe
  df_numero_2 <- data.frame(col1 = c(1113, 45200, 99008))
  expect_equal(nomes_cnae_classe(df_numero_2,"col1"),
               data.frame(col1 = c(1113, 45200, 99008),
                          nomes_cnae_classe = c("Cultivo De Cereais",
                                                "Manutenção E Reparação De Veículos Automotores",
                                                "Organismos Internacionais E Outras Instituições Extraterritoriais")))

  # nomes_cnae_grupo
  df_numero_3 <- data.frame(col1 = c(11, 452, 990))
  expect_equal(nomes_cnae_grupo(df_numero_3,"col1"),
               data.frame(col1 = c(11, 452, 990),
                          nomes_cnae_grupo = c("Produção De Lavouras Temporárias",
                                               "Manutenção E Reparação De Veículos Automotores",
                                               "Organismos Internacionais E Outras Instituições Extraterritoriais")))

  # nomes_cnae_divisao
  df_numero_4 <- data.frame(col1 = c(1, 45, 99))
  expect_equal(nomes_cnae_divisao(df_numero_4,"col1"),
               data.frame(col1 = c(1, 45, 99),
                          nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados",
                                                 "Comércio E Reparação De Veículos Automotores E Motocicletas",
                                                 "Organismos Internacionais E Outras Instituições Extraterritoriais")))

  # nomes_cbo
  df_numero_6 <- data.frame(col1 = c(10105, 515220, 992115))
  expect_equal(nomes_cbo(df_numero_6,"col1"),
               data.frame(col1 = c(10105, 515220, 992115),
                          nomes_cbo = c("Oficial General da Aeronautica",
                                        "Auxiliar de Laboratorio de Imunobiologicos",
                                        "Borracheiro")))

  # nomes_municipio 7 números
  df_numero_7 <- data.frame(col1 = c(5219704, 3123858, 2111532))
  expect_equal(nomes_municipio(df_numero_7,"col1"),
               data.frame(col1 = c(5219704, 3123858, 2111532),
                          nome_municipio = c("Santa Terezinha de Goiás",
                                             "Entre Folhas",
                                             "São Pedro da Água Branca")))

  # nomes_municipio 6 números
  df_numero_8 <- data.frame(col1 = c(521970, 312385, 211153))
  expect_equal(nomes_municipio(df_numero_8,"col1"),
               data.frame(col1 = c(521970, 312385, 211153),
                          nome_municipio = c("Santa Terezinha de Goiás",
                                             "Entre Folhas",
                                             "São Pedro da Água Branca")))

  # nomes_uf
  df_numero_9 <- data.frame(col1 = c(11, 22, 43))
  expect_equal(nomes_uf(df_numero_9,"col1"),
               data.frame(col1 = c(11, 22, 43),
                          nome_uf = c("Rondônia",
                                      "Piauí",
                                      "Rio Grande do Sul")))

})
