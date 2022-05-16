
#_________________________________________________________________________________________________
#_________________________________________NIVEL SUBCLASSE_________________________________________
#_________________________________________________________________________________________________

test_that("nivel subclasse",{
  df_carac_cheio <- data.frame(col1 = c("0111302", "9609201"))
  df_carac_semi <- data.frame(col1 = c("111302", "9609201"))
  df_num <- data.frame(col1 = c(111302, 9609201))

  # Caractere cheio
  expect_equal(nomes_cnae(df_carac_cheio, "col1"),
               data.frame(col1 = df_carac_cheio, nomes_cnae_subclasse = c("Cultivo de Milho", "Clínicas de estética e similares")))

  # Caractere semi
  expect_equal(nomes_cnae(df_carac_semi, "col1"),
               data.frame(col1 = df_carac_semi, nomes_cnae_subclasse = c("Cultivo de Milho", "Clínicas de estética e similares")))

  # Numero
  expect_equal(nomes_cnae(df_num, "col1"),
               data.frame(col1 = df_num, nomes_cnae_subclasse = c("Cultivo de Milho", "Clínicas de estética e similares")))
})

#_________________________________________________________________________________________________
#__________________________________________NIVEL CLASSE___________________________________________
#_________________________________________________________________________________________________

test_that("nivel classe",{
  df_carac_cheio <- data.frame(col1 = c("01113", "96092"))
  df_carac_semi <- data.frame(col1 = c("1113", "96092"))
  df_num <- data.frame(col1 = c(1113, 96092))

  # Caractere cheio
  expect_equal(nomes_cnae(df_carac_cheio, "col1", nivel = "classe"),
               data.frame(col1 = df_carac_cheio, nomes_cnae_classe = c("Cultivo De Cereais", "Atividades De Serviços Pessoais Não Especificadas Anteriormente")))

  # Caractere semi
  expect_equal(nomes_cnae(df_carac_semi, "col1", nivel = "classe"),
               data.frame(col1 = df_carac_semi, nomes_cnae_classe = c("Cultivo De Cereais", "Atividades De Serviços Pessoais Não Especificadas Anteriormente")))

  # Numero
  expect_equal(nomes_cnae(df_num, "col1", nivel = "classe"),
               data.frame(col1 = df_num, nomes_cnae_classe = c("Cultivo De Cereais", "Atividades De Serviços Pessoais Não Especificadas Anteriormente")))
})

#_________________________________________________________________________________________________
#___________________________________________NIVEL GRUPO___________________________________________
#_________________________________________________________________________________________________

test_that("nivel grupo",{
  df_carac_cheio <- data.frame(col1 = c("011", "960"))
  df_carac_semi <- data.frame(col1 = c("11", "960"))
  df_num <- data.frame(col1 = c(11, 960))

  # Caractere cheio
  expect_equal(nomes_cnae(df_carac_cheio, "col1", nivel = "grupo"),
               data.frame(col1 = df_carac_cheio, nomes_cnae_grupo = c("Produção De Lavouras Temporárias", "Outras Atividades De Serviços Pessoais")))

  # Caractere semi
  expect_equal(nomes_cnae(df_carac_semi, "col1", nivel = "grupo"),
               data.frame(col1 = df_carac_semi, nomes_cnae_grupo = c("Produção De Lavouras Temporárias", "Outras Atividades De Serviços Pessoais")))

  # Numero
  expect_equal(nomes_cnae(df_num, "col1", nivel = "grupo"),
               data.frame(col1 = df_num, nomes_cnae_grupo = c("Produção De Lavouras Temporárias", "Outras Atividades De Serviços Pessoais")))
})

#_________________________________________________________________________________________________
#_________________________________________ NIVEL DIVISAO _________________________________________
#_________________________________________________________________________________________________

test_that("nivel divisao",{
  df_carac_cheio <- data.frame(col1 = c("01", "96"))
  df_carac_semi <- data.frame(col1 = c("1", "96"))
  df_num <- data.frame(col1 = c(1, 96))

  # Caractere cheio
  expect_equal(nomes_cnae(df_carac_cheio, "col1", nivel = "divisão"),
               data.frame(col1 = df_carac_cheio, nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados", "Outras Atividades De Serviços Pessoais")))

  # Caractere semi
  expect_equal(nomes_cnae(df_carac_semi, "col1", nivel = "divisão"),
               data.frame(col1 = df_carac_semi, nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados", "Outras Atividades De Serviços Pessoais")))

  # Numero
  expect_equal(nomes_cnae(df_num, "col1", nivel = "divisão"),
               data.frame(col1 = df_num, nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados", "Outras Atividades De Serviços Pessoais")))
})


#_________________________________________________________________________________________________
#__________________________________________ NIVEL SECAO __________________________________________
#_________________________________________________________________________________________________

test_that("nivel secao",{
  df_carac <- data.frame(col1 = c("A", "S"))

  # Caractere
  expect_equal(nomes_cnae(df_carac, "col1", nivel = "seção"),
               data.frame(col1 = df_carac, nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")))

  #erro
  expect_error(nomes_cnae(df_carac_cheio, "col1", nivel = "errado"),
               "Por favor, escolha um n\u00edvel CNAE v\u00e1lido: subclasse, classe, grupo, divis\u00e3o ou se\u00e7\u00e3o")
  })

