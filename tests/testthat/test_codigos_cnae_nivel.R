
# _________________________________________________________________________________________________
# _________________________________________INPUT SUBCLASSE_________________________________________
# _________________________________________________________________________________________________


test_that("input subclasse", {
  df_carac_cheio <- data.frame(col1 = c("0111302", "9609201"))
  df_carac_semi <- data.frame(col1 = c("111302", "9609201"))
  df_num <- data.frame(col1 = c(111302, 9609201))

  # Input caracteres cheio
  expect_equal(
    codigos_cnae_nivel(df_carac_cheio, "col1", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_cheio,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input caracteres semi
  expect_equal(
    codigos_cnae_nivel(df_carac_semi, "col1", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_semi,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input numeros
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Output divisao
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_output = "divisão", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_divisao = c(1, 96),
      nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados", "Outras Atividades De Serviços Pessoais")
    )
  )

  # Output grupo
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_output = "grupo", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_grupo = c(11, 960),
      nomes_cnae_grupo = c("Produção De Lavouras Temporárias", "Outras Atividades De Serviços Pessoais")
    )
  )

  # Output classe
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_output = "classe", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_classe = c(1113, 96092),
      nomes_cnae_classe = c("Cultivo De Cereais", "Atividades De Serviços Pessoais Não Especificadas Anteriormente")
    )
  )

  # erro de output
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_output = "subclasse", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE acima de subclasse para output."
  )

  # erro de input
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "seção", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE abaixo de se\u00e7\u00e3o para input"
  )
})

# _________________________________________________________________________________________________
# __________________________________________INPUT CLASSE___________________________________________
# _________________________________________________________________________________________________

test_that("input classe", {
  df_carac_cheio <- data.frame(col1 = c("01113", "96092"))
  df_carac_semi <- data.frame(col1 = c("1113", "96092"))
  df_num <- data.frame(col1 = c(1113, 96092))

  # Input caracteres cheio
  expect_equal(
    codigos_cnae_nivel(df_carac_cheio, "col1", nivel_input = "classe", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_cheio,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input caracteres semi
  expect_equal(
    codigos_cnae_nivel(df_carac_semi, "col1", nivel_input = "classe", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_semi,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input numeros
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "classe", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Output divisao
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "classe", nivel_output = "divisão", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_divisao = c(1, 96),
      nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados", "Outras Atividades De Serviços Pessoais")
    )
  )

  # Output grupo
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "classe", nivel_output = "grupo", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_grupo = c(11, 960),
      nomes_cnae_grupo = c("Produção De Lavouras Temporárias", "Outras Atividades De Serviços Pessoais")
    )
  )

  # Output classe
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "classe", nivel_output = "classe", add_nomes = TRUE),
    "Por favor, para input classe, escolha como output grupo, divis\u00e3o ou se\u00e7\u00e3o"
  )

  # erro de output
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "classe", nivel_output = "subclasse", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE acima de subclasse para output."
  )

  # erro de input
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "seção", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE abaixo de se\u00e7\u00e3o para input"
  )
})

# _________________________________________________________________________________________________
# ___________________________________________INPUT GRUPO___________________________________________
# _________________________________________________________________________________________________

test_that("input grupo", {
  df_carac_cheio <- data.frame(col1 = c("011", "960"))
  df_carac_semi <- data.frame(col1 = c("11", "960"))
  df_num <- data.frame(col1 = c(11, 960))

  # Input caracteres cheio
  expect_equal(
    codigos_cnae_nivel(df_carac_cheio, "col1", nivel_input = "grupo", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_cheio,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input caracteres semi
  expect_equal(
    codigos_cnae_nivel(df_carac_semi, "col1", nivel_input = "grupo", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_semi,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input numeros
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "grupo", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Output divisao
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "grupo", nivel_output = "divisão", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_divisao = c(1, 96),
      nomes_cnae_divisao = c("Agricultura, Pecuária E Serviços Relacionados", "Outras Atividades De Serviços Pessoais")
    )
  )

  # Output grupo
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "grupo", nivel_output = "grupo", add_nomes = TRUE),
    "Por favor, para input grupo, escolha como output divis\u00e3o ou se\u00e7\u00e3o"
  )

  # Output classe
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "grupo", nivel_output = "classe", add_nomes = TRUE),
    "Por favor, para input grupo, escolha como output divis\u00e3o ou se\u00e7\u00e3o"
  )

  # erro de output
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "grupo", nivel_output = "subclasse", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE acima de subclasse para output."
  )

  # erro de input
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "seção", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE abaixo de se\u00e7\u00e3o para input"
  )
})


# _________________________________________________________________________________________________
# __________________________________________INPUT DIVISÃO__________________________________________
# _________________________________________________________________________________________________

test_that("input divisao", {
  df_carac_cheio <- data.frame(col1 = c("01", "96"))
  df_carac_semi <- data.frame(col1 = c("1", "96"))
  df_num <- data.frame(col1 = c(1, 96))

  # Input caracteres cheio
  expect_equal(
    codigos_cnae_nivel(df_carac_cheio, "col1", nivel_input = "divisão", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_cheio,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input caracteres semi
  expect_equal(
    codigos_cnae_nivel(df_carac_semi, "col1", nivel_input = "divisão", add_nomes = TRUE),
    data.frame(
      col1 = df_carac_semi,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Input numeros
  expect_equal(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "divisão", add_nomes = TRUE),
    data.frame(
      col1 = df_num,
      codigo_cnae_secao = c("A", "S"),
      nomes_cnae_secao = c("Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura", "Outras Atividades de Serviços")
    )
  )

  # Output divisao
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "divisão", nivel_output = "divisão", add_nomes = TRUE),
    "Por favor, para input divis\u00e3o, escolha como output se\u00e7\u00e3o"
  )

  # Output grupo
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "divisão", nivel_output = "grupo", add_nomes = TRUE),
    "Por favor, para input divis\u00e3o, escolha como output se\u00e7\u00e3o"
  )

  # Output classe
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "divisão", nivel_output = "classe", add_nomes = TRUE),
    "Por favor, para input divis\u00e3o, escolha como output se\u00e7\u00e3o"
  )

  # erro de output
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "divisão", nivel_output = "subclasse", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE acima de subclasse para output."
  )

  # erro de input
  expect_error(
    codigos_cnae_nivel(df_num, "col1", nivel_input = "seção", add_nomes = TRUE),
    "Por favor, escolha um n\u00edvel CNAE abaixo de se\u00e7\u00e3o para input"
  )
})
