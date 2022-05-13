
test_that("input caractere", {
  df_carac_7 <- data.frame(col1 = c("9609201", "0111302"))
  df_carac_6 <- data.frame(col1 = c("9609201", "111302"))

  # 7 caracteres classe
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "classe"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_classe = c("96092", "01113")
    )
  )

  # 6 caracteres classe
  expect_equal(
    codigos_cnae_nivel(df_carac_6, "col1", "classe"),
    data.frame(
      col1 = c("9609201", "111302"),
      codigo_cnae_classe = c("96092", "01113")
    )
  )

  # 7 caracteres grupo
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "grupo"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_grupo = c("960", "011")
    )
  )

  # 6 caracteres grupo
  expect_equal(
    codigos_cnae_nivel(df_carac_6, "col1", "grupo"),
    data.frame(
      col1 = c("9609201", "111302"),
      codigo_cnae_grupo = c("960", "011")
    )
  )

  # 7 caracteres divisão
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "divisão"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_divisao = c("96", "01")
    )
  )

  # 6 caracteres divisão
  expect_equal(
    codigos_cnae_nivel(df_carac_6, "col1", "divisão"),
    data.frame(
      col1 = c("9609201", "111302"),
      codigo_cnae_divisao = c("96", "01")
    )
  )

  # 7 caracteres seção
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "seção"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_secao = c("S", "A")
    )
  )

  # 6 caracteres seção
  expect_equal(
    codigos_cnae_nivel(df_carac_6, "col1", "seção"),
    data.frame(
      col1 = c("9609201", "111302"),
      codigo_cnae_secao = c("S", "A")
    )
  )

  # caracteres + nomes classe
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "classe", add_nomes = T),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_classe = c("96092", "01113"),
      nomes_cnae_classe = c(
        "Atividades De Serviços Pessoais Não Especificadas Anteriormente",
        "Cultivo De Cereais"
      )
    )
  )

  # caracteres + nomes grupo
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "grupo", add_nomes = T),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_grupo = c("960", "011"),
      nomes_cnae_grupo = c(
        "Outras Atividades De Serviços Pessoais",
        "Produção De Lavouras Temporárias"
      )
    )
  )

  # caracteres + nomes divisão
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "divisão", add_nomes = T),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_divisao = c("96", "01"),
      nomes_cnae_divisao = c(
        "Outras Atividades De Serviços Pessoais",
        "Agricultura, Pecuária E Serviços Relacionados"
      )
    )
  )

  # caracteres + nomes seção
  expect_equal(
    codigos_cnae_nivel(df_carac_7, "col1", "seção", add_nomes = T),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_secao = c("S", "A"),
      nomes_cnae_secao = c(
        "Outras Atividades de Serviços",
        "Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura"
      )
    )
  )

  # nivel errado
  expect_error(
    codigos_cnae_nivel(df_carac_7, "col1", "errado", add_nomes = T),
    "Por favor, escolha um nivel CNAE acima de subclasse."
  )
})

test_that("input numero", {
  df_num_7 <- data.frame(col1 = c(9609201, 0111302))
  df_num_6 <- data.frame(col1 = c(9609201, 111302))

  # 7 numeros classe
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "classe"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_classe = c(96092, 1113)
    )
  )

  # 6 numeros classe
  expect_equal(
    codigos_cnae_nivel(df_num_6, "col1", "classe"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_classe = c(96092, 1113)
    )
  )

  # 7 numeros grupo
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "grupo"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_grupo = c(960, 11)
    )
  )

  # 6 numeros grupo
  expect_equal(
    codigos_cnae_nivel(df_num_6, "col1", "grupo"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_grupo = c(960, 11)
    )
  )

  # 7 numeros divisão
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "divisão"),
    data.frame(
      col1 = c(9609201, 0111302),
      codigo_cnae_divisao = c(96, 1)
    )
  )

  # 6 numeros divisão
  expect_equal(
    codigos_cnae_nivel(df_num_6, "col1", "divisão"),
    data.frame(
      col1 = c(9609201, 0111302),
      codigo_cnae_divisao = c(96, 1)
    )
  )

  # 7 numeros seção
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "seção"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_secao = c("S", "A")
    )
  )

  # 6 numeros seção
  expect_equal(
    codigos_cnae_nivel(df_num_6, "col1", "seção"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_secao = c("S", "A")
    )
  )

  # numeros + nomes classe
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "classe", add_nomes = T),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_classe = c(96092, 1113),
      nomes_cnae_classe = c(
        "Atividades De Serviços Pessoais Não Especificadas Anteriormente",
        "Cultivo De Cereais"
      )
    )
  )

  # numeros + nomes grupo
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "grupo", add_nomes = T),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_grupo = c(960, 11),
      nomes_cnae_grupo = c(
        "Outras Atividades De Serviços Pessoais",
        "Produção De Lavouras Temporárias"
      )
    )
  )

  # numeros + nomes divisão
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "divisão", add_nomes = T),
    data.frame(
      col1 = c(9609201, 0111302),
      codigo_cnae_divisao = c(96, 1),
      nomes_cnae_divisao = c(
        "Outras Atividades De Serviços Pessoais",
        "Agricultura, Pecuária E Serviços Relacionados"
      )
    )
  )

  # numeros + nomes seção
  expect_equal(
    codigos_cnae_nivel(df_num_7, "col1", "seção", add_nomes = T),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_secao = c("S", "A"),
      nomes_cnae_secao = c(
        "Outras Atividades de Serviços",
        "Agricultura, Pecuária, Produção Florestal, Pesca e AqÜIcultura"
      )
    )
  )

  # nivel errado
  expect_error(
    codigos_cnae_nivel(df_num_7, "col1", "errado", add_nomes = T),
    "Por favor, escolha um nivel CNAE acima de subclasse."
  )
})
