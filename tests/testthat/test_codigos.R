

test_that("codigos_municipio_uf", {
  df_cod_mun_carac_7 <- data.frame(col1 = c("5219704", "3123858", "2111532"))
  df_cod_mun_carac_6 <- data.frame(col1 = c("521970", "312385", "211153"))
  df_cod_mun_num_7 <- data.frame(col1 = c(5219704, 3123858, 2111532))
  df_cod_mun_num_6 <- data.frame(col1 = c(521970, 312385, 211153))

  # 7 caracteres
  expect_equal(
    codigos_municipio_uf(df_cod_mun_carac_7, "col1"),
    data.frame(
      col1 = c("5219704", "3123858", "2111532"),
      codigo_uf = c("52", "31", "21")
    )
  )

  # 6 caracteres
  expect_equal(
    codigos_municipio_uf(df_cod_mun_carac_6, "col1"),
    data.frame(
      col1 = c("521970", "312385", "211153"),
      codigo_uf = c("52", "31", "21")
    )
  )

  # 7 números
  expect_equal(
    codigos_municipio_uf(df_cod_mun_num_7, "col1"),
    data.frame(
      col1 = c(5219704, 3123858, 2111532),
      codigo_uf = c("52", "31", "21")
    )
  )

  # 6 números
  expect_equal(
    codigos_municipio_uf(df_cod_mun_num_6, "col1"),
    data.frame(
      col1 = c(521970, 312385, 211153),
      codigo_uf = c("52", "31", "21")
    )
  )

  # caracteres com nomes
  expect_equal(
    codigos_municipio_uf(df_cod_mun_carac_7, "col1", add_nomes = T),
    data.frame(
      col1 = c("5219704", "3123858", "2111532"),
      codigo_uf = c("52", "31", "21"),
      nome_uf = c("Goiás", "Minas Gerais", "Maranhão")
    )
  )

  # números com nomes
  expect_equal(
    codigos_municipio_uf(df_cod_mun_num_7, "col1", add_nomes = T),
    data.frame(
      col1 = c(5219704, 3123858, 2111532),
      codigo_uf = c("52", "31", "21"),
      nome_uf = c("Goiás", "Minas Gerais", "Maranhão")
    )
  )
})

test_that("codigos_cnae_nivel", {
  df_cod_cnae_carac_7 <- data.frame(col1 = c("9609201", "0111302"))
  df_cod_cnae_carac_6 <- data.frame(col1 = c("9609201", "111302"))
  df_cod_cnae_num <- data.frame(col1 = c(9609201, 111302))

  # 7 caracteres
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_carac_7, "col1", "classe"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_classe = c("96092", "01113")
    )
  )

  # 6 caracteres
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_carac_6, "col1", "classe"),
    data.frame(
      col1 = c("9609201", "111302"),
      codigo_cnae_classe = c("96092", "01113")
    )
  )

  # números
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_num, "col1", "classe"),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_classe = c(96092, 1113)
    )
  )

  # grupo
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_carac_7, "col1", "grupo"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_grupo = c("960", "011")
    )
  )

  # divisão
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_carac_7, "col1", "divisão"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_divisao = c("96", "01")
    )
  )

  # seção
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_carac_7, "col1", "seção"),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_secao = c("S", "A")
    )
  )

  # caracteres + nomes
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_carac_7, "col1", "classe", add_nomes = T),
    data.frame(
      col1 = c("9609201", "0111302"),
      codigo_cnae_classe = c("96092", "01113"),
      nomes_cnae_classe = c(
        "Atividades De Serviços Pessoais Não Especificadas Anteriormente",
        "Cultivo De Cereais"
      )
    )
  )

  # números + nomes
  expect_equal(
    codigos_cnae_nivel(df_cod_cnae_num, "col1", "grupo", add_nomes = T),
    data.frame(
      col1 = c(9609201, 111302),
      codigo_cnae_grupo = c(960, 11),
      nomes_cnae_grupo = c(
        "Outras Atividades De Serviços Pessoais",
        "Produção De Lavouras Temporárias"
      )
    )
  )
})
