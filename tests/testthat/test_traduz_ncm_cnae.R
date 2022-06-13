
test_that("input caractere", {
  df_carac_8 <- data.frame(col1 = c("01012100", "35052000", "84459030"))
  df_carac_7 <- data.frame(col1 = c("1012100", "35052000", "84459030"))


  # 8 caracteres
  expect_equal(
    traduz_ncm_cnae(df_carac_8, "col1"),
    data.frame(
      col1 = c("01012100", "35052000", "84459030"),
      cnae_classe = c("01521", "20916", "28631")
    )
  )

  # 7 caracteres
  expect_equal(
    traduz_ncm_cnae(df_carac_7, "col1"),
    data.frame(
      col1 = c("1012100", "35052000", "84459030"),
      cnae_classe = c("01521", "20916", "28631")
    )
  )

  # 8 caracteres com nomes
  expect_equal(
    traduz_ncm_cnae(df_carac_8, "col1", add_nomes = T),
    data.frame(
      col1 = c("01012100", "35052000", "84459030"),
      cnae_classe = c("01521", "20916", "28631"),
      nome_cnae_classe = c(
        "Criação De Outros Animais De Grande Porte",
        "Fabricação De Adesivos E Selantes",
        "Fabricação De Máquinas E Equipamentos Para A Indústria Têxtil"
      )
    )
  )

  # 7 caracteres com nomes
  expect_equal(
    traduz_ncm_cnae(df_carac_7, "col1", add_nomes = T),
    data.frame(
      col1 = c("1012100", "35052000", "84459030"),
      cnae_classe = c("01521", "20916", "28631"),
      nome_cnae_classe = c(
        "Criação De Outros Animais De Grande Porte",
        "Fabricação De Adesivos E Selantes",
        "Fabricação De Máquinas E Equipamentos Para A Indústria Têxtil"
      )
    )
  )
})

test_that("input numero", {
  df_num_8 <- data.frame(col1 = c(01012100, 35052000, 84459030))
  df_num_7 <- data.frame(col1 = c(1012100, 35052000, 84459030))

  # 8 numeros
  expect_equal(
    traduz_ncm_cnae(df_num_8, "col1"),
    data.frame(
      col1 = c(1012100, 35052000, 84459030),
      cnae_classe = c(1521, 20916, 28631)
    )
  )

  # 7 numeros
  expect_equal(
    traduz_ncm_cnae(df_num_7, "col1"),
    data.frame(
      col1 = c(1012100, 35052000, 84459030),
      cnae_classe = c(1521, 20916, 28631)
    )
  )

  # 8 numeros com nomes
  expect_equal(
    traduz_ncm_cnae(df_num_8, "col1", add_nomes = T),
    data.frame(
      col1 = c(1012100, 35052000, 84459030),
      cnae_classe = c(1521, 20916, 28631),
      nome_cnae_classe = c(
        "Criação De Outros Animais De Grande Porte",
        "Fabricação De Adesivos E Selantes",
        "Fabricação De Máquinas E Equipamentos Para A Indústria Têxtil"
      )
    )
  )

  # 7 numeros com nomes
  expect_equal(
    traduz_ncm_cnae(df_num_7, "col1", add_nomes = T),
    data.frame(
      col1 = c(1012100, 35052000, 84459030),
      cnae_classe = c(1521, 20916, 28631),
      nome_cnae_classe = c(
        "Criação De Outros Animais De Grande Porte",
        "Fabricação De Adesivos E Selantes",
        "Fabricação De Máquinas E Equipamentos Para A Indústria Têxtil"
      )
    )
  )
})
