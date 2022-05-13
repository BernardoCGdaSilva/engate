
test_that("Input caractere", {
  df_carac_2 <- data.frame(col1 = c("01", "45", "99"))
  df_carac_1 <- data.frame(col1 = c("1", "45", "99"))

  expect_equal(
    nomes_cnae_divisao(df_carac_2, "col1"),
    data.frame(
      col1 = c("01", "45", "99"),
      nomes_cnae_divisao = c(
        "Agricultura, Pecuária E Serviços Relacionados",
        "Comércio E Reparação De Veículos Automotores E Motocicletas",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )

  expect_equal(
    nomes_cnae_divisao(df_carac_1, "col1"),
    data.frame(
      col1 = c("1", "45", "99"),
      nomes_cnae_divisao = c(
        "Agricultura, Pecuária E Serviços Relacionados",
        "Comércio E Reparação De Veículos Automotores E Motocicletas",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )
})

test_that("Input numero", {
  df_num_2 <- data.frame(col1 = c(01, 45, 99))
  df_num_1 <- data.frame(col1 = c(1, 45, 99))

  expect_equal(
    nomes_cnae_divisao(df_num_2, "col1"),
    data.frame(
      col1 = c(1, 45, 99),
      nomes_cnae_divisao = c(
        "Agricultura, Pecuária E Serviços Relacionados",
        "Comércio E Reparação De Veículos Automotores E Motocicletas",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )

  expect_equal(
    nomes_cnae_divisao(df_num_1, "col1"),
    data.frame(
      col1 = c(1, 45, 99),
      nomes_cnae_divisao = c(
        "Agricultura, Pecuária E Serviços Relacionados",
        "Comércio E Reparação De Veículos Automotores E Motocicletas",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )
})
