
test_that("Input caractere", {
  df_carac_5 <- data.frame(col1 = c("01113", "45200", "99008"))
  df_carac_4 <- data.frame(col1 = c("1113", "45200", "99008"))

  expect_equal(
    nomes_cnae_classe(df_carac_5, "col1"),
    data.frame(
      col1 = c("01113", "45200", "99008"),
      nomes_cnae_classe = c(
        "Cultivo De Cereais",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )

  expect_equal(
    nomes_cnae_classe(df_carac_4, "col1"),
    data.frame(
      col1 = c("1113", "45200", "99008"),
      nomes_cnae_classe = c(
        "Cultivo De Cereais",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )
})

test_that("Input numero", {
  df_num_5 <- data.frame(col1 = c(01113, 45200, 99008))
  df_num_4 <- data.frame(col1 = c(1113, 45200, 99008))

  expect_equal(
    nomes_cnae_classe(df_num_5, "col1"),
    data.frame(
      col1 = c(1113, 45200, 99008),
      nomes_cnae_classe = c(
        "Cultivo De Cereais",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )

  expect_equal(
    nomes_cnae_classe(df_num_4, "col1"),
    data.frame(
      col1 = c(1113, 45200, 99008),
      nomes_cnae_classe = c(
        "Cultivo De Cereais",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )
})
