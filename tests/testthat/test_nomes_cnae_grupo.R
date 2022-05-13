
test_that("Input caractere", {
  df_carac_3 <- data.frame(col1 = c("011", "452", "990"))
  df_carac_2 <- data.frame(col1 = c("11", "452", "990"))

  expect_equal(
    nomes_cnae_grupo(df_carac_3, "col1"),
    data.frame(
      col1 = c("011", "452", "990"),
      nomes_cnae_grupo = c(
        "Produção De Lavouras Temporárias",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )

  expect_equal(
    nomes_cnae_grupo(df_carac_2, "col1"),
    data.frame(
      col1 = c("11", "452", "990"),
      nomes_cnae_grupo = c(
        "Produção De Lavouras Temporárias",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )
})

test_that("Input numero", {
  df_num_3 <- data.frame(col1 = c(011, 452, 990))
  df_num_2 <- data.frame(col1 = c(11, 452, 990))

  expect_equal(
    nomes_cnae_grupo(df_num_3, "col1"),
    data.frame(
      col1 = c(11, 452, 990),
      nomes_cnae_grupo = c(
        "Produção De Lavouras Temporárias",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )

  expect_equal(
    nomes_cnae_grupo(df_num_2, "col1"),
    data.frame(
      col1 = c(11, 452, 990),
      nomes_cnae_grupo = c(
        "Produção De Lavouras Temporárias",
        "Manutenção E Reparação De Veículos Automotores",
        "Organismos Internacionais E Outras Instituições Extraterritoriais"
      )
    )
  )
})
