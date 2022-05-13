
test_that("Input caractere", {
  df_carac_7 <- data.frame(col1 = c("0111301", "4520008", "9900800"))
  df_carac_6 <- data.frame(col1 = c("111301", "4520008", "9900800"))

  expect_equal(
    nomes_cnae_subclasse(df_carac_7, "col1"),
    data.frame(
      col1 = c("0111301", "4520008", "9900800"),
      nomes_cnae_subclasse = c(
        "Cultivo de Arroz",
        "Serviços de Capotaria",
        "Organismos Internacionais e Outras Instituições Extraterritoriais"
      )
    )
  )
  expect_equal(
    nomes_cnae_subclasse(df_carac_6, "col1"),
    data.frame(
      col1 = c("111301", "4520008", "9900800"),
      nomes_cnae_subclasse = c(
        "Cultivo de Arroz",
        "Serviços de Capotaria",
        "Organismos Internacionais e Outras Instituições Extraterritoriais"
      )
    )
  )
})

test_that("Input numero", {
  df_num_7 <- data.frame(col1 = c(0111301, 4520008, 9900800))
  df_num_6 <- data.frame(col1 = c(111301, 4520008, 9900800))

  expect_equal(
    nomes_cnae_subclasse(df_num_7, "col1"),
    data.frame(
      col1 = c(111301, 4520008, 9900800),
      nomes_cnae_subclasse = c(
        "Cultivo de Arroz",
        "Serviços de Capotaria",
        "Organismos Internacionais e Outras Instituições Extraterritoriais"
      )
    )
  )
  expect_equal(
    nomes_cnae_subclasse(df_num_6, "col1"),
    data.frame(
      col1 = c(111301, 4520008, 9900800),
      nomes_cnae_subclasse = c(
        "Cultivo de Arroz",
        "Serviços de Capotaria",
        "Organismos Internacionais e Outras Instituições Extraterritoriais"
      )
    )
  )
})
