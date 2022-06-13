
test_that("Input caractere", {
  df_carac_7 <- data.frame(col1 = c("5219704", "3123858", "2111532"))
  df_carac_6 <- data.frame(col1 = c("521970", "312385", "211153"))

  expect_equal(
    nomeia_municipio(df_carac_7, "col1"),
    data.frame(
      col1 = c("5219704", "3123858", "2111532"),
      nome_municipio = c(
        "Santa Terezinha de Goiás",
        "Entre Folhas",
        "São Pedro da Água Branca"
      )
    )
  )

  expect_equal(
    nomeia_municipio(df_carac_6, "col1"),
    data.frame(
      col1 = c("521970", "312385", "211153"),
      nome_municipio = c(
        "Santa Terezinha de Goiás",
        "Entre Folhas",
        "São Pedro da Água Branca"
      )
    )
  )
})

test_that("Input numero", {
  df_num_7 <- data.frame(col1 = c(5219704, 3123858, 2111532))
  df_num_6 <- data.frame(col1 = c(521970, 312385, 211153))

  expect_equal(
    nomeia_municipio(df_num_7, "col1"),
    data.frame(
      col1 = c(5219704, 3123858, 2111532),
      nome_municipio = c(
        "Santa Terezinha de Goiás",
        "Entre Folhas",
        "São Pedro da Água Branca"
      )
    )
  )

  expect_equal(
    nomeia_municipio(df_num_6, "col1"),
    data.frame(
      col1 = c(521970, 312385, 211153),
      nome_municipio = c(
        "Santa Terezinha de Goiás",
        "Entre Folhas",
        "São Pedro da Água Branca"
      )
    )
  )
})
