
test_that("input caracter", {
  df_carac_7 <- data.frame(col1 = c("5219704", "3123858", "2111532"))
  df_carac_6 <- data.frame(col1 = c("521970", "312385", "211153"))

  # 7 caracteres
  expect_equal(
    codigos_municipio_uf(df_carac_7, "col1"),
    data.frame(
      col1 = c("5219704", "3123858", "2111532"),
      codigo_uf = c("52", "31", "21")
    )
  )

  # 6 caracteres
  expect_equal(
    codigos_municipio_uf(df_carac_6, "col1"),
    data.frame(
      col1 = c("521970", "312385", "211153"),
      codigo_uf = c("52", "31", "21")
    )
  )

  # 7 caracteres com nomes
  expect_equal(
    codigos_municipio_uf(df_carac_7, "col1", add_nomes = T),
    data.frame(
      col1 = c("5219704", "3123858", "2111532"),
      codigo_uf = c("52", "31", "21"),
      nome_uf = c("Goiás", "Minas Gerais", "Maranhão")
    )
  )

  # 6 caracteres com nomes
  expect_equal(
    codigos_municipio_uf(df_carac_6, "col1", add_nomes = T),
    data.frame(
      col1 = c("521970", "312385", "211153"),
      codigo_uf = c("52", "31", "21"),
      nome_uf = c("Goiás", "Minas Gerais", "Maranhão")
    )
  )
})

test_that("input numero", {
  df_num_7 <- data.frame(col1 = c(5219704, 3123858, 2111532))
  df_num_6 <- data.frame(col1 = c(521970, 312385, 211153))

  # 7 números
  expect_equal(
    codigos_municipio_uf(df_num_7, "col1"),
    data.frame(
      col1 = c(5219704, 3123858, 2111532),
      codigo_uf = c(52, 31, 21)
    )
  )

  # 6 números
  expect_equal(
    codigos_municipio_uf(df_num_6, "col1"),
    data.frame(
      col1 = c(521970, 312385, 211153),
      codigo_uf = c(52, 31, 21)
    )
  )
  # 7 números com nomes
  expect_equal(
    codigos_municipio_uf(df_num_7, "col1", add_nomes = T),
    data.frame(
      col1 = c(5219704, 3123858, 2111532),
      codigo_uf = c(52, 31, 21),
      nome_uf = c("Goiás", "Minas Gerais", "Maranhão")
    )
  )
  # 6 números com nomes
  expect_equal(
    codigos_municipio_uf(df_num_6, "col1", add_nomes = T),
    data.frame(
      col1 = c(521970, 312385, 211153),
      codigo_uf = c(52, 31, 21),
      nome_uf = c("Goiás", "Minas Gerais", "Maranhão")
    )
  )
})
