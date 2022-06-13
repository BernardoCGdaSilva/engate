
test_that("Input caractere", {
  df_carac <- data.frame(col1 = c("11", "22", "43"), col2 = c("a", "b", "c"))

  # caracteres
  expect_equal(
    nomeia_uf(df_carac, "col1"),
    data.frame(
      col1 = c("11", "22", "43"),
      nome_uf = c(
        "Rondônia",
        "Piauí",
        "Rio Grande do Sul"
      ),
      col2 = c("a", "b", "c")
    )
  )

  # caracteres + nome + sigla
  expect_equal(
    nomeia_uf(df_carac, "col1", sigla = T),
    data.frame(
      col1 = c("11", "22", "43"),
      nome_uf = c(
        "Rondônia",
        "Piauí",
        "Rio Grande do Sul"
      ),
      sigla_uf = c("RO", "PI", "RS"),
      col2 = c("a", "b", "c")
    )
  )

  # caracteres - nome + sigla
  expect_equal(
    nomeia_uf(df_carac, "col1", sigla = T, nome = F),
    data.frame(
      col1 = c("11", "22", "43"),
      sigla_uf = c("RO", "PI", "RS"),
      col2 = c("a", "b", "c")
    )
  )

  # caracteres - nome - sigla
  expect_error(
    nomeia_uf(df_carac, "col1", sigla = F, nome = F),
    "Por favor, defina TRUE em ao menos um argumento nome ou sigla"
  )
})

test_that("Input numero", {
  df_num <- data.frame(col1 = c(11, 22, 43), col2 = c("a", "b", "c"))

  # numeros
  expect_equal(
    nomeia_uf(df_num, "col1"),
    data.frame(
      col1 = c(11, 22, 43),
      nome_uf = c(
        "Rondônia",
        "Piauí",
        "Rio Grande do Sul"
      ),
      col2 = c("a", "b", "c")
    )
  )

  # numeros + nome + sigla
  expect_equal(
    nomeia_uf(df_num, "col1", sigla = T),
    data.frame(
      col1 = c(11, 22, 43),
      nome_uf = c(
        "Rondônia",
        "Piauí",
        "Rio Grande do Sul"
      ),
      sigla_uf = c("RO", "PI", "RS"),
      col2 = c("a", "b", "c")
    )
  )

  # numeros - nome + sigla
  expect_equal(
    nomeia_uf(df_num, "col1", sigla = T, nome = F),
    data.frame(
      col1 = c(11, 22, 43),
      sigla_uf = c("RO", "PI", "RS"),
      col2 = c("a", "b", "c")
    )
  )

  # numeros - nome - sigla
  expect_error(
    nomeia_uf(df_num, "col1", sigla = F, nome = F),
    "Por favor, defina TRUE em ao menos um argumento nome ou sigla"
  )
})
