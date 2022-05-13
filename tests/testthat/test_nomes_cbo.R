
test_that("Input caractere", {
  df_carac_6 <- data.frame(col1 = c("010105", "515220", "992115"))
  df_carac_5 <- data.frame(col1 = c("10105", "515220", "992115"))

  expect_equal(
    nomes_cbo(df_carac_6, "col1"),
    data.frame(
      col1 = c("010105", "515220", "992115"),
      nomes_cbo = c(
        "Oficial General da Aeronautica",
        "Auxiliar de Laboratorio de Imunobiologicos",
        "Borracheiro"
      )
    )
  )

  expect_equal(
    nomes_cbo(df_carac_5, "col1"),
    data.frame(
      col1 = c("10105", "515220", "992115"),
      nomes_cbo = c(
        "Oficial General da Aeronautica",
        "Auxiliar de Laboratorio de Imunobiologicos",
        "Borracheiro"
      )
    )
  )
})

test_that("Input numero", {
  df_num_6 <- data.frame(col1 = c(010105, 515220, 992115))
  df_num_5 <- data.frame(col1 = c(10105, 515220, 992115))

  expect_equal(
    nomes_cbo(df_num_6, "col1"),
    data.frame(
      col1 = c(10105, 515220, 992115),
      nomes_cbo = c(
        "Oficial General da Aeronautica",
        "Auxiliar de Laboratorio de Imunobiologicos",
        "Borracheiro"
      )
    )
  )

  expect_equal(
    nomes_cbo(df_num_5, "col1"),
    data.frame(
      col1 = c(10105, 515220, 992115),
      nomes_cbo = c(
        "Oficial General da Aeronautica",
        "Auxiliar de Laboratorio de Imunobiologicos",
        "Borracheiro"
      )
    )
  )
})
