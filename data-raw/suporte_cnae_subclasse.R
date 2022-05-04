## code to prepare `suporte_cnae_subclasse` dataset goes here

suporte_cnae_subclasse <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_subclasse")
suporte_cnae_subclasse$cod <- sprintf("%07d", suporte_cnae_subclasse$cod)

usethis::use_data(suporte_cnae_subclasse, overwrite = TRUE)
