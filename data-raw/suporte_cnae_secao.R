## code to prepare `suporte_cnae_secao` dataset goes here

suporte_cnae_secao <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("text", "text"), sheet = "CNAE_secao")

usethis::use_data(suporte_cnae_secao, overwrite = TRUE)
