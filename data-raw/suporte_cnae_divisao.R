## code to prepare `suporte_cnae_divisao` dataset goes here

suporte_cnae_divisao <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_divisao")
suporte_cnae_divisao$cod <- sprintf("%02d", suporte_cnae_divisao$cod)

usethis::use_data(suporte_cnae_divisao, overwrite = TRUE)
