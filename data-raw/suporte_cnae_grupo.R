## code to prepare `suporte_cnae_grupo` dataset goes here

suporte_cnae_grupo <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_grupo")
suporte_cnae_grupo$cod <- sprintf("%03d", suporte_cnae_grupo$cod)

usethis::use_data(suporte_cnae_grupo, overwrite = TRUE)
