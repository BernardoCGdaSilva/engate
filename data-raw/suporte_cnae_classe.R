## code to prepare `suporte_cnae_classe` dataset goes here

suporte_cnae_classe <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_classe")
suporte_cnae_classe$cod <- sprintf("%05d", suporte_cnae_classe$cod)

usethis::use_data(suporte_cnae_classe, overwrite = TRUE)
