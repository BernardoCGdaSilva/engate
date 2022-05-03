## code to prepare `suporte_cbo` dataset goes here

suporte_cbo <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CBO")
suporte_cbo$cod <- sprintf("%06d", suporte_cbo$cod)

usethis::use_data(suporte_cbo, overwrite = TRUE, internal = TRUE)
