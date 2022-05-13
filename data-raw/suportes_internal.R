## code to prepare `suportes_internal` dataset goes here

# suporte cbo
suporte_cbo <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CBO")
suporte_cbo$cod <- sprintf("%06d", suporte_cbo$cod)

# suporte cnae subclasse
suporte_cnae_subclasse <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_subclasse")
suporte_cnae_subclasse$cod <- sprintf("%07d", suporte_cnae_subclasse$cod)

# suporte cnae classe
suporte_cnae_classe <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_classe")
suporte_cnae_classe$cod <- sprintf("%05d", suporte_cnae_classe$cod)

# suporte cnae grupo
suporte_cnae_grupo <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_grupo")
suporte_cnae_grupo$cod <- sprintf("%03d", suporte_cnae_grupo$cod)

# suporte cnae divisão
suporte_cnae_divisao <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_divisao")
suporte_cnae_divisao$cod <- sprintf("%02d", suporte_cnae_divisao$cod)

# suporte cnae seção
suporte_cnae_secao <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("text", "text"), sheet = "CNAE_secao")

# tradutor cnae seção-divisão
tradutor_cnae_secao_divisao <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "text"), sheet = "CNAE_secao_divisao")
tradutor_cnae_secao_divisao$cnae_divisao <- sprintf("%02d", tradutor_cnae_secao_divisao$cnae_divisao)

# suporte municípios
suporte_municipios <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("text", "text"), sheet = "Municipios")
suporte_municipios$cod <- substr(suporte_municipios$cod, 1, 6)

# suporte uf
suporte_uf <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("text", "text", "text"), sheet = "UFs")

# tradutor ncm-cnae_classe
tradutor_ncm_cnae_classe <- readxl::read_xlsx("data-raw/Suporte.xlsx", col_types = c("numeric", "numeric"), sheet = "NCM_CNAE_classe")
tradutor_ncm_cnae_classe$ncm <- sprintf("%08d", tradutor_ncm_cnae_classe$ncm)
tradutor_ncm_cnae_classe$cnae_classe <- sprintf("%05d", tradutor_ncm_cnae_classe$cnae_classe)

usethis::use_data(suporte_cbo,
  suporte_cnae_subclasse,
  suporte_cnae_classe,
  suporte_cnae_grupo,
  suporte_cnae_divisao,
  suporte_cnae_secao,
  tradutor_cnae_secao_divisao,
  suporte_municipios,
  suporte_uf,
  tradutor_ncm_cnae_classe,
  overwrite = TRUE,
  internal = TRUE
)
