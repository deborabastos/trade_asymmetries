# Download dos dados de comércio exterior do Brasil de
# https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

## MELHORIA identificar país pelo nome em vez do código https://balanca.economia.gov.br/balanca/bd/tabelas/TABELAS_AUXILIARES.xlsx

library(httr)
library(dplyr)
library(readr)

get_data_br <- function(
  year_ini,
  year_end,
  type = c("EXP", "IMP"),
  cnty_cod,
  del_parts = FALSE,
  overwrite = FALSE
) {
  type <- match.arg(type)
  years <- year_ini:year_end
  url_base <- "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/"

  if (!dir.exists("data")) {
    dir.create("data")
  }

  # Download dos arquivos
  for (year in years) {
    file_name <- paste0(type, "_", year, ".csv")
    url <- paste0(url_base, file_name)
    folder <- file.path("data", file_name)
    if (!file.exists(folder) || overwrite == TRUE) {
      resp <- GET(url, write_disk(folder, overwrite = overwrite))
      cat("File downloaded:", folder, "\n")
    }
  }

  # Lista arquivos baixados
  files_list <- list.files(
    "data",
    pattern = paste0("^", type, "_\\d{4}\\.csv$"),
    full.names = TRUE
  )

  # Filtra dados de interesse
  compiled_file = paste0(
    "data/",
    type,
    "_brasil_",
    cnty_cod,
    "_",
    year_ini,
    "_",
    year_end,
    ".csv"
  )

  if (!file.exists(compiled_file) || overwrite == TRUE) {
    # Lê, filtra e empilha os dados
    dados_filtrados <- files_list %>%
      lapply(function(arquivo) {
        df <- read_csv2(arquivo, show_col_types = FALSE)
        filter(df, grepl(cnty_cod, CO_PAIS, ignore.case = TRUE))
      }) %>%
      bind_rows()

    # Salva o arquivo compilado
    write_csv2(dados_filtrados, compiled_file)
  }

  # Apaga files_list individuais se solicitado
  if (del_parts) {
    file.remove(files_list)
    cat("Arquivos individuais removidos.\n")
  }
}
