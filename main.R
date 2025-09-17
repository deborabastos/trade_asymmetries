library(tidyverse)
library(tidyr)
library(readxl)

setwd("C:/Users/debora.bastos/OneDrive - mtegovbr/Documentos/trade_asymmetries")

source("./R/get_data_br.R")


###################### DADOS BRASILEIROS DE EXPORTAÇÃO BRASIL - EUA #######################

# Baixar dados do Brasil
get_data_br(2015, 2024, type = "EXP", cnty_cod = "249")
get_data_br(2015, 2024, type = "IMP", cnty_cod = "249")

exp_br_eua <- readr::read_csv2(
  "data/EXP_brasil_249_2015_2024.csv",
  show_col_types = FALSE
)

# Visualizar resultado
head(exp_br_eua)

# Supondo que exp_br_eua tenha as colunas CO_ANO, CO_NCM e VL_FOB (ou similar)
exp_br_eua_ano_ncm <- exp_br_eua %>%
  group_by(CO_ANO, CO_NCM) %>%
  summarise(TOTAL = sum(VL_FOB, na.rm = TRUE), .groups = "drop")

# Visualizar resultado
head(exp_br_eua_ano_ncm)


# Total por ano (somando todos os NCM)
exp_br_eua_total_ano <- exp_br_eua_ano_ncm %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum(TOTAL, na.rm = TRUE), .groups = "drop")

# Visualizar resultado
print(exp_br_eua_total_ano)


############################## DADOS AMERICANOS DE IMPORTAÇÃO EUA - BR ##############################
# Ler o arquivo compilado de importação EUA - BR
imp_eua_br <- readxl::read_excel(
  "data/IMP_eua_br_2015_2024.xlsx",
  sheet = "General Customs Value",
  range = "B3:L4218"
)

# Visualizar resultado
head(imp_eua_br)
tail(imp_eua_br)

# Arruma o dataframe
imp_eua_br_hts <- imp_eua_br %>%
  pivot_longer(
    cols = matches("^\\d{4}$"), # Seleciona colunas de ano
    names_to = "ANO", # Nome da coluna de ano
    values_to = "VALOR" # Nome da coluna de valor
  ) %>%
  rename(HTS = `HTS Number`) # Renomeia coluna HTS

imp_eua_br_hts <- imp_eua_br_hts %>%
  select(ANO, HTS, VALOR)

imp_eua_br_hts <- imp_eua_br_hts %>%
  arrange(ANO)

# Visualizar resultado
head(imp_eua_br_hts)

imp_eua_br_ano <- imp_eua_br_hts %>%
  group_by(ANO) %>%
  summarise(TOTAL_ANO = sum(VALOR, na.rm = TRUE), .groups = "drop")


print(imp_eua_br_ano)


df <- data.frame(
  x = exp_br_eua_total_ano$CO_ANO,
  EXP_BR_EUA = exp_br_eua_total_ano$TOTAL_ANO,
  IMP_EUA_BR = imp_eua_br_ano$TOTAL_ANO
)

df

ggplot(df, mapping = aes(x = x)) +
    geom_line(aes(y = c(EXP_BR_EUA, color = "ExpBR")) +
    geom_line(aes(y = IMP_EUA_BR, color = "ImpUSA"))



