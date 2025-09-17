library(tidyverse)
library(tidyr)
library(scales)
library(readxl)

setwd("C:/Users/debora.bastos/OneDrive - mtegovbr/Documentos/trade_asymmetries")

source("./R/get_data_br.R")


################## DADOS BRASILEIROS DE EXPORTAÇÃO BRASIL - EUA ##################

# Baixar dados do Brasil
get_data_br(2015, 2024, type = "EXP", cnty_cod = "249")
get_data_br(2015, 2024, type = "IMP", cnty_cod = "249")


#Limpar ambiente
rm(list = ls())

# Salva dados em tabela
exp_br_eua <- readr::read_csv2("data/EXP_brasil_249_2015_2024.csv", show_col_types = FALSE)

# Visualizar resultado
head(exp_br_eua)

# Total das exportações do Brasil para os EUA por ano
exp_br_eua_total_ano <- exp_br_eua %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum(VL_FOB, na.rm = TRUE), .groups = "drop")

# Definindo variável ANO como número inteiro
exp_br_eua_total_ano$CO_ANO = as.integer(exp_br_eua_total_ano$CO_ANO)

# Visualizar resultado
exp_br_eua_total_ano


################## DADOS AMERICANOS DE IMPORTAÇÃO EUA - BR ##################
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

imp_eua_br_hts

imp_eua_br_ano <- imp_eua_br_hts %>%
  group_by(ANO) %>%
  summarise(TOTAL_ANO = sum(VALOR, na.rm = TRUE), .groups = "drop")

# Definindo variável ANO como número inteiro
imp_eua_br_ano$ANO = as.integer(imp_eua_br_ano$ANO)

imp_eua_br_ano


# colocando dados em dataframe único
exp_br_imp_usa <- data.frame(
  x = exp_br_eua_total_ano$CO_ANO,
  exp_br = exp_br_eua_total_ano$TOTAL_ANO,
  imp_usa = imp_eua_br_ano$TOTAL_ANO,
)

# Renomear a coluna x para ano
exp_br_imp_usa <- exp_br_imp_usa %>%
  rename(ano = x)


exp_br_imp_usa

#Plota gráfico
ggplot(exp_br_imp_usa, mapping = aes(x = ano)) +
  geom_line(aes(y = exp_br/1000000000, color = "ExpBR")) +
  geom_line(aes(y = imp_usa/1000000000, color = "ImpUSA")) +
  scale_x_continuous(breaks = exp_br_imp_usa$ano, labels = as.integer(exp_br_imp_usa$ano)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Exportações Brasil vs. Importações EUA",
    x = "Anos",
    y = "Valores (bilhões)",
    color = ""
  )