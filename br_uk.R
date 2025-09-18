# Load necessary libraries
library(tidyverse)
library(scales)
library(readxl)

# Clear the environment
rm(list = ls())

# Load custom function to get Brazilian trade data
source("./R/get_data_br.R")

# Set working directory 
setwd("C:/Users/debora.bastos/OneDrive - mtegovbr/Documentos/trade_asymmetries")

#-------------------------------------------------------------------------------
## Brazilian Exports to the UK
#-------------------------------------------------------------------------------
# Download and read Brazil's export data
# get_data_br(2015, 2024, type = "EXP", cnty_cod = "628")
exp_br_uk <- read_csv2("data/EXP_brasil_628_2015_2024.csv", show_col_types = FALSE)

# Summarize Brazil's total exports to the UK by year
exp_br_uk_ano <- exp_br_uk %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum(VL_FOB, na.rm = TRUE)) %>%
  rename(ano = CO_ANO, exp_br = TOTAL_ANO)

exp_br_uk_ano

#-------------------------------------------------------------------------------
## UK Imports from Brazil
#-------------------------------------------------------------------------------
# Read UK import data
imp_uk_br <- read_excel(
  "data/uk_br_imp_exp.xlsx",
)

imp_uk_br

# Tidy the UK import data and summarize by year
imp_uk_br_ano <- imp_uk_br %>%
  filter(flowDesc == "Import") %>%
  select(refYear, primaryValue) %>%
  rename(ano = refYear, imp_uk = primaryValue) %>%
  mutate(ano = as.integer(ano))

imp_uk_br_ano

#-------------------------------------------------------------------------------
## Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
exp_br_imp_uk <- full_join(exp_br_uk_ano, imp_uk_br_ano, by = "ano")

exp_br_imp_uk

# Convert to a "long" format for easier plotting with ggplot2
exp_br_imp_uk_long <- exp_br_imp_uk %>%
  pivot_longer(
    cols = c(exp_br, imp_uk),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

exp_br_imp_uk_long

# Create the plot
ggplot(exp_br_imp_uk_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("exp_br" = "#1f78b4", "imp_uk" = "#e31a1c"),
    labels = c("Exportações do Brasil (para os UK)", "Importações dos UK (do Brasil)")
  ) +
  scale_x_continuous(breaks = unique(exp_br_imp_uk_long$ano)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")) +
  labs(
    title = "Asimetrias de Comércio: Brasil vs. UK (2015-2024)",
    subtitle = "Comparação dos dados de exportação do Brasil e de importação dos UK",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
## Brazilian Imports from the UK
#-------------------------------------------------------------------------------
# Download and read Brazil's import data
# get_data_br(2015, 2024, type = "IMP", cnty_cod = "628")
imp_br_uk <- read_csv2("data/IMP_brasil_628_2015_2024.csv", show_col_types = FALSE)

imp_br_uk

# Summarize Brazil's total imports from the UK by year
imp_br_uk_ano <- imp_br_uk %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum(VL_FOB, na.rm = TRUE)) %>%
  rename(ano = CO_ANO, imp_br = TOTAL_ANO)

imp_br_uk_ano


#-------------------------------------------------------------------------------
## UK Exports to Brazil
## Data from comtrade
#-------------------------------------------------------------------------------
# Read UK export data
exp_uk_br <- read_excel(
  "data/uk_br_imp_exp.xlsx"
)

exp_uk_br

# Tidy the U.S. export data and summarize by year
exp_uk_br_ano <- exp_uk_br %>%
  filter(flowDesc == "Export") %>%
  select(refYear, primaryValue) %>%
  rename(ano = refYear, exp_uk = primaryValue) %>%
  mutate(ano = as.integer(ano))

exp_uk_br_ano


#-------------------------------------------------------------------------------
## Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
imp_br_exp_uk <- full_join(imp_br_uk_ano, exp_uk_br_ano, by = "ano")

imp_br_exp_uk

# Convert to a "long" format for easier plotting with ggplot2
imp_br_exp_uk_long <- imp_br_exp_uk %>%
  pivot_longer(
    cols = c(imp_br, exp_uk),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

imp_br_exp_uk_long

# Create the plot
ggplot(imp_br_exp_uk_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#1f78b4", "exp_uk" = "#e31a1c"),
    labels = c("Importações do Brasil (para os UK)", "Exportações do UK (do Brasil)")
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_uk_long$ano)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")) +
  labs(
    title = "Asimetrias de Comércio: Brasil vs. UK (2015-2024)",
    subtitle = "Comparação dos dados de importação do Brasil e de exportação do UK",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
