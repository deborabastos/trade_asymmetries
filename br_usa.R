# Clear the environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(scales)
library(readxl)

# Load custom function to get Brazilian trade data
source("./R/get_data_br.R")

# Set working directory
#setwd("C:/Users/debora.bastos/OneDrive - mtegovbr/Documentos/trade_asymmetries")

#-------------------------------------------------------------------------------
## Brazilian Exports to the U.S.
#-------------------------------------------------------------------------------
# Download and read Brazil's export data
# get_data_br(2015, 2024, type = "EXP", cnty_cod = "249")
exp_br_eua <- read_csv2("data/EXP_brasil_249_2015_2024.csv", show_col_types = FALSE)

# Summarize Brazil's total exports to the U.S. by year
exp_br_eua_ano <- exp_br_eua %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum(VL_FOB, na.rm = TRUE)) %>%
  rename(ano = CO_ANO, exp_br = TOTAL_ANO)

exp_br_eua_ano

#-------------------------------------------------------------------------------
## U.S. Imports from Brazil
#-------------------------------------------------------------------------------
# Read U.S. import data
imp_eua_br <- read_excel(
  "data/IMP_eua_br_2015_2024.xlsx",
  sheet = "General Customs Value",
  range = "B3:L4218"
)

# Tidy the U.S. import data and summarize by year
imp_eua_br_ano <- imp_eua_br %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "ANO",
    values_to = "VALOR"
  ) %>%
  group_by(ANO) %>%
  summarise(TOTAL_ANO = sum(VALOR, na.rm = TRUE)) %>%
  rename(ano = ANO, imp_usa = TOTAL_ANO) %>%
  mutate(ano = as.integer(ano))

imp_eua_br_ano

#-------------------------------------------------------------------------------
## Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
exp_br_imp_usa <- full_join(exp_br_eua_ano, imp_eua_br_ano, by = "ano")

exp_br_imp_usa

# Convert to a "long" format for easier plotting with ggplot2
exp_br_imp_usa_long <- exp_br_imp_usa %>%
  pivot_longer(
    cols = c(exp_br, imp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

exp_br_imp_usa_long

# Create the plot
ggplot(exp_br_imp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("exp_br" = "#1f78b4", "imp_usa" = "#e31a1c"),
    labels = c("Exportações do Brasil (para os EUA)", "Importações dos EUA (do Brasil)")
  ) +
  scale_x_continuous(breaks = unique(exp_br_imp_usa_long$ano)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")) +
  labs(
    title = "Asimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "Comparação dos dados de exportação do Brasil e de importação dos EUA",
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
## Brazilian Imports from the U.S.
#-------------------------------------------------------------------------------
# Download and read Brazil's import data
# get_data_br(2015, 2024, type = "IMP", cnty_cod = "249")
imp_br_eua <- read_csv2("data/IMP_brasil_249_2015_2024.csv", show_col_types = FALSE)

imp_br_eua

# Summarize Brazil's total imports from the U.S. by year
imp_br_eua_ano <- imp_br_eua %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum(VL_FOB, na.rm = TRUE)) %>%
  rename(ano = CO_ANO, imp_br = TOTAL_ANO)

imp_br_eua_ano


#-------------------------------------------------------------------------------
## U.S. Exports to Brazil
## Data from https://dataweb.usitc.gov/trade/search/TotExp/HTS
## Used Exports: Total
#-------------------------------------------------------------------------------
# Read U.S. export data
exp_eua_br <- read_excel(
  "data/EXP_eua_br_2015_2024.xlsx",
  sheet = "FAS Value",
  range = "b3:L4980"
)

exp_eua_br

# Tidy the U.S. export data and summarize by year
exp_eua_br_ano <- exp_eua_br %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "ANO",
    values_to = "VALOR"
  ) %>%
  group_by(ANO) %>%
  summarise(TOTAL_ANO = sum(VALOR, na.rm = TRUE)) %>%
  rename(ano = ANO, exp_usa = TOTAL_ANO) %>%
  mutate(ano = as.integer(ano))

exp_eua_br_ano

#-------------------------------------------------------------------------------
## Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
imp_br_exp_usa <- full_join(imp_br_eua_ano, exp_eua_br_ano, by = "ano")

imp_br_exp_usa

# Convert to a "long" format for easier plotting with ggplot2
imp_br_exp_usa_long <- imp_br_exp_usa %>%
  pivot_longer(
    cols = c(imp_br, exp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

imp_br_exp_usa_long

# Create the plot
ggplot(imp_br_exp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#1f78b4", "exp_usa" = "#e31a1c"),
    labels = c("Importações do Brasil (para os EUA)", "Exportações dos EUA (do Brasil)")
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_long$ano)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")) +
  labs(
    title = "Asimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "Comparação dos dados de importação do Brasil e de exportação total dos EUA",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")








#-------------------------------------------------------------------------------
## U.S. Exports to Brazil
## Data from https://dataweb.usitc.gov/trade/search/TotExp/HTS
## Used Exports: Domestic
#-------------------------------------------------------------------------------
# Read U.S. export data
exp_eua_br <- read_excel(
  "data/EXP_eua_dom_br_2015_2024.xlsx",
  sheet = "FAS Value",
  range = "b3:L4845"
)

exp_eua_br

# Tidy the U.S. export data and summarize by year
exp_eua_br_ano <- exp_eua_br %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "ANO",
    values_to = "VALOR"
  ) %>%
  group_by(ANO) %>%
  summarise(TOTAL_ANO = sum(VALOR, na.rm = TRUE)) %>%
  rename(ano = ANO, exp_usa = TOTAL_ANO) %>%
  mutate(ano = as.integer(ano))

exp_eua_br_ano

#-------------------------------------------------------------------------------
## Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
imp_br_exp_usa <- full_join(imp_br_eua_ano, exp_eua_br_ano, by = "ano")

imp_br_exp_usa

# Convert to a "long" format for easier plotting with ggplot2
imp_br_exp_usa_long <- imp_br_exp_usa %>%
  pivot_longer(
    cols = c(imp_br, exp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

imp_br_exp_usa_long

# Create the plot
ggplot(imp_br_exp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#1f78b4", "exp_usa" = "#e31a1c"),
    labels = c("Importações do Brasil (para os EUA)", "Exportações dos EUA (do Brasil)")
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_long$ano)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")) +
  labs(
    title = "Asimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "Comparação dos dados de importação do Brasil e de exportação doméstica dos EUA",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")







################################ COMTRADE DATA ##############################
#-------------------------------------------------------------------------------
## U.S. Exports to Brazil
## Data from https://comtradeplus.un.org/
#-------------------------------------------------------------------------------
# Read U.S. export data
exp_eua_br <- read_excel(
  "data/EXP_eua_br_comtrade.xlsx",
)

exp_eua_br_ano <- exp_eua_br %>%
  select("refYear", "fobvalue") %>%
  rename(ano = `refYear`, exp_usa = `fobvalue`) %>%
  mutate(ano = as.integer(ano))

exp_eua_br_ano

#-------------------------------------------------------------------------------
## Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
imp_br_exp_usa <- full_join(imp_br_eua_ano, exp_eua_br_ano, by = "ano")

imp_br_exp_usa

# Convert to a "long" format for easier plotting with ggplot2
imp_br_exp_usa_long <- imp_br_exp_usa %>%
  pivot_longer(
    cols = c(imp_br, exp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

imp_br_exp_usa_long

# Create the plot
ggplot(imp_br_exp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#1f78b4", "exp_usa" = "#e31a1c"),
    labels = c("Importações do Brasil (para os EUA)", "Exportações dos EUA (do Brasil)")
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_long$ano)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")) +
  labs(
    title = "Asimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "COMTRADE - Comparação dos dados de importação do Brasil e de exportação dos EUA",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



