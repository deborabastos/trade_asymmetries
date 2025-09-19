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
exp_br_eua <- read_csv2(
  "./data/input/EXP_brasil_249_2015_2024.csv",
  show_col_types = FALSE
)

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
  "./data/input/IMP_eua_br_2015_2024.xlsx",
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

# Diff

# Calcula a diferença percentual e adiciona a nova coluna
exp_br_imp_usa_diff <- exp_br_imp_usa %>%
  mutate(diferenca_percentual = ((imp_usa - exp_br) / exp_br) * 100) %>%
  mutate(diferenca_valor = ((imp_usa - exp_br)))

# Exibe o dataframe resultante
print(exp_br_imp_usa_diff)

# Convert to a "long" format for easier plotting with ggplot2
exp_br_imp_usa_long <- exp_br_imp_usa %>%
  pivot_longer(
    cols = c(exp_br, imp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

exp_br_imp_usa_long
write_csv2(exp_br_imp_usa_long, "./data/output/exp_br_imp_usa_long.csv")

# Create the plot
ggplot(exp_br_imp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("exp_br" = "#098e68ff", "imp_usa" = "#e31a1c"),
    labels = c(
      "exp_br" = "Exportações do Brasil (para os EUA)",
      "imp_usa" = "Importações dos EUA (do Brasil)"
    )
  ) +
  scale_x_continuous(breaks = unique(exp_br_imp_usa_long$ano)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")
  ) +
  labs(
    title = "Assimetrias de Comércio: Brasil vs. EUA (2015-2024)",
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
imp_br_eua <- read_csv2(
  "./data/input/IMP_brasil_249_2015_2024.csv",
  show_col_types = FALSE
)

imp_br_eua[, c("CO_ANO", "VL_FOB", "VL_FRETE", "VL_SEGURO")]

# Summarize Brazil's total imports from the U.S. by year
imp_br_eua_ano <- imp_br_eua %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum((VL_FOB), na.rm = TRUE)) %>%
  rename(ano = CO_ANO, imp_br = TOTAL_ANO)

imp_br_eua_ano

imp_br_eua_ano_CIF <- imp_br_eua %>%
  group_by(CO_ANO) %>%
  summarise(TOTAL_ANO = sum((VL_FOB+VL_FRETE+VL_SEGURO), na.rm = TRUE)) %>%
  rename(ano = CO_ANO, imp_br = TOTAL_ANO)

imp_br_eua_ano_CIF

#-------------------------------------------------------------------------------
## U.S. Exports to Brazil
## Data from https://dataweb.usitc.gov/trade/search/TotExp/HTS
## Used Exports: Total
#-------------------------------------------------------------------------------
# Read U.S. export data
exp_eua_br <- read_excel(
  "./data/input/EXP_eua_br_2015_2024.xlsx",
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
## FOB Combine and Plot the Data
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
write_csv2(imp_br_exp_usa_long, "data/output/imp_br_exp_total_usa_long.csv")


# Create the plot
ggplot(imp_br_exp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#098e68ff", "exp_usa" = "#e31a1c"),
    labels = c(
      "imp_br" = "Importações do Brasil (para os EUA) - FOB",
      "exp_usa" = "Exportações dos EUA (do Brasil) - FOB"
    )
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_long$ano)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")
  ) +
  labs(
    title = "Assimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "Comparação dos dados de importação do Brasil e de exportação total dos EUA",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")




#-------------------------------------------------------------------------------
## CIF Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
imp_br_exp_usa_CIF <- full_join(imp_br_eua_ano_CIF, exp_eua_br_ano, by = "ano")

imp_br_exp_usa_CIF

# Convert to a "long" format for easier plotting with ggplot2
imp_br_exp_usa_CIF_long <- imp_br_exp_usa_CIF %>%
  pivot_longer(
    cols = c(imp_br, exp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

imp_br_exp_usa_CIF_long
write_csv2(imp_br_exp_usa_CIF_long, "data/output/imp_br_exp_CIF_total_usa_long.csv")


# Create the plot
ggplot(imp_br_exp_usa_CIF_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#098e68ff", "exp_usa" = "#e31a1c"),
    labels = c(
      "imp_br" = "Importações do Brasil (para os EUA) - CIF",
      "exp_usa" = "Exportações dos EUA (do Brasil) - FOB"
    )
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_CIF_long$ano)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")
  ) +
  labs(
    title = "Assimetrias de Comércio: Brasil vs. EUA (2015-2024)",
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
  "./data/input/EXP_eua_dom_br_2015_2024.xlsx",
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
write_csv2(imp_br_exp_usa_long, "data/output/imp_br_exp_domestic_usa_long.csv")


# Create the plot
ggplot(imp_br_exp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#098e68ff", "exp_usa" = "#e31a1c"),
    labels = c(
      "imp_br" = "Importações do Brasil (para os EUA) - FOB",
      "exp_usa" = "Exportações dos EUA (do Brasil) - FOB"
    )
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_long$ano)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")
  ) +
  labs(
    title = "Assimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "Comparação dos dados de importação do Brasil e de exportação doméstica dos EUA",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")




#-------------------------------------------------------------------------------
## CIF Combine and Plot the Data
#-------------------------------------------------------------------------------
# Join the two data frames
imp_br_exp_usa_CIF <- full_join(imp_br_eua_ano_CIF, exp_eua_br_ano, by = "ano")

imp_br_exp_usa_CIF

# Convert to a "long" format for easier plotting with ggplot2
imp_br_exp_usa_CIF_long <- imp_br_exp_usa_CIF %>%
  pivot_longer(
    cols = c(imp_br, exp_usa),
    names_to = "tipo",
    values_to = "valor_bilhoes"
  ) %>%
  mutate(valor_bilhoes = valor_bilhoes / 1e9) # Convert to billions

imp_br_exp_usa_CIF_long
write_csv2(imp_br_exp_usa_CIF_long, "data/output/imp_br_exp_CIF_domestic_usa_long.csv")


# Create the plot
ggplot(imp_br_exp_usa_CIF_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#098e68ff", "exp_usa" = "#e31a1c"),
    labels = c(
      "imp_br" = "Importações do Brasil (para os EUA) - CIF",
      "exp_usa" = "Exportações dos EUA (do Brasil) - FOB"
    )
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_CIF_long$ano)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")
  ) +
  labs(
    title = "Assimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "Comparação dos dados de importação do Brasil e de exportação domesticas dos EUA",
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
  "./data/input/EXP_eua_br_comtrade.xlsx",
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
write_csv2(imp_br_exp_usa_long, "data/output/imp_br_exp_comtrade_usa_long.csv")

# Create the plot
ggplot(imp_br_exp_usa_long, aes(x = ano, y = valor_bilhoes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("imp_br" = "#098e68ff", "exp_usa" = "#e31a1c"),
    labels = c(
      "imp_br" = "Importações do Brasil (para os EUA)",
      "exp_usa" = "Exportações dos EUA (do Brasil)"
    )
  ) +
  scale_x_continuous(breaks = unique(imp_br_exp_usa_long$ano)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", scale = 1, suffix = "B")
  ) +
  labs(
    title = "Assimetrias de Comércio: Brasil vs. EUA (2015-2024)",
    subtitle = "COMTRADE - Comparação dos dados de importação do Brasil e de exportação dos EUA",
    x = "Ano",
    y = "Valor (bilhões USD)",
    color = "Dados"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")




