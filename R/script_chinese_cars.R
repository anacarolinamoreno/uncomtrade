

#Using the comtradr package
#Source: https://github.com/ropensci/comtradr

#Main packages
library(tidyverse)
library(tidylog)
library(janitor)
library(data.table)
library(beepr)
beep()

#SETTING UP ACCESS TO THE DATA

#Install comtradr package
install.packages("comtradr")

#Load comtradr package
library(comtradr)

#To use this package you need a free API key from UN Comtrade:
#Link to get one: https://uncomtrade.org/docs/api-subscription-keys/

#Storing your API key
Sys.setenv('COMTRADE_PRIMARY' = 'ff4a2a1810b845a2ae373955eac70133')


#TIPS ON HOW THE DATA IS STORED

#Country names are in ISO3 format:
#https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3

#Package allows you to request up to 12 years intervals from the API

#Data dictionary: https://comtradeapi.un.org/files/v1/app/reference/HS.json

#Example 1: total trade between China and Germany and Argentina, as reported by China
example1 <- comtradr::ct_get_data(
  reporter = 'CHN',
  partner = c('ARG', 'DEU'),
  start_date = 2010,
  end_date = 2012
)

#Example 2: finding a specific product:
wine_codes <- ct_commodity_lookup("wine", return_code = TRUE, return_char = TRUE)
example2 <- ct_get_data(
  reporter =  "ARG",
  flow_direction = "export",
  partner = "all_countries",
  start_date = 2007,
  end_date = 2011,
  commodity_code = wine_codes
)

#Example 3: finding a class of itens using a code and exporting country to all partners:

cars_codes2 <- ct_commodity_lookup(8703, return_code = TRUE, return_char = TRUE)

example3 <- ct_get_data(
  reporter =  "CHN",
  flow_direction = "export",
  partner = "all_countries",
  start_date = 2014,
  end_date = 2023,
  commodity_code = cars_codes2
)

example4 <- ct_get_data(
  reporter =  "CHN",
  flow_direction = "export",
  partner = "all_countries",
  start_date = 2023,
  end_date = 2023
)


#Example 4: getting a list of all itens
all_codes <- as.dataframe(ct_commodity_lookup("", return_code = TRUE, return_char = TRUE))


# ANALYSE DATA ABOUT CHINESE ELECTRIC CARS

#Cleaning data
df <- example3 |>
  mutate(tipo_veiculo = case_when(
    cmd_code == 870380 ~ "eletrico_puro",
    cmd_code >= 870340 & cmd_code <= 870350 ~ "hibridos",
    cmd_code >= 870360 & cmd_code <= 870370 ~ "hibridos_com_plug",
    T ~ "outros"
  ))

#Grouping by year and type of vehicle
historico <- df |>
  filter(cmd_code != 8703 & cmd_code != 870390) |>
  group_by(ref_year, tipo_veiculo) |>
  summarise(contagem = n(),
            unidades = round((sum(qty)),0),
            valor = sum(primary_value))

eletrico_2023 <- df |>
  filter(ref_year == 2023 & (tipo_veiculo == "eletrico_puro" | tipo_veiculo == "hibridos_com_plug")) |>
  group_by(partner_desc) |>
  summarise(unidades = sum(qty),
            valor = sum(primary_value))

tradicionais_2023 <- df |>
  filter(ref_year == 2023 & tipo_veiculo == "outros" & cmd_code != 8703 & cmd_code != 870390) |>
  group_by(partner_desc) |>
  summarise(unidades = sum(qty),
            valor = sum(primary_value))

so_tradicionais <- left_join(x = tradicionais_2023,
                             y = eletrico_2023,
                             by = "partner_desc") |>
  filter(is.na(unidades.y))

so_eletricos <- left_join(x = eletrico_2023,
                             y = tradicionais_2023,
                             by = "partner_desc") |>
  filter(is.na(unidades.y))


tipo_2023 <- df |>
  filter(ref_year == 2023 & (tipo_veiculo == "eletrico_puro" | tipo_veiculo == "hibridos_com_plug")) |>
  group_by(partner_desc, tipo_veiculo) |>
  summarise(unidades = sum(qty)) |>
  pivot_wider(names_from = tipo_veiculo, values_from = unidades)

write.csv2(tipo_2023, "data/tipo_2023.csv", row.names = F)

historico_wide <- historico |>
  select(ref_year, tipo_veiculo, unidades) |>
  pivot_wider(names_from = tipo_veiculo, values_from = unidades)

write.csv(historico_wide, "data/historico_wide.csv", row.names = F)

#Organizing by type of car
distinct <- df |>
  filter(ref_year == 2023) |>
  group_by(cmd_code, cmd_desc) |>
  summarise(unidades = round((sum(qty)),0))

distinct2 <- df |>
  distinct(cmd_code, cmd_desc)

distinct3 <- left_join(x = distinct2, y = distinct, by = c("cmd_code", "cmd_desc"))
write.csv2(distinct3, "data/lista_codigos.csv", row.names = F)


#Group by countries

#Load iso3 country code database
nomes_paises <- read_delim("~/Documents/Codigos/utils/de-para_nomes-paises.csv",
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
  rename(partner_iso = codigo_iso3)

df_total <- left_join(x = df,
                      y = nomes_paises,
                      by = "partner_iso")



por_pais <- df_total |>
  filter(tipo_veiculo == "eletrico_puro"
         | tipo_veiculo == "hibridos_com_plug") |>
  group_by(ref_year, name_flourish, nome_portugues, continente, latitude, longitude, image_url) |>
  summarise(contagem = n(),
            unidades = round((sum(qty)),0),
            valor = sum(primary_value)) |>
  mutate(continente = case_when(
    continente == "América Norte" ~ "América do Norte",
    T ~ continente
  ))

write.csv(por_pais, "data/por_pais.csv", row.names = F)
