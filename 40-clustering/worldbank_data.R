library(tidyverse)
library(rnaturalearth)
library(rgeos)

if (!"wbstats" %in% installed.packages()) {
  library(devtools)
  devtools::install_github("nset-ornl/wbstats")
}

library(wbstats)

## buscar indicadores online o con :
# View(wb_search(pattern="gdp.+ppp"))

# definir indicadores a traer
indicadores = c(
  pib_capita = "NY.GDP.PCAP.PP.KD" # PIB per capita PPP
  ,pob = "SP.POP.TOTL" # poblacion
  ,exp_vida = "SP.DYN.LE00.IN" # expectativa de vida al nacer
  ,densidad = "EN.POP.DNST" # densidad poblacional
  ,porc_urban = "SP.URB.TOTL.IN.ZS" # % de poblacion urbana
  ,porc_65 = "SP.POP.65UP.TO.ZS" # % poblacion mayor a 65
  ,porc_14 = "SP.POP.0014.TO.ZS" # % poblacion menor a 14
  ,uhc = "SH.UHC.SRVS.CV.XD" # indicador de acceso a la salud
)

# get datos (los mrnev registros mas nuevos por pais)
# if bad request --> volver a probar :)
dat = wb_data(indicadores, freq="Y", mrv=10)

# omit NA y keep 2017
dat_filtrada = na.omit(dat) %>% 
  filter(date==2017)
cat(nrow(dat_filtrada), "paises en la base")

# merge con metadata paises
dat_paises = wb_countries() %>% 
  select(iso3c, region, income_level)
dat_final = dat_filtrada %>% 
  left_join(dat_paises, by="iso3c") %>% 
  select(-iso2c)

# metadata de los indicadores
latest_info = wb_cache()
metadata = latest_info$indicators %>% 
  filter(indicator_id %in% indicadores) %>% 
  select(indicator_id, indicator, indicator_desc, source_org, source_id)

# sf paises
sf_data = ne_countries(returnclass="sf") %>% 
  rename(iso3c = iso_a3)

# save data
dir.create("clustering/data")
write_csv(dat_final, "clustering/data/worldbank.csv")
saveRDS(sf_data, "clustering/data/worldbank_sf.rds")
write_csv(metadata, "clustering/data/worldbank_metadata.csv")
