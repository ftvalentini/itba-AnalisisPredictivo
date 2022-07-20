
# TODO
# downloading with curl didn't worK:
# 
# library(curl)
# library(data.table)
# url_file = 
#   "http://www.ibge.gov.br/Pib_Municipios/2019/base/base_de_dados_2010_2019_xls.zip"
# destination_file = "clase_02/data/raw/pib/base_de_dados_2010_2019_xls.zip"
# curl_download(url=url_file, destfile=destination_file)
# 
# it only gets 44kb of data.
# 
# Bajamos 
#    * base_de_dados_2010_2019_xls.zip
#    * Grau_de_urbanizacao.xlsx
#    * Tipologia_municipal_rural_urbano.xlsx
# directamente de:
# https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=downloads
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15790-classificacao-e-caracterizacao-dos-espacos-rurais-e-urbanos-do-brasil.html?=&t=downloads
# 
# 
# tambien fallaron:
# 
# library(RCurl)
# f = CFILE(destination_file, mode="wb")
# curlPerform(url=url_file, writedata=f@ref)
# close(f)
# 
# httr::GET(
#   url_file,
#   httr::write_disk(path=destination_file, overwrite=TRUE))
# 
# download.file(url_file, destination_file, method="curl")
# 

library(tidyverse)

dir.create("clase_02/data/working", recursive=T)
dir.create("clase_02/data/raw", recursive=T)

# PIB ---------------------------------------------------------------------

file_pib = "clase_02/data/raw/ibge/PIB dos Municípios - base de dados 2010-2019.xls"
dat = readxl::read_excel(file_pib, guess_max=5000)
dat = dat %>% janitor::clean_names()

# keep useful vars
dat = dat %>% select(
  ano
  ,codigo_da_grande_regiao
  ,nome_da_grande_regiao
  ,codigo_da_unidade_da_federacao
  ,sigla_da_unidade_da_federacao
  ,nome_da_unidade_da_federacao
  ,codigo_do_municipio
  ,nome_do_municipio
  ,semiarido
  ,hierarquia_urbana_principais_categorias
  ,valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000
  ,valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000
  ,produto_interno_bruto_a_precos_correntes_r_1_000
  ,produto_interno_bruto_per_capita_a_precos_correntes_r_1_00
  ,atividade_com_maior_valor_adicionado_bruto
)

# rename vars
dat = dat %>% rename(
  vab_agro = valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000
  ,vab_industria = valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000
  ,pib = produto_interno_bruto_a_precos_correntes_r_1_000
  ,pib_percapita = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00
  ,atividade_maior_vab = atividade_com_maior_valor_adicionado_bruto
)  

# filter anio
dat = dat %>% dplyr::filter(ano == 2019)


# urbanizacion ------------------------------------------------------------

file_1 = "clase_02/data/raw/ibge/Grau_de_urbanizacao.xlsx"
dat1 = readxl::read_excel(file_1, sheet=2, guess_max=5000)
dat1 = dat1 %>% janitor::clean_names()
dat1 = dat1 %>% select(cd_gcmun, gr_urb)

file_2 = "clase_02/data/raw/ibge/Tipologia_municipal_rural_urbano.xlsx"
dat2 = readxl::read_excel(file_2, sheet=2, guess_max=5000)
dat2 = dat2 %>% janitor::clean_names()
dat2 = dat2 %>% select(cd_gcmun, tipo)

dat_urb = inner_join(dat1, dat2, by="cd_gcmun")
dat_urb = dat_urb %>% rename(
  grado_urbanizacion = gr_urb,
  tipo_urbano = tipo,
  codigo_do_municipio = cd_gcmun
)

# join --------------------------------------------------------------------

dat = dat %>% 
  left_join(dat_urb, on="codigo_do_municipio")

# cods = c("1504752", "4212650", "4220000", "4314548", "5006275")
# dat %>% 
#   filter(is.na(tipo_urbano)) %>%
#   # filter(codigo_do_municipio %in% cods) %>% 
#   select(nome_do_municipio, codigo_do_municipio, 
#          grado_urbanizacion, tipo_urbano)

# save --------------------------------------------------------------------

saveRDS(dat, file="clase_02/data/working/ibge_municipios_2019.rds")
