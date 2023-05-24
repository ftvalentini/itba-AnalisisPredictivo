
library(curl)
library(data.table)
library(janitor)
library(tidyverse)

dir.create("clase_02/data/working", recursive=T)
dir.create("clase_02/data/raw", recursive=T)

base_url = "https://transparencia.gov.br/download-de-dados/bolsa-familia-pagamentos"
period = "201906"

df_total = data.table()

url = paste0(base_url, "/", period)
tmp = tempfile()
curl_download(url, tmp)

df = fread(cmd=paste0("unzip -p ", tmp), dec=",", fill=T)
df = df %>% clean_names()

# cantidad de obs por municipio
df = df %>% 
  group_by(uf, codigo_municipio_siafi, nome_municipio) %>% 
  mutate(num_beneficiarios = uniqueN(nis_favorecido)) %>% 
  as.data.table()

# municipios chicos con outliers
df[num_beneficiarios <= 50][ order(-valor_parcela) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)
df[num_beneficiarios <= 200][ order(-valor_parcela) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)
df[num_beneficiarios <= 200][ order(valor_parcela) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)
df[num_beneficiarios <= 500][ order(-valor_parcela) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)
df[num_beneficiarios >= 1000][ order(num_beneficiarios) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)
df[num_beneficiarios >= 10000][ order(valor_parcela) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)
df[num_beneficiarios >= 20000][ order(-valor_parcela) ][
  ,.(nome_municipio, num_beneficiarios, valor_parcela)] %>% head(10)

munis_to_keep = 
  c("LAURENTINO","ITATI","VITOR MEIRELES", "PAULISTAS", "ITAGUACU", "BACABAL", 
    "JUAZEIRO")

df = df[ nome_municipio %in% munis_to_keep ]

df = df[ ,
         .(valor = sum(valor_parcela, na.rm=T))  
         ,by = .(nis_favorecido, nome_municipio)
]

# check no dups
df %>% janitor::get_dupes(nis_favorecido)

# means by group
df[ ,
    .(x = mean(valor, na.rm=T), n = .N)  
    ,by = .(nome_municipio)
]

# densidades
ggplot(df) + 
  geom_density(aes(x=valor, fill=nome_municipio), alpha=0.3) +
  facet_wrap(nome_municipio ~ .) +
  NULL

df = df %>% 
  rename(nis=nis_favorecido, municipio=nome_municipio)
df$municipio = df$municipio %>% tolower() %>% str_replace(" ","_")

# write
write_csv(df, "clase_02/data/working/bolsafamilia_sample_201906.csv")


# n_by_muni = df[ ,
#                 .(num_beneficiarios = uniqueN(nis_favorecido))  
#                 ,by = .(uf, codigo_municipio_siafi, nome_municipio)
# ]
# # n_by_muni[num_beneficiarios >= 200][ order(num_beneficiarios) ] %>% head()
# # n_by_muni[num_beneficiarios >= 500][ order(num_beneficiarios) ] %>% head()
# # n_by_muni[num_beneficiarios >= 1000][ order(num_beneficiarios) ] %>% head()
# # n_by_muni[num_beneficiarios >= 10000][ order(num_beneficiarios) ] %>% head()
# # n_by_muni[num_beneficiarios >= 20000][ order(num_beneficiarios) ] %>% head()
