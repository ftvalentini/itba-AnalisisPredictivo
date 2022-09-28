
# TODO
# usar library(ipeadatar)
# o python: https://github.com/luanborelli/ipeadatapy
# o API: http://www.ipeadata.gov.br/api/
# 
# La documentacion no era buena entonces bajamos los datos manualmente desde:
# http://www.ipeadata.gov.br/Default.aspx
# 
# * Expos e impos:
#     regional --> temas --> comercio exterior
# 
# * Area geografica:
#     regional --> temas --> geografico --> 
#     Área Geográfica publicada nos Censos
# 
# * Despesas:
#     regional --> temas --> geografico --> Finanças públicas --> 
#     Despesa por função - [] - empenhada - municipal
# 
# * Populaçao:
#     regional --> temas --> populacao --> População residente - 1º de julho - estimativas
# 


library(tidyverse)
library(data.table)

anio = "2019"

# file names
files = list.files("clase_02/data/raw/ipea/", full.names=T)

# read data and correct names
dats = list()
for (f in files) {
  basef = basename(f)
  basef_sinext = str_remove(basef, "(\\.csv)")
  dats[[basef]] = fread(f, skip=1, fill=T, encoding="UTF-8")
  dats[[basef]] = dats[[basef]] %>% rename(!!basef_sinext := all_of(anio))
  dats[[basef]] = dats[[basef]] %>% janitor::clean_names()
  dats[[basef]][, v5 := NULL]
}

# joins
dat = Reduce(f=function(a,b) merge(a,b,all=F), x=dats)

# integer64 as numeric
dat = dat %>% mutate_if(is.numeric, as.numeric)

# save data as RDS
saveRDS(dat, file="clase_02/data/working/varios_municipios_2019.rds")
