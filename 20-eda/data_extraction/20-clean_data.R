
library(data.table)
library(fuzzyjoin)
library(tidyverse)

# clean string function
clean_str = function(vchar) {
  vchar %>% 
    tolower() %>% 
    janitor::make_clean_names()
}

# read and transform datasets
dats = list()
  # bolsa familia
dats[["bolsafamilia_municipios_2019"]] = read_csv(
  "clase_02/data/working/bolsa-familia_mensual_2019.csv"
)
dats[[1]] = dats[[1]] %>% 
  group_by(uf, codigo_municipio_siafi, nome_municipio) %>% 
  summarise(
    num_beneficiarios = mean(num_beneficiarios, na.rm=T),
    suma_valor = sum(suma_valor, na.rm=T)
  ) %>% 
  ungroup()
  # ibge
dats[["ibge_municipios_2019"]] = readRDS(
  "clase_02/data/working/ibge_municipios_2019.rds"
)

  # varios
dats[["varios_municipios_2019"]] = readRDS(
  "clase_02/data/working/varios_municipios_2019.rds"
)


# nombres ibge-bolsa ---------------------------------------------------------

# clean names
n_bolsa = dats[["bolsafamilia_municipios_2019"]]$nome_municipio %>% 
  sort() %>% clean_str() %>% tibble(n_bolsa=.)
n_ibge = dats[["ibge_municipios_2019"]]$nome_do_municipio %>% 
  sort() %>% clean_str() %>% tibble(n_ibge=.)

# no matches
nomatch_bolsa = n_bolsa %>% anti_join(n_ibge, by=c("n_bolsa"="n_ibge"))
nomatch_ibge = n_ibge %>% anti_join(n_bolsa, by=c("n_ibge"="n_bolsa"))

# join by strdist
matches_bolsaibge = stringdist_inner_join(nomatch_bolsa, nomatch_ibge
                                         , by=c("n_bolsa"="n_ibge")
                                         ,max_dist=3)

# no matches 2
nomatch_bolsa2 = nomatch_bolsa %>% 
  dplyr::filter(!n_bolsa %in% matches_bolsaibge$n_bolsa)
nomatch_ibge2 = nomatch_ibge %>% 
  dplyr::filter(!n_ibge %in% matches_bolsaibge$n_ibge)

# join by regex match (y faltantes a mano)
matches2_bolsaibge = 
  regex_inner_join(nomatch_bolsa2, nomatch_ibge2, by=c("n_bolsa"="n_ibge")) %>% 
  bind_rows(
    regex_inner_join(nomatch_ibge2, nomatch_bolsa2, by=c("n_ibge"="n_bolsa"))
  ) %>% 
  bind_rows(
    tribble(~n_bolsa,                 ~n_ibge
            ,"sao_domingos_de_pombal", "sao_domingos_5"
            ,"campo_grande_ex_augusto_severo", "campo_grande_3"
    )
  )

# tabla correspondencia final
tab_noms_bolsaibge = bind_rows(matches_bolsaibge, matches2_bolsaibge)


# match ibge-bolsa -------------------------------------------------------------------

# clean names (new municipio column)
bolsa = dats[["bolsafamilia_municipios_2019"]] %>% 
  mutate(municipio = clean_str(nome_municipio))
ibge = dats[["ibge_municipios_2019"]] %>% 
  mutate(municipio = clean_str(nome_do_municipio))

# se conservan nombres ibge (Modificamos bolsa)
bolsa_f = bolsa %>% 
  left_join(tab_noms_bolsaibge, by=c("municipio"="n_bolsa")) %>% 
  mutate(municipio = coalesce(n_ibge, municipio)) %>% 
  select(-n_ibge)

# chequeo:
setdiff(bolsa_f$municipio, ibge$municipio)

# nombres ibge-varios ---------------------------------------------------------

# clean names
n_varios = dats[["varios_municipios_2019"]][["municipio"]]  %>% 
  sort() %>% clean_str() %>% tibble(n_varios=.)

# no matches
nomatch_varios = n_varios %>% anti_join(n_ibge, by=c("n_varios"="n_ibge"))
nomatch_ibge = n_ibge %>% anti_join(n_varios, by=c("n_ibge"="n_varios"))

# join by strdist
matches_ = stringdist_inner_join(nomatch_varios, nomatch_ibge, by=c("n_varios"="n_ibge")
                                ,max_dist=3)

# no matches 2
nomatch_varios2 = nomatch_varios %>% 
  dplyr::filter(!n_varios %in% matches_$n_varios)
nomatch_ibge2 = nomatch_ibge %>% 
  dplyr::filter(!n_ibge %in% matches_$n_ibge)

# join by regex match (y faltantes a mano)
matches2 = 
  regex_inner_join(nomatch_varios2, nomatch_ibge2, by=c("n_varios"="n_ibge")) %>% 
  bind_rows(
    regex_inner_join(nomatch_ibge2, nomatch_varios2, by=c("n_ibge"="n_varios"))
  ) %>%
  bind_rows(
    tribble(~n_varios,                    ~n_ibge
            ,"presidente_juscelino_3",    "serra_caiada"
            ,"vila_alta",                 "alto_paraiso_2"
            ,"governador_lomanto_junior", "barro_preto"
            ,"santarem_2",                "joca_claudino"
            ,"sao_miguel_de_touros",      "sao_miguel_do_gostoso"
            ,"campo_de_santana",          "tacima"
            ,"sao_domingos_de_pombal",    "sao_domingos_5"
    )
  )

# tabla correspondencia final
tab_noms_ibgevarios = bind_rows(matches_, matches2)

# match ibge-varios -------------------------------------------------------------------

# clean names (new municipio column)
varios = dats[["varios_municipios_2019"]] %>% 
  rename(municipio_old = municipio) %>% 
  mutate(municipio = clean_str(municipio_old))

# se conservan nombres ibge (Modificamos varios)
varios_f = varios %>% 
  left_join(tab_noms_ibgevarios, by=c("municipio"="n_varios")) %>% 
  mutate(municipio = coalesce(n_ibge, municipio)) %>% 
  select(-n_ibge)

# chequeo:
setdiff(varios_f$municipio, ibge$municipio)
  # solo quedan los municipios demas en varios

# ajustes finales ---------------------------------------------------------

# drop irrelevant vars / rename
varios_f = varios_f %>% 
  select(-c(
    codigo,municipio_old,sigla
  )) %>% 
  arrange(municipio)
bolsa_f = bolsa_f %>% 
  select(-c(
    codigo_municipio_siafi,nome_municipio
  )) %>% 
  rename(
    num_beneficiarios_mean = num_beneficiarios
    # ,suma_valor_2016 = suma_valor
  ) %>% 
  arrange(municipio)
ibge_f = ibge %>% 
  select(-c(
    ano,codigo_da_grande_regiao, nome_do_municipio,sigla_da_unidade_da_federacao
  )) %>% 
  rename(
    codigo_uf = codigo_da_unidade_da_federacao
    ,codigo_municipio = codigo_do_municipio
  ) %>% 
  arrange(municipio)

# write csvs --------------------------------------------------------------

varios_f %>% readr::write_csv("clase_02/data/working/varios_municipios_2019.csv")
ibge_f %>% readr::write_csv("clase_02/data/working/ibge_municipios_2019.csv")
bolsa_f %>% readr::write_csv("clase_02/data/working/bolsafamilia_municipios_2019.csv")

