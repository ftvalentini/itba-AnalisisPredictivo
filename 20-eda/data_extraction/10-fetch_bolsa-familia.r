
library(curl)
library(data.table)
library(janitor)

dir.create("clase_02/data/working", recursive=T)
dir.create("clase_02/data/raw", recursive=T)

base_url = "https://transparencia.gov.br/download-de-dados/bolsa-familia-pagamentos"
months = sprintf("%02d", 1:12)
year = "2019" 

df_total = data.table()

for (month in months) {
  
  attempt = 1
  res = NULL
  url = paste0(base_url, "/", year, month)
  cat("Downloading ", url, "\n")
  tmp = tempfile()
  while( is.null(res) && attempt <= 3 ) {
    attempt = attempt + 1
    try({
      curl_download(url, tmp)
      res = "OK"
    })
  } 

  cat("Reading ", year, month, "\n")
  df = fread(cmd=paste0("unzip -p ", tmp), dec=",", fill=T)
  
  cat("Processing ", year, month, "\n")
  df = df %>% clean_names()
  df = df[ ,
           .(num_beneficiarios = uniqueN(nis_favorecido)
             ,suma_valor = sum(valor_parcela))  
           ,by = .(uf, codigo_municipio_siafi, nome_municipio, mes_referencia)
  ]
  
  cat("Appending", "\n")
  
  df_total = rbindlist(list(df_total, df))

  cat("\n")
}

fwrite(df_total, "clase_02/data/working/bolsa-familia_mensual_2019.csv")

