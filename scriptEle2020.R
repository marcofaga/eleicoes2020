# Eleições 2020
# Marco Antonio Faganello - marcofaga@gmail.com - http://github.com/marcofaga
# Data de início: 13 de novembro de 2020
# Codificação: Banco em ASCII e Script em UTF-8
# Descrição do Script: Script com banco de dados

# AREA DO A4 na ABNT (com 5 cm de margens): 
# Full page: 160x247mm em in 6.3x9.7 em px com 300 dpi 1890x2917
# Meia Página: 160X124mm / 6.3x4.9in / 1890x1470 (300 dpi)
# Áurea menor: 160x094mm / 6.3x3.7in / 1890x1110 (300 dpi)
# Áurea maior: 160X153mm / 6.3x6.0in / 1890X1800 (300 dpi)

# colorBrewer Divergente (4 cores): #e66101, #fdb863, #b2abd2, #5e3c99 
# colorBrewer Sequencial (4 cores): #fef0d9, #fdcc8a, #fc8d59, #d7301f

# libraries =================================================================

library(tidyverse)
library(beepr)
library(rjson)
library(kableExtra)


# options====================================================================
options(stringsAsFactors = F)
options(knitr.kable.NA = '-')

# functions===================================================================
apl <- list.files("functions", full.names = T)
lapply(apl, source)
remove(apl)
stop()

# script ===================================================================

# Desempenho dos partidos em relação a prefeituras
# Desempenho dos partidos em relação a vereadores
# Comparativo com os anos no desempenho dos partidos
# Comparativo da fragmentação
# dança das cadeiras
# 

# Pegando os dados dos anos anteriores.

load("doutorado.RData")

## Ending ===================================================================
dateEle2020 <- Sys.time()
save.image()

ap <- bd03Pref %>% filter(eleito == 1, capital == 1, ano == 2016) %>% select(uf, nomMun, partOrig, regiao)
ap2 <- ap %>% group_by(partOrig) %>% summarise(n = n())


## Pegando JSONS
apcodtse <- unique(bd01Ver$codMun)
apuf <- unique(bd01Ver$uf)
apx <- 1
aptab <- tibble()

for(apu in apuf) {
  
  apcodtse <- unique(bd01Ver$codMun[bd01Ver$uf == apu])
  apufs <- tolower(apu)
  print(apu)
  
  for(api in apcodtse) {
  
    aplink <- sprintf("https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados-simplificados/%s/%s%s-c0013-e000426-r.json", apufs, apufs, api)
    apjson <- fromJSON(paste(readLines(aplink), collapse=""))
    apjson <- toJSON(apjson)
    
    apname <- paste0("vereador/", api, ".json")
    write(apjson, apname)
    
    
  }

}

apcodtse <- unique(bd01Ver$codMun)
apuf <- unique(bd01Ver$uf)

for(apu in apuf) {
  
  apcodtse <- unique(bd01Ver$codMun[bd01Ver$uf == apu])
  apufs <- tolower(apu)
  print(apu)
  
  for(api in apcodtse) {
    
    aplink <- sprintf("https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados-simplificados/%s/%s%s-c0011-e000426-r.json", apufs, apufs, api)
    apjson <- fromJSON(paste(readLines(aplink), collapse=""))
    apjson <- toJSON(apjson)
    
    apname <- paste0("prefeito/", api, ".json")
    write(apjson, apname)
    
    
  }
  
}

#aplast <- aptab$X.00019.[nrow(aptab)]
#apquais <- unique(aptab$X.00019.)
#apquais <- apquais[apquais != aplast]
#aptab <- aptab %>% filter(X.00019. != aplast)


# compilando vereador
apx <- 1
aptab <- tibble()

apf <- list.files("vereador", full.names = T)
#apf2 <- unlist(lapply(apquais, function(x) which(grepl(x, apf))))
#apf <- apf[-apf2]

#aptab2 <- aptab

for(apff in apf) {
  
  apjson <- fromJSON(paste(readLines(apff), collapse=""))
  
  apele <- apjson$ele
  apcod <- apjson$cdabr
  apdt <- apjson$dt
  aphora <- apjson$ht
  apporc <- apjson$psa
  apturno <- apjson$t
  apfase <- apjson$f
  apcands <- apjson$cand
  
  apnumcand <- sapply(apcands, function(x) x$n)
  apnome <- sapply(apcands, function(x) x$nm)
  appart <-  sapply(apcands, function(x) x$cc)
  apvap <-  sapply(apcands, function(x) x$vap)
  appvap <-  sapply(apcands, function(x) x$pvap)
  apele2 <-  sapply(apcands, function(x) x$st)
  
  aptib <- tibble(apele, apcod, apdt, aphora, apporc, apturno, apfase, apnumcand,
                 apnome, appart, apvap, appvap, apele2)
  names(aptib) <- names(aptab)
  aptab <- rbind(aptab, aptib)
  
  print(apx/length(apf)*100)
  apx <- apx + 1
  
}


apver <- aptab
names(apver) <- c("codele", "codtse", "data", "hora", "apuracao", "turno", "fase", "ncand", "nome", "partido", "votabs", "votpor", "situacao")
apver$cargo <- "vereador"
apver$codibge <- tabMunHist$codIbgeDv[match(apver$codtse, tabMunHist$codTse)]
apver$uf <- tabMunHist$uf[match(apver$codtse, tabMunHist$codTse)]
apver$regiao <- tabMunHist$regiao[match(apver$codtse, tabMunHist$codTse)]
apver$compilado <- Sys.time()

save(apver, file = "ver.RData")
write.csv2(apver, file = "vereadores2020.csv", row.names = F)
save.image()

## PREFEITOS

apx <- 1
aptab <- tibble()

apf <- list.files("prefeito", full.names = T)
#apf2 <- unlist(lapply(apquais, function(x) which(grepl(x, apf))))
#apf <- apf[-apf2]

#aptab2 <- aptab

for(apff in apf) {
  
  apjson <- fromJSON(paste(readLines(apff), collapse=""))
  
  apele <- apjson$ele
  apcod <- apjson$cdabr
  apdt <- apjson$dt
  aphora <- apjson$ht
  apporc <- apjson$psa
  apturno <- apjson$t
  apfase <- apjson$f
  apcands <- apjson$cand
  
  apnumcand <- sapply(apcands, function(x) x$n)
  apnome <- sapply(apcands, function(x) x$nm)
  appart <-  sapply(apcands, function(x) x$cc)
  apvap <-  sapply(apcands, function(x) x$vap)
  appvap <-  sapply(apcands, function(x) x$pvap)
  apele2 <-  sapply(apcands, function(x) x$st)
  
  aptib <- tibble(apele, apcod, apdt, aphora, apporc, apturno, apfase, apnumcand,
                  apnome, appart, apvap, appvap, apele2)
  names(aptib) <- names(aptab)
  aptab <- rbind(aptab, aptib)
  
  print(apx/length(apf)*100)
  apx <- apx + 1
  
}


apver <- aptab
names(apver) <- c("codele", "codtse", "data", "hora", "apuracao", "turno", "fase", "ncand", "nome", "partido", "votabs", "votpor", "situacao")
apver$cargo <- "prefeito"
apver$codibge <- tabMunHist$codIbgeDv[match(apver$codtse, tabMunHist$codTse)]
apver$uf <- tabMunHist$uf[match(apver$codtse, tabMunHist$codTse)]
apver$regiao <- tabMunHist$regiao[match(apver$codtse, tabMunHist$codTse)]
apver$compilado <- Sys.time()

save(apver, file = "pref.RData")
write.csv2(apver, file = "prefeitos2020.csv", row.names = F)
save.image()