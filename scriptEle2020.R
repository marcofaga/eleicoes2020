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

# Pegando dados de partido
load("tabPart2011180005.RData")

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

# atualizando banco só com municípios que não tiveram 100% das urnas apuradas.
bd08Ver2020 <- read.csv2("vereadores2020.csv", colClasses = rep("character", 18))


apcodtse <- unique(bd08Ver2020$codtse[bd08Ver2020$situacao == ""])
apx <- 1

for(apu in apcodtse) {
  
  apufs <- unique(tolower(bd08Ver2020$uf[bd08Ver2020$codtse == apu]))

  aplink <- sprintf("https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados-simplificados/%s/%s%s-c0013-e000426-r.json", apufs, apufs, apu)
  apjson <- fromJSON(paste(readLines(aplink), collapse=""))
  apjson <- toJSON(apjson)
  
  apname <- paste0("vereador/", apu, ".json")
  write(apjson, apname)
  
  print(apx/length(apcodtse)*100)
  apx <- apx + 1
  
}

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

apcodtse <- unique(apver$codtse)
bd08Ver2020 <- bd08Ver2020 %>% filter(!codtse %in% apcodtse)
bd08Ver2020 <- rbind(bd08Ver2020, apver)
save(bd08Ver2020, file = "vereador.RData")
write.csv2(bd08Ver2020, file = "vereadores2020.csv", row.names = F)
dateEle2020 <- Sys.time()
save.image()

## Atualizando Prefeitos
# atualizando banco só com municípios que não tiveram 100% das urnas apuradas.
bd07Pref2020 <- read.csv2("prefeitos2020.csv", colClasses = rep("character", 18))


apcodtse <- unique(bd07Pref2020$codtse[bd07Pref2020$situacao == ""])
apx <- 1

for(apu in apcodtse) {
  
  apufs <- unique(tolower(bd07Pref2020$uf[bd07Pref2020$codtse == apu]))
  
  aplink <- sprintf("https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados-simplificados/%s/%s%s-c0011-e000426-r.json", apufs, apufs, apu)
  apjson <- fromJSON(paste(readLines(aplink), collapse=""))
  apjson <- toJSON(apjson)
  
  apname <- paste0("prefeito/", apu, ".json")
  write(apjson, apname)
  
  print(apx/length(apcodtse)*100)
  apx <- apx + 1
  
}

# compilando vereador
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

apcodtse <- unique(apver$codtse)
bd07Pref2020 <- bd07Pref2020 %>% filter(!codtse %in% apcodtse)
bd07Pref2020 <- rbind(bd07Pref2020, apver)
save(bd07Pref2020, file = "prefeito.RData")
write.csv2(bd07Pref2020, file = "prefeitos2020.csv", row.names = F)
clsap()
save.image()

bd08Ver2020$compilado <- Sys.time()
bd07Pref2020$compilado <- Sys.time()
save(bd07Pref2020, file = "prefeito.RData")
write.csv2(bd07Pref2020, file = "prefeitos2020.csv", row.names = F)
save(bd08Ver2020, file = "vereador.RData")
write.csv2(bd08Ver2020, file = "vereadores2020.csv", row.names = F)
save.image()

### Dados de partidos.

apf <- list.files("getjson/jsons", full.names = T)

aptab2 <- tibble()
apl <- length(apf)
apx <- 1

for(apff in apf) {
  
  apjson <- fromJSON(paste(readLines(apff), collapse=""))
  
  apele <- apjson$ele
  apdt <- apjson$dg
  aphora <- apjson$hg
  apmun <- apjson$abr[[1]]$cdabr
  
  apcands <- apjson$abr[[1]]$part
  
  apnpart <- as_tibble(t(sapply(apcands, function(x) x)))
  aptib <- tibble(apele, apdt, aphora, apmun, apnpart)
  aptab2 <- rbind(aptab2, aptib)
  
  print(apx/apl*100)
  apx <- apx + 1
  
}

names(aptab2) <- c("codele", "data", "hora", "codtse", "npart", "nominaistotalizados", "nominaiscomputados", "legendatotalizados", "legendacomputados")
aptab2[,c(6:9)] <- lapply(aptab2[,c(6:9)], as.numeric)
aptab2$validostotalizados <- aptab2$nominaistotalizados + aptab2$legendatotalizados
aptab2$validoscomputados <- aptab2$nominaiscomputados + aptab2$legendacomputados

aptab2$uf <- tabMunHist$uf[match(aptab2$codtse, tabMunHist$codTse)]
aptab2$codibge <- tabMunHist$codIbgeDv[match(aptab2$codtse, tabMunHist$codTse)]
aptab2$regiao <- tabMunHist$regiao[match(aptab2$codtse, tabMunHist$codTse)]
aptab2$siglapart <- tabPart$simpAtual[match(aptab2$npart, tabPart$num)]
aptab2 <- aptab2[,c(1:5, 15, 6:14)]
bd09Part2020 <- aptab2
bd09Part2020$cargo <- "vereador"
bd09Part2020$ano <- "2020"
bd09Part2020$nomemun <- bd03Pref$nomMun[match(bd09Part2020$codtse, bd03Pref$codMun)]
bd09Part2020$npart <- as.character(bd09Part2020$npart)
write.csv2(bd09Part2020, file = "partidos2020.csv", row.names = F)

####################################################################################
clsap()
dateEle2020 <- Sys.time()
save.image()
