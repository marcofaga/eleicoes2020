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

### Dados de vereador

apf <- list.files("getjson/jsons/vereadores", full.names = T)

aptab <- tibble()
apl <- length(apf)
apx <- 1

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
  #stop()
  
}

names(aptab) <- c("codele", "codtse", "data", "hora", "apuracao", "turno", "divulgação", "ncand", "nomeurna", "partido", "votosabs", "votosporc", "situacao")
aptab[,c(1:13)] <- lapply(aptab[,c(1:13)], function(x) trimws(x, "both"))
aptab[,c(9,13)] <- lapply(aptab[,c(9,13)], function(x) iconv(x,to="ASCII//TRANSLIT"))
aptab$situacao <- toupper(aptab$situacao)
aptab$divulgação <- "oficial"
aptab$cargo <- "vereador"
aptab$uf <- tabMunHist$uf[match(aptab$codtse, tabMunHist$codTse)]
aptab$codibge <- tabMunHist$codIbgeDv[match(aptab$codtse, tabMunHist$codTse)]
aptab$regiao <- tabMunHist$regiao[match(aptab$codtse, tabMunHist$codTse)]
aptab$ano <- "2020"
aptab$nomemun <- tabMunHist$nomeMun[match(aptab$codtse, tabMunHist$codTse)]
aptab$situacao[aptab$situacao == ""] <- "NA"
bd01Ver <- aptab
apx <- substr(bd01Ver$ncand, 1, 2)
bd01Ver$partido <- tabPart$simpAtual[match(apx, tabPart$num)]
  
write.csv2(bd01Ver, file = "vereador2020.csv", row.names = F)


### Dados de prefeito

apf <- list.files("getjson/jsons/prefeitos", full.names = T)

aptab <- tibble()
apl <- length(apf)
apx <- 1

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
  #stop()
  
}

names(aptab) <- c("codele", "codtse", "data", "hora", "apuracao", "turno", "divulgação", "ncand", "nomeurna", "partido", "votosabs", "votosporc", "situacao")
aptab[,c(1:13)] <- lapply(aptab[,c(1:13)], function(x) trimws(x, "both"))
aptab[,c(9,13)] <- lapply(aptab[,c(9,13)], function(x) iconv(x,to="ASCII//TRANSLIT"))
aptab$situacao <- toupper(aptab$situacao)
aptab$divulgação <- "oficial"
aptab$cargo <- "prefeito"
aptab$uf <- tabMunHist$uf[match(aptab$codtse, tabMunHist$codTse)]
aptab$codibge <- tabMunHist$codIbgeDv[match(aptab$codtse, tabMunHist$codTse)]
aptab$regiao <- tabMunHist$regiao[match(aptab$codtse, tabMunHist$codTse)]
aptab$ano <- "2020"
aptab$nomemun <- tabMunHist$nomeMun[match(aptab$codtse, tabMunHist$codTse)]
aptab$situacao[aptab$situacao == ""] <- "NA"
aptab$partido <- tabPart$simpAtual[match(aptab$ncand, tabPart$num)]
bd03Pref <- aptab

write.csv2(bd03Pref, file = "prefeito2020.csv", row.names = F)


####################################################################################
clsap()
dateEle2020 <- Sys.time()
save.image()
