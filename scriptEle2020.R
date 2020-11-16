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
    
    #apele <- apjson$ele
    #apcod <- apjson$cdabr
    #apdt <- apjson$dt
    #aphora <- apjson$ht
    #apporc <- apjson$psa
    #apturno <- apjson$t
    #apfase <- apjson$f
    #apcands <- apjson$cand
    
    
    #for(apc in apcands) {
      
    #  apnumcand <- apc$n
    #  apnome <- apc$nm
    #  appart <- apc$cc
    #  apvap <- apc$vap
    #  appvap <- apc$pvap
    #  apele2 <- apc$st
      
    #  apline <- c(apele, 
    #              apcod,
    #              apdt, 
    #              aphora, 
    #              apporc, 
    #              apturno, 
    #              apfase, 
    #              apnumcand,
    #              apnome,
    #              appart,
    #              apvap,
    #              appvap,
    #              apele2)
      
    #  aptab <- rbind(aptab, apline)
      
     #}
  
  }

}

aplast <- aptab$X.00019.[nrow(aptab)]
apquais <- unique(aptab$X.00019.)
apquais <- apquais[apquais != aplast]
aptab <- aptab %>% filter(X.00019. != aplast)

apf <- list.files("vereador", full.names = T)
apf2 <- unlist(lapply(apquais, function(x) which(grepl(x, apf))))
apf <- apf[-apf2]
apx <- 1

aptab2 <- aptab

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
save(apver, file = "ver.RData")
write.csv2(apver, file = "vereadores2020.csv", row.names = F)
save.image()

#atualização
apver <- read.csv2("vereadores2020.csv", colClasses = rep("character", 14))
apver$codibge <- tabMunHist$codIbgeDv[match(apver$codtse, tabMunHist$codTse)]
apver$uf <- tabMunHist$uf[match(apver$codibge, tabMunHist$codIbgeDv)]
apver$regiao <- tabMunHist$regiao[match(apver$codibge, tabMunHist$codIbgeDv)]

############ Banco 2020 #############################################3
clsap()
apver <- read.csv2("vereadores2020.csv", colClasses = rep("character", 14)) 
apver$apuracao <- gsub(",", "\\.", apver$apuracao)
apver[,c(5, 11)] <- lapply(apver[,c(5, 11)], as.numeric)
apver <- apver %>% filter(apuracao == 100)
apver$part <- tabPart$simpAtual[match(apver$partido, tabPart$original)]

ap2 <- apver %>% group_by(part) %>% summarise(vot = sum(votabs)) %>% mutate(porc = vot/sum(vot)*100)
apmun <- unique(apver$codtse)
ap3 <- bd01Ver %>% filter(ano == 2016, codMun %in% apmun) %>% group_by(part) %>% summarise(vot = sum(votosNom)) %>% mutate(porc = vot/sum(vot)*100)
ap2$votabs16 <- ap3$vot[match(ap2$part, ap3$part)]  
ap2$votpor16 <- ap3$porc[match(ap2$part, ap3$part)]
ap2$diff <- ((ap2$porc/ap2$votpor16)-1)*100
ap2$diffabs <- ap2$vot - ap2$votabs16
ap2$diffpor <- ap2$porc-ap2$votpor16
ap2 <- ap2 %>% arrange(desc(diffabs))
ap2 <- ap2[,c(1, 2, 4, 3, 5, 8, 6, 7)]

ap2 %>%
  kable("latex",
        booktabs = T,
        digits = 1,
        col.names = c("partido", "2020", "2016", "2020", "2016", "\\%", "crescimento", "abs"),
        #caption = '\\label{tab:xyz}Nome da tabela (lugar, ano)',
        escape = F,
        format.args = list(decimal.mark = ','),
        #align = c("l", rep("c", 3)),
        linesep = "",
        row.names = F) %>%
  #kable_styling(font_size = 8, latex_options = "hold_position") %>%
  #add_header_above(c(" " = 1, "Modelos" = 3)) %>%
  footnote(#number = c("Erros-padrão robustos",
            #          "Significância: $^{*}$p$<$0,05; $^{**}$p$<$0,01; $^{***}$p$<$0,001;",
            #          "Todos os modelos estão controlados pela variável uf"),
           #general_title = "",
           #general = "Fonte: Elaborado pelos autores a partir de dados do TSE.",
           threeparttable = T,
           #footnote_order = c("number", "general"),
           escape = F) %>%
#collapse_rows(columns = 1:2, valign = "middle", latex_hline = "none") %>%

  #add_indent(c(3, 4, 6, 9, 10, 13, 14)) %>%
#row_spec(15, hline_after = TRUE, extra_latex_after = "\\rule{0pt}{3ex}") %>%
save_kable(file = "tabelas/a01.png")
set