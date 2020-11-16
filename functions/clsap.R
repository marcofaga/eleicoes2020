# Função em R que remove da memória todos os objetos que começam com a sigla "ap".
# versão: 1.0

clsap <- function() {
  
  workspace <- ls(envir = globalenv())
  ap <- workspace[substr(workspace, 1, 2) == "ap"]
  rm(list = workspace[workspace %in% ap], pos = ".GlobalEnv")

}
