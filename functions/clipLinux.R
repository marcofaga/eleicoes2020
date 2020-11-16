#permite copiar para o clipboard no Linux
#instruções para instalação:
#https://stackoverflow.com/questions/10959521/how-to-write-to-clipboard-on-ubuntu-linux-in-r
#É preciso ter isntalado o xclip
#sudo pacman -S xclip


clip <- function(x, sep="\t", row.names=FALSE, col.names=TRUE) {
  
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
  close(con)
  
}