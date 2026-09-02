suppressMessages(library(readxl))
prop <- as.data.frame(read_excel("../Datos parámetros/BD para propensiones.xlsx", sheet=1))
names(prop)[1:7] <- c("t","C","Y","G","YD","S","H")
pib <- as.data.frame(read_excel("../Datos parámetros/PIB composición.xlsx", sheet="Hoja1", col_names=FALSE))

cat("ENCABEZADO DEL ARCHIVO DEL BCCR (PIB composición.xlsx):\n")
cat("  ", as.character(pib[1,1]), "\n")
cat("  ", as.character(pib[2,1]), "\n\n")

## fila 5 = anios ; fila 9 = PIB ; fila 12 = consumo hogares ; fila 13 = consumo gobierno
anios <- suppressWarnings(as.numeric(unlist(pib[5,])))
getf <- function(fila, anio) suppressWarnings(as.numeric(pib[fila, which(anios==anio)]))

cat("COTEJO: BD para propensiones  vs  PIB composición (BCCR)\n")
cat(sprintf("%-6s %-22s %18s %18s %8s\n","Anio","Variable","BD propensiones","BCCR","Coincide"))
for(a in c(1991,2000,2010,2022,2024)){
  i <- which(prop$t==a)
  for(v in list(c("Y","PIB precios mercado",9), c("C","Consumo hogares",12), c("G","Consumo gobierno",13))){
    x <- prop[[v[1]]][i]; y <- getf(as.numeric(v[3]), a)
    cat(sprintf("%-6d %-22s %18.2f %18.2f %8s\n", a, v[2], x, y, ifelse(abs(x-y)<0.01,"SI","NO")))}}

cat("\nMAGNITUD DEL PROBLEMA:\n")
cat(sprintf("  PIB nominal 1991 = %s  ->  2024 = %s   (x%.1f)\n",
  format(round(prop$Y[prop$t==1991]),big.mark=","), format(round(prop$Y[prop$t==2024]),big.mark=","),
  prop$Y[prop$t==2024]/prop$Y[prop$t==1991]))
