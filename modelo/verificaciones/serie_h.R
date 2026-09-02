suppressMessages(library(readxl))
d<-as.data.frame(read_excel("../Datos parámetros/BD para propensiones.xlsx",sheet=1))
names(d)[1:7]<-c("t","C","Y","G","YD","S","H")
cat("Construccion de la serie H en la hoja de calculo:\n")
cat("  H(1991) = S(1991) =",format(round(d$H[1]),big.mark=",")," <- supone riqueza CERO antes de 1991\n")
cat("  H(t)    = H(t-1) + YD(t) - C(t)\n\n")
cat("Razon H/YD a lo largo del tiempo:\n")
for(a in c(1991,1995,2000,2005,2010,2015,2020,2022,2024)){
  i<-which(d$t==a); cat(sprintf("  %d : %.3f\n",a,d$H[i]/d$YD[i]))}
cat("\nSube de forma monotona: la serie parte de un nivel arbitrario y se va\n")
cat("llenando. Promediar toda la muestra mezcla anios no comparables.\n")
