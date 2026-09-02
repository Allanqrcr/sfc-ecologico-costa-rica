PIB22<-87862.80504186137; ETOT<-63577.8+118864.17
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
SIG0<-c(0.0375607072618798,0.034431221957007,0.0312981162551694,0.0281686828642582,0.0250392494477191,0.0219098160230718)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIGC<-META/(PHI*PIB22); ANIOS<-c(2025,2030,2035,2040,2045,2050)

cat("=== CONTRA QUE SE CALIBRO sigma, Y CONTRA QUE LO USA EL MODELO ===\n")
cat(sprintf("  Energia total del Balance 2022                = %9.0f TJ\n", ETOT))
cat(sprintf("  Energia que implica el modelo, phi x PIB2022:\n"))
cat(sprintf("    tramo 2025: %9.0f TJ   tramo 2050: %9.0f TJ\n", PHI[1]*PIB22, PHI[6]*PIB22))
cat("  La intensidad energetica baja en cada tramo por la ganancia de eficiencia,\n")
cat("  de modo que la energia del modelo se aleja cada vez mas de la del Balance.\n\n")

cat(sprintf("%6s %11s %14s %14s %9s %14s %9s\n","anio","meta Gg",
    "sigma x Etot","sigma x phi x Y","desvio","sigma corregida","desvio"))
for(i in 1:6){
  a<-SIG0[i]*ETOT; b<-SIG0[i]*PHI[i]*PIB22; c<-SIGC[i]*PHI[i]*PIB22
  cat(sprintf("%6d %11.1f %14.1f %14.1f %8.1f%% %14.1f %8.1f%%\n",
    ANIOS[i],META[i],a,b,100*(b/META[i]-1),c,100*(c/META[i]-1)))}

cat("\n=== EL PARAMETRO ===\n")
cat(sprintf("%6s %16s %16s %8s\n","anio","sigma original","sigma corregida","cambio"))
for(i in 1:6) cat(sprintf("%6d %16.7f %16.7f %7.1f%%\n",ANIOS[i],SIG0[i],SIGC[i],100*(SIGC[i]/SIG0[i]-1)))
