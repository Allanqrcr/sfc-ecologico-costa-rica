suppressMessages(library(readxl))
PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.159
PHI<-2.07644144656066; SIG<-0.0410390766992924
S0<-SIG*PHI*PIB22/RHO; D0<-KA*S0
YD22<-PIB22*(1-TH-D0)

cat("=== 1. DE QUE DEPENDE CADA COSA ===\n")
cat("H(-1) = ( PIB * [1 - alpha1*(1-theta-d)] - G ) / alpha2\n")
cat("  -> depende de alpha1 Y de alpha2.\n")
cat("alpha2 * H(-1) = PIB * [1 - alpha1*(1-theta-d)] - G\n")
cat("  -> depende de alpha1 pero NO de alpha2. Por eso al cambiar solo alpha2\n")
cat("     las cifras de 2022 no se movieron.\n\n")
H0<-function(a1,a2) (PIB22*(1-a1*(1-TH-D0))-G)/a2
cat(sprintf("%-8s %-8s %12s %10s\n","alpha1","alpha2","H(2021)","% del PIB"))
for(a1 in c(0.7707,0.8011,0.8509)) for(a2 in c(0.1555)){
  cat(sprintf("%-8.4f %-8.4f %12.1f %9.1f%%\n",a1,a2,H0(a1,a2),100*H0(a1,a2)/PIB22))}
for(a2 in c(0.3595,0.1793,0.1555,0.0855)){
  cat(sprintf("%-8.4f %-8.4f %12.1f %9.1f%%\n",0.8011,a2,H0(0.8011,a2),100*H0(0.8011,a2)/PIB22))}

cat("\n=== 2. LA RIQUEZA OBSERVADA EN LOS DATOS ===\n")
d<-as.data.frame(read_excel("../Datos parámetros/BD para propensiones.xlsx",sheet=1))
names(d)[1:7]<-c("t","C","Y","G","YD","S","H"); d$H_lag<-c(NA,head(d$H,-1))
h21<-d$H[d$t==2021]/510
cat(sprintf("  H(2021) de la base = %.0f millones de colones = %.1f millones USD (TC 510)\n",
    d$H[d$t==2021], h21))
cat(sprintf("  equivale al %.1f%% del PIB de 2022\n",100*h21/PIB22))
cat(sprintf("  razon H/YD observada en 2022 = %.3f\n",h21/YD22))
cat(sprintf("  razon H/YD media de la muestra 1992-2024 = %.3f\n",mean(d$H_lag/d$YD,na.rm=TRUE)))

cat("\n=== 3. DE DONDE VIENE LA DIFERENCIA ===\n")
cat("  El estado estacionario del modelo exige  H*/YD* = (1-alpha1)/alpha2.\n")
cat(sprintf("  Con alpha1=0.8011 y alpha2=0.1555 eso da %.3f, que es justamente la\n",
    (1-0.8011)/0.1555))
cat("  MEDIA de la muestra, porque asi se calibro alpha2.\n")
cat(sprintf("  Pero la razon observada en 2022 es %.3f: la serie tiene tendencia y\n",h21/YD22))
cat("  la media no representa el nivel del anio base.\n")

cat("\n=== 4. LA CALIBRACION QUE RECONCILIA AMBAS COSAS ===\n")
a2_rec<-(PIB22*(1-0.8011*(1-TH-D0))-G)/h21
cat(sprintf("  Si se exige que el modelo reproduzca el PIB de 2022 Y la riqueza\n"))
cat(sprintf("  observada de 2022, alpha2 queda determinado: alpha2 = %.4f\n",a2_rec))
cat(sprintf("  Con ese valor  H*/YD* = (1-alpha1)/alpha2 = %.3f  vs observado %.3f\n",
    (1-0.8011)/a2_rec, h21/YD22))
cat("  Es decir: la economia de 2022 queda simultaneamente en su PIB y en su\n")
cat("  riqueza de estado estacionario. Es la calibracion mas coherente.\n")
