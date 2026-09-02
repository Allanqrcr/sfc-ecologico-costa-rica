## Diagrama de flujo del modelo SFC-E (punto A.17 del compendio)
png(file.path("salidas","Figura_diagrama_modelo.png"),
    width = 3300, height = 2200, res = 300)
par(mar = c(0,0,2.4,0), xaxs = "i", yaxs = "i")
plot(NA, xlim = c(0,100), ylim = c(0,70), axes = FALSE, xlab = "", ylab = "")

AZ<-"#1F3864"; AZC<-"#D9E2F3"; VE<-"#1E5631"; VEC<-"#DCEBD8"
NAR<-"#7F5200"; NARC<-"#FBE7C6"; GR<-"#555555"; RO<-"#B00000"

caja <- function(x,y,w,txt,bo,re,cex=0.72,h=5.5){
  rect(x-w/2,y-h/2,x+w/2,y+h/2,border=bo,col=re,lwd=2)
  text(x,y,txt,cex=cex,font=2,col=bo)
}
fl <- function(x1,y1,x2,y2,col=GR,lwd=1.8,lty=1)
  arrows(x1,y1,x2,y2,length=0.085,angle=22,col=col,lwd=lwd,lty=lty)
etq <- function(x,y,t,col=GR,cex=0.58,srt=0)
  text(x,y,t,cex=cex,col=col,font=3,srt=srt)

title(main="Estructura del modelo SFC-E aplicado a Costa Rica",
      cex.main=1.08,col.main=AZ,font.main=2,line=0.5)

## ================= BLOQUE MACROECONÓMICO-FINANCIERO =================
rect(2,16,46,65,border=AZ,col=NA,lwd=1.2,lty=3)
text(24,63.4,"BLOQUE MACROECONÓMICO-FINANCIERO",col=AZ,cex=0.7,font=2)

caja(12,56,17,"Gasto público  G",AZ,AZC)
caja(33,56,17,"Consumo  C",AZ,AZC)
caja(22.5,47.5,24,"Producto  Y = C + G",AZ,AZC)
caja(11,39,15,"Empleo  N = Y / W",AZ,AZC,0.66)
caja(31,39,15,"Impuestos  T = θ · Y",AZ,AZC,0.66)
caja(22.5,30,36,"Ingreso disponible  YD = Y − T − Daño",AZ,AZC,0.70)
caja(22.5,21,36,"Riqueza  H = H(−1) + YD − C",AZ,AZC,0.70)

fl(12,53.25,18,50.25); fl(33,53.25,27,50.25)
fl(16,44.75,11,41.75); fl(29,44.75,31,41.75)
fl(21,44.75,21,32.75); etq(19.2,38.5,"producto",AZ,0.55,90)
fl(31,36.25,31,32.75)
fl(22.5,27.25,22.5,23.75); etq(24.6,25.5,"ahorro",AZ,0.55)

## YD -> C  (función de consumo), por la derecha
segments(40.5,30,43.5,30,col=AZ,lwd=1.8); segments(43.5,30,43.5,56,col=AZ,lwd=1.8)
fl(43.5,56,41.5,56,AZ); etq(45.1,43,"función de consumo",AZ,0.56,90)
## H(-1) -> C  (efecto riqueza), por la izquierda
segments(4.5,21,3.2,21,col=AZ,lwd=1.8); segments(3.2,21,3.2,60.5,col=AZ,lwd=1.8)
segments(3.2,60.5,33,60.5,col=AZ,lwd=1.8); fl(33,60.5,33,58.75,AZ)
etq(4.9,41,"efecto riqueza",AZ,0.56,90)

## ========================= BLOQUE BIOFÍSICO =========================
rect(52,16,98,65,border=VE,col=NA,lwd=1.2,lty=3)
text(75,63.4,"BLOQUE BIOFÍSICO",col=VE,cex=0.7,font=2)

caja(66,56,24,"Energía  E = φ · Y",VE,VEC)
caja(66,47.5,24,"Emisiones  CO2 = σ · E",VE,VEC,0.70)
caja(75,39,38,"Acervo de emisiones acumuladas  S = (1 − ρ) · S(−1) + CO2",VE,VEC,0.66)
caja(75,30,24,"Daño  d = κ · S",VE,VEC)

fl(34.5,48.6,54,55.2,VE); etq(44,53.4,"φ",VE,0.85)
fl(66,53.25,66,50.25,VE)
fl(66,44.75,70,41.75,VE)
fl(75,36.25,75,32.75,VE)

## ====================== RETROALIMENTACIÓN ======================
fl(63,30,41,30,RO,2.4)
text(52,33.4,"el daño reduce el ingreso disponible",col=RO,cex=0.62,font=2)
text(52,27.6,"y tiene contrapartida en el balance",col=RO,cex=0.56,font=3)
text(52,25.8,"del gobierno (gasto de adaptación)",col=RO,cex=0.56,font=3)

## ====================== PARÁMETROS DE ESCENARIO ======================
rect(2,2,98,14,border=NAR,col=NARC,lwd=2)
text(50,12.2,"PARÁMETROS QUE DEFINEN CADA ESCENARIO",col=NAR,cex=0.74,font=2)
px<-c(14,38,62,86)
pl<-c("G  gasto público","θ  tasa impositiva efectiva",
      "φ  intensidad energética","σ  intensidad de carbono")
pd<-c("Cuentas Nacionales 2022","reproduce el déficit de 2022",
      "Balance de Energía 2022","Inventario GEI y metas del PND")
for(i in 1:4){ text(px[i],9.3,pl[i],col=NAR,cex=0.64,font=2)
               text(px[i],7.4,pd[i],col=NAR,cex=0.55,font=3) }
text(50,5.0,"Fijos en todos los escenarios:   W  productividad    a1, a2  propensiones a consumir    ρ  disipación del acervo    κ  sensibilidad del daño",
     col=NAR,cex=0.6)
text(50,3.2,"Escenarios:  (1) sin bloque biofísico   ·   (2) global mixto   ·   (3) totalmente renovable   ·   (4) Plan Nacional de Descarbonización",
     col=NAR,cex=0.58,font=3)
dev.off(); cat("Diagrama generado.\n")
