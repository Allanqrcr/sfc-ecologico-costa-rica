###############################################################################
##  SFC-E Costa Rica — VERSIÓN REVISADA FINAL
##  Incorpora todas las correcciones derivadas del dictamen de REVIBEC y de la
##  verificación del código y de las hojas de cálculo. Produce las cifras que
##  se citan en las secciones redactadas del artículo.
###############################################################################
suppressMessages(library(sfcr))
OUT <- "salidas"; if (!dir.exists(OUT)) dir.create(OUT)

## --- 1. Parámetros del año base 2022 ---------------------------------------
PIB22  <- 87862.80504186137     # PIB 2022, millones USD constantes (TC = 510)
OCUP22 <- 2172975               # ocupados 2022
W      <- 0.0404327375264023    # productividad media del trabajo
G      <- 13527.2509115691      # gasto de consumo final del gobierno
TH     <- 0.129036878894848     # tasa impositiva efectiva (reproduce el déficit 2022)
RHO    <- 0.218080022778761     # tasa de absorción
ETOT22 <- 63577.8 + 118864.17   # energía total 2022 (TJ)

## --- 2. Factor de daño -----------------------------------------------------
##  La hoja de calibración original calculaba 60/1,1, lo que invierte la
##  conversión de euros a dólares. KAPPA0 es aquel valor; el factor lo corrige.
KAPPA0 <- 6.20802562807628E-07
## El referente de 60 EUR/t de OCDE (2021) esta referenciado a 2020. Se convierte
## al tipo de cambio de 2020 (1.14220 USD/EUR) y se expresa en dolares de 2022
## aplicando el deflactor del PIB de EE.UU. (112.000/100.000 = 1.12).
## Factor sobre el valor original de la hoja (que usaba 60/1.1): 1.14220*1.12*1.1
KAPPA  <- KAPPA0 * 1.4071907

## --- 3. Propensiones a consumir --------------------------------------------
##  (véase Datos parámetros/Propensiones_riqueza_alternativas.R)
##  alpha1 se ESTIMA en primeras diferencias sobre series deflactadas; el valor
##  es robusto a la definición de riqueza empleada (0,7168 a 0,7271).
##  alpha2 se CALIBRA, no se estima: en el modelo SIM fija la razón riqueza
##  sobre ingreso del estado estacionario, (1-alpha1)/alpha2. Se ancla en la
##  razón observada entre déficit público acumulado e ingreso disponible en el
##  año base, que vale 0,76.
ALPHA1 <- 0.7219               # estimado
ALPHA2 <- 0.3681               # calibrado = (1 - ALPHA1) / 0.76

## --- 4. Escenarios: intensidad energética (phi) y de carbono (sigma) -------
PHI_GLOBAL <- 2.07644144656066 ; SIG_GLOBAL <- 0.0410390766992924
PHI_RENOV  <- 1.28975587832427 ; SIG_RENOV  <- 0.00882132065193575
PHI_PND    <- c(1.94815033931687, 1.8455609450008, 1.75313427460553,
                1.66961697683439, 1.59369517358915, 1.52437777251918)
META_PND   <- c(6852.646424416003, 6281.7042423699995, 5710.091940392017,
                5139.149758346013, 4568.207576300009, 3997.2653942540055)

## --- 5. Recalibración de la intensidad de carbono del escenario del plan ---
##  sigma se calibra contra la energía que el modelo implica (phi x PIB), no
##  contra la energía total del balance del año base. La diferencia entre ambas
##  bases es lo que hacía que el escenario sobrecumpliera las metas del plan.
SIG_PND_ORIG <- c(0.0375607072618798, 0.034431221957007, 0.0312981162551694,
                  0.0281686828642582, 0.0250392494477191, 0.0219098160230718)
SIG_PND      <- META_PND / (PHI_PND * PIB22)

cat("=== A.15 RECALIBRACIÓN DE LA INTENSIDAD DE CARBONO DEL ESCENARIO PND ===\n")
cat("sigma reproducía la meta contra la energía del balance, pero el modelo\n")
cat("calcula sigma*phi*Y, y phi*PIB cae de", round(PHI_PND[1]*PIB22), "a",
    round(PHI_PND[6]*PIB22), "TJ frente a", round(ETOT22), "TJ del balance.\n\n")
print(data.frame(anio = c(2025,2030,2035,2040,2045,2050),
  meta_Gg = round(META_PND,1),
  sigma_x_Etot = round(SIG_PND_ORIG*ETOT22,1),
  emis_con_original = round(SIG_PND_ORIG*PHI_PND*PIB22,1),
  desvio_pct = round(100*(SIG_PND_ORIG*PHI_PND*PIB22/META_PND-1),1),
  sigma_corregida = signif(SIG_PND,6)))

## --- 6. Modelo: solucionador analítico, consistente stock-flujo ------------
simular <- function(phi, sigma, n = 29, Hh0, Cstock0, Gd = G, theta = TH,
                    rho = RHO, kappa = KAPPA, a1 = ALPHA1, a2 = ALPHA2) {
  if (length(phi) == 1)   phi   <- rep(phi, n)
  if (length(sigma) == 1) sigma <- rep(sigma, n)
  o <- data.frame(period = 1:n, anio = 2021 + 1:n)
  Hh_1 <- Hh0; Cs_1 <- Cstock0
  for (t in 1:n) {
    f <- function(Y) {
      d <- kappa * ((1 - rho) * Cs_1 + sigma[t] * phi[t] * Y)
      (a2 * Hh_1 + Gd) / (1 - a1 * (1 - theta - d)) - Y
    }
    Y  <- uniroot(f, c(1, 1e7), tol = 1e-10)$root
    CO2 <- sigma[t] * phi[t] * Y; Cst <- (1 - rho) * Cs_1 + CO2; d <- kappa * Cst
    YD <- Y * (1 - theta - d); Cd <- a1 * YD + a2 * Hh_1; Hh <- Hh_1 + YD - Cd
    o[t, c("Y","Y_eff","E","CO2","Cstock","damage","YD","Cd","Hh","TXd","Ns","def")] <-
      c(Y, Y*(1-d), phi[t]*Y, CO2, Cst, d, YD, Cd, Hh, theta*Y, Y/W,
        100*(Gd - theta*Y)/Y)
    Hh_1 <- Hh; Cs_1 <- Cst
  }
  o
}

## --- 7. Calibración de las condiciones iniciales de 2022 -------------------
CO2_22   <- SIG_GLOBAL * PHI_GLOBAL * PIB22
CSTOCK_0 <- CO2_22 / RHO
D_22     <- KAPPA * CSTOCK_0
HH_0     <- (PIB22 * (1 - ALPHA1 * (1 - TH - D_22)) - G) / ALPHA2

## Cada escenario tiene un daño distinto ya en el primer período, porque su
## composición energética difiere. Para que los cuatro arranquen en el producto
## observado de 2022 —y las variaciones porcentuales resulten comparables entre
## sí— la riqueza inicial se despeja por escenario. El acervo de partida es
## común: es el estado ambiental heredado, que no depende del escenario.
H0_escenario <- function(phi1, sigma1, kappa1 = KAPPA) {
  d0 <- kappa1 * ((1 - RHO) * CSTOCK_0 + sigma1 * phi1 * PIB22)
  (PIB22 * (1 - ALPHA1 * (1 - TH - d0)) - G) / ALPHA2
}

cat("\n=== CALIBRACIÓN 2022 ===\n")
cat("Emisiones 2022 =", round(CO2_22,1), "Gg | stock inicial =", round(CSTOCK_0,1),
    "Gg | daño 2022 =", round(100*D_22,3), "% del PIB\n")
cat("Riqueza inicial de los hogares =", round(HH_0,1), "millones USD (",
    round(100*HH_0/PIB22,1), "% del PIB )\n")

## --- 8. Simulación de los cuatro escenarios --------------------------------
phi_t <- approx(c(2022,2025,2030,2035,2040,2045,2050), c(PHI_GLOBAL, PHI_PND), xout = 2022:2050)$y
sig_t <- approx(c(2022,2025,2030,2035,2040,2045,2050), c(SIG_GLOBAL, SIG_PND), xout = 2022:2050)$y

s1 <- simular(PHI_GLOBAL, SIG_GLOBAL, kappa = 0, Cstock0 = CSTOCK_0,
              Hh0 = H0_escenario(PHI_GLOBAL, SIG_GLOBAL, kappa1 = 0))
s2 <- simular(PHI_GLOBAL, SIG_GLOBAL, Cstock0 = CSTOCK_0, Hh0 = HH_0)
s3 <- simular(PHI_RENOV,  SIG_RENOV,  Cstock0 = CSTOCK_0,
              Hh0 = H0_escenario(PHI_RENOV, SIG_RENOV))
s4 <- simular(phi_t,      sig_t,      Cstock0 = CSTOCK_0, Hh0 = HH_0)

res <- function(d, n) data.frame(escenario = n,
  PIB_2022 = round(d$Y[1],1), PIB_2050 = round(d$Y[29],1),
  PIB_efectivo_2050 = round(d$Y_eff[29],1),
  var_PIB_pct = round(100*(d$Y[29]/d$Y[1]-1),2),
  emis_2022 = round(d$CO2[1],1), emis_2050 = round(d$CO2[29],1),
  var_emis_pct = round(100*(d$CO2[29]/d$CO2[1]-1),2),
  stock_2050 = round(d$Cstock[29],1), dano_2050_pct = round(100*d$damage[29],3),
  empleo_2050 = round(d$Ns[29]), var_empleo_pct = round(100*(d$Ns[29]/d$Ns[1]-1),2),
  deficit_2050_pct = round(d$def[29],3))

cuadro <- rbind(res(s1,"1. SFC sin componente ecologico"), res(s2,"2. SFC-E global"),
                res(s3,"3. Totalmente renovable"), res(s4,"4. PND"))
write.csv(cuadro, file.path(OUT,"cuadro_sintesis_FINAL.csv"), row.names=FALSE, fileEncoding="UTF-8")
cat("\n=== CUADRO DE SÍNTESIS FINAL ===\n"); print(t(cuadro))

ser <- rbind(cbind(s1, esc="1. Sin componente ecologico"), cbind(s2, esc="2. Global"),
             cbind(s3, esc="3. Totalmente renovable"),     cbind(s4, esc="4. PND"))
write.csv(ser, file.path(OUT,"series_FINAL.csv"), row.names=FALSE, fileEncoding="UTF-8")

cat("\n=== EMISIONES DEL ESCENARIO PND FRENTE A LA META ===\n")
idx <- match(c(2025,2030,2035,2040,2045,2050), s4$anio)
print(data.frame(anio=c(2025,2030,2035,2040,2045,2050), meta_Gg=round(META_PND,1),
      modelo_Gg=round(s4$CO2[idx],1), desvio_pct=round(100*(s4$CO2[idx]/META_PND-1),1)))


###############################################################################
## --- 9. VERIFICACIONES ----------------------------------------------------
##
##  Son las cuatro comprobaciones que la sección 3.10 del artículo reporta.
##  Se ejecutan aquí para que la afirmación sea reproducible desde este mismo
##  archivo. Las tres primeras se apoyan en la librería sfcr, que resuelve el
##  sistema completo con un algoritmo iterativo independiente del solucionador
##  analítico empleado en la sección 6; la cuarta las contrasta entre sí.
###############################################################################

eqs_ver <- sfcr_set(
  TXs ~ TXd, Nd ~ Y / W, Ns ~ Nd, DAM ~ damage * Y,
  YD  ~ W * Ns - TXs - DAM,
  Cd  ~ alpha1 * YD + alpha2 * Hh[-1], Cs ~ Cd, Gs ~ Gd, Y ~ Cs + Gs,
  TXd ~ theta * W * Ns,
  Hh  ~ YD - Cd + Hh[-1],
  Hs  ~ Gd - TXd - DAM + Hs[-1],
  E   ~ phi * Y, CO2 ~ sigma * E,
  Cstock ~ Cstock[-1] + CO2 - rho * Cstock[-1],
  damage ~ kappa * Cstock, Y_eff ~ Y * (1 - damage))

ext_ver <- eval(parse(text = sprintf(
  "sfcr_set(Gd ~ %.15g, W ~ %.15g, alpha1 ~ %.15g, alpha2 ~ %.15g, theta ~ %.15g,
            phi ~ %.15g, sigma ~ %.15g, rho ~ %.15g, kappa ~ %.15g)",
  G, W, ALPHA1, ALPHA2, TH, PHI_GLOBAL, SIG_GLOBAL, RHO, KAPPA)))

## sfcr fija con `initial` los valores del PERIODO 1, mientras que HH_0 y
## CSTOCK_0 son los acervos rezagados que entran a ese periodo. Se pasan por
## tanto los valores del periodo 1 que produce el solucionador analitico.
ini_ver <- eval(parse(text = sprintf(
  "sfcr_set(Y ~ %.15g, Hh ~ %.15g, Hs ~ %.15g, Cstock ~ %.15g, Cd ~ %.15g, Ns ~ %.15g)",
  s2$Y[1], s2$Hh[1], s2$Hh[1], s2$Cstock[1], s2$Cd[1], s2$Ns[1])))

cat("\n=========== VERIFICACIONES (sección 3.10 del artículo) ===========\n")

## V1. Ecuación redundante. sfcr rechaza el modelo si no se cumple.
v1 <- tryCatch({
  mv <- sfcr_baseline(eqs_ver, ext_ver, periods = 29, initial = ini_ver,
                      hidden = c("Hh" = "Hs"))
  sprintf("sfcr acepta el modelo. max|Hh - Hs| = %.2e", max(abs(mv$Hh - mv$Hs)))},
  error = function(e) paste("RECHAZADO:", conditionMessage(e)))
cat("V1. Ecuación redundante Hh = Hs, verificada con hidden = c('Hh'='Hs'):\n    ", v1, "\n")

mv <- sfcr_baseline(eqs_ver, ext_ver, periods = 29, initial = ini_ver)

## V2. Calibración al año base
cat(sprintf("V2. Producto del período 1 = %.5f  |  PIB observado de 2022 = %.5f\n",
            s2$Y[1], PIB22))
cat(sprintf("    Déficit del período 1 = %.3f %%  |  déficit observado de 2022 = 2,492 %%\n",
            s2$def[1]))
cat(sprintf("    Empleo del período 1 = %.0f  |  ocupados observados en 2022 = %d\n",
            s2$Ns[1], OCUP22))

## V3. Estado estacionario analítico frente al simulado
cat(sprintf("V3. Estado estacionario G/(theta+dano) = %.1f  |  simulado en 2050 = %.1f\n",
            G / (TH + s2$damage[29]), s2$Y[29]))

## V4. Solucionador analítico frente a la librería sfcr
cat(sprintf("V4. Discrepancia relativa máxima en Y entre ambos métodos = %.2e\n",
            max(abs(s2$Y / mv$Y - 1))))

cat("\n=== COMPARACIONES ENTRE ESCENARIOS EN 2050 ===\n")
cmp <- function(a,b,na,nb) cat(sprintf("%-24s vs %-18s: PIB %+6.2f%% | PIB efectivo %+6.2f%% | deficit %.2f vs %.2f pp\n",
  na,nb,100*(a$Y[29]/b$Y[29]-1),100*(a$Y_eff[29]/b$Y_eff[29]-1),a$def[29],b$def[29]))
cmp(s3,s2,"Totalmente renovable","Global"); cmp(s4,s2,"PND","Global"); cmp(s3,s4,"Totalmente renovable","PND")
cat("\nEstado estacionario Y* = G/(theta+dano*):\n")
for(x in list(list(s2,"Global"),list(s3,"Renovable"),list(s4,"PND")))
  cat(sprintf("  %-12s dano* = %.4f -> Y* = %8.1f\n", x[[2]], x[[1]]$damage[29], G/(TH+x[[1]]$damage[29])))
cat("\nIdentidad del deficit en el estado estacionario: deficit/PIB = dano\n")
cat("  porque Y* = G/(theta+dano) implica (G - theta*Y*)/Y* = dano\n")
cat(sprintf("  Continuidad (ya en estado estacionario): dano %.3f%% vs deficit %.3f%%\n",
            100*s2$damage[29], s2$def[29]))
cat(sprintf("  Renovable y PND no lo alcanzan en 2050: deficit %.3f%% y %.3f%%,\n",
            s3$def[29], s4$def[29]))
cat(sprintf("  frente a danos de %.3f%% y %.3f%%\n", 100*s3$damage[29], 100*s4$damage[29]))
cat("\nRiqueza inicial vs estado estacionario H* = (1-a1)*YD*/a2:\n")
Hstar <- (1-ALPHA1)*s2$YD[29]/ALPHA2
cat("  HH_0 =",round(HH_0,1)," | H* =",round(Hstar,1),sprintf(" -> la riqueza de 2022 esta %+.1f%% respecto de su nivel estacionario,",100*(HH_0/Hstar-1)),"
     coherente con un producto que tambien parte por encima y converge a la baja
")

###############################################################################
## 13. ANÁLISIS DE VARIABILIDAD DE LOS RESULTADOS ---------------------------
##
##  El estado estacionario es la raíz de  a*Y^2 + theta*Y - G = 0, donde
##  a = kappa * sigma * phi / rho. Es decir: TODO el bloque biofísico entra
##  por un único parámetro compuesto. De ahí se derivan tres hechos que
##  ordenan la discusión sobre la robustez de los resultados.
###############################################################################

Yss <- function(a) (-TH + sqrt(TH^2 + 4*G*a)) / (2*a)
spG <- SIG_GLOBAL * PHI_GLOBAL
spR <- SIG_RENOV  * PHI_RENOV
spP <- SIG_PND[6] * PHI_PND[6]

cat("\n=========== ANÁLISIS DE VARIABILIDAD ===========\n")
cat("HECHO 1. El modelo depende de un único parámetro compuesto a = kappa*sigma*phi/rho.\n")
cat("HECHO 2. La razón entre escenarios NO depende de kappa ni de rho, que son comunes:\n")
cat(sprintf("         sigma*phi: continuidad %.6f | renovable %.6f (÷%.2f) | plan 2050 %.6f (÷%.2f)\n",
            spG, spR, spG/spR, spP, spG/spP))
cat("         Ese cociente está fijado por el Balance de Energía y el Inventario GEI.\n")
cat("HECHO 3. La elasticidad local de Y* respecto de a es -dano/(theta+2*dano):\n")
aG <- KAPPA*spG/RHO; dG <- aG*Yss(aG)
cat(sprintf("         con dano = %.4f la elasticidad local es %.3f.\n",
            dG, -dG/(TH+2*dG)))
cat(sprintf("         Efectos finitos: duplicar 'a' mueve Y* %.1f%%; a la mitad, %+.1f%%.\n",
            100*(Yss(2*aG)/Yss(aG)-1), 100*(Yss(aG/2)/Yss(aG)-1)))
cat("         La elasticidad es local: no es el efecto de duplicar a.\n")

## Tabla: resultados según el nivel de daño que se suponga para la continuidad
variabilidad <- do.call(rbind, lapply(c(0.010,0.015,0.0292,0.035,0.050,0.070), function(d){
  YG <- G/(TH+d); aG <- d*(TH+d)/G
  YR <- Yss(aG*spR/spG); YP <- Yss(aG*spP/spG)
  data.frame(dano_continuidad_pct = round(100*d,2),
             PIB_continuidad = round(YG), PIB_renovable = round(YR),
             ventaja_renovable_pct = round(100*(YR/YG-1),1),
             PIB_plan = round(YP), ventaja_plan_pct = round(100*(YP/YG-1),1))}))
write.csv(variabilidad, file.path(OUT,"variabilidad_segun_dano.csv"),
          row.names=FALSE, fileEncoding="UTF-8")
cat("\nResultados según el nivel de daño supuesto para la continuidad\n")
cat("(la fila de 2,92 % es la calibración central adoptada):\n")
print(variabilidad, row.names=FALSE)

cat("\nCONCLUSIÓN: la única fuente relevante de incertidumbre es el NIVEL del daño.\n")
cat("Los datos de energía y emisiones, el gasto público, la tasa impositiva y las\n")
cat("propensiones a consumir no afectan a la comparación entre escenarios.\n")

###############################################################################
## 14. COTA INFERIOR: ESPECIFICACIÓN SIN ACUMULACIÓN -------------------------
##
##  El acervo del modelo mide la presión ambiental acumulada de origen
##  doméstico, no el acervo atmosférico de carbono (véanse las secciones 2.2 y
##  3.7 del artículo). El supuesto de acumulación es la decisión de modelación
##  con mayor incidencia sobre la magnitud de los resultados, de modo que se
##  contrasta contra una variante en la que el daño depende del flujo anual de
##  emisiones y no del acervo:  d = kappa_f * CO2.
##  kappa_f se calibra con el mismo precio sombra, de modo que el daño del año
##  base sea el costo social de las emisiones de ese año.
###############################################################################

PRECIO_USD_T <- 60 * 1.14220 * 1.12          # referente OCDE, 2020, en USD de 2022
KAPPA_F <- (PRECIO_USD_T * 1000 / 1e6) / PIB22   # fracción del PIB por Gg emitido

ss_flujo <- function(ph, sg) {
  a <- KAPPA_F * sg * ph
  Y <- (-TH + sqrt(TH^2 + 4 * G * a)) / (2 * a)
  c(Y = Y, dano = a * Y)
}
cat("\n=========== COTA INFERIOR: DAÑO SOBRE EL FLUJO, SIN ACERVO ===========\n")
fl_g <- ss_flujo(PHI_GLOBAL, SIG_GLOBAL)
fl_r <- ss_flujo(PHI_RENOV,  SIG_RENOV)
fl_p <- ss_flujo(PHI_PND[6], SIG_PND[6])
cat(sprintf("  Continuidad          Y* = %8.1f   daño = %.2f %%\n", fl_g[1], 100*fl_g[2]))
cat(sprintf("  Totalmente renovable Y* = %8.1f   ventaja %+.1f %%\n", fl_r[1], 100*(fl_r[1]/fl_g[1]-1)))
cat(sprintf("  Plan (composición 2050) Y* = %8.1f ventaja %+.1f %%\n", fl_p[1], 100*(fl_p[1]/fl_g[1]-1)))
cat("\n  Frente a la especificación adoptada, con acumulación:\n")
a_g <- KAPPA*SIG_GLOBAL*PHI_GLOBAL/RHO; a_r <- KAPPA*SIG_RENOV*PHI_RENOV/RHO
Y_g <- (-TH+sqrt(TH^2+4*G*a_g))/(2*a_g); Y_r <- (-TH+sqrt(TH^2+4*G*a_r))/(2*a_r)
cat(sprintf("  Continuidad          Y* = %8.1f   daño = %.2f %%\n", Y_g, 100*a_g*Y_g))
cat(sprintf("  Totalmente renovable Y* = %8.1f   ventaja %+.1f %%\n", Y_r, 100*(Y_r/Y_g-1)))
cat("\n  La diferencia entre ambas columnas es lo que aporta el supuesto de\n")
cat("  acumulación, y por eso se reporta en la sección 3.11 del artículo.\n")

###############################################################################
## --- 15. TABLA DE PARÁMETROS (fuente de la Tabla 1 del artículo) -----------
###############################################################################

tabla1 <- data.frame(
  simbolo = c("W","alpha1","alpha2","theta","G","phi","phi","sigma","sigma","rho","kappa"),
  descripcion = c(
    "Productividad media del trabajo",
    "Propensión a consumir del ingreso disponible",
    "Propensión a consumir de la riqueza",
    "Tasa impositiva efectiva",
    "Gasto público",
    "Intensidad energética, escenario de continuidad",
    "Intensidad energética, totalmente renovable",
    "Intensidad de carbono, escenario de continuidad",
    "Intensidad de carbono, totalmente renovable",
    "Fracción de emisiones compensada por sumideros",
    "Sensibilidad del daño al acervo de emisiones acumuladas"),
  valor = c(W, ALPHA1, ALPHA2, TH, G, PHI_GLOBAL, PHI_RENOV,
            SIG_GLOBAL, SIG_RENOV, RHO, KAPPA),
  unidad = c("millones USD por ocupado","fracción","fracción","fracción",
             "millones USD","TJ por millón USD","TJ por millón USD",
             "Gg CO2 por TJ","Gg CO2 por TJ","fracción anual",
             "fracción del producto por Gg"),
  condicion = c("Calibrado","Estimado","Calibrado","Calibrado","Calibrado",
                "Calibrado","Calibrado","Calibrado","Calibrado","Calibrado","Calibrado"),
  stringsAsFactors = FALSE)
write.csv(tabla1, file.path(OUT, "tabla_parametros_FINAL.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\n=========== TABLA DE PARÁMETROS (Tabla 1 del artículo) ===========\n")
print(tabla1[, c("simbolo","descripcion","valor","condicion")], row.names = FALSE)

cat("\n\nArchivos generados en ./", OUT, "/\n", sep = "")
cat("  Los que llevan el sufijo FINAL son los que alimentan el artículo.\n")
cat("  Los restantes provienen del script de diagnóstico y corresponden a\n")
cat("  etapas intermedias; se conservan como respaldo del proceso.\n")
