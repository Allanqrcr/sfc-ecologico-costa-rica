###############################################################################
##  Re-estimación de las propensiones a consumir en TÉRMINOS REALES
##
##  Motivo: el dictamen preguntó si las magnitudes eran nominales o reales. Se
##  verificó que las series de "BD para propensiones.xlsx" son las del BCCR a
##  precios corrientes (coinciden dígito por dígito con "PIB composición.xlsx",
##  cuyo encabezado dice «En millones de colones corrientes»). La estimación
##  original se corrió, por tanto, sobre series nominales con tendencia común
##  dominada por la inflación.
##
##  Este archivo: (1) deflacta las series a colones constantes de 2022,
##  (2) reconstruye la riqueza acumulando ahorro real, (3) estima ocho
##  especificaciones y (4) las evalúa por signo, magnitud y coherencia con el
##  modelo SFC.
##
##  Deflactores: implícitos de las Cuentas Nacionales de Costa Rica publicadas
##  por el Banco Mundial (cociente entre serie a precios corrientes y serie a
##  precios constantes), reescalados a base 2022 = 1. Se usan deflactores
##  específicos para PIB, consumo de los hogares y consumo del gobierno.
###############################################################################

need <- c("readxl", "lmtest", "sandwich")
new <- need[!need %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(need, require, character.only = TRUE))

RUTA_BD  <- "BD para propensiones.xlsx"
RUTA_DEF <- "deflactores.csv"
SALIDA   <- "Propensiones_series_reales.csv"

## --- 1. Series nominales del BCCR -------------------------------------------
d <- as.data.frame(readxl::read_excel(RUTA_BD, sheet = 1))
names(d)[1:7] <- c("t", "C", "Y", "G", "YD", "S", "H")
d <- d[, c("t", "C", "Y", "G")]
d$t <- as.numeric(as.character(d$t))

## --- 2. Deflactores (base 2022 = 1) -----------------------------------------
def <- read.csv(RUTA_DEF)
d <- merge(d, def[, c("t", "defY", "defC", "defG")], by = "t")

## --- 3. Series reales, en millones de colones constantes de 2022 ------------
d$Cr  <- d$C / d$defC
d$Yr  <- d$Y / d$defY
d$Gr  <- d$G / d$defG
d$YDr <- d$Yr - d$Gr          # misma definición que la hoja original
d$Sr  <- d$YDr - d$Cr

## Riqueza real: se acumula el ahorro real. El nivel del primer año es una
## convención (se supone riqueza nula antes de 1991), igual que en la hoja
## original; su consecuencia se examina más abajo.
d$Hr <- cumsum(d$Sr)
d$Hr_lag <- c(NA, head(d$Hr, -1))
e <- d[!is.na(d$Hr_lag), ]

write.csv(d, SALIDA, row.names = FALSE, fileEncoding = "UTF-8")

cat("=== SERIES REALES (millones de colones constantes de 2022) ===\n")
print(round(d[d$t %in% c(1991, 2000, 2010, 2020, 2022, 2024),
              c("t", "Cr", "Yr", "Gr", "YDr", "Hr")]))
cat("\nCrecimiento real del PIB 1991-2024: x", round(d$Yr[nrow(d)] / d$Yr[1], 2),
    " (nominal: x", round(d$Y[nrow(d)] / d$Y[1], 1), ")\n", sep = "")

## Objetivos de coherencia SFC, tomados del año base
R_2022 <- e$Hr_lag[e$t == 2022] / e$YDr[e$t == 2022]   # razón riqueza/ingreso
cat("\nRazón riqueza/ingreso disponible observada en 2022 (real) =",
    round(R_2022, 3), "\n")

###############################################################################
## 4. ESPECIFICACIONES ------------------------------------------------------
###############################################################################

res <- list()
add <- function(nombre, a1, a2, r2, dw, nota = "") {
  res[[length(res) + 1]] <<- data.frame(
    especificacion = nombre, alpha1 = a1, alpha2 = a2,
    R2 = r2, DW_p = dw, nota = nota, stringsAsFactors = FALSE)
}

## (1) Dos regresiones bivariadas separadas y sin constante (la original)
m1a <- lm(Cr ~ 0 + YDr, e); m1b <- lm(Cr ~ 0 + Hr_lag, e)
add("1. Separadas, sin constante (original)", coef(m1a)[1], coef(m1b)[1],
    summary(m1a)$r.squared, dwtest(m1a)$p.value, "cada regresor absorbe toda la variación")

## (2) Conjunta en niveles, con constante
m2 <- lm(Cr ~ YDr + Hr_lag, e)
add("2. Conjunta en niveles, con constante", coef(m2)[2], coef(m2)[3],
    summary(m2)$r.squared, dwtest(m2)$p.value, "")

## (3) Conjunta en niveles, sin constante
m3 <- lm(Cr ~ 0 + YDr + Hr_lag, e)
add("3. Conjunta en niveles, sin constante", coef(m3)[1], coef(m3)[2],
    summary(m3)$r.squared, dwtest(m3)$p.value, "")

## (4) Primeras diferencias, con constante
f4 <- data.frame(dC = diff(e$Cr), dYD = diff(e$YDr), dH = diff(e$Hr_lag))
m4 <- lm(dC ~ dYD + dH, f4)
add("4. Primeras diferencias, con constante", coef(m4)[2], coef(m4)[3],
    summary(m4)$r.squared, dwtest(m4)$p.value, "")

## (5) Primeras diferencias, sin constante
m5 <- lm(dC ~ 0 + dYD + dH, f4)
add("5. Primeras diferencias, sin constante", coef(m5)[1], coef(m5)[2],
    summary(m5)$r.squared, dwtest(m5)$p.value, "")

## (6) Normalizada por el ingreso disponible
e$cy <- e$Cr / e$YDr; e$hy <- e$Hr_lag / e$YDr
m6 <- lm(cy ~ hy, e)
add("6. Normalizada por el ingreso", coef(m6)[1], coef(m6)[2],
    summary(m6)$r.squared, dwtest(m6)$p.value, "la constante es alpha1")

## (7) Modelo de corrección de error
lr <- lm(log(Cr) ~ log(YDr) + log(Hr_lag), e)
f7 <- data.frame(dlC = diff(log(e$Cr)), dlYD = diff(log(e$YDr)),
                 dlH = diff(log(e$Hr_lag)), ec = head(resid(lr), -1))
m7 <- lm(dlC ~ dlYD + dlH + ec, f7)
## elasticidades de largo plazo convertidas a propensiones en el punto medio
el1 <- coef(lr)[2]; el2 <- coef(lr)[3]
a1_7 <- el1 * mean(e$Cr) / mean(e$YDr); a2_7 <- el2 * mean(e$Cr) / mean(e$Hr_lag)
add("7. Corrección de error (largo plazo)", a1_7, a2_7,
    summary(lr)$r.squared, dwtest(m7)$p.value, "elasticidades convertidas a propensiones")

## (8) Sistema con la restricción de coherencia SFC impuesta
##     Se estima  Cr = a1*YDr + a2*Hr_lag  sujeto a  (1 - a1)/a2 = R_2022,
##     es decir  a2 = (1 - a1)/R_2022. Sustituyendo queda una sola incógnita:
##       Cr - Hr_lag/R_2022 = a1 * (YDr - Hr_lag/R_2022)
z  <- e$Hr_lag / R_2022
m8 <- lm(I(Cr - z) ~ 0 + I(YDr - z), e)
a1_8 <- coef(m8)[1]; a2_8 <- (1 - a1_8) / R_2022
add("8. Restringida a la coherencia SFC", a1_8, a2_8,
    summary(m8)$r.squared, dwtest(m8)$p.value, "impone (1-a1)/a2 = razón observada de 2022")

R <- do.call(rbind, res)
row.names(R) <- NULL

###############################################################################
## 5. EVALUACIÓN --------------------------------------------------------------
###############################################################################

## Parámetros del modelo SFC necesarios para evaluar la coherencia
PIB22 <- 87862.80504186137; G <- 13527.2509115691; TH <- 0.129036878894848
RHO <- 0.218080022778761; KA <- 6.20802562807628E-07 * 1.159
D0 <- KA * (0.0410390766992924 * 2.07644144656066 * PIB22) / RHO
H_obs_2022 <- e$Hr_lag[e$t == 2022] / 510   # riqueza real observada, millones USD

R$razon_SFC   <- (1 - R$alpha1) / R$alpha2          # H*/YD* implícita
R$H0_millUSD  <- (PIB22 * (1 - R$alpha1 * (1 - TH - D0)) - G) / R$alpha2
R$H0_pct_PIB  <- 100 * R$H0_millUSD / PIB22
R$signo_ok    <- R$alpha1 > 0 & R$alpha1 < 1 & R$alpha2 > 0
R$magnitud_ok <- R$alpha1 >= 0.60 & R$alpha1 <= 0.98 & R$alpha2 >= 0.02 & R$alpha2 <= 0.20
R$sfc_ok      <- abs(R$razon_SFC / R_2022 - 1) < 0.25

cat("\n\n=== RESULTADOS DE LAS OCHO ESPECIFICACIONES ===\n")
print(data.frame(especificacion = R$especificacion,
                 alpha1 = round(R$alpha1, 4), alpha2 = round(R$alpha2, 4),
                 R2 = round(R$R2, 4), DW_p = signif(R$DW_p, 3)), row.names = FALSE)

cat("\n=== EVALUACIÓN POR LOS TRES CRITERIOS ===\n")
cat("Referencias: razón riqueza/ingreso observada en 2022 =", round(R_2022, 3),
    "| riqueza observada 2022 =", round(H_obs_2022), "millones USD (",
    round(100 * H_obs_2022 / PIB22), "% del PIB )\n\n")
print(data.frame(especificacion = substr(R$especificacion, 1, 38),
                 razon_SFC = round(R$razon_SFC, 2),
                 H0_pctPIB = round(R$H0_pct_PIB, 1),
                 signo = ifelse(R$signo_ok, "ok", "FALLA"),
                 magnitud = ifelse(R$magnitud_ok, "ok", "FALLA"),
                 coher_SFC = ifelse(R$sfc_ok, "ok", "FALLA")), row.names = FALSE)

write.csv(R, "Propensiones_resultados_especificaciones.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== DETALLE DE LAS ESPECIFICACIONES QUE SUPERAN LOS TRES CRITERIOS ===\n")
ok <- which(R$signo_ok & R$magnitud_ok & R$sfc_ok)
if (length(ok) == 0) cat("Ninguna supera los tres criterios simultáneamente.\n")
for (i in ok) cat(sprintf("  %s\n    alpha1 = %.4f  alpha2 = %.4f  razón SFC = %.2f  riqueza inicial = %.0f%% del PIB\n",
                          R$especificacion[i], R$alpha1[i], R$alpha2[i],
                          R$razon_SFC[i], R$H0_pct_PIB[i]))

cat("\n=== COEFICIENTES DE LA ESPECIFICACIÓN RESTRINGIDA (8) ===\n")
print(round(summary(m8)$coefficients, 6))
cat("alpha1 =", round(a1_8, 4), " -> alpha2 = (1 - alpha1)/", round(R_2022, 3),
    " =", round(a2_8, 4), "\n")
## La especificacion 8 se estima sin constante y con un unico regresor, de modo
## que la regresion auxiliar de Breusch-Pagan no es identificable. Se reporta
## solo el contraste de autocorrelacion.
bp8 <- tryCatch(signif(bptest(m8)$p.value, 4),
                error = function(e) "no aplicable (modelo sin constante)")
cat("Breusch-Pagan p =", bp8,
    " | Durbin-Watson p =", signif(dwtest(m8)$p.value, 4), "\n")
cat("Errores robustos HAC:\n"); print(round(coeftest(m8, vcov. = NeweyWest(m8))[, , drop = FALSE], 6))
