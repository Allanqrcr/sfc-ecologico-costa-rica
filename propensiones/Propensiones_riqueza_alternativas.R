###############################################################################
##  Determinación de las propensiones a consumir:
##  tres definiciones alternativas de riqueza x seis especificaciones
##
##  Contexto. La serie de riqueza empleada originalmente se construyó como el
##  ahorro acumulado, con el ingreso disponible definido como Y - G. Pero por
##  la identidad de cuentas nacionales Y - C - G = I + (X - M), de modo que esa
##  serie acumula inversión y exportaciones netas: es un acervo de capital más
##  la posición externa, no un activo financiero de los hogares. Alcanza el
##  325 % del PIB en 2022, magnitud incompatible con la variable del modelo.
##
##  En el modelo SIM la riqueza de los hogares es dinero emitido por el
##  gobierno: Hh = Hs = déficits públicos acumulados. Este archivo evalúa tres
##  candidatos observables y cruza cada uno con seis especificaciones
##  econométricas, sometiendo todos los resultados a los mismos criterios.
###############################################################################

need <- c("readxl", "lmtest", "sandwich")
new <- need[!need %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(need, require, character.only = TRUE))

###############################################################################
## 1. BASE REAL --------------------------------------------------------------
###############################################################################
d <- as.data.frame(readxl::read_excel("BD para propensiones.xlsx", sheet = 1))
names(d)[1:7] <- c("t", "C", "Y", "G", "YD", "S", "H")
d <- d[, c("t", "C", "Y", "G")]
d$t <- as.numeric(as.character(d$t))
d <- merge(d, read.csv("deflactores.csv")[, c("t", "defY", "defC", "defG")], by = "t")
d <- merge(d, read.csv("m2.csv")[, c("t", "M2_cn")], by = "t", all.x = TRUE)
dfc <- read.csv("deficit_pctPIB.csv")
d <- merge(d, dfc, by = "t", all.x = TRUE)

d$Cr  <- d$C / d$defC
d$Yr  <- d$Y / d$defY
d$Gr  <- d$G / d$defG
d$YDr <- d$Yr - d$Gr

###############################################################################
## 2. TRES DEFINICIONES DE RIQUEZA (reales, colones constantes de 2022) ------
###############################################################################

## H1. Ahorro privado acumulado. Es la serie original deflactada. Se conserva
##     como referencia para documentar por qué no sirve.
d$H1 <- cumsum(d$YDr - d$Cr)

## H2. Dinero en sentido amplio. Es el activo que los hogares efectivamente
##     mantienen y el análogo observable más directo de la H del modelo SIM.
d$H2 <- d$M2_cn / d$defC

## H3. Déficit público acumulado. Es la definición literal del modelo, donde la
##     riqueza de los hogares es la contrapartida de la emisión del gobierno.
##     Se acumula desde el primer año con dato, tomando como nivel inicial el
##     dinero amplio de ese año (el modelo exige Hh = Hs, de modo que anclar el
##     arranque en el acervo monetario observado es lo coherente).
d$def_abs_r <- (d$deficit_pctPIB / 100) * d$Yr        # déficit real, signo: + = déficit
i0 <- which(!is.na(d$def_abs_r))[1]
d$H3 <- NA
d$H3[i0] <- d$H2[i0]
for (i in (i0 + 1):nrow(d)) d$H3[i] <- d$H3[i - 1] + d$def_abs_r[i]

cat("=== LAS TRES DEFINICIONES DE RIQUEZA, EN % DEL PIB ===\n")
sel <- d$t %in% c(1991, 2000, 2010, 2015, 2022, 2024)
print(data.frame(anio = d$t[sel],
                 H1_ahorro_acum = round(100 * d$H1[sel] / d$Yr[sel]),
                 H2_dinero_M2   = round(100 * d$H2[sel] / d$Yr[sel]),
                 H3_deficit_acum = round(100 * d$H3[sel] / d$Yr[sel])), row.names = FALSE)

###############################################################################
## 3. GRILLA: 3 definiciones x 6 especificaciones ---------------------------
###############################################################################

evaluar <- function(nombreH, Hserie) {
  b <- data.frame(t = d$t, Cr = d$Cr, YDr = d$YDr, H = Hserie)
  b$Hlag <- c(NA, head(b$H, -1))
  b <- b[complete.cases(b), ]
  if (nrow(b) < 12) return(NULL)
  ib <- which(b$t == 2022)
  if (length(ib) == 0) ib <- nrow(b)
  R_obs <- b$Hlag[ib] / b$YDr[ib]   # razón observada en el año base

  out <- list()
  agr <- function(esp, a1, a2, dwp, coint = NA) {
    out[[length(out) + 1]] <<- data.frame(
      riqueza = nombreH, especificacion = esp, alpha1 = a1, alpha2 = a2,
      razon_implicita = (1 - a1) / a2, razon_observada = R_obs,
      DW_p = dwp, coint_p = coint, stringsAsFactors = FALSE)
  }

  ## S1. Niveles, conjunta, con constante
  m <- lm(Cr ~ YDr + Hlag, b); agr("S1 niveles con constante", coef(m)[2], coef(m)[3], dwtest(m)$p.value)
  ## S2. Niveles, conjunta, sin constante
  m <- lm(Cr ~ 0 + YDr + Hlag, b); agr("S2 niveles sin constante", coef(m)[1], coef(m)[2], dwtest(m)$p.value)
  ## S3. Primeras diferencias, con constante
  f <- data.frame(dC = diff(b$Cr), dYD = diff(b$YDr), dH = diff(b$Hlag))
  m <- lm(dC ~ dYD + dH, f); agr("S3 primeras diferencias", coef(m)[2], coef(m)[3], dwtest(m)$p.value)
  ## S4. Normalizada por el ingreso disponible
  m <- lm(I(Cr / YDr) ~ I(Hlag / YDr), b); agr("S4 normalizada por YD", coef(m)[1], coef(m)[2], dwtest(m)$p.value)
  ## S5. Corrección de error. El coeficiente de corrección mide cointegración.
  lr <- lm(log(Cr) ~ log(YDr) + log(Hlag), b)
  fe <- data.frame(dlC = diff(log(b$Cr)), dlYD = diff(log(b$YDr)),
                   dlH = diff(log(b$Hlag)), ec = head(resid(lr), -1))
  me <- lm(dlC ~ dlYD + dlH + ec, fe)
  a1 <- coef(lr)[2] * mean(b$Cr) / mean(b$YDr)
  a2 <- coef(lr)[3] * mean(b$Cr) / mean(b$Hlag)
  agr("S5 corrección de error", a1, a2, dwtest(me)$p.value,
      summary(me)$coefficients["ec", 4])
  ## S6. Restringida: impone (1 - a1)/a2 = razón observada del año base
  z <- b$Hlag / R_obs
  m <- lm(I(Cr - z) ~ 0 + I(YDr - z), b)
  a1 <- coef(m)[1]; agr("S6 restringida a la razón observada", a1, (1 - a1) / R_obs, dwtest(m)$p.value)

  do.call(rbind, out)
}

G <- rbind(evaluar("H1 ahorro acumulado", d$H1),
           evaluar("H2 dinero amplio",    d$H2),
           evaluar("H3 déficit acumulado", d$H3))

###############################################################################
## 4. CRITERIOS DE ADMISIBILIDAD --------------------------------------------
###############################################################################
G$signo    <- G$alpha1 > 0 & G$alpha1 < 1 & G$alpha2 > 0
G$magnitud <- G$alpha1 >= 0.55 & G$alpha1 <= 0.95 & G$alpha2 > 0 & G$alpha2 <= 0.60
G$coherencia <- is.finite(G$razon_implicita) & G$razon_implicita > 0 &
                abs(G$razon_implicita / G$razon_observada - 1) <= 0.30
G$admisible <- G$signo & G$magnitud & G$coherencia

cat("\n\n=== GRILLA COMPLETA ===\n")
cat(sprintf("%-22s %-36s %7s %8s %9s %9s %6s\n",
            "riqueza", "especificacion", "alpha1", "alpha2", "razon_imp", "razon_obs", "adm"))
for (i in 1:nrow(G)) cat(sprintf("%-22s %-36s %7.4f %8.4f %9.2f %9.2f %6s\n",
  G$riqueza[i], G$especificacion[i], G$alpha1[i], G$alpha2[i],
  G$razon_implicita[i], G$razon_observada[i], ifelse(G$admisible[i], "SI", "-")))

write.csv(G, "Propensiones_grilla_riqueza.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== ESPECIFICACIONES ADMISIBLES ===\n")
A <- G[G$admisible, ]
if (nrow(A) == 0) {
  cat("Ninguna combinación supera los tres criterios.\n")
} else {
  for (i in 1:nrow(A)) cat(sprintf("  %s | %s\n    alpha1 = %.4f  alpha2 = %.4f  razón implícita %.2f vs observada %.2f\n",
    A$riqueza[i], A$especificacion[i], A$alpha1[i], A$alpha2[i],
    A$razon_implicita[i], A$razon_observada[i]))
}

cat("\n=== RESUMEN POR DEFINICIÓN DE RIQUEZA ===\n")
for (h in unique(G$riqueza)) {
  s <- G[G$riqueza == h, ]
  cat(sprintf("  %-22s razón observada %5.2f | alpha2 positivo en %d de %d | admisibles %d\n",
              h, s$razon_observada[1], sum(s$alpha2 > 0), nrow(s), sum(s$admisible)))
}
