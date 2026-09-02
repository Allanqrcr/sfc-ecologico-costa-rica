# Verificacion numerica de las cifras derivadas que aparecen en el manuscrito.
PIB22  <- 87862.80504186137
G      <- 13527.2509115691
TH     <- 0.129036878894848
RHO    <- 0.218080022778761
KAPPA  <- 6.20802562807628E-07 * 1.4071907
PHI_G  <- 2.07644144656066; SIG_G <- 0.0410390766992924
PHI_R  <- 1.28975587832427; SIG_R <- 0.00882132065193575

Yss <- function(a, theta = TH, Gd = G)
  (-theta + sqrt(theta^2 + 4 * a * Gd)) / (2 * a)

a_de <- function(phi, sigma) KAPPA * sigma * phi / RHO
aG <- a_de(PHI_G, SIG_G)

cat("== Elasticidad de Y* respecto del parametro compuesto a ==\n")
Y0 <- Yss(aG); d0 <- aG * Y0
cat(sprintf("  Y*  = %.1f   dano* = %.4f %%\n", Y0, 100 * d0))
e_an <- -d0 / (2 * d0 + TH)
cat(sprintf("  formula  -d*/(2d*+theta)   = %.4f\n", e_an))
h <- 1e-4
e_num <- (log(Yss(aG * (1 + h))) - log(Yss(aG * (1 - h)))) / (2 * h)
cat(sprintf("  numerica (diferencias)     = %.4f\n", e_num))
cat(sprintf("  duplicar a mueve Y* en      %.2f %%\n",
            100 * (Yss(2 * aG) / Y0 - 1)))

cat("\n== Cocientes sigma*phi entre escenarios ==\n")
sfG <- SIG_G * PHI_G; sfR <- SIG_R * PHI_R
cat(sprintf("  global      = %.6f\n  renovable   = %.6f\n  cociente    = %.2f\n",
            sfG, sfR, sfG / sfR))

# escenario PND en 2050: ultimo valor de las trayectorias del script principal
s <- read.csv("../salidas/series_FINAL.csv", stringsAsFactors = FALSE)
p <- s[s$esc == "4. PND", ]
sf_pnd50 <- (p$CO2[nrow(p)] / p$E[nrow(p)]) * (p$E[nrow(p)] / p$Y[nrow(p)])
cat(sprintf("  PND 2050    = %.6f\n  global/PND  = %.2f\n", sf_pnd50, sfG / sf_pnd50))

cat("\n== Diferencias porcentuales entre escenarios (2050) ==\n")
u <- function(e, v) { z <- s[s$esc == e, v]; z[length(z)] }
cmp <- function(a, b, v) 100 * (u(a, v) / u(b, v) - 1)
cat(sprintf("  renovable vs global   Y = %5.2f %%   Y_eff = %5.2f %%\n",
            cmp("3. Totalmente renovable", "2. Global", "Y"),
            cmp("3. Totalmente renovable", "2. Global", "Y_eff")))
cat(sprintf("  PND vs global         Y = %5.2f %%   Y_eff = %5.2f %%\n",
            cmp("4. PND", "2. Global", "Y"), cmp("4. PND", "2. Global", "Y_eff")))
cat(sprintf("  renovable vs PND      Y = %5.2f %%   Y_eff = %5.2f %%\n",
            cmp("3. Totalmente renovable", "4. PND", "Y"),
            cmp("3. Totalmente renovable", "4. PND", "Y_eff")))
cat(sprintf("  brecha sin-eco vs global (millones) = %.0f\n",
            u("1. Sin componente ecologico", "Y") - u("2. Global", "Y")))

cat("\n== Bloque del dano ==\n")
cat(sprintf("  precio del carbono          = %.2f USD/t\n", 60 * 1.14220 * 1.12))
e22 <- s$CO2[1]; st22 <- s$Cstock[s$esc == "2. Global"][1]
cat(sprintf("  dano de un ano de emisiones = %.2f %% del producto\n",
            100 * KAPPA * e22))
cat(sprintf("  acervo inicial / emisiones  = %.2f anos\n", st22 / e22))
cat(sprintf("  acervo 2050 / emisiones 2050 (global) = %.2f anos\n",
            u("2. Global", "Cstock") / u("2. Global", "CO2")))
