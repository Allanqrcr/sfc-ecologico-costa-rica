## Reproduce el ejercicio completo desde cero.
## Ejecutar desde la raiz del repositorio:  Rscript ejecutar_todo.R
raiz <- getwd()

message("\n[1/2] Estimacion de las propensiones a consumir")
setwd(file.path(raiz, "propensiones"))
source("Propensiones_reestimacion.R", encoding = "UTF-8")
source("Propensiones_riqueza_alternativas.R", encoding = "UTF-8")

message("\n[2/2] Modelo, verificaciones y figuras")
setwd(file.path(raiz, "modelo"))
source("01_modelo_SFCE.R", encoding = "UTF-8")
source("03_diagrama.R",    encoding = "UTF-8")
source("04_tablas_complementarias.R", encoding = "UTF-8")
source("02_figuras.R",     encoding = "UTF-8")

setwd(raiz)
message("\nListo. Resultados en modelo/salidas/")
