# Fuentes de los datos

Los archivos de esta carpeta son series derivadas, construidas por los autores a
partir de estadísticas públicas. Se distribuyen para permitir la reproducción de
la estimación de las propensiones a consumir. Las fuentes primarias son:

| Archivo | Contenido | Fuente primaria |
|---|---|---|
| `BD para propensiones.xlsx` | Consumo, ingreso disponible y agregados anuales | Cuentas Nacionales, Banco Central de Costa Rica |
| `deflactores.csv` | Deflactores implícitos del producto, el consumo y el gasto público | Cuentas Nacionales, BCCR |
| `m2.csv` | Agregado monetario M2 en colones corrientes | Estadísticas monetarias, BCCR |
| `deficit_pctPIB.csv` | Resultado financiero del Gobierno Central como porcentaje del producto | Ministerio de Hacienda de Costa Rica |

Los archivos `Propensiones_*.csv` son salidas de los scripts de esta carpeta.

## Fuentes que no se redistribuyen

La calibración del modelo emplea además publicaciones oficiales que no se
reproducen aquí por estar sujetas a los términos de sus editores. Se citan en el
artículo y son de acceso público:

- Balance Nacional de Energía 2022. Ministerio de Ambiente y Energía (MINAE).
- Inventario Nacional de Gases de Efecto Invernadero. MINAE / IMN.
- Matriz Insumo-Producto de Costa Rica 2017. BCCR.
- Encuesta Continua de Empleo. INEC.
- Plan Nacional de Descarbonización 2018-2050. Gobierno de Costa Rica.
- OECD (2021). *Effective Carbon Rates 2021*. https://doi.org/10.1787/0e8e24f5-en

Los valores que de ellas se derivan están incorporados como parámetros
explícitos en `modelo/01_modelo_SFCE.R`, con su fuente indicada en el propio
código y en `modelo/salidas/tabla_parametros_FINAL.csv`.
