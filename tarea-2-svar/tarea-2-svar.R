## ============================================================
##  PRÁCTICA 3: MODELOS SVAR
##  Series de Tiempo — Maestría en Economía Aplicada (UNIBE)
##  Autor: Stephannie Acosta 25 0802
## ============================================================

# ---- 0. PAQUETES ----
paquetes <- c("readxl", "tidyverse", "tseries", "zoo",
              "seasonal", "vars", "svars", "ggplot2",
              "gridExtra", "kableExtra", "urca")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


# ============================================================
# SECCIÓN b: IMPORTACIÓN Y PREPARACIÓN DE DATOS
# ============================================================

# ---- b.1 Importar base ----
install.packages("readxl")  
library(readxl)
raw <- read_excel("data/base_tarea_2.xlsx", sheet = "DATA")

# Limpiar nombres de columnas (tienen espacios al final)
names(raw) <- trimws(names(raw))

# Revisar estructura
glimpse(raw)


# ---- b.2 Construir fecha y convertir a ts ----
meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

raw <- raw %>%
  mutate(mes_num = match(Período, meses_es),
         fecha   = as.Date(paste(Año, mes_num, "01", sep = "-"))) %>%
  arrange(fecha)


# ---- b.3 Imputar valores faltantes ----

## IMAE_OG: 2006 completo → imputar con la media histórica (no de 2006)
media_imae <- mean(raw$IMAE_OG, na.rm = TRUE)
raw <- raw %>%
  mutate(IMAE_OG = ifelse(is.na(IMAE_OG), media_imae, IMAE_OG))

## IMAE Nov–Dic 2024 y WTI Dic 2011 → LOCF (último valor disponible)
raw <- raw %>%
  mutate(
    IMAE_OG       = zoo::na.locf(IMAE_OG,       na.rm = FALSE),
    CRUDE_OIL_WTI = zoo::na.locf(CRUDE_OIL_WTI, na.rm = FALSE)
  )

# Verificar que no queden NA
cat("NAs restantes por variable:\n")
print(colSums(is.na(raw[, c("IPC_TOTAL","IMAE_OG","TMB_PASIVA",
                            "CRUDE_OIL_WTI","TASA_CAMBIO","INDPRO")])))


# ---- b.4 Ajuste estacional (X-13 ARIMA-SEATS) ----
# Convertir a objetos ts (frecuencia mensual, inicio Ene-2006)
inicio <- c(2006, 1)

ts_ipc   <- ts(raw$IPC_TOTAL,       start = inicio, frequency = 12)
ts_imae  <- ts(raw$IMAE_OG,         start = inicio, frequency = 12)
ts_wti   <- ts(raw$CRUDE_OIL_WTI,   start = inicio, frequency = 12)
ts_tcn   <- ts(raw$TASA_CAMBIO,     start = inicio, frequency = 12)
ts_tasa  <- ts(raw$TMB_PASIVA,      start = inicio, frequency = 12)
ts_indpro<- ts(raw$INDPRO,          start = inicio, frequency = 12)

# Test de estacionalidad (Kruskall-Wallis sobre dummies mensuales)
test_estac <- function(x, nombre) {
  mes <- cycle(x)
  p   <- kruskal.test(as.numeric(x) ~ mes)$p.value
  cat(sprintf("%-15s  p-valor Kruskal-Wallis = %.4f  %s\n",
              nombre, p, ifelse(p < 0.05, "→ ESTACIONAL", "→ sin estacionalidad")))
}

cat("\n--- Test de estacionalidad ---\n")
test_estac(ts_ipc,    "IPC")
test_estac(ts_imae,   "IMAE")
test_estac(ts_wti,    "WTI")
test_estac(ts_tcn,    "TCN")
test_estac(ts_tasa,   "Tasa interbancaria")
test_estac(ts_indpro, "INDPRO")


# Ninguna serie presenta estacionalidad → se usan en niveles sin ajuste
sa_ipc    <- ts_ipc
sa_imae   <- ts_imae
sa_wti    <- ts_wti
sa_tcn    <- ts_tcn
sa_tasa   <- ts_tasa
sa_indpro <- ts_indpro
# Desestacionalización con seas() de las series que la presenten
# (típicamente IMAE e INDPRO)
sa_imae   <- final(seas(ts_imae))
sa_indpro <- final(seas(ts_indpro))

# Sin estacionalidad → se usan directamente sin ajuste
sa_ipc    <- ts_ipc
sa_imae   <- ts_imae
sa_wti    <- ts_wti
sa_tcn    <- ts_tcn
sa_tasa   <- ts_tasa
sa_indpro <- ts_indpro



# ---- b.5 Primeras diferencias del logaritmo ----
dlog <- function(x) diff(log(x)) * 100   # en % para interpretabilidad

dl_indpro <- dlog(sa_indpro)
dl_wti    <- dlog(sa_wti)
dl_imae   <- dlog(sa_imae)
dl_ipc    <- dlog(sa_ipc)
dl_tcn    <- dlog(sa_tcn)
d_tasa    <- diff(sa_tasa)

datos_var <- ts.union(dl_indpro, dl_wti, dl_imae, dl_ipc, d_tasa, dl_tcn)
colnames(datos_var) <- c("dlog_indpro","dlog_wti","dlog_imae",
                         "dlog_ipc","d_tasa","dlog_tcn")
datos_var <- na.omit(datos_var)

cat(sprintf("Dimensión final: %d obs × %d variables\n",
            nrow(datos_var), ncol(datos_var)))

# Tasa interbancaria: primera diferencia en niveles (no log, es ya una tasa)
d_tasa <- diff(sa_tasa)

# Alinear en una sola matriz (pierden 1 obs por el diff)
datos_var <- ts.union(dl_indpro, dl_wti, dl_imae, dl_ipc, d_tasa, dl_tcn)
colnames(datos_var) <- c("dlog_indpro","dlog_wti","dlog_imae",
                         "dlog_ipc","d_tasa","dlog_tcn")
datos_var <- na.omit(datos_var)
cat(sprintf("\nDimensión final del VAR: %d observaciones × %d variables\n",
            nrow(datos_var), ncol(datos_var)))


# ---- b.6 Estadísticas descriptivas ----
desc_stats <- function(x) {
  c(Media = mean(x), SD = sd(x), Min = min(x), Max = max(x))
}

tabla_desc <- t(sapply(as.data.frame(datos_var), desc_stats))
tabla_desc <- round(tabla_desc, 4)
print(tabla_desc)

# ---- b.7 Panel de gráficos ----
df_long <- as.data.frame(datos_var) %>%
  mutate(fecha = seq(as.Date("2006-02-01"),
                     by = "month",
                     length.out = nrow(datos_var))) %>%
  pivot_longer(-fecha, names_to = "variable", values_to = "valor")

etiquetas <- c(
  dlog_indpro = "∆log INDPRO (EEUU)",
  dlog_wti    = "∆log WTI",
  dlog_imae   = "∆log IMAE",
  dlog_ipc    = "Inflación mensual (∆log IPC)",
  d_tasa      = "∆ Tasa interbancaria",
  dlog_tcn    = "∆log Tipo de cambio"
)

p_panel <- ggplot(df_long, aes(x = fecha, y = valor)) +
  geom_line(linewidth = 0.4, color = "#2c5f8a") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.3) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2,
             labeller = as_labeller(etiquetas)) +
  labs(title = "Series transformadas para el VAR",
       subtitle = "Primeras diferencias del logaritmo — Febrero 2006 a Diciembre 2024",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8),
        panel.grid.minor = element_blank())

print(p_panel)
# ggsave("panel_series.png", p_panel, width = 10, height = 8, dpi = 150)

cat("
=================================================================
INTERPRETACIÓN ECONÓMICA — ESTADÍSTICAS DESCRIPTIVAS
=================================================================

INDPRO (Producción industrial EE.UU.)

Media ≈ 0.009% mensual — crecimiento casi nulo en promedio.
SD = 1.32%: serie estable salvo en crisis.
Mínimo de -14.1%: colapso de producción en abril 2020 (COVID-19).
La baja volatilidad relativa refleja la madurez del ciclo industrial
estadounidense, cuyas fluctuaciones se transmiten a la RD vía comercio
y precios de commodities.

∆log WTI (Precio del petróleo)

Variable más volátil del sistema (SD = 10.86%).
Mínimo de -59.3%: desplome histórico de abril 2020 (demanda global
colapsó y los contratos de futuros entraron en terreno negativo).
Máximo de +54.7%: rebote post-crisis financiera global 2009.
Esta alta volatilidad justifica su rol como shock externo clave
para analizar la transmisión inflacionaria en economías importadoras
de petróleo como la dominicana.

 IMAE (Actividad económica dominicana)

Media exactamente 0.000% — sin tendencia en primeras diferencias.
SD = 6.3%: alta variabilidad, refleja la sensibilidad del ciclo
económico dominicano a shocks externos.
Mínimo de -46.0%: contracción severa durante COVID-19 (abril-mayo 2020),
especialmente por el colapso del turismo, principal sector exportador.

IPC (Inflación mensual)

Media = 0.376% mensual ≈ 4.5% anual, consistente con el rango
meta del BCRD (4% ± 1%).
SD = 0.57%: la inflación mensual fluctúa de forma moderada.
Mínimo de -3.3%: episodios de deflación mensual durante la caída
de precios de commodities en 2014-2015.
La baja volatilidad relativa del IPC sugiere cierto grado de
anclaje de expectativas inflacionarias en el período analizado.

Tasa interbancaria

Media ≈ 0.000 pp — sin tendencia sistemática al alza ni a la baja.
SD = 0.61 pp: variabilidad moderada, refleja ciclos de política monetaria.
Rango: -2.97 a +2.30 pp. Los valores extremos negativos corresponden
a los recortes agresivos del BCRD durante COVID-19; los positivos,
al ciclo de alzas post-pandemia para contener la inflación de 2021-2022.

TCN (Tipo de cambio nominal RD$/US$):

Media = 0.25% mensual ≈ 3% anual de depreciación del peso.
SD = 0.93%: baja volatilidad relativa, evidencia de un régimen
de flotación administrada por el BCRD.
La depreciación sostenida pero gradual implica un canal de transmisión
cambiario activo hacia la inflación (pass-through).

En conclusión, las variables externas (WTI e INDPRO) son las más volátiles,
mientras que las variables de política y precios domésticos
(IPC y TCN) son las más estables. Esta asimetría de volatilidades
es consistente con el ordenamiento de Cholesky propuesto:
las variables externas van primero por ser más exógenas y
tener mayor varianza no explicada por el sistema doméstico.

")

# ============================================================
# SECCIÓN c: ESTIMACIÓN DEL VAR
# ============================================================

# ---- c.1 Selección de rezagos ----
cat("\n--- Criterios de selección de rezagos (máx 12) ---\n")
lag_sel <- VARselect(datos_var, lag.max = 12, type = "const")
print(lag_sel$criteria)
cat(sprintf("\nAIC → p=%d  |  BIC(SC) → p=%d  |  HQ → p=%d\n",
            lag_sel$selection["AIC(n)"],
            lag_sel$selection["SC(n)"],
            lag_sel$selection["HQ(n)"]))

# Usar el rezago sugerido por BIC/HQ (más parsimonioso)
p_opt <- lag_sel$selection["SC(n)"]
cat(sprintf("Rezago seleccionado: p = %d (BIC)\n", p_opt))


# ---- c.2 Estimación del VAR ----
var_est <- VAR(datos_var, p = p_opt, type = "const")
cat("\n--- Resumen del VAR ----\n")
summary(var_est)


# ---- c.3 Tests de diagnóstico ----

## Autocorrelación de residuos (Portmanteau / Breusch-Godfrey multivariado)
cat("\n--- Test de autocorrelación (Portmanteau) ---\n")
pt_test <- serial.test(var_est, lags.pt = 16, type = "PT.asymptotic")
print(pt_test)

## Heterocedasticidad (ARCH multivariado)
cat("\n--- Test ARCH multivariado ---\n")
arch_test <- arch.test(var_est, lags.multi = 5)
print(arch_test)

## Normalidad
cat("\n--- Test de normalidad (Jarque-Bera) ---\n")
norm_test <- normality.test(var_est)
print(norm_test)

## Estabilidad (raíces del polinomio)
# ---- Estabilidad (raíces del polinomio) ----
cat("\n--- Raíces del polinomio VAR ---\n")
raices <- roots(var_est)
cat("Módulos de las raíces:\n")
print(round(raices, 4))
cat(sprintf("¿Todas < 1? %s → Sistema %s\n",
            all(raices < 1),
            ifelse(all(raices < 1), "ESTABLE", "INESTABLE")))

# Gráfico círculo unitario
r_complejas <- roots(var_est, modulus = FALSE)
plot(Re(r_complejas), Im(r_complejas),
     xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
     asp  = 1, pch = 19, col = "#2c5f8a", cex = 1.5,
     xlab = "Parte real", ylab = "Parte imaginaria",
     main = "Raíces del VAR(1) — Círculo unitario")
theta <- seq(0, 2*pi, length.out = 300)
lines(cos(theta), sin(theta), col = "gray60", lwd = 1)
abline(h = 0, v = 0, col = "gray80", lty = 2)
legend("topright",
       legend = sprintf("Raíces (máx módulo = %.4f)", max(raices)),
       pch = 19, col = "#2c5f8a", bty = "n")


cat("
=================================================================
SECCIÓN c: INTERPRETACIÓN ECONÓMICA — DIAGNÓSTICO DEL VAR(1)
=================================================================

SELECCIÓN DE REZAGOS: p = 1
-----------------------------
Que un solo rezago sea suficiente indica que la información
relevante del mes anterior agota la dinámica predecible del
sistema. En otras palabras, conocer lo que ocurrió hace un mes
con el petróleo, la actividad y la inflación es suficiente para
proyectar el mes siguiente. Esto es coherente con mercados que
incorporan información rápidamente y con una economía pequeña
y abierta como la dominicana, donde los shocks externos se
transmiten de forma casi inmediata.

ESTABILIDAD: todas las raíces < 1
-----------------------------------
El sistema no tiene memoria larga ni dinámicas explosivas.
Económicamente significa que un shock en el precio del petróleo,
por ejemplo, no genera una espiral inflacionaria permanente:
su efecto sobre la inflación dominicana se agota gradualmente.
Esta propiedad es fundamental para que las IRF sean interpretables
como efectos temporales y no como divergencias sin fin.

AUTOCORRELACIÓN: p-valor = 0.716 → no hay autocorrelación
-----------------------------------------------------------
El modelo no deja información sin capturar. Si hubiera
autocorrelación, significaría que los agentes económicos tienen
patrones predecibles que el VAR no recoge, lo que invalidaría
las IRF. Al no haberla, confiamos en que las respuestas estimadas
reflejan relaciones económicas reales y no artefactos de una
especificación incompleta.

HETEROCEDASTICIDAD: p < 2.2e-16 → existe ARCH
-----------------------------------------------
La volatilidad del sistema no es constante en el tiempo.
Económicamente esto refleja que hay períodos de alta
incertidumbre, como la caída del petróleo en 2014-2016 y
el COVID-19 en 2020, donde los shocks son mucho más grandes
que en tiempos normales. No es un fallo del modelo sino una
característica real de la economía dominicana: su exposición
a shocks externos genera volatilidad variable que ningún modelo
lineal puede eliminar por completo.

NORMALIDAD: p < 2.2e-16 → residuos no normales
------------------------------------------------
Los residuos tienen colas pesadas por los valores extremos
de la pandemia. Económicamente confirma que la economía
dominicana está sujeta a shocks de gran magnitud poco
frecuentes, que no siguen una distribución normal. Esto no
sorprende: una economía altamente dependiente del turismo,
las remesas y los combustibles importados es vulnerable a
eventos extremos globales que por definición son atípicos.
=================================================================
")

# ============================================================
# SECCIÓN d: IDENTIFICACIÓN RECURSIVA — CHOLESKY
# ============================================================
# Ordenamiento: dlog_indpro → dlog_wti → dlog_imae →
#               dlog_ipc → d_tasa → dlog_tcn

# ---- d.1 Ordenamiento original ----
orden1 <- c("dlog_indpro","dlog_wti","dlog_imae",
            "dlog_ipc","d_tasa","dlog_tcn")
datos_ord1 <- datos_var[, orden1]
var_ord1   <- VAR(datos_ord1, p = p_opt, type = "const")

## IRF: inflación ante shock en WTI
irf_ord1 <- irf(var_ord1,
                impulse  = "dlog_wti",
                response = "dlog_ipc",
                n.ahead  = 24,
                boot     = TRUE,
                ci       = 0.90,
                runs     = 500)

plot(irf_ord1,
     main = "IRF: Inflación ante shock WTI — Ordenamiento 1 (Cholesky)",
     ylab = "Respuesta ∆log IPC (%)",
     xlab = "Meses")

## Descomposición de varianza: inflación
fevd_ord1 <- fevd(var_ord1, n.ahead = 24)
cat("\n--- FEVD de la inflación (∆log IPC) — Ordenamiento 1 ---\n")
print(round(fevd_ord1$dlog_ipc, 4))

p_fevd1 <- plot(fevd_ord1, names = "dlog_ipc",
                main = "FEVD Inflación — Ordenamiento 1")


# ---- d.2 Ordenamiento alternativo ----
# dlog_indpro → dlog_wti → dlog_imae → d_tasa → dlog_tcn → dlog_ipc
orden2 <- c("dlog_indpro","dlog_wti","dlog_imae",
            "d_tasa","dlog_tcn","dlog_ipc")
datos_ord2 <- datos_var[, orden2]
var_ord2   <- VAR(datos_ord2, p = p_opt, type = "const")

irf_ord2 <- irf(var_ord2,
                impulse  = "dlog_wti",
                response = "dlog_ipc",
                n.ahead  = 24,
                boot     = TRUE,
                ci       = 0.90,
                runs     = 500)

plot(irf_ord2,
     main = "IRF: Inflación ante shock WTI — Ordenamiento 2 (Cholesky)",
     ylab = "Respuesta ∆log IPC (%)",
     xlab = "Meses")

fevd_ord2 <- fevd(var_ord2, n.ahead = 24)
cat("\n--- FEVD de la inflación — Ordenamiento 2 ---\n")
print(round(fevd_ord2$dlog_ipc, 4))


# ============================================================
# SECCIÓN e: SVAR — RESTRICCIONES AB (CONTEMPORÁNEAS)
# ============================================================
# Variables (mismo orden que ord1): indpro, wti, imae, ipc, tasa, tcn
# n = 6 variables

n <- 6

## Matriz A: restricciones sobre el impacto contemporáneo
## Restricciones económicas:
##   - Externas (indpro, wti) son exógenas → no reaccionan entre sí (en el corto plazo)
##   - IMAE responde a indpro y wti contemporáneamente
##   - IPC responde a indpro, wti e IMAE contemporáneamente
##   - Tasa reacciona a inflación contemporáneamente (regla monetaria)
##   - TCN responde a tasa contemporáneamente

## Convención vars: A %*% e_t = B %*% u_t
## NA = parámetro libre, 0 = restricción cero, 1 = diagonal normalizada

A_mat <- diag(n)           # Matriz identidad como base
A_mat[3, 1] <- NA          # IMAE ← indpro
A_mat[3, 2] <- NA          # IMAE ← wti
A_mat[4, 1] <- NA          # IPC  ← indpro
A_mat[4, 2] <- NA          # IPC  ← wti
A_mat[4, 3] <- NA          # IPC  ← IMAE
A_mat[5, 4] <- NA          # tasa ← IPC (regla monetaria)
A_mat[6, 5] <- NA          # TCN  ← tasa

B_mat <- diag(n)           # B diagonal (shocks ortogonales)
B_mat[B_mat == 1] <- NA    # todos los elementos diagonales libres

cat("\n--- Matriz A propuesta ---\n")
print(A_mat)

## Estimación SVAR
svar_ab <- SVAR(var_ord1,
                Amat = A_mat,
                Bmat = B_mat,
                method = "scoring",
                max.iter = 1000,
                hessian = TRUE)

cat("\n--- Parámetros estimados SVAR-AB ---\n")
print(svar_ab)

## IRF estructural
irf_svar <- irf(svar_ab,
                impulse  = "dlog_wti",
                response = "dlog_ipc",
                n.ahead  = 24,
                boot     = TRUE,
                ci       = 0.90,
                runs     = 500)

plot(irf_svar,
     main = "IRF estructural: Inflación ante shock WTI — SVAR-AB",
     ylab = "Respuesta ∆log IPC (%)",
     xlab = "Meses")


# ============================================================
# SECCIÓN f: RESTRICCIONES DE SIGNO
# ============================================================
# Paquete svars — función id.sr()
# Restricciones para shock positivo de precio del petróleo:
#   + dlog_wti    > 0   (el precio sube)
#   + dlog_ipc    > 0   (inflación aumenta)
#   + dlog_imae   < 0   (actividad doméstica cae)
# Horizontes 1–6

## Reestimamos con el ordenamiento 1
var_sign <- VAR(datos_ord1, p = p_opt, type = "const")

## Construir matriz de restricciones de signo
## Filas = variables (orden: indpro, wti, imae, ipc, tasa, tcn)
## Columnas = shocks
## Solo identificamos el shock de petróleo (columna 2)
## 1 = positivo, -1 = negativo, NA = sin restricción

sign_mat <- matrix(NA, nrow = 6, ncol = 6)
rownames(sign_mat) <- orden1
colnames(sign_mat) <- paste0("shock_", 1:6)

# Shock de petróleo (columna 2 = shock de wti)
sign_mat["dlog_wti",  2] <-  1   # WTI sube
sign_mat["dlog_ipc",  2] <-  1   # inflación sube
sign_mat["dlog_imae", 2] <- -1   # actividad cae

cat("\n--- Matriz de restricciones de signo ---\n")
print(sign_mat)



# SECCIÓN f: RESTRICCIONES DE SIGNO — VERSIÓN CORREGIDA

set.seed(42)

# Extraer objetos del VAR
Sigma  <- crossprod(residuals(var_ord1)) / nrow(residuals(var_ord1))
P      <- t(chol(Sigma))
A1     <- t(sapply(coef(var_ord1), function(eq) eq[1:n_vars, 1]))

n_vars  <- 6
n_ahead <- 24
n_draws <- 5000
idx_wti  <- 2; idx_imae <- 3; idx_ipc <- 4

# Calcular IRF para todas las variables dado shock j
calc_irf <- function(A1, impact_col, h) {
  irf_mat <- matrix(0, h+1, n_vars)
  irf_mat[1, ] <- impact_col
  for (k in 1:h) {
    irf_mat[k+1, ] <- A1 %*% irf_mat[k, ]
  }
  irf_mat
}

# Loop de rotaciones
irf_store <- list()
n_keep    <- 0

for (d in 1:n_draws) {
  Q   <- qr.Q(qr(matrix(rnorm(n_vars^2), n_vars, n_vars)))
  PQ  <- P %*% Q
  
  # IRF del shock de WTI (columna idx_wti de PQ)
  ir  <- calc_irf(A1, PQ[, idx_wti], n_ahead)
  
  # Restricciones en horizontes 1 a 6 (índices 1:6, h=0 es contemporáneo)
  ok <- all(ir[1:6, idx_wti]  > 0) &&
    all(ir[1:6, idx_ipc]  > 0) &&
    all(ir[1:6, idx_imae] < 0)
  
  if (ok) {
    n_keep <- n_keep + 1
    irf_store[[n_keep]] <- ir
  }
}

cat(sprintf("Draws aceptados: %d de %d (%.1f%%)\n",
            n_keep, n_draws, 100*n_keep/n_draws))

# Convertir a array y graficar
irf_array <- array(unlist(irf_store), dim = c(n_ahead+1, n_vars, n_keep))

irf_med <- apply(irf_array, c(1,2), median)
irf_lo  <- apply(irf_array, c(1,2), quantile, 0.05)
irf_hi  <- apply(irf_array, c(1,2), quantile, 0.95)

horizons <- 0:n_ahead
plot(horizons, irf_med[, idx_ipc],
     type = "l", lwd = 2, col = "#2c5f8a",
     ylim = range(irf_lo[, idx_ipc], irf_hi[, idx_ipc]),
     xlab = "Meses", ylab = "Respuesta ∆log IPC (%)",
     main = "IRF: Inflación ante shock WTI — Restricciones de signo")
lines(horizons, irf_lo[, idx_ipc], lty = 2, col = "red")
lines(horizons, irf_hi[, idx_ipc], lty = 2, col = "red")
abline(h = 0, col = "red", lwd = 1)
legend("topright", legend = c("Mediana", "IC 90%"),
       lty = c(1,2), col = c("#2c5f8a","red"), bty = "n")

# ============================================================
# EJERCICIO 2: VAR BIVARIADO — BLANCHARD-QUAH (1989)
# ============================================================
# Variables: crecimiento del producto (dl_imae) e inflación (dl_ipc)

bq_data <- ts.union(dlog(sa_imae), dlog(sa_ipc))
colnames(bq_data) <- c("crec_imae", "inflacion")
bq_data <- na.omit(bq_data)

## Selección de rezagos
lag_bq <- VARselect(bq_data, lag.max = 12, type = "const")
p_bq   <- lag_bq$selection["SC(n)"]
cat(sprintf("\nBlanchard-Quah — rezago BIC: p = %d\n", p_bq))

## Estimación VAR bivariado
var_bq <- VAR(bq_data, p = p_bq, type = "const")

## Identificación de largo plazo (Blanchard-Quah)
## Restricción: shocks de demanda no tienen efecto permanente sobre el producto
## Implementada via BQ() del paquete vars
bq_model <- BQ(var_bq)

cat("\n--- Blanchard-Quah: matrices de impacto de largo plazo ---\n")
print(bq_model)

## IRF: respuestas del producto y la inflación a shocks de oferta y demanda
irf_bq <- irf(bq_model,
              n.ahead = 40,
              boot    = TRUE,
              ci      = 0.90,
              runs    = 500)

plot(irf_bq,
     main = "IRF Blanchard-Quah: shocks de oferta y demanda")


# ============================================================
# GUARDAR RESULTADOS (opcional)
# ============================================================
# save(var_est, var_ord1, var_ord2, svar_ab, svar_sr, var_bq, bq_model,
#      irf_ord1, irf_ord2, irf_svar, irf_sr, irf_bq,
#      file = "resultados_SVAR.RData")

cat("\n\n✓ Script completado exitosamente.\n")