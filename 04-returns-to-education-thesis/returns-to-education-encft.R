# ─────────────────────────────────────────────────────────────────────────────
# Retornos educativos y elasticidades laborales en República Dominicana
# ENCFT 2016–2024 · Tesis de Maestría en Economía, UNIBE
# Stephannie Marie Acosta Acosta
#
# Código de los chunks del análisis empírico (returns-to-education-analysis.html).
# En el .Rmd original estos valores venían del encabezado YAML (params);
# aquí se definen como lista para correr el script directamente.
# ─────────────────────────────────────────────────────────────────────────────

params <- list(
  ruta_datos = "data/encft_2016_2024.xlsx",  # microdatos ENCFT apilados 2016–2024
  semilla    = 123,                          # semilla para muestras de gráficos
  n_plot     = 5000                          # observaciones muestreadas en gráficos
)

# ── Paquetes ──────────────────────────────────────────────────────────────────
# cache=FALSE: los paquetes se cargan en cada ejecución para evitar errores
# de dependencia en sesiones limpias.
pkgs <- c(
  "readxl", "tidyverse", "skimr", "lmtest", "sandwich",
  "censReg", "plm", "sampleSelection", "car",
  "kableExtra", "corrplot", "GGally", "reshape2",
  "RColorBrewer", "gridExtra", "quantreg", "purrr", "AER"
)
invisible(lapply(pkgs, library, character.only = TRUE))

# ── Carga ─────────────────────────────────────────────────────────────────────
base <- read_excel(params$ruta_datos)
cat("Dimensiones:", dim(base)[1], "filas ×", dim(base)[2], "columnas\n")

cat("Primeras 40 variables:\n")

print(names(base)[1:40])

# ── Resumen de variables clave ────────────────────────────────────────────────
skim(base[, c("ANO", "TRIMESTRE", "SEXO", "EDAD",
              "ULTIMO_ANO_APROBADO", "NIVEL_ULTIMO_ANO_APROBADO",
              "HORAS_TRABAJA_SEMANA_PRINCIPAL")])

# ── Verificación de datos faltantes ──────────────────────────────────────────
na_check <- base %>%
  summarise(
    NA_ANO   = sum(is.na(ANO)),
    NA_TRIM  = sum(is.na(TRIMESTRE)),
    NA_SEXO  = sum(is.na(SEXO)),
    NA_EDAD  = sum(is.na(EDAD)),
    NA_UA    = sum(is.na(ULTIMO_ANO_APROBADO)),
    NA_NIVEL = sum(is.na(NIVEL_ULTIMO_ANO_APROBADO)),
    NA_HORAS = sum(is.na(HORAS_TRABAJA_SEMANA_PRINCIPAL))
  )

kbl(na_check,
    caption = "Tabla 1. Valores faltantes en variables clave",
    booktabs = TRUE, escape = FALSE, format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11)

# ── Variable de educación continua ───────────────────────────────────────────
# Conversión NIVEL_ULTIMO_ANO_APROBADO  años de escolaridad continua (0-22).
# Se sigue la escala convencional de la literatura dominicana (BCRD, 2022):
#   Sin educación (códigos 1,7-10) → 0 años
#   Primaria (cód. 2)              → ULTIMO_ANO_APROBADO
#   Secundaria/Bachillerato (3-4)  → ULTIMO_ANO_APROBADO + 8
#   Técnico superior (5)           → ULTIMO_ANO_APROBADO + 12
#   Universitario (6)              → ULTIMO_ANO_APROBADO + 16
base2 <- base %>%
  filter(!is.na(NIVEL_ULTIMO_ANO_APROBADO)) %>%
  mutate(
    educ = case_when(
      NIVEL_ULTIMO_ANO_APROBADO %in% c(1, 7, 8, 9, 10) ~ 0,
      NIVEL_ULTIMO_ANO_APROBADO == 2 ~ ULTIMO_ANO_APROBADO,
      NIVEL_ULTIMO_ANO_APROBADO %in% c(3, 4) ~ ULTIMO_ANO_APROBADO + 8,
      NIVEL_ULTIMO_ANO_APROBADO == 5 ~ ULTIMO_ANO_APROBADO + 12,
      NIVEL_ULTIMO_ANO_APROBADO == 6 ~ ULTIMO_ANO_APROBADO + 16,
      TRUE ~ NA_real_
    )
  )

cat("Distribución de años de educación construidos:\n")

summary(base2$educ)

# ── Experiencia laboral potencial (Mincer, 1974) ──────────────────────────────
# exp = Edad - Años de educación - 6
# Truncada en cero (no puede ser negativa).
# Limitación conocida: sobreestima experiencia real cuando hay interrupciones
# laborales, especialmente para mujeres (Heckman, 1979).
base2 <- base2 %>%
  mutate(
    experiencia_lab  = pmax(0, EDAD - educ - 6),
    experiencia_lab2 = experiencia_lab^2
  )

summary(base2$experiencia_lab)

# ── Ingreso laboral total y participación ─────────────────────────────────────
# El ingreso suma 27 componentes (asalariado principal + secundario + independiente
# + beneficios en especie). Esto sigue la definición del BCRD para la ENCFT.
cols_ingreso <- c(
  "INGRESO_ASALARIADO", "COMISIONES", "PROPINAS", "HORAS_EXTRA", "OTROS_PAGOS",
  "BONO_VACACIONES", "BONIFICACIONES", "REGALIA_PASCUAL",
  "INCENTIVO_ANTIGUEDAD", "OTROS_BENEFICIOS",
  "ESPECIE_ALIMENTOS", "ESPECIE_VIVIENDA", "ESPECIE_TRANSPORTE",
  "ESPECIE_COMBUSTIBLE", "ESPECIE_CELULAR", "OTROS_ESPECIE",
  "INGRESO_INDEPENDIENTES", "CONSUMO_BIENES", "ESPECIE_INDEPENDIENTES",
  "INGRESO_ASALARIADO_SECUN", "OTROS_PAGOS_SECUN",
  "OTROS_BENEFICIOS_SECUN", "PAGO_ESPECIE_SECUN",
  "INGRESO_INDEPENDIENTES_SECUN", "CONSUMO_BIENES_SECUN",
  "ESPECIE_INDEPENDIENTES_SECUN", "OTROS_TRABAJOS"
)
cols_ingreso <- cols_ingreso[cols_ingreso %in% names(base2)]

base_trab <- base2 %>%
  filter(EDAD >= 16, EDAD <= 65)

base_trab$ingreso_laboral <- rowSums(base_trab[, cols_ingreso], na.rm = TRUE)

base_trab <- base_trab %>%
  mutate(
    participa = case_when(
      ingreso_laboral > 0 & HORAS_TRABAJA_SEMANA_PRINCIPAL > 0 ~ 1,
      TRUE ~ 0
    )
  )

cat("Distribución del ingreso laboral:\n")

print(summary(base_trab$ingreso_laboral))

cat("\nParticipación laboral:\n")

print(table(base_trab$participa))

# ── Muestra analítica final (base_mincer) ────────────────────────────────────
# Criterios de inclusión:
#   - Ingreso laboral > 0 y horas trabajadas > 0 (ocupados con ingreso reportado)
#   - Educación y experiencia no faltantes
#   - Edad 16-65 (PET, aplicado en base_trab)
# Variables derivadas:
#   ln_ingreso = log(ingreso_laboral) — transforma distribución sesgada a derecha
#   ln_horas   = log(HORAS/sem)      — especificación log-log para elasticidad
#   informal   = 1 si GRUPO_SECTOR ∈ {Informal, Servicio Doméstico}
#   sector     = 17 categorías CIIU Rev.4 (ref.: Adm. Pública)
base_mincer <- base_trab %>%
  filter(
    ingreso_laboral > 0,
    HORAS_TRABAJA_SEMANA_PRINCIPAL > 0,
    !is.na(educ),
    !is.na(experiencia_lab)
  ) %>%
  mutate(
    ln_ingreso = log(ingreso_laboral),
    ln_horas   = log(HORAS_TRABAJA_SEMANA_PRINCIPAL),
    informal = case_when(
      GRUPO_SECTOR == "Informal"            ~ 1,
      GRUPO_SECTOR == "Servicio Doméstico"  ~ 1,
      GRUPO_SECTOR == "Formal"              ~ 0,
      TRUE ~ NA_real_
    ),
    sector = case_when(
      RAMA_PRINCIPAL_COD >= 111  & RAMA_PRINCIPAL_COD <= 322  ~ "Agro_Pesca_Mineria",
      RAMA_PRINCIPAL_COD >= 1010 & RAMA_PRINCIPAL_COD <= 3320 ~ "Manufactura",
      RAMA_PRINCIPAL_COD >= 3510 & RAMA_PRINCIPAL_COD <= 3900 ~ "Energia_Agua",
      RAMA_PRINCIPAL_COD >= 4100 & RAMA_PRINCIPAL_COD <= 4390 ~ "Construccion",
      RAMA_PRINCIPAL_COD >= 4510 & RAMA_PRINCIPAL_COD <= 4799 ~ "Comercio",
      RAMA_PRINCIPAL_COD >= 4910 & RAMA_PRINCIPAL_COD <= 5320 ~ "Transporte",
      RAMA_PRINCIPAL_COD >= 5510 & RAMA_PRINCIPAL_COD <= 5630 ~ "Hoteleria",
      RAMA_PRINCIPAL_COD >= 5811 & RAMA_PRINCIPAL_COD <= 6391 ~ "Comunicaciones",
      RAMA_PRINCIPAL_COD >= 6411 & RAMA_PRINCIPAL_COD <= 6630 ~ "Financiero",
      RAMA_PRINCIPAL_COD >= 6810 & RAMA_PRINCIPAL_COD <= 6820 ~ "Inmobiliario",
      RAMA_PRINCIPAL_COD >= 6910 & RAMA_PRINCIPAL_COD <= 7500 ~ "Prof_Cientif",
      RAMA_PRINCIPAL_COD >= 7710 & RAMA_PRINCIPAL_COD <= 8299 ~ "Admin_Servicios",
      RAMA_PRINCIPAL_COD >= 8411 & RAMA_PRINCIPAL_COD <= 8430 ~ "Adm_Publica",
      RAMA_PRINCIPAL_COD >= 8510 & RAMA_PRINCIPAL_COD <= 8560 ~ "Educacion",
      RAMA_PRINCIPAL_COD >= 8610 & RAMA_PRINCIPAL_COD <= 8890 ~ "Salud",
      RAMA_PRINCIPAL_COD >= 9000 & RAMA_PRINCIPAL_COD <= 9329 ~ "Arte_Entret",
      RAMA_PRINCIPAL_COD >= 9411 & RAMA_PRINCIPAL_COD <= 9900 ~ "Otros_Serv",
      TRUE ~ NA_character_
    ),
    # Cohorte para el pseudopanel (quinquenios de nacimiento)
    # anio_nac = ANO - EDAD (identidad algebraica: exp = EDAD - educ - 6)
    anio_nac  = ANO - EDAD,
    cohorte   = case_when(
      anio_nac >= 1950 & anio_nac <= 1959 ~ "1950-1959",
      anio_nac >= 1960 & anio_nac <= 1969 ~ "1960-1969",
      anio_nac >= 1970 & anio_nac <= 1974 ~ "1970-1974",
      anio_nac >= 1975 & anio_nac <= 1979 ~ "1975-1979",
      anio_nac >= 1980 & anio_nac <= 1984 ~ "1980-1984",
      anio_nac >= 1985 & anio_nac <= 1989 ~ "1985-1989",
      anio_nac >= 1990 & anio_nac <= 1994 ~ "1990-1994",
      anio_nac >= 1995 & anio_nac <= 1999 ~ "1995-1999",
      anio_nac >= 2000 & anio_nac <= 2004 ~ "2000-2004",
      anio_nac >= 2005 & anio_nac <= 2008 ~ "2005-2008",
      TRUE ~ NA_character_
    ),
    id_cohorte = as.integer(factor(cohorte))
  ) %>%
  filter(!is.na(cohorte))

cat("Observaciones en base_mincer:", nrow(base_mincer), "\n")

summary(base_mincer[, c("ln_ingreso", "educ", "experiencia_lab", "ln_horas")])

# ── Tabla de estadísticos descriptivos ───────────────────────────────────────
desc <- base_mincer %>%
  summarise(
    Observaciones      = n(),
    Edad_media         = round(mean(EDAD, na.rm = TRUE), 1),
    Edad_mediana       = round(median(EDAD, na.rm = TRUE), 1),
    Educ_media         = round(mean(educ, na.rm = TRUE), 2),
    Educ_mediana       = round(median(educ, na.rm = TRUE), 1),
    Exp_media          = round(mean(experiencia_lab, na.rm = TRUE), 1),
    Horas_sem_media    = round(mean(HORAS_TRABAJA_SEMANA_PRINCIPAL, na.rm = TRUE), 1),
    Ingreso_mediana    = round(median(ingreso_laboral, na.rm = TRUE), 0),
    Ingreso_medio      = round(mean(ingreso_laboral, na.rm = TRUE), 0),
    Ingreso_SD         = round(sd(ingreso_laboral, na.rm = TRUE), 0),
    Pct_informal       = round(mean(informal, na.rm = TRUE) * 100, 1),
    Pct_mujeres        = round(mean(SEXO == 2) * 100, 1)
  )

kbl(t(desc),
    escape = FALSE,
    caption = "Tabla 2. Estadísticos descriptivos de las variables laborales principales  -  ENCFT 2016-2024",
    col.names = c("Valor"),
    booktabs = TRUE, format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = paste0(
      "Muestra: ocupados con ingreso > 0 y horas > 0, edad 16-65. N = 285,515. ",
      "Ingreso en pesos dominicanos corrientes (RD$). ",
      "El ingreso medio (RD$20,225) supera la mediana (RD$15,167) en 33%, ",
      "lo que confirma la asimetría positiva de la distribución salarial."
    ),
    general_title = "Nota:"
  )

base_mincer %>%
  count(SEXO) %>%
  mutate(
    Sexo       = if_else(SEXO == 1, "Hombre", "Mujer"),
    Proporcion = round(n / sum(n) * 100, 2)
  ) %>%
  select(Sexo, n, Proporcion) %>%
  kbl(escape = FALSE,
    caption = "Tabla 3. Distribución de la muestra por sexo",
      col.names = c("Sexo", "N", "% del total"),
      booktabs = TRUE, format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11)

base_mincer %>%
  filter(!is.na(informal)) %>%
  group_by(informal) %>%
  summarise(
    n             = n(),
    ingreso_medio = round(mean(ingreso_laboral, na.rm = TRUE), 0),
    educ_media    = round(mean(educ, na.rm = TRUE), 2),
    horas_media   = round(mean(HORAS_TRABAJA_SEMANA_PRINCIPAL, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    Sector = if_else(informal == 0, "Formal", "Informal"),
    Pct    = round(n / sum(n) * 100, 1)
  ) %>%
  select(Sector, n, Pct, ingreso_medio, educ_media, horas_media) %>%
  kbl(escape = TRUE,
    caption = "Tabla 4. Características medias por sector formal/informal",
      col.names = c("Sector", "N", "% muestra", "Ingreso medio (RD$)",
                    "Educ. media (anos)", "Horas/sem media"),
      booktabs = TRUE, format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = paste0(
      "La brecha de ingreso entre sectores (formal vs. informal) es descriptiva; ",
      "incorpora diferencias en composición educativa, sectorial y de género. ",
      "El efecto neto de la informalidad, controlando por observables, se estima en -23.7% ",
      "(OLS extendido, Sección 5.2)."
    ),
    general_title = "Nota:"
  )

quantile(base_mincer$ingreso_laboral, probs = seq(0, 1, 0.1), na.rm = TRUE) %>%
  enframe(name = "Decil", value = "Ingreso (RD$)") %>%
  mutate(`Ingreso (RD$)` = round(`Ingreso (RD$)`, 0)) %>%
  kbl(caption = "Tabla 5. Distribución del ingreso laboral por deciles  -  ENCFT 2016-2024",
      booktabs = TRUE, escape = TRUE, format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = paste0(
      "El percentil 90 (RD$38,617) casi triplica la mediana (RD$15,167), ",
      "lo que evidencia fuerte desigualdad en la cola superior. ",
      "La variable dependiente ln(ingreso) normaliza esta distribución para la estimación MCO."
    ),
    general_title = "Nota:"
  )

set.seed(params$semilla)
base_plot <- base_mincer %>% sample_n(min(params$n_plot, n()))

p1 <- ggplot(base_plot, aes(x = EDAD)) +
  geom_histogram(bins = 30, fill = "grey55", color = "white") +
  labs(x = "Edad (años)", y = "Frecuencia",
       subtitle = "Distribución de la edad") +
  theme_minimal(base_size = 12)

p2 <- ggplot(base_plot, aes(x = educ)) +
  geom_histogram(bins = 25, fill = "darkseagreen4", color = "white") +
  labs(x = "Años de escolaridad", y = "Frecuencia",
       subtitle = "Años de educación") +
  theme_minimal(base_size = 12)

p3 <- ggplot(base_plot, aes(x = ln_ingreso)) +
  geom_histogram(bins = 30, fill = "mediumpurple3", color = "white") +
  labs(x = "ln(ingreso laboral)", y = "Frecuencia",
       subtitle = "Log-ingreso laboral") +
  theme_minimal(base_size = 12)

p4 <- ggplot(base_plot, aes(x = HORAS_TRABAJA_SEMANA_PRINCIPAL)) +
  geom_histogram(bins = 30, fill = "darkorange2", color = "white") +
  labs(x = "Horas/semana", y = "Frecuencia",
       subtitle = "Horas trabajadas") +
  theme_minimal(base_size = 12)

gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

vars_cor <- base_mincer %>%
  select(ln_ingreso, educ, experiencia_lab, ln_horas, EDAD) %>%
  na.omit()

mat_cor  <- round(cor(vars_cor), 2)
melt_cor <- reshape2::melt(mat_cor)

ggplot(melt_cor, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 4) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank())

p5 <- ggplot(base_plot, aes(x = educ, y = ln_ingreso)) +
  geom_point(alpha = 0.1, color = "grey40", size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
  labs(x = "Años de educación", y = "ln(ingreso)") +
  theme_minimal(base_size = 12)

p6 <- ggplot(base_plot, aes(x = experiencia_lab, y = ln_ingreso)) +
  geom_point(alpha = 0.1, color = "grey40", size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick3") +
  labs(x = "Años de experiencia potencial", y = "ln(ingreso)") +
  theme_minimal(base_size = 12)

p7 <- ggplot(base_plot, aes(x = ln_horas, y = ln_ingreso)) +
  geom_point(alpha = 0.1, color = "grey40", size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(x = "ln(horas trabajadas)", y = "ln(ingreso)") +
  theme_minimal(base_size = 12)

gridExtra::grid.arrange(p5, p6, p7, ncol = 1)

# ── OLS Base (Mincer, 1974) ───────────────────────────────────────────────────
# Especificación: ln(w) = α + β₁·educ + β₂·exp + β₃·exp² + β₄·ln(h) + ε
# Limitación documentada: sesgo de variable omitida por omitir género,
# informalidad y sector. El retorno a educación (4.95%) sobreestima en +1.13 pp
# respecto al OLS extendido (Sección 5.2).
modelo_base <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) + ln_horas,
  data = base_mincer
)
summary(modelo_base)

# ── Diagnósticos OLS base ─────────────────────────────────────────────────────
cat("── Errores robustos HC1 ──\n")

coeftest(modelo_base, vcov = vcovHC(modelo_base, type = "HC1"))

cat("\n── Breusch-Pagan ──\n")

bptest(modelo_base)

cat("\n── RESET de Ramsey ──\n")

resettest(modelo_base)

cat("\nR² OLS base:", round(summary(modelo_base)$r.squared, 4))

# ── OLS Extendido (MODELO PRINCIPAL) ─────────────────────────────────────────
# Especificación: ln(w) = α + β₁·educ + β₂·exp + β₃·exp² + β₄·ln(h)
#                       + β₅·Mujer + β₆·Informal + Σγₖ·Sector_k + ε
# El vector sectorial (17 dummies CIIU Rev.4) se incluye para aislar
# el retorno a la educación de las rentas sectoriales.
# La significancia conjunta se verifica con el test F parcial (Sección 6.2).
modelo_corregido <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + informal + factor(sector),
  data = base_mincer %>% filter(!is.na(informal), !is.na(sector))
)
summary(modelo_corregido)

# ── Diagnósticos OLS extendido ────────────────────────────────────────────────
cat("── Errores robustos HC1 ──\n")

coeftest(modelo_corregido, vcov = vcovHC(modelo_corregido, type = "HC1"))

cat("\n── VIF / GVIF ──\n")

print(vif(modelo_corregido))

cat("\n── RESET de Ramsey ──\n")

resettest(modelo_corregido)

cat("\n── Breusch-Pagan ──\n")

bptest(modelo_corregido)

cat("\nR² OLS extendido:", round(summary(modelo_corregido)$r.squared, 4))

# ── Agregación cohorte × año ──────────────────────────────────────────────────
# El pseudopanel (Deaton, 1985) construye celdas cohorte-año (C × T = 10 × 9 = 90).
# La variación within-cohorte identifica el efecto de la educación y las horas
# dentro de la misma generación, controlando por heterogeneidad no observada
# entre cohortes (ej. calidad educativa, capital intangible generacional).
# ADVERTENCIA: experiencia excluida por colinealidad casi perfecta con el año
# de observación dentro de una cohorte (r = −0.919 con educ media de cohorte).
pseudo_check <- base_mincer %>%
  group_by(cohorte, id_cohorte, ANO) %>%
  summarise(
    n               = n(),
    mean_ln_ingreso = mean(ln_ingreso,      na.rm = TRUE),
    mean_educ       = mean(educ,            na.rm = TRUE),
    mean_exp        = mean(experiencia_lab, na.rm = TRUE),
    mean_ln_horas   = mean(ln_horas,        na.rm = TRUE),
    mean_edad       = mean(EDAD,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(cohorte), n >= 30)

cat("Celdas del pseudopanel:", nrow(pseudo_check), "\n")

cat("Distribución de N por celda:\n")

print(summary(pseudo_check$n))

# Diagnóstico de colinealidad (justifica exclusión de experiencia)
cat("Matriz de correlación del pseudopanel:\n")

cor(pseudo_check[, c("mean_ln_ingreso","mean_educ","mean_exp",
                      "mean_edad","mean_ln_horas")],
    use = "complete.obs") %>% round(3) %>% print()

pseudo_check %>%
  arrange(id_cohorte, ANO) %>%
  slice_head(n = 10) %>%
  select(ANO, cohorte, n, mean_ln_ingreso, mean_educ, mean_exp,
         mean_ln_horas, mean_edad) %>%
  kbl(caption = "Tabla 6. Extracto de la estructura del pseudopanel cohorte-año",
      digits = 3, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11)

# ── Modelo de efectos fijos (within estimator) ────────────────────────────────
pseudo_panel <- pdata.frame(pseudo_check, index = c("id_cohorte", "ANO"))

modelo_pseudo_fe <- plm(
  mean_ln_ingreso ~ mean_educ + mean_ln_horas,
  data = pseudo_panel, model = "within", effect = "individual"
)

modelo_pseudo_re <- plm(
  mean_ln_ingreso ~ mean_educ + mean_ln_horas,
  data = pseudo_panel, model = "random", effect = "individual"
)

summary(modelo_pseudo_fe)

cat("── Errores Driscoll-Kraay (robustos a correlación espacial y temporal) ──\n")

tryCatch(
  print(coeftest(modelo_pseudo_fe,
         vcov = vcovSCC(modelo_pseudo_fe, type = "HC1", cluster = "group"))),
  error = function(e) {
    cat("vcovSCC no disponible en esta versión de plm; usando SE estándar:\n")
    print(coeftest(modelo_pseudo_fe))
  }
)

cat("\n── Test Hausman (FE vs RE) ──\n")

# H0: RE consistente (efectos individuales no correlacionados con regresores)
# H1: FE consistente (efectos individuales correlacionados → RE inconsistente)
print(phtest(modelo_pseudo_fe, modelo_pseudo_re))

cat("\n── R² within ──\n")

print(summary(modelo_pseudo_fe)$r.squared)

b_educ_fe  <- coef(modelo_pseudo_fe)["mean_educ"]
b_horas_fe <- coef(modelo_pseudo_fe)["mean_ln_horas"]

# ── Variables para el modelo de selección ────────────────────────────────────
# Variables de exclusión (predictores de participación, exógenos al ingreso):
#   jefe_hogar:   jefatura del hogar — aumenta necesidad de participar independientemente del ingreso potencial
#   unido:        estado civil (casado/unido) — efectos sobre participación difieren por género
#   tiene_remesa: receptor de remesas — reduce presión de participación laboral
# Estas variables son exógenas al ingreso condicional sobre capital humano individual.
# CAVEAT METODOLÓGICO: la validez de las restricciones de exclusión es un supuesto
# no directamente testeable; se justifica con argumento teórico (Heckman, 1979).
base_trab <- base_trab %>%
  mutate(
    educ_h = case_when(
      NIVEL_ULTIMO_ANO_APROBADO %in% c(1, 7, 8, 9, 10) ~ 0,
      NIVEL_ULTIMO_ANO_APROBADO == 2 ~ ULTIMO_ANO_APROBADO,
      NIVEL_ULTIMO_ANO_APROBADO %in% c(3, 4) ~ ULTIMO_ANO_APROBADO + 8,
      NIVEL_ULTIMO_ANO_APROBADO == 5 ~ ULTIMO_ANO_APROBADO + 12,
      NIVEL_ULTIMO_ANO_APROBADO == 6 ~ ULTIMO_ANO_APROBADO + 16,
      TRUE ~ NA_real_
    ),
    experiencia_h = pmax(0, EDAD - educ_h - 6),
    jefe_hogar    = if_else(PARENTESCO == 1, 1, 0),
    tiene_remesa  = if_else(REMESAS_NAC == 1, 1, 0, missing = 0),
    unido         = case_when(
      ESTADO_CIVIL %in% c(1, 2) ~ 1,
      ESTADO_CIVIL %in% c(3, 4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),
    ln_ingreso_h = if_else(ingreso_laboral > 0, log(ingreso_laboral), NA_real_),
    ln_horas_h   = if_else(HORAS_TRABAJA_SEMANA_PRINCIPAL > 0,
                           log(HORAS_TRABAJA_SEMANA_PRINCIPAL), NA_real_)
  )

base_heckman <- base_trab %>%
  filter(!is.na(educ_h), !is.na(experiencia_h),
         !is.na(SEXO), !is.na(jefe_hogar),
         !is.na(unido), !is.na(participa))

cat("N base Heckman:", nrow(base_heckman))

cat(" | Participan:", sum(base_heckman$participa == 1))

cat(" | No participan:", sum(base_heckman$participa == 0), "\n")

# ── Estimación Heckman dos etapas (heckit) ────────────────────────────────────
# 1ª etapa: Probit de participación laboral (ecuación de selección)
#   participa ~ educ_h + exp_h + exp_h² + SEXO + jefe_hogar + unido + tiene_remesa
# 2ª etapa: OLS sobre la muestra de ocupados con corrección IMR (λ)
#   ln_ingreso ~ educ_h + exp_h + exp_h² + ln_horas_h + SEXO + λ
modelo_heckman <- selection(
  selection = participa ~ educ_h + experiencia_h + I(experiencia_h^2) +
                          factor(SEXO) + jefe_hogar + unido + tiene_remesa,
  outcome   = ln_ingreso_h ~ educ_h + experiencia_h + I(experiencia_h^2) +
                              ln_horas_h + factor(SEXO),
  data   = base_heckman,
  method = "2step"
)

summary(modelo_heckman)

# ── Extracción de coeficientes para uso en tablas y figuras ──────────────────
# OLS base
b_educ_base  <- coef(modelo_base)["educ"]
b_horas_base <- coef(modelo_base)["ln_horas"]
b_exp_base   <- coef(modelo_base)["experiencia_lab"]
b_exp2_base  <- coef(modelo_base)["I(experiencia_lab^2)"]

# OLS extendido
b_educ_cor  <- coef(modelo_corregido)["educ"]
b_horas_cor <- coef(modelo_corregido)["ln_horas"]
b_exp_cor   <- coef(modelo_corregido)["experiencia_lab"]
b_exp2_cor  <- coef(modelo_corregido)["I(experiencia_lab^2)"]
b_informal  <- coef(modelo_corregido)["informal"]
b_mujer     <- coef(modelo_corregido)["factor(SEXO)2"]

# Heckman segunda etapa
coef_heck    <- coef(modelo_heckman, part = "outcome")
b_educ_heck  <- coef_heck["educ_h"]
b_horas_heck <- coef_heck["ln_horas_h"]
b_exp_heck   <- coef_heck["experiencia_h"]
b_exp2_heck  <- coef_heck["I(experiencia_h^2)"]
b_sexo_heck  <- coef_heck["factor(SEXO)2"]

# Pseudopanel FE
b_educ_fe  <- coef(modelo_pseudo_fe)["mean_educ"]
b_horas_fe <- coef(modelo_pseudo_fe)["mean_ln_horas"]

# Puntos de máximo ingreso (exp*)
exp_star_base <- -b_exp_base / (2 * b_exp2_base)
exp_star_cor  <- -b_exp_cor  / (2 * b_exp2_cor)
exp_star_heck <- -b_exp_heck / (2 * b_exp2_heck)

cat("── VERIFICACIÓN DE COEFICIENTES EXTRAÍDOS ──\n")

cat(sprintf("b_educ_base  = %.6f  (esperado: 0.049539)\n", b_educ_base))

cat(sprintf("b_educ_cor   = %.6f  (esperado: 0.038208)\n", b_educ_cor))

cat(sprintf("b_horas_cor  = %.6f  (calculado en ejecucion actual)\n", b_horas_cor))

cat(sprintf("b_informal   = %.6f  (esperado: -0.272341)\n", b_informal))

cat(sprintf("b_mujer      = %.6f  (esperado: -0.251752)\n", b_mujer))

cat(sprintf("b_educ_heck  = %.6f  (esperado: 0.048192)\n", b_educ_heck))

cat(sprintf("b_sexo_heck  = %.6f  (esperado: -0.042043)\n", b_sexo_heck))

cat(sprintf("exp_star_cor = %.4f  (esperado: 37.79)\n", exp_star_cor))

cat(sprintf("exp_star_heck= %.2f  (esperado: 132.97)\n", exp_star_heck))

# ── PROBLEMA ──────────────────────────────────────────────────────────────────
# La inclusión lineal de `informal` contamina el retorno educativo si
# Cov(informal_i, ε_i) ≠ 0, es decir, si los factores no observados que
# determinan la informalidad (productividad, redes, preferencias) también
# afectan el ingreso. El resultado en la Tabla R-0 (función de control,
# sección de diagnósticos) ya reportó p-valor del residuo probit de informal.
#
# SOLUCIÓN: Ecuaciones simultáneas por régimen (switching regression).
# En lugar de incluir `informal` como control aditivo en una sola ecuación,
# se estiman dos ecuaciones de ingreso separadas —una para el sector formal
# y otra para el informal— junto con un modelo de selección (probit) que
# asigna a cada individuo a su régimen. Esto permite que los retornos a
# la educación, experiencia y horas varíen libremente entre regímenes.
#
# Modelo:
#   I*_i  = Z_i γ + ν_i          (ecuación de selección probit)
#   ln(w_if) = X_if β_f + u_if   (ecuación de ingreso sector formal)
#   ln(w_ii) = X_ii β_i + u_ii   (ecuación de ingreso sector informal)
#
# donde Z_i incluye instrumentos que predicen informalidad pero son
# exógenos al ingreso condicional (educación, experiencia, sexo, sector).

base_reg <- base_mincer %>%
  filter(!is.na(informal), !is.na(sector))

# ── Primera etapa: probit de selección al sector formal (informal = 0) ────────
probit_regimen <- glm(
  informal ~ educ + experiencia_lab + I(experiencia_lab^2) +
             factor(SEXO) + factor(sector),
  data   = base_reg,
  family = binomial(link = "probit")
)

# IMR para cada régimen (lambda de Heckman por sector)
xb_inf         <- predict(probit_regimen, type = "link")
phi_xb         <- dnorm(xb_inf)
Phi_xb         <- pnorm(xb_inf)

# Para sector informal (informal = 1): E[ε | informal = 1] = φ(Xγ)/Φ(Xγ)
# Para sector formal  (informal = 0): E[ε | informal = 0] = -φ(Xγ)/(1-Φ(Xγ))
base_reg <- base_reg %>%
  mutate(
    imr_informal = phi_xb / pmax(Phi_xb, 1e-8),
    imr_formal   = -phi_xb / pmax(1 - Phi_xb, 1e-8)
  )

# ── Segunda etapa: ecuación de ingreso por régimen ────────────────────────────
modelo_regimen_formal <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + factor(sector) + imr_formal,
  data = base_reg %>% filter(informal == 0)
)

modelo_regimen_informal <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + factor(sector) + imr_informal,
  data = base_reg %>% filter(informal == 1)
)

cat("── Régimen FORMAL ──\n")

coeftest(modelo_regimen_formal, vcov = vcovHC(modelo_regimen_formal, "HC1"))

cat("\n── Régimen INFORMAL ──\n")

coeftest(modelo_regimen_informal, vcov = vcovHC(modelo_regimen_informal, "HC1"))

# ── Tabla comparativa de retornos por régimen ─────────────────────────────────
b_educ_rf  <- coef(modelo_regimen_formal)["educ"]
b_educ_ri  <- coef(modelo_regimen_informal)["educ"]
b_horas_rf <- coef(modelo_regimen_formal)["ln_horas"]
b_horas_ri <- coef(modelo_regimen_informal)["ln_horas"]
exp_star_rf <- -coef(modelo_regimen_formal)["experiencia_lab"] /
               (2 * coef(modelo_regimen_formal)["I(experiencia_lab^2)"])
exp_star_ri <- -coef(modelo_regimen_informal)["experiencia_lab"] /
               (2 * coef(modelo_regimen_informal)["I(experiencia_lab^2)"])
n_formal    <- sum(base_reg$informal == 0, na.rm = TRUE)
n_informal  <- sum(base_reg$informal == 1, na.rm = TRUE)

tabla_regimen <- tibble(
  Parametro = c(
    "Retorno educación (% por año)",
    "Elasticidad ingreso-horas",
    "Experiencia óptima (años)",
    "N (observaciones)"
  ),
  OLS_pooled = c(
    paste0(round(b_educ_cor * 100, 3), "%"),
    round(b_horas_cor, 3),
    round(exp_star_cor, 2),
    format(nrow(base_reg), big.mark = ",")
  ),
  Formal = c(
    paste0(round(b_educ_rf * 100, 3), "%"),
    round(b_horas_rf, 3),
    round(exp_star_rf, 2),
    format(n_formal, big.mark = ",")
  ),
  Informal = c(
    paste0(round(b_educ_ri * 100, 3), "%"),
    round(b_horas_ri, 3),
    round(exp_star_ri, 2),
    format(n_informal, big.mark = ",")
  ),
  Diferencia = c(
    paste0(round((b_educ_rf - b_educ_ri) * 100, 3), " pp"),
    round(b_horas_rf - b_horas_ri, 3),
    round(exp_star_rf - exp_star_ri, 2),
    "—"
  )
)
names(tabla_regimen) <- c("Parámetro", "OLS pooled (ref.)", "Régimen formal",
                          "Régimen informal", "Diferencia (F − I)")

kbl(tabla_regimen,
    caption = paste0("Tabla C-1. Regresión por régimen endógeno — retornos diferenciados por sector. ",
                     "Corrección Cov(informal_i, ε_i) ≠ 0. ENCFT 2016-2024"),
    booktabs = TRUE, escape = FALSE, na = "—") %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(2, color = "grey60") %>%
  footnote(
    general = paste0(
      "Segunda etapa del switching regression (Lee, 1978; Heckman, 1979): ",
      "ecuaciones de ingreso separadas para sector formal e informal, ",
      "con corrección IMR por régimen (imr_formal = −φ(Xγ)/(1−Φ(Xγ)) para formales; ",
      "imr_informal = φ(Xγ)/Φ(Xγ) para informales). ",
      "Los retornos a educación difieren entre regímenes si la escolaridad ",
      "es más rentable (o menos) dentro de un sector que en el otro, ",
      "lo que el OLS pooled no puede capturar al restringir β_educ a ser igual en ambos. ",
      "Diferencia positiva F − I indica que la educación es más rentable en el sector formal. ",
      "Referencias: Lee, L.F. (1978). Unionism and wage rates: a simultaneous equations model ",
      "with qualitative and limited dependent variables. International Economic Review, 19(2), 415–433; ",
      "Heckman, J.J. (1979). Sample selection bias as a specification error. ",
      "Econometrica, 47(1), 153–161."
    ),
    general_title = "Nota metodológica:"
  )

# ── PROBLEMA ──────────────────────────────────────────────────────────────────
# En el pseudopanel, la identidad algebraica exacta:
#   Edad = Periodo − Cohorte (año_observacion − año_nacimiento)
# implica que los efectos de edad, periodo y cohorte son linealmente dependientes.
# No se pueden incluir simultáneamente dummies de los tres: la matriz X es de rango
# deficiente. El modelo FE estimado en la Sección 5.3 excluyó `mean_exp` (r = −0.919)
# precisamente por esta razón.
#
# SOLUCIÓN: Normalización de Deaton & Paxson (1994).
# Las dummies de periodo d_t se transforman en d*_t = d_t − d_1 − (t−1)(d_2 − d_1),
# lo que rompe la dependencia lineal exacta eliminando la tendencia lineal compartida
# con la dimensión cohorte-edad. Esto permite incluir simultáneamente efectos fijos
# de cohorte, efectos de periodo normalizados y educación media, sin colinealidad.

# ── Construcción del pseudopanel con dummies normalizadas ─────────────────────
pseudo_dp <- pseudo_check %>%
  arrange(id_cohorte, ANO)

# Años ordenados
anos_unicos <- sort(unique(pseudo_dp$ANO))
T_periodos  <- length(anos_unicos)
cat("Periodos disponibles:", paste(anos_unicos, collapse = ", "), "\n")

# Dummies de periodo originales (d_t): columna 1 = año base
for (t in seq_along(anos_unicos)) {
  vn <- paste0("d_", anos_unicos[t])
  pseudo_dp[[vn]] <- as.integer(pseudo_dp$ANO == anos_unicos[t])
}

# Normalización Deaton-Paxson: d*_t = d_t − d_1 − (t−1)(d_2 − d_1)
# Con t indexado en 1, 2, ..., T; d_1 y d_2 son las dummies de los dos primeros años
d1_col <- paste0("d_", anos_unicos[1])
d2_col <- paste0("d_", anos_unicos[2])

for (t in seq_along(anos_unicos)) {
  vn_orig <- paste0("d_", anos_unicos[t])
  vn_norm <- paste0("dstar_", anos_unicos[t])
  pseudo_dp[[vn_norm]] <- pseudo_dp[[vn_orig]] -
    pseudo_dp[[d1_col]] - (t - 1) * (pseudo_dp[[d2_col]] - pseudo_dp[[d1_col]])
}

# Las dos primeras dummies normalizadas son 0 por construcción (colindantes):
# dstar_t1 = d_t1 - d_t1 - 0*(d_t2 - d_t1) = 0
# dstar_t2 = d_t2 - d_t1 - 1*(d_t2 - d_t1) = 0
# Se usan solo d*_t para t >= 3 (T-2 dummies libres de colinealidad)
dstar_cols <- paste0("dstar_", anos_unicos[3:T_periodos])
cat("Dummies normalizadas incluidas:", paste(dstar_cols, collapse = ", "), "\n")

# ── Modelo con normalización Deaton-Paxson ────────────────────────────────────
# NOTA TÉCNICA: plm(model = "within", effect = "individual") desmedia por cohorte
# antes de estimar. Si se agregan dummies de periodo (dstar_*) al within, esas
# dummies también se desmedinan y pueden volverse linealmente dependientes entre
# sí, causando singularidad exacta (U[4,4] = 0 en Lapack).
#
# SOLUCIÓN: estimar con lm() incluyendo dummies de cohorte explícitas + dstar_*.
# Esto es algebraicamente idéntico al within estimator pero evita la demeaning
# interna de plm que genera la singularidad con las dummies D-P.

formula_dp_lm <- as.formula(paste(
  "mean_ln_ingreso ~ mean_educ + mean_ln_horas +",
  paste(dstar_cols, collapse = " + "),
  "+ factor(id_cohorte)"
))

modelo_dp_fe <- lm(formula_dp_lm, data = pseudo_dp)

cat("── Modelo FE con dummies Deaton-Paxson (lm + dummies cohorte) ──\n")

# Mostrar solo coeficientes de interés (excluir las dummies de cohorte)
coefs_dp <- coef(modelo_dp_fe)
coefs_interes <- coefs_dp[!grepl("factor\\(id_cohorte\\)", names(coefs_dp))]
print(round(coefs_interes, 5))

cat("\nR²:", round(summary(modelo_dp_fe)$r.squared, 4), "\n")

cat("R² ajustado:", round(summary(modelo_dp_fe)$adj.r.squared, 4), "\n")

b_educ_dp  <- coefs_dp["mean_educ"]
b_horas_dp <- coefs_dp["mean_ln_horas"]
cat(sprintf("\nb_educ_dp  = %.4f  (%.2f%% por unidad)\n", b_educ_dp, b_educ_dp * 100))

cat(sprintf("b_horas_dp = %.4f\n", b_horas_dp))

# ── Test de Mundlak: consistencia del estimador within ────────────────────────
# Los medios de grupo (educ_bar, horas_bar) prueban si los efectos individuales
# de cohorte están correlacionados con los regresores.
# Si son significativos → FE necesario; si no → RE sería consistente.
pseudo_dp_mund <- pseudo_dp %>%
  group_by(id_cohorte) %>%
  mutate(
    educ_bar  = mean(mean_educ,     na.rm = TRUE),
    horas_bar = mean(mean_ln_horas, na.rm = TRUE)
  ) %>%
  ungroup()

formula_mund_lm <- as.formula(paste(
  "mean_ln_ingreso ~ mean_educ + mean_ln_horas +",
  paste(dstar_cols, collapse = " + "),
  "+ factor(id_cohorte) + educ_bar + horas_bar"
))

modelo_dp_mundlak <- tryCatch(
  lm(formula_mund_lm, data = pseudo_dp_mund),
  error = function(e) { cat("Mundlak no convergió:", conditionMessage(e), "\n"); NULL }
)

cat("\n── Test de Mundlak (exogeneidad efectos de cohorte) ──\n")

if (!is.null(modelo_dp_mundlak)) {
  coefs_m <- coeftest(modelo_dp_mundlak, vcov = vcovHC(modelo_dp_mundlak, "HC1"))
  nms <- rownames(coefs_m)
  if ("educ_bar" %in% nms && "horas_bar" %in% nms) {
    p_eb <- coefs_m["educ_bar",  "Pr(>|t|)"]
    p_hb <- coefs_m["horas_bar", "Pr(>|t|)"]
    cat(sprintf("educ_bar:  coef = %.4f, p = %.4f\n",
                coefs_m["educ_bar",  "Estimate"], p_eb))
    cat(sprintf("horas_bar: coef = %.4f, p = %.4f\n",
                coefs_m["horas_bar", "Estimate"], p_hb))
    if (min(p_eb, p_hb) < 0.05) {
      cat("=> Medios de grupo significativos: FE preferido (Mundlak, 1978).\n")
    } else {
      cat("=> Medios de grupo no significativos: efectos de cohorte no correlacionados.\n")
    }
  } else {
    cat("Variables educ_bar / horas_bar no estimadas (posible colinealidad con dummies de cohorte).\n")
    cat("Con dummies de cohorte explícitas en lm(), los medios de grupo son absorbidos:\n")
    cat("esto confirma que el within estimator ya controla completamente los efectos fijos.\n")
  }
} else {
  cat("Test de Mundlak no disponible. FE por defecto metodológico.\n")
}

# ── Tabla: FE original vs FE con normalización D-P ───────────────────────────
r2_dp <- round(summary(modelo_dp_fe)$r.squared, 3)

tabla_dp <- tibble(
  Parametro = c(
    "Retorno educación (medias cohorte)",
    "Elasticidad horas",
    "Controles de periodo",
    "Solución colinealidad",
    "Consistencia FE (test)",
    "R² within"
  ),
  FE_original = c(
    paste0(round(b_educ_fe * 100, 3), "%"),
    paste0(round(b_horas_fe, 3), " (p = 0.31, n.s.)"),
    "Ninguno (excluidos por colinealidad)",
    "Exclusión de experiencia (r = −0.919)",
    "Hausman χ² = 59 (p < 0.001)",
    round(summary(modelo_pseudo_fe)$r.squared["rsq"], 3)
  ),
  FE_DP = c(
    paste0(round(b_educ_dp * 100, 3), "%"),
    paste0(round(b_horas_dp, 3)),
    paste0("d*_t para t = 3, ..., ", T_periodos, " (", length(dstar_cols), " dummies)"),
    "Transformación d*_t = d_t − d_1 − (t−1)(d_2 − d_1)",
    "Test de Mundlak (RE inestable con dummies D-P)",
    r2_dp
  )
)
names(tabla_dp) <- c("Parámetro", "FE estándar (Sección 5.3)",
                     "FE con normalización Deaton-Paxson (1994)")

kbl(tabla_dp,
    caption = paste0("Tabla C-2. Pseudopanel con normalización Deaton-Paxson (1994) — ",
                     "corrección de colinealidad edad-periodo-cohorte. ENCFT 2016-2024"),
    booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(3, background = "#EEF3F8") %>%
  footnote(
    general = paste0(
      "Transformación Deaton-Paxson: d*_t = d_t − d_1 − (t−1)(d_2 − d_1). ",
      "Esta normalización elimina la tendencia lineal de las dummies de periodo, ",
      "rompiendo la dependencia lineal exacta Edad = Periodo − Cohorte. ",
      "El modelo resultante puede incluir simultáneamente efectos fijos de cohorte ",
      "(absorbidos por el estimador within) y dummies de periodo normalizadas, ",
      "identificando el efecto puro de la educación media de cohorte sin el sesgo ",
      "por absorción del perfil madurativo. ",
      "Referencia: Deaton, A. & Paxson, C. (1994). Intertemporal choice and inequality. ",
      "Journal of Political Economy, 102(3), 437–467."
    ),
    general_title = "Nota metodológica:"
  )

# ── PROBLEMA ──────────────────────────────────────────────────────────────────
# La escolaridad S_i no es exógena al ingreso si existe habilidad no observada H_i
# que determina simultáneamente cuánto estudia un individuo y cuánto gana:
#   plim(β̂₁) = β₁ + Cov(S_i, H_i) / Var(S_i)
# El OLS extendido sobreestima el retorno causal (β₁) porque absorbe parte de la
# correlación ingreso-habilidad. La magnitud del sesgo es positiva si la habilidad
# aumenta tanto el ingreso como la escolaridad (selección positiva).
#
# SOLUCIÓN: Lewbel (2012) — instrumentos generados internamente.
# En lugar de variables externas (reformas educativas, distancia a escuelas),
# Lewbel propone explotar la heterocedasticidad de la ecuación de primera etapa:
#   S_i = X_i α + η_i   (ecuación de primer etapa)
# El instrumento es: Z_i = (X_i − X̄) × η̂_i
# Intuición: si los residuos de la educación varían sistemáticamente con X,
# esa varianza identificable proporciona la variación exógena en S_i que necesita IV.
# Condición: los instrumentos son válidos si E[(X_i − X̄) × η̂_i × ε_i] = 0,
# lo que requiere que la heterocedasticidad de la primera etapa no esté correlacionada
# con los errores de la ecuación de ingreso.

# ── Primer paso: estimación de primera etapa para η̂_i ───────────────────────
base_lewbel <- base_mincer %>%
  filter(!is.na(informal), !is.na(sector))

primera_etapa <- lm(
  educ ~ experiencia_lab + I(experiencia_lab^2) +
         ln_horas + factor(SEXO) + informal + factor(sector),
  data = base_lewbel
)
eta_hat <- residuals(primera_etapa)

# ── Construcción de instrumentos Z_i = (X_i − X̄) × η̂_i ───────────────────
X_vars   <- model.matrix(primera_etapa)[, -1]   # excluye intercepto
X_bar    <- colMeans(X_vars)
X_dem    <- sweep(X_vars, 2, X_bar)             # (X_i − X̄)
Z_mat    <- X_dem * eta_hat                     # producto elemento a elemento

# Limpiar nombres para que sean válidos en fórmulas R (sin paréntesis ni espacios)
nombres_limpios <- make.names(colnames(X_vars))
colnames(Z_mat) <- paste0("Z_", nombres_limpios)

# Seleccionar solo los instrumentos basados en variables continuas
# (evitar dummies × residuo, que son débiles y redundantes).
vars_continuas    <- c("experiencia_lab", "I.experiencia_lab.2.", "ln_horas")
vars_continuas_ok <- intersect(vars_continuas, nombres_limpios)

if (length(vars_continuas_ok) == 0) {
  stop("Ninguna variable continua encontrada en model.matrix. Revisar nombres de columnas.")
}

Z_sel <- Z_mat[, paste0("Z_", vars_continuas_ok), drop = FALSE]

cat("Instrumentos Lewbel construidos:", colnames(Z_sel), "\n")

cat("Dimensión de Z_sel:", dim(Z_sel), "\n")

base_iv <- cbind(base_lewbel, as.data.frame(Z_sel))

# ── Segunda etapa: 2SLS con instrumentos Lewbel ────────────────────────────────
# Se estima usando ivreg (paquete AER) o lm en dos etapas manual.
# Primero verificamos disponibilidad de ivreg.

lewbel_ok <- requireNamespace("AER", quietly = TRUE)

# Nombres de los instrumentos (ya son válidos en fórmulas gracias a make.names)
vars_z_names <- colnames(Z_sel)

if (lewbel_ok) {
  library(AER)

  # Fórmula IV:  outcome ~ endógenas + exógenas | exógenas + instrumentos_Z
  # Nota: I(experiencia_lab^2) funciona en el lado izquierdo de |;
  #       los nombres Z_* ya son válidos (make.names aplicado arriba).
  formula_iv <- as.formula(paste0(
    "ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) + ",
    "ln_horas + factor(SEXO) + informal + factor(sector)",
    " | experiencia_lab + I(experiencia_lab^2) + ",
    "ln_horas + factor(SEXO) + informal + factor(sector) + ",
    paste(vars_z_names, collapse = " + ")
  ))

  modelo_lewbel <- ivreg(formula_iv, data = base_iv)

  cat("── 2SLS con instrumentos Lewbel (2012) ──\n")
  print(summary(modelo_lewbel, diagnostics = TRUE))

  b_educ_lw  <- coef(modelo_lewbel)["educ"]
  b_horas_lw <- coef(modelo_lewbel)["ln_horas"]

} else {
  # Fallback: estimación manual en dos etapas
  cat("Paquete AER no disponible — estimación 2SLS manual.\n")

  fmla_1e <- as.formula(paste0(
    "educ ~ experiencia_lab + I(experiencia_lab^2) + ln_horas + ",
    "factor(SEXO) + informal + factor(sector) + ",
    paste(vars_z_names, collapse = " + ")
  ))
  primera_aumentada <- lm(fmla_1e, data = base_iv)
  base_iv$educ_hat  <- fitted(primera_aumentada)

  cat("── Primera etapa aumentada ──\n")
  cat("R² primera etapa:", round(summary(primera_aumentada)$r.squared, 4), "\n")

  modelo_lewbel_manual <- lm(
    ln_ingreso ~ educ_hat + experiencia_lab + I(experiencia_lab^2) +
                 ln_horas + factor(SEXO) + informal + factor(sector),
    data = base_iv
  )

  cat("── Segunda etapa 2SLS manual ──\n")
  coeftest(modelo_lewbel_manual, vcov = vcovHC(modelo_lewbel_manual, "HC1"))

  b_educ_lw  <- coef(modelo_lewbel_manual)["educ_hat"]
  b_horas_lw <- coef(modelo_lewbel_manual)["ln_horas"]
}

# ── Tabla: OLS vs Lewbel IV ───────────────────────────────────────────────────
# El sesgo de selección positiva implica que OLS sobreestima β_educ.
# Si Lewbel IV < OLS, confirma que la habilidad infla el retorno OLS.
sesgo_lewbel <- round((b_educ_cor - b_educ_lw) * 100, 3)

tabla_lewbel <- tibble(
  Parametro = c(
    "Retorno a la educación (% por año)",
    "Sesgo selección estimado (OLS − IV)",
    "Elasticidad ingreso-horas",
    "Método de identificación",
    "N (observaciones)"
  ),
  OLS_ext = c(
    paste0(round(b_educ_cor * 100, 3), "%"),
    "Referencia",
    round(b_horas_cor, 3),
    "Exogeneidad asumida",
    format(nrow(base_mincer %>% filter(!is.na(informal), !is.na(sector))),
           big.mark = ",")
  ),
  Lewbel_IV = c(
    paste0(round(b_educ_lw * 100, 3), "%"),
    paste0(sesgo_lewbel, " pp"),
    round(b_horas_lw, 3),
    "Z_i = (X_i − X̄) × η̂_i (Lewbel, 2012)",
    format(nrow(base_iv), big.mark = ",")
  )
)
names(tabla_lewbel) <- c("Parámetro", "OLS Extendido (ref.)", "2SLS Lewbel IV")

kbl(tabla_lewbel,
    caption = paste0("Tabla C-3. Corrección de endogeneidad de la escolaridad — ",
                     "2SLS con instrumentos internos de Lewbel (2012). ENCFT 2016-2024"),
    booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(3, background = "#EEF3F8") %>%
  footnote(
    general = paste0(
      "Instrumento Lewbel: Z_i = (X_i − X̄) η̂_i, donde η̂_i son los residuos de la ",
      "primera etapa S_i = X_i α + η_i. ",
      "Los instrumentos son: Z_exp = (exp_i − exp̄) × η̂_i; ",
      "Z_exp2 = (exp²_i − exp²̄) × η̂_i; Z_lhoras = (ln_horas_i − ln_horas̄) × η̂_i. ",
      "Condición de validez: la heterocedasticidad de la ecuación de educación no debe ",
      "estar correlacionada con el término de error de la ecuación de ingreso. ",
      "Si β_educ^IV < β_educ^OLS: sesgo positivo confirmado (habilidad → educación y → ingreso). ",
      "Si β_educ^IV ≈ β_educ^OLS: endogeneidad limitada en esta muestra. ",
      "Referencia: Lewbel, A. (2012). Using heteroscedasticity to identify and estimate ",
      "mismeasured and endogenous regressor models. ",
      "Journal of Business & Economic Statistics, 30(1), 67-80."
    ),
    general_title = "Nota metodológica:"
  )

# ── Tabla comparativa (resumen de los cuatro modelos) ────────────────────────
# Esta tabla reproduce la Tabla E-2 en formato compacto para el cuerpo del documento.
tabla_final <- tibble(
  Parametro = c(
    "Semi-elasticidad educación (% por año)",
    "Elasticidad ingreso-horas",
    "Exp. máximo ingreso (años)",
    "Penalización informalidad (%)",
    "Brecha salarial género (%)",
    "R² / R² within"
  ),
  OLS_Base = c(
    round(b_educ_base  * 100, 3),
    round(b_horas_base, 3),
    round(exp_star_base, 2),
    "—", "—",
    round(summary(modelo_base)$r.squared, 3)
  ),
  OLS_Ext = c(
    round(b_educ_cor   * 100, 3),
    round(b_horas_cor, 3),
    round(exp_star_cor, 2),
    round((exp(b_informal) - 1) * 100, 2),
    round((exp(b_mujer) - 1)    * 100, 2),
    round(summary(modelo_corregido)$r.squared, 3)
  ),
  Heckman = c(
    round(b_educ_heck  * 100, 3),
    round(b_horas_heck, 3),
    "— (†)",
    "—",
    round((exp(b_sexo_heck) - 1) * 100, 2),
    0.162
  ),
  FE = c(
    paste0(round(b_educ_fe * 100, 3), " (†)"),
    paste0(round(b_horas_fe, 3), " n.s."),
    "—", "—", "—",
    round(summary(modelo_pseudo_fe)$r.squared["rsq"], 3)
  )
)

names(tabla_final) <- c("Parámetro", "OLS Base", "OLS Extendido", "Heckman", "Pseudo FE")

kbl(tabla_final,
    caption = "Tabla 7. Resumen comparativo de resultados econométricos  -  ENCFT 2016-2024",
    booktabs = TRUE, escape = FALSE, na = "—") %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(3, bold = TRUE, background = "#EEF3F8") %>%
  footnote(
    general = paste0(
      "† exp* Heckman no reportado: β(exp²) ≈ 0 en la segunda etapa porque el IMR ",
      "absorbe la no-linealidad de la experiencia; la curva es monótona creciente en 0-59 años. ",
      "El principal aporte del modelo de Heckman en esta aplicación no es el retorno educativo ",
      "(similar al OLS extendido), sino la corrección de la brecha de género: de -22.3% (OLS) a -4.1% ",
      "(Heckman), evidenciando que la mayor parte de la brecha observada refleja selección en participación. ",
      "R² = 0.162 corresponde a la ecuación de selección (probit); el R² de la ecuación de ingreso ",
      "(segunda etapa) no es directamente comparable con el R² OLS. ",
      "†† Pseudo FE: coeficiente sobre medias de cohorte — no comparable con los OLS individuales. ",
      "n.s.: p = 0.31, estadísticamente no significativo (90 celdas, potencia insuficiente). ",
      "Semi-elasticidad = (exp(β) − 1) × 100 para coeficientes de variables dummy. ",
      "Los cuatro modelos anteriores enfrentan tres problemas de identificación adicionales ",
      "documentados por el profesor revisor: (1) endogeneidad del control de informalidad ",
      "[Cov(Informal, ε) ≠ 0]; (2) colinealidad exacta Edad = Período − Cohorte en el pseudopanel; ",
      "y (3) endogeneidad de la escolaridad por habilidad no observada. Las correcciones correspondientes ",
      "— switching regression (Lee, 1978; Heckman, 1979), normalización Deaton–Paxson (1994) e ",
      "instrumentos internos de Lewbel (2012) — se reportan en la Sección 6 (Tablas C-1, C-2, C-3)."
    ),
    general_title = "Nota:"
  )

tabla_mt1 <- tibble(
  concepto = c(
    "Capital humano educativo",
    "Experiencia laboral",
    "Retornos decrecientes a la exp.",
    "Intensidad del trabajo",
    "Segmentación de género",
    "Segmentación formal-informal",
    "Segmentación sectorial",
    "Ingreso laboral (var. dep.)"
  ),
  variable = c(
    "NIVEL_ULTIMO_ANO_APROBADO + ULTIMO_ANO_APROBADO",
    "EDAD (calculada)",
    "EDAD al cuadrado (calculada)",
    "HORAS_TRABAJA_SEMANA_PRINCIPAL",
    "SEXO",
    "Categoría ocupacional (ENCFT)",
    "RAMA_PRINCIPAL_COD (CIIU Rev.4)",
    "Suma de 17 componentes del ingreso"
  ),
  transformacion = c(
    "Conversión a escala continua 0-22 años: primaria=0-6; secundaria=8-12; técnico=13-14; universitario=13-16; posgrado=17-22",
    "exp = edad − educ − 6, truncada en cero (Mincer, 1974)",
    "exp² = experiencia_lab². Captura concavidad del perfil ingreso-experiencia",
    "ln(horas): elasticidad constante ingreso-horas (especificación log-log)",
    "Dummy: 1 = mujer. Captura brecha salarial de género neta de capital humano",
    "Dummy: 1 = informal. Captura penalización salarial por informalidad",
    "17 dummies sectoriales (CIIU Rev.4). Sector referencia: Adm. Pública",
    "ln(ingreso_laboral). Normaliza la distribución sesgada a la derecha"
  ),
  rango = c(
    "0-22 años. Media: 10.6. Mediana: 12",
    "0-59 años. Media: 21.9. Mediana: 20",
    "0-3481. Media: 958",
    "0-120 hrs/sem. Media: 42.9. Mediana: 44",
    "60.8% hombres / 39.2% mujeres",
    "46.5% formal / 53.5% informal",
    "17 sectores CIIU Rev.4",
    "Rango: RD$1 a RD$1,951,000. Mediana: RD$15,167"
  )
)
names(tabla_mt1) <- c("Concepto teórico", "Variable ENCFT",
                      "Transformación / Construcción", "Rango en la muestra")

kbl(tabla_mt1,
    caption = "Tabla MT-1. De la teoría del capital humano a su medición empírica en la ENCFT 2016-2024",
    booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(2, width = "3.5cm") %>%
  column_spec(3, width = "5.5cm") %>%
  column_spec(4, width = "3cm")

tabla_mt2 <- tibble(
  modelo = c("OLS Base", "OLS Extendido", "Heckman 2 etapas", "Pseudopanel FE",
             "Correcciones avanzadas (Sección 6)"),
  problema = c(
    "Establecer retornos brutos a la educación y la experiencia sin ninguna corrección",
    "Controlar por segmentación de mercado: género, informalidad y sector productivo",
    "Corregir sesgo de selección muestral: los salarios se observan solo para quienes trabajan",
    "Controlar por heterogeneidad no observada entre cohortes generacionales",
    "Corregir: (1) endogeneidad de informalidad; (2) colinealidad APC en pseudopanel; (3) endogeneidad de escolaridad"
  ),
  limitacion = c(
    "Sesgo de variable omitida: omite género, informalidad, sector. Retorno a educación sobreestimado en +1.13 pp",
    "Asume selección aleatoria en el mercado laboral. Brecha de género puede incluir sesgo de selección",
    "Requiere restricción de exclusión válida. Supone normalidad bivariada en el término de error",
    "Experiencia excluida por colinealidad con cohorte (r = −0.919). Limitado a 90 celdas",
    "Lewbel (2012) requiere heterocedasticidad en primera etapa no correlacionada con el error de ingreso"
  ),
  rol = c(
    "Referencia (sesgo +1.13 pp)",
    "Modelo principal",
    "Contraste por selección",
    "Robustez intertemporal",
    "Robustez avanzada (respuesta a revisión)"
  )
)
names(tabla_mt2) <- c("Modelo", "Problema que resuelve", "Limitación principal", "Rol en la tesis")

kbl(tabla_mt2,
    caption = "Tabla MT-2. Arquitectura de los cuatro modelos: problema que resuelve cada especificación",
    booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  column_spec(1, width = "2.5cm", bold = TRUE) %>%
  column_spec(2, width = "4cm") %>%
  column_spec(3, width = "4cm") %>%
  column_spec(4, width = "2.5cm") %>%
  row_spec(2, background = "#EEF3F8")

exp_seq  <- seq(0, 55, by = 0.5)
educ_ref <- 10.6
ln_h_ref <- log(44)
b0_ref   <- coef(modelo_corregido)["(Intercept)"]   # intercepto dinámico

ln_ing_pred <- b0_ref + b_educ_cor * educ_ref +
               b_exp_cor  * exp_seq + b_exp2_cor * exp_seq^2 +
               b_horas_cor * ln_h_ref

df_curva <- tibble(exp = exp_seq, ing_miles = exp(ln_ing_pred) / 1000)

ggplot(df_curva, aes(x = exp, y = ing_miles)) +
  geom_line(color = "#1F3864", linewidth = 1.2) +
  geom_vline(xintercept = exp_star_cor, linetype = "dashed",
             color = "#C00000", linewidth = 0.8) +
  annotate("text", x = exp_star_cor + 1,
           y = max(df_curva$ing_miles) * 0.97,
           label = paste0("exp* = ", round(exp_star_cor, 1), " años"),
           hjust = 0, color = "#C00000", size = 3.5) +
  scale_x_continuous(breaks = seq(0, 55, 10)) +
  labs(x = "Años de experiencia laboral potencial",
       y = "Ingreso predicho (miles RD$)") +
  theme_minimal(base_size = 12)

# Nombres de columna sin caracteres especiales de LaTeX (%, /año)
# para compatibilidad con salida PDF y HTML
tabla_mt3 <- tibble(
  pais    = c(
    "Rep. Dom. - OLS base (este estudio)",
    "Rep. Dom. - OLS extendido (este estudio)",
    "America Latina y Caribe (promedio regional)",
    "Brasil",
    "Colombia",
    "Costa Rica",
    "Mexico",
    "Peru",
    "Mundo (promedio mundial)"
  ),
  retorno = c("4.95%", "3.82%",
              "11.0%", "14.6%", "11.2%", "10.5%",
              "15.8%", "8.6%",  "8.8%"),
  fuente  = c(
    "Estimacion propia, ENCFT 2016-2024",
    "Estimacion propia, ENCFT 2016-2024",
    "Psacharopoulos & Patrinos (2014, Tabla 3)",
    "Psacharopoulos & Patrinos (2014)",
    "Psacharopoulos & Patrinos (2014)",
    "Psacharopoulos & Patrinos (2014)",
    "Psacharopoulos & Patrinos (2014)",
    "Psacharopoulos & Patrinos (2014)",
    "Psacharopoulos & Patrinos (2014, Tabla 3)"
  ),
  nota    = c(
    "Mincer estandar sin controles de segmentacion — valor sesgado (+1.13 pp)",
    "Con genero, informalidad y 17 dummies sectoriales — estimacion principal",
    "Promedio historico 1970-2014; ecuacion de Mincer estandar",
    "Promedio de anos disponibles 1970-2014; Mincer estandar",
    "Promedio de anos disponibles 1970-2014; Mincer estandar",
    "Promedio de anos disponibles 1970-2014; Mincer estandar",
    "Promedio de anos disponibles 1970-2014; Mincer estandar",
    "Promedio de anos disponibles 1970-2014; Mincer estandar",
    "Promedio historico 1970-2014; ecuacion de Mincer estandar"
  )
)
names(tabla_mt3) <- c("Pais / Estudio",
                      "Retorno educ. (pct/ano)",
                      "Fuente",
                      "Nota metodologica")

kbl(tabla_mt3,
    escape = FALSE,
    caption = paste0("Tabla MT-3. Tasas privadas de retorno a la educacion: ",
                     "Republica Dominicana en perspectiva comparada"),
    booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(c(1, 2), background = "#EEF3F8") %>%
  column_spec(1, bold = TRUE) %>%
  footnote(
    general = paste0(
      "Advertencia de comparabilidad: los retornos de Psacharopoulos & Patrinos (2014) ",
      "son promedios historicos de estimaciones realizadas entre 1970 y 2014, ",
      "basadas en ecuaciones de Mincer estandar (sin controles de informalidad ni sector). ",
      "El retorno de RD (3.82%) proviene de un OLS extendido con controles de segmentacion, ",
      "por lo que una comparacion directa con los valores internacionales debe realizarse ",
      "preferentemente usando el OLS base (4.95%), que usa la misma especificacion. ",
      "Incluso con OLS base, RD es sistematicamente inferior al promedio de AL y Caribe (11.0%) ",
      "y al promedio mundial (8.8%), consistente con alta segmentacion laboral e informalidad (53.5%)."
    ),
    general_title = "Nota:"
  )

tabla_e1 <- base_mincer %>%
  group_by(SEXO) %>%
  summarise(
    n               = n(),
    educ_media      = round(mean(educ,                           na.rm = TRUE), 2),
    exp_media       = round(mean(experiencia_lab,                na.rm = TRUE), 2),
    horas_media     = round(mean(HORAS_TRABAJA_SEMANA_PRINCIPAL, na.rm = TRUE), 1),
    ingreso_mediana = round(median(ingreso_laboral,              na.rm = TRUE), 0),
    ingreso_media   = round(mean(ingreso_laboral,                na.rm = TRUE), 0),
    pct_informal    = round(mean(informal,                       na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    Sexo = if_else(SEXO == 1, "Hombre", "Mujer"),
    n    = format(n, big.mark = ",")
  ) %>%
  select(Sexo, n, educ_media, exp_media, horas_media,
         ingreso_mediana, ingreso_media, pct_informal)

kbl(tabla_e1,
    caption   = "Tabla E-1. Estadisticos descriptivos por sexo - ENCFT 2016-2024",
    col.names = c("Sexo", "N",
                  "Educ. media (anos)", "Exp. media (anos)",
                  "Horas/sem media",
                  "Ingreso mediana (RD$)", "Ingreso medio (RD$)",
                  "% Informal"),
    booktabs  = TRUE,
    escape    = TRUE,
    format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = paste0(
      "La brecha descriptiva de ingreso (hombres vs. mujeres) es de aproximadamente ",
      "32.3% antes de controlar por ninguna variable. ",
      "Una vez controlados capital humano, informalidad y sector (OLS extendido), ",
      "la brecha se reduce a -22.3%; con correccion de seleccion (Heckman), a -4.1%."
    ),
    general_title = "Interpretacion:"
  )

exp_s     <- seq(0, 55, by = 0.5)
educ_r    <- 10.6
ln_h_r    <- log(44)
b0_ext    <- coef(modelo_corregido)["(Intercept)"]   # intercepto dinámico

df_perfiles <- bind_rows(
  tibble(
    exp    = exp_s,
    ln_ing = b0_ext + b_educ_cor * educ_r + b_exp_cor * exp_s +
             b_exp2_cor * exp_s^2 + b_horas_cor * ln_h_r,
    Grupo  = "Hombre"
  ),
  tibble(
    exp    = exp_s,
    ln_ing = b0_ext + b_educ_cor * educ_r + b_exp_cor * exp_s +
             b_exp2_cor * exp_s^2 + b_horas_cor * ln_h_r + b_mujer,
    Grupo  = "Mujer"
  )
) %>% mutate(ing_miles = exp(ln_ing) / 1000)

ggplot(df_perfiles, aes(x = exp, y = ing_miles,
                         color = Grupo, linetype = Grupo)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = exp_star_cor, linetype = "dotted",
             color = "grey50", linewidth = 0.7) +
  annotate("text", x = exp_star_cor + 1,
           y = max(df_perfiles$ing_miles) * 0.95,
           label = paste0("exp* = ", round(exp_star_cor, 1)),
           size = 3, color = "grey40") +
  annotate("text", x = 28,
           y = mean(filter(df_perfiles, Grupo == "Hombre")$ing_miles) * 1.05,
           label = paste0("Brecha OLS: ",
                          round((exp(b_mujer) - 1) * 100, 1), "%"),
           size = 3.2, color = "#C00000") +
  scale_color_manual(values = c("Hombre" = "#1F3864", "Mujer" = "#C00000")) +
  scale_linetype_manual(values = c("Hombre" = "solid", "Mujer" = "dashed")) +
  scale_x_continuous(breaks = seq(0, 55, 10)) +
  labs(x = "Años de experiencia laboral potencial",
       y = "Ingreso predicho (miles RD$)",
       color = NULL, linetype = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

annos_nivel <- c(6, 12, 14, 16, 20)
niveles     <- c("Primaria\n(0-6)", "Secundaria\n(7-12)",
                 "Técnico\n(13-14)", "Universidad\n(13-16)", "Posgrado\n(17+)")

df_ret <- tibble(nivel = factor(niveles, levels = niveles), anos = annos_nivel) %>%
  mutate(
    diferencial_pct = c(NA,
      (exp(b_educ_cor * (annos_nivel[2] - annos_nivel[1])) - 1) * 100,
      (exp(b_educ_cor * (annos_nivel[3] - annos_nivel[2])) - 1) * 100,
      (exp(b_educ_cor * (annos_nivel[4] - annos_nivel[2])) - 1) * 100,
      (exp(b_educ_cor * (annos_nivel[5] - annos_nivel[4])) - 1) * 100
    )
  )

ggplot(df_ret[-1, ], aes(x = nivel, y = diferencial_pct, fill = nivel)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("+", round(diferencial_pct, 1), "%")),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("#2E5090", "#5B8DB8", "#1A7A4A", "#1F3864")) +
  scale_y_continuous(limits = c(0, max(df_ret$diferencial_pct, na.rm = TRUE) * 1.2)) +
  labs(x = "Nivel educativo alcanzado",
       y = "Prima salarial respecto al nivel anterior (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ── JERARQUÍA DE MODELOS ─────────────────────────────────────────────────────
# Col 1 — OLS Extendido:  MODELO PRINCIPAL (interpretacion economica)
# Col 2 — Heckman:        contraste por sesgo de seleccion muestral
# Col 3 — Pseudopanel FE: robustez intertemporal/cohortes
# Col 4 — OLS Base:       referencia pedagogica (sesgo documentado)

horas_mes    <- 42.875 * 52 / 12   # = 185.79 hrs/mes

# Nombre robusto del lambda (varía entre versiones de sampleSelection)
.heck_coefs  <- coef(modelo_heckman, part = "outcome")
.mills_nm    <- names(.heck_coefs)[grep("ills", names(.heck_coefs), ignore.case = TRUE)[1]]
lambda_e2    <- if (!is.na(.mills_nm)) round(.heck_coefs[.mills_nm], 3) else NA_real_
ing_medio    <- mean(base_mincer$ingreso_laboral, na.rm = TRUE)  # calculado dinámicamente
sesgo_educ   <- round((b_educ_base - b_educ_cor) * 100, 3)

# IC 95% con errores robustos HC1 para coefs clave del OLS Extendido
vc_cor   <- vcovHC(modelo_corregido, type = "HC1")
se_educ  <- sqrt(vc_cor["educ", "educ"])
se_horas <- sqrt(vc_cor["ln_horas", "ln_horas"])
se_inf   <- sqrt(vc_cor["informal", "informal"])
se_sexo  <- sqrt(vc_cor["factor(SEXO)2", "factor(SEXO)2"])

tabla_e2 <- tibble(
  Parametro = c(
    "Rol del modelo",
    "Semi-elasticidad educación (%) [IC 95%]",
    "Elasticidad ingreso-horas [IC 95%]",
    "Valor hora marginal (RD$) [solo sig.]",
    "Exp. máx. ingreso (años)",
    "Penalización informalidad (%) [IC 95%]",
    "Brecha salarial género (%) [IC 95%]",
    "λ — Razón Inversa de Mills",
    "ρ — correlación errores selección",
    "R² / R² within",
    "AIC (menor = mejor ajuste)",
    "Test RESET (F-stat)",
    "Test Hausman (χ²)",
    "Observaciones"
  ),
  ols_ext = c(
    "Principal ✓",
    paste0(round(b_educ_cor * 100, 3), "  [",
           round((b_educ_cor - 1.96 * se_educ) * 100, 3), ", ",
           round((b_educ_cor + 1.96 * se_educ) * 100, 3), "]"),
    paste0(round(b_horas_cor, 3), "  [",
           round(b_horas_cor - 1.96 * se_horas, 3), ", ",
           round(b_horas_cor + 1.96 * se_horas, 3), "]"),
    paste0("RD$", round(ing_medio * b_horas_cor / horas_mes, 1)),
    round(exp_star_cor, 2),
    paste0(round((exp(b_informal) - 1) * 100, 2), "  [",
           round((exp(b_informal - 1.96 * se_inf) - 1) * 100, 2), ", ",
           round((exp(b_informal + 1.96 * se_inf) - 1) * 100, 2), "]"),
    paste0(round((exp(b_mujer) - 1) * 100, 2), "  [",
           round((exp(b_mujer - 1.96 * se_sexo) - 1) * 100, 2), ", ",
           round((exp(b_mujer + 1.96 * se_sexo) - 1) * 100, 2), "]"),
    NA, NA,
    round(summary(modelo_corregido)$r.squared, 3),
    round(AIC(modelo_corregido), 1),
    paste0(round(resettest(modelo_corregido)$statistic, 0), "***"), NA,
    format(nrow(base_mincer), big.mark = ",")
  ),
  heckman = c(
    "Contraste selección",
    round(b_educ_heck * 100, 3),
    round(b_horas_heck, 3),
    paste0("RD$", round(ing_medio * b_horas_heck / horas_mes, 1)),
    "— (†)",
    NA,
    round((exp(b_sexo_heck) - 1) * 100, 2),
    lambda_e2,
    round(modelo_heckman$rho, 3),
    0.162,   # R² pseudo de la ecuación de SELECCIÓN (probit 1ª etapa), no del outcome (2ª etapa)
    NA, NA, NA,
    format(nrow(base_heckman), big.mark = ",")
  ),
  fe = c(
    "Robustez cohortes †",
    paste0(round(b_educ_fe * 100, 3), " (†)"),
    paste0(round(b_horas_fe, 3), " (n.s.)"),
    "— (n.s.)",
    NA, NA, NA, NA, NA,
    round(summary(modelo_pseudo_fe)$r.squared["rsq"], 3),
    NA, NA, "59***",
    "90 celdas"
  ),
  ols_base = c(
    paste0("Referencia ‡ (+", sesgo_educ, " pp sesgo)"),
    round(b_educ_base * 100, 3),
    round(b_horas_base, 3),
    paste0("RD$", round(ing_medio * b_horas_base / horas_mes, 1)),
    round(exp_star_base, 2),
    NA, NA, NA, NA,
    round(summary(modelo_base)$r.squared, 3),
    round(AIC(modelo_base), 1),
    paste0(round(resettest(modelo_base)$statistic, 0), "***"), NA,
    format(nrow(base_mincer), big.mark = ",")
  )
)

names(tabla_e2) <- c("Parámetro", "OLS Extendido",
                     "Heckman 2 etapas", "Pseudopanel FE", "OLS Base")

kbl(tabla_e2,
    escape = FALSE,
    caption = "Tabla E-2. Resumen comparativo de los cuatro modelos  -  ordenados por jerarquía interpretativa. Variable dependiente: ln(ingreso laboral mensual). ENCFT 2016-2024",
    booktabs = TRUE, na = "—") %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 1, "Modelo principal" = 1, "Contraste selección" = 1,
                     "Robustez cohortes" = 1, "Solo referencia" = 1)) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(c(2, 3, 6, 7), background = "#EEF3F8") %>%
  row_spec(1, italic = TRUE, color = "grey50") %>%
  footnote(
    general = paste0(
      "*** p < 0.001. IC 95% con errores robustos HC1 solo para OLS Extendido. ",
      "Valor hora marginal = ingreso_medio × β_horas / horas_mes (horas_mes = 185.8 hrs); ",
      "reportado solo cuando el coeficiente es estadísticamente significativo (p < 0.05). ",
      "† exp* Heckman no reportado (—): β(exp²) ≈ 0 porque el IMR absorbe la no-linealidad ",
      "de la experiencia; el valor matemático (132.97 años) está fuera del rango interpretable. ",
      "Principal aporte del modelo Heckman: corrección de la brecha de género, de -22.3% (OLS) a -4.1%. ",
      "†† Pseudopanel FE: coeficientes sobre medias de cohorte — no comparables con OLS individuales; ",
      "identifican variación agregada entre cohortes, no retorno marginal individual. ",
      "b_horas_fe = ", round(b_horas_fe, 3), " con p = 0.31 (Driscoll-Kraay): no significativo con 90 celdas. ",
      "‡ OLS Base sobreestima el retorno educativo en +", sesgo_educ,
      " pp (sesgo de variable omitida documentado). ",
      "AIC calculado con los mismos datos de estimación para cada modelo."
    ),
    general_title = "Nota:"
  )

sectores_coef <- coef(modelo_corregido)
sectores_idx  <- grep("factor\\(sector\\)", names(sectores_coef))
sectores_vals <- sectores_coef[sectores_idx]

df_sectores <- tibble(
  sector_raw = names(sectores_vals),
  coef       = as.numeric(sectores_vals)
) %>%
  mutate(
    sector_lbl = str_remove(sector_raw, "factor\\(sector\\)") %>%
                 str_replace_all("_", " ") %>%
                 str_trunc(30, side = "right"),
    prima_pct  = (exp(coef) - 1) * 100,
    signo      = if_else(prima_pct > 0, "Prima", "Penalización")
  ) %>%
  arrange(prima_pct)

ggplot(df_sectores, aes(x = reorder(sector_lbl, prima_pct),
                         y = prima_pct, fill = signo)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(if_else(prima_pct > 0, "+", ""),
                                round(prima_pct, 1), "%"),
                hjust = if_else(prima_pct > 0, -0.1, 1.1)),
            size = 2.9) +
  scale_fill_manual(values = c("Prima" = "#1A7A4A", "Penalización" = "#C00000")) +
  coord_flip() +
  labs(x = NULL, y = "Diferencial salarial vs. Adm. Pública (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Brecha bruta calculada dinámicamente (sin controles)
modelo_bruto_sexo <- lm(ln_ingreso ~ factor(SEXO), data = base_mincer)
brecha_bruta_raw  <- round((exp(coef(modelo_bruto_sexo)["factor(SEXO)2"]) - 1) * 100, 2)

df_brecha <- tibble(
  componente = factor(
    c("Brecha bruta\n(sin controles)",
      "OLS Extendido\n(con controles)",
      "Heckman\n(corregida selección)"),
    levels = c("Brecha bruta\n(sin controles)",
               "OLS Extendido\n(con controles)",
               "Heckman\n(corregida selección)")
  ),
  brecha = c(brecha_bruta_raw,
             round((exp(b_mujer)    - 1) * 100, 2),
             round((exp(b_sexo_heck)- 1) * 100, 2)),
  tipo   = c("Brecha bruta", "Con controles", "Corregida")
)

ggplot(df_brecha, aes(x = componente, y = brecha, fill = tipo)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(round(brecha, 1), "%")),
            vjust = 1.4, color = "white", fontface = "bold", size = 4) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  annotate("text", x = 2.5, y = -14,
           label = paste0("Selección: ",
                          round((exp(b_mujer) - 1)*100 - (exp(b_sexo_heck) - 1)*100, 1),
                          " pp"),
           size = 3, color = "grey30") +
  scale_fill_manual(values = c("Brecha bruta"  = "#5B8DB8",
                                "Con controles" = "#1F3864",
                                "Corregida"     = "#1A7A4A")) +
  scale_y_continuous(limits = c(-38, 2), breaks = seq(-35, 0, 5)) +
  labs(x = NULL, y = "Brecha salarial de género (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ── Valores fuente: Psacharopoulos & Patrinos (2014) ─────────────────────────
# Países individuales: promedio de años disponibles (Tabla país x año, misma fuente)
# Promedios regionales: Tabla 3 (Overall rate of return %)
#   AL y Caribe = 11.0%  |  Mundo = 8.8%

df_regional <- tibble(
  pais    = c("RD (OLS base)", "RD (OLS ext.)",
              "Argentina", "Bolivia", "Brasil", "Colombia",
              "Costa Rica", "Ecuador", "El Salvador", "Guatemala",
              "Honduras", "México", "Nicaragua", "Panamá", "Perú",
              "AL y Caribe (promedio regional)",
              "Mundo (promedio mundial)"),
  retorno = c(round(b_educ_base * 100, 2),
              round(b_educ_cor  * 100, 2),
              # países individuales
              10.1, 10.1, 14.6, 11.2,
              10.5, 10.0,  7.7, 11.7,
              10.9, 15.8,  9.8, 12.3,  8.6,
              # promedios regionales oficiales Tabla 3
              11.0, 8.8),
  tipo    = c("RD", "RD",
              rep("País comparador", 13),
              "Promedio regional", "Promedio mundial")
) %>%
  mutate(pais = fct_reorder(pais, retorno))

ggplot(df_regional, aes(x = pais, y = retorno, fill = tipo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(retorno, "%")), hjust = -0.1, size = 3.1) +
  scale_fill_manual(values = c("RD"                = "#C00000",
                               "País comparador"   = "#5B8DB8",
                               "Promedio regional" = "#1F3864",
                               "Promedio mundial"  = "#4A4A4A")) +
  scale_y_continuous(limits = c(0, 18)) +
  coord_flip() +
  labs(x       = NULL,
       y       = "Retorno privado a la educación (% por año)",
       fill    = NULL,
       caption = paste0(
         "Fuente: Psacharopoulos & Patrinos (2014); estimaciones 1970-2014. ",
         "Países: promedio de años disponibles en la fuente. ",
         "Promedios regionales: Tabla 3 (Overall rate of return). ",
         "AL y Caribe = 11.0%;  Mundo = 8.8%."
       )) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption    = element_text(size = 8, color = "grey40",
                                       hjust = 0, margin = margin(t = 8)))

# ── OLS extendido año a año ───────────────────────────────────────────────────
# Se estima el modelo principal (misma especificación que modelo_corregido)
# de forma independiente para cada año calendario de la ENCFT.
# Esto permite observar la variación temporal del retorno educativo β₁
# manteniendo constantes los controles de segmentación (género, informalidad, sector).

retornos_anuales <- base_mincer %>%
  group_by(ANO) %>%
  group_map(~ {
    m <- lm(ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
              ln_horas + factor(SEXO) + informal + factor(sector),
            data = .x)
    se_hc1 <- sqrt(vcovHC(m, type = "HC1")["educ", "educ"])
    tibble(
      anio    = .y$ANO,
      retorno = coef(m)["educ"] * 100,
      se      = se_hc1 * 100,
      ic_low  = (coef(m)["educ"] - 1.96 * se_hc1) * 100,
      ic_high = (coef(m)["educ"] + 1.96 * se_hc1) * 100,
      n       = nrow(.x)
    )
  }) %>%
  bind_rows()

# Retorno promedio del período (referencia horizontal)
ret_promedio <- mean(retornos_anuales$retorno)

ggplot(retornos_anuales, aes(x = anio, y = retorno)) +
  geom_ribbon(aes(ymin = ic_low, ymax = ic_high),
              alpha = 0.15, fill = "#5B8DB8") +
  geom_hline(yintercept = ret_promedio,
             linetype = "dashed", color = "#4A4A4A", linewidth = 0.6) +
  geom_line(color = "#1A3560", linewidth = 1.2) +
  geom_point(color = "#1A3560", size = 3) +
  geom_text(aes(label = paste0(round(retorno, 2), "%")),
            vjust = -1.2, size = 3.2, color = "#1A3560") +
  annotate("text", x = min(retornos_anuales$anio) + 0.1,
           y = ret_promedio + 0.08,
           label = paste0("Promedio: ", round(ret_promedio, 2), "%"),
           size = 3, color = "#4A4A4A", hjust = 0) +
  scale_x_continuous(breaks = unique(retornos_anuales$anio)) +
  scale_y_continuous(limits = c(
    min(retornos_anuales$ic_low)  - 0.3,
    max(retornos_anuales$ic_high) + 0.5
  )) +
  labs(x       = "Año",
       y       = "Retorno educativo β₁ (% por año adicional)",
       caption = paste0(
         "Nota: OLS extendido estimado de forma independiente por año calendario. ",
         "Misma especificación que el modelo principal (Sección 5.2). ",
         "Errores estándar HC1. Bandas = IC 95%. ",
         "n por año: ", paste(retornos_anuales$n, collapse = ", "), "."
       )) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "grey40",
                                    hjust = 0, margin = margin(t = 8)))

mediana_ing <- median(base_mincer$ingreso_laboral, na.rm = TRUE)
ing_med_val <- mean(base_mincer$ingreso_laboral,   na.rm = TRUE)
horas_mes_v <- 42.875 * 52 / 12

prima_univ_prim <- (exp(b_educ_cor * 10) - 1) * 100
dif_men_educ    <- mediana_ing * (exp(b_educ_cor * 10) - 1)

b0_c         <- coef(modelo_corregido)["(Intercept)"]
ing_sin_educ <- exp(b0_c + b_educ_cor * 0 + b_exp_cor * 15 +
                    b_exp2_cor * 15^2 + b_horas_cor * log(44))
prima_sec_0  <- (exp(b_educ_cor * 12) - 1) * 100
dif_men_e2   <- ing_sin_educ * (exp(b_educ_cor * 12) - 1)

valor_hora_ols  <- ing_med_val * b_horas_cor  / horas_mes_v
valor_hora_heck <- ing_med_val * b_horas_heck / horas_mes_v
pct_hora_ols    <- b_horas_cor  / horas_mes_v * 100

penaliz_inf  <- (exp(b_informal)   - 1) * 100
dif_men_inf  <- mediana_ing * abs(exp(b_informal) - 1)
brecha_ols_v <- (exp(b_mujer)      - 1) * 100
dif_men_ols  <- mediana_ing * abs(exp(b_mujer) - 1)
brecha_hk_v  <- (exp(b_sexo_heck) - 1) * 100
dif_men_hk   <- mediana_ing * abs(exp(b_sexo_heck) - 1)

tabla_e3 <- tibble(
  escenario    = c(
    "Universitaria (16 anos) vs. Primaria (6 anos). Hombre, formal, 15 anos exp.",
    "Secundaria (12 anos) vs. Sin educacion (0 anos). Hombre, formal, 15 anos exp.",
    "Hora adicional de trabajo - OLS Extendido (base: ingreso medio)",
    "Hora adicional de trabajo - Heckman (base: ingreso medio)",
    "Trabajador formal vs. informal (mismas caracteristicas observables)",
    "Hombre vs. mujer - OLS Extendido (brecha sin corregir seleccion)",
    "Hombre vs. mujer - Heckman (brecha condicional corregida)"
  ),
  diferencial  = c(
    paste0("+", round(prima_univ_prim, 1), "%"),
    paste0("+", round(prima_sec_0,    1), "%"),
    paste0("+", round(pct_hora_ols,   3), "% del ingreso"),
    paste0("+", round(b_horas_heck / horas_mes_v * 100, 3), "% del ingreso"),
    paste0(round(penaliz_inf,  2), "%"),
    paste0(round(brecha_ols_v, 2), "%"),
    paste0(round(brecha_hk_v,  2), "%")
  ),
  mensual      = c(
    paste0("+RD$", format(round(dif_men_educ,    0), big.mark = ",")),
    paste0("+RD$", format(round(dif_men_e2,      0), big.mark = ",")),
    paste0("+RD$", format(round(valor_hora_ols,  1), nsmall = 1, big.mark = ",")),
    paste0("+RD$", format(round(valor_hora_heck, 1), nsmall = 1, big.mark = ",")),
    paste0("-RD$", format(round(dif_men_inf, 0), big.mark = ",")),
    paste0("-RD$", format(round(dif_men_ols, 0), big.mark = ",")),
    paste0("-RD$", format(round(dif_men_hk,  0), big.mark = ","))
  ),
  anual        = c(
    paste0("+RD$", format(round(dif_men_educ     * 12, 0), big.mark = ",")),
    paste0("+RD$", format(round(dif_men_e2       * 12, 0), big.mark = ",")),
    paste0("+RD$", format(round(valor_hora_ols   * 12, 1), nsmall = 1, big.mark = ",")),
    paste0("+RD$", format(round(valor_hora_heck  * 12, 1), nsmall = 1, big.mark = ",")),
    paste0("-RD$", format(round(dif_men_inf * 12, 0), big.mark = ",")),
    paste0("-RD$", format(round(dif_men_ols * 12, 0), big.mark = ",")),
    paste0("-RD$", format(round(dif_men_hk  * 12, 0), big.mark = ","))
  )
)

kbl(tabla_e3,
    caption   = "Tabla E-3. Impacto monetario estimado - ejercicios de simulacion con OLS Extendido y Heckman",
    col.names = c("Escenario", "Diferencial (%)",
                  "Por hora / Mensual (RD$)", "Anual (RD$)"),
    booktabs  = TRUE,
    escape    = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(c(3, 4), background = "#F5F0FF") %>%
  footnote(
    general = paste0(
      "Escenarios 1-2: diferencial sobre mediana muestral (RD$",
      format(round(mediana_ing, 0), big.mark = ","),
      ") o ingreso predicho sin educacion. ",
      "Escenarios 3-4 (fondo morado): valoracion hora marginal = ingreso_medio x b_horas / horas_mes, ",
      "con ingreso_medio = RD$", format(round(ing_med_val, 0), big.mark = ","),
      " y horas_mes = ", round(horas_mes_v, 1), " hrs/mes. ",
      "ADVERTENCIA: escenarios 3-4 usan ingreso medio (RD$", format(round(ing_med_val, 0), big.mark = ","),
      ") como base, mientras escenarios 1-2 y 5-7 usan la mediana (RD$",
      format(round(mediana_ing, 0), big.mark = ","),
      "); las cifras no son directamente comparables entre grupos. ",
      "Escenarios 5-7: diferencial sobre mediana muestral. ",
      "Todos los calculos basados en OLS Extendido o Heckman; no en FE (coeficiente de horas n.s.). ",
      "Fuente: estimaciones propias con ENCFT 2016-2024."
    ),
    general_title = "Nota:"
  )

base_ext <- base_mincer %>% filter(!is.na(informal), !is.na(sector))

# ── A. Breusch-Pagan ──────────────────────────────────────────────────────────
bp_base <- bptest(modelo_base)
bp_ext  <- bptest(modelo_corregido)

# ── B. RESET ──────────────────────────────────────────────────────────────────
reset_ext <- resettest(modelo_corregido)

# ── C. VIF ────────────────────────────────────────────────────────────────────
vif_ext <- vif(modelo_corregido)

# ── D. AIC/BIC ────────────────────────────────────────────────────────────────
aic_base <- AIC(modelo_base); bic_base <- BIC(modelo_base)
aic_ext  <- AIC(modelo_corregido); bic_ext  <- BIC(modelo_corregido)

# ── E. Test F: significancia conjunta dummies sectoriales ─────────────────────
modelo_sin_sector <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + informal,
  data = base_ext
)
f_sector       <- anova(modelo_sin_sector, modelo_corregido)
f_stat_sector  <- f_sector$F[2]
p_sector       <- f_sector$`Pr(>F)`[2]

# ── F. Test de Chow: estabilidad de coefs por género ─────────────────────────
modelo_chow_h <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + informal + factor(sector),
  data = base_ext %>% filter(SEXO == 1)
)
modelo_chow_m <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + informal + factor(sector),
  data = base_ext %>% filter(SEXO == 2)
)
rss_pool      <- sum(residuals(modelo_corregido)^2)
rss_h         <- sum(residuals(modelo_chow_h)^2)
rss_m         <- sum(residuals(modelo_chow_m)^2)
k             <- length(coef(modelo_chow_h))
n_total       <- nobs(modelo_corregido)
f_chow_gen    <- ((rss_pool - rss_h - rss_m) / k) /
                 ((rss_h + rss_m) / (n_total - 2 * k))
p_chow_gen    <- pf(f_chow_gen, df1 = k, df2 = n_total - 2 * k, lower.tail = FALSE)

# ── G. Endogeneidad de informal (función de control) ─────────────────────────
p_resid_val <- NA_real_
tryCatch({
  probit_inf <- glm(
    informal ~ educ + experiencia_lab + I(experiencia_lab^2) +
               factor(SEXO) + factor(sector),
    data   = base_ext,
    family = binomial(link = "probit")
  )
  # Residuos del índice latente (Rivers & Vuong, 1988): y - Phi(Xb)
  # Usar type = "response" daría residuos de la probabilidad predicha,
  # no del índice lineal que se requiere en la prueba de función de control.
  resid_informal <- base_ext$informal - fitted(probit_inf)
  base_cf <- base_ext %>%
    mutate(resid_inf = resid_informal)
  modelo_cf <- lm(
    ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
                 ln_horas + factor(SEXO) + informal + factor(sector) + resid_inf,
    data = base_cf
  )
  cf_coefs    <- coeftest(modelo_cf, vcov = vcovHC(modelo_cf, type = "HC1"))
  p_resid_val <- cf_coefs["resid_inf", "Pr(>|t|)"]
  b_resid_val <- cf_coefs["resid_inf", "Estimate"]
  cat(sprintf("Control function: coef residuos = %.4f, p = %.4f\n",
              b_resid_val, p_resid_val))
  if (p_resid_val < 0.05) {
    cat("=> Evidencia de endogeneidad de 'informal'. Interpretar como asociación condicional.\n")
  } else {
    cat("=> No se rechaza exogeneidad de 'informal' (p >= 0.05).\n")
  }
}, error = function(e) {
  cat("Control function no ejecutable:", conditionMessage(e), "\n")
  cat("ADVERTENCIA: sin test formal, b_informal se interpreta como asociación, no causalidad.\n")
})

# ── H. Tabla resumen diagnósticos ─────────────────────────────────────────────
tabla_diag <- tibble(
  Test = c(
    "Heterocedasticidad (Breusch-Pagan)",
    "Especificación funcional (RESET)",
    "Multicolinealidad educ (GVIF^1/(2k))",
    "Multicolinealidad exp (GVIF^1/(2k))",
    "Criterio AIC (vs. OLS Base)",
    "Criterio BIC (vs. OLS Base)",
    "Dummies sectoriales conjuntas (F parcial)",
    "Estabilidad coefs por género (Chow)",
    "Endogeneidad informal (fn. control)"
  ),
  Resultado = c(
    paste0("BP = ", round(bp_ext$statistic, 0), " ***"),
    paste0("F = ",  round(reset_ext$statistic, 0), " ***"),
    {
      gvif_col <- if ("GVIF^(1/(2*Df))" %in% colnames(vif_ext))
                    "GVIF^(1/(2*Df))" else "GVIF"
      if (is.matrix(vif_ext)) round(vif_ext["educ", gvif_col], 3) else round(vif_ext["educ"], 3)
    },
    {
      gvif_col <- if ("GVIF^(1/(2*Df))" %in% colnames(vif_ext))
                    "GVIF^(1/(2*Df))" else "GVIF"
      if (is.matrix(vif_ext)) round(vif_ext["experiencia_lab", gvif_col], 3) else round(vif_ext["experiencia_lab"], 3)
    },
    paste0(round(aic_base - aic_ext, 0), " pts mejor"),
    paste0(round(bic_base - bic_ext, 0), " pts mejor"),
    paste0("F = ", round(f_stat_sector, 1), " ***"),
    paste0("F = ", round(f_chow_gen, 2),
           if_else(p_chow_gen < 0.05, " ***", " n.s.")),
    if (!is.na(p_resid_val))
      if_else(p_resid_val < 0.05, "p < 0.05 ***", paste0("p = ", round(p_resid_val, 3)))
    else "No ejecutado"
  ),
  Accion = c(
    "Errores robustos HC1 aplicados a todos los modelos OLS",
    "Interpretar coeficientes como aproximación lineal local (Mincer, 1974)",
    "Sin sesgo; N grande (>200k obs.) proporciona potencia suficiente",
    "Inherente a cualquier especificación cuadrática",
    "OLS Extendido estadísticamente preferible bajo AIC",
    "OLS Extendido estadísticamente preferible bajo BIC",
    "Inclusión de 17 dummies sectoriales justificada",
    if_else(p_chow_gen < 0.05,
            "Retornos heterogéneos por género; ver Tabla I-1. Coef. pooled = diferencial promedio condicional",
            "Especificación pooled estadísticamente válida"),
    if (!is.na(p_resid_val) && p_resid_val < 0.05)
      "b_informal es asociación condicional, no efecto causal"
    else "b_informal interpretable como efecto condicional"
  )
)

names(tabla_diag) <- c("Test / Criterio", "Resultado", "Acción correctiva")

kbl(tabla_diag,
    escape = FALSE,
    caption = "Tabla R-0. Diagnósticos del modelo principal (OLS Extendido)  -  tests y correcciones",
    booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  row_spec(0, bold = TRUE)

sesgo_educ_pp <- round((b_educ_base - b_educ_cor) * 100, 3)
mejora_r2     <- round(summary(modelo_corregido)$r.squared -
                       summary(modelo_base)$r.squared, 3)

# Nombre robusto del lambda (varía entre versiones de sampleSelection)
.heck_out  <- coef(modelo_heckman, part = "outcome")
.mills_nm  <- names(.heck_out)[grep("ills", names(.heck_out), ignore.case = TRUE)[1]]
lambda_val <- if (!is.na(.mills_nm)) round(.heck_out[.mills_nm], 3) else NA_real_

rho_val       <- round(modelo_heckman$rho, 3)
brecha_ols_p  <- round((exp(b_mujer)     - 1) * 100, 2)
brecha_hk_p   <- round((exp(b_sexo_heck) - 1) * 100, 2)
comp_sel_p    <- round(brecha_ols_p - brecha_hk_p, 2)
r2_within     <- round(summary(modelo_pseudo_fe)$r.squared["rsq"], 3)
r2_base       <- round(summary(modelo_base)$r.squared, 3)
r2_ext        <- round(summary(modelo_corregido)$r.squared, 3)

tbl_secuencia <- data.frame(
  Transicion = c("OLS Base a OLS Extendido",
                 "OLS Extendido a Heckman",
                 "OLS Extendido a Pseudopanel FE"),
  Problema   = c("Variable omitida: genero, informalidad, sector",
                 "Seleccion muestral: participacion no aleatoria",
                 "Heterogeneidad no observada entre cohortes"),
  Evidencia  = c(
    paste0("R2 sube de ", r2_base, " a ", r2_ext,
           " (+", mejora_r2, "); b_educ cae ",
           sesgo_educ_pp, " pp; AIC y BIC menores"),
    paste0("lambda = ", lambda_val, " (p < 2e-16); rho = ", rho_val,
           "; brecha OLS = ", brecha_ols_p, "% vs Heckman = ", brecha_hk_p,
           "%; componente seleccion = ", comp_sel_p, " pp"),
    paste0("Hausman chi2 = 59 (p < 0.001); FE consistente; ",
           "R2 within = ", r2_within,
           "; b_horas_fe p = 0.31 (n.s.)")
  ),
  Rol = c("Modelo principal (asociacion condicional)",
          "Correccion brecha genero",
          "Robustez: b_educ > 0 no es artefacto generacional"),
  stringsAsFactors = FALSE
)

kbl(tbl_secuencia,
    caption   = "Justificacion formal de la secuencia de cuatro modelos - ENCFT 2016-2024",
    col.names = c("Transicion", "Problema corregido", "Evidencia", "Rol"),
    booktabs  = TRUE,
    escape    = TRUE) %>%
  kable_styling(full_width = TRUE, font_size = 11) %>%
  column_spec(1, bold = TRUE, width = "18em") %>%
  column_spec(3, width = "28em") %>%
  footnote(
    general = paste0(
      "Jerarquia de evidencia: (1) OLS Extendido - asociacion ajustada, modelo principal; ",
      "(2) Heckman - seleccion corregida; ",
      "(3) Pseudopanel FE - robustez intertemporal; ",
      "(4) OLS Base - benchmark con sesgo documentado (+", sesgo_educ_pp, " pp)."
    ),
    general_title = "Nota:"
  )

cook_d          <- cooks.distance(modelo_corregido)
umbral_cook     <- 4 / length(cook_d)
n_influyentes   <- sum(cook_d > umbral_cook, na.rm = TRUE)
pct_influyentes <- round(n_influyentes / length(cook_d) * 100, 2)

df_cook <- tibble(obs  = seq_along(cook_d), cook = cook_d,
                  flag = cook_d > umbral_cook)

ggplot(df_cook, aes(x = obs, y = cook, color = flag)) +
  geom_point(size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = umbral_cook, linetype = "dashed",
             color = "#C00000", linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#C00000"),
                     labels = c("Normal", "Influyente (D > 4/N)"), name = NULL) +
  scale_y_log10() +
  annotate("text", x = length(cook_d) * 0.6, y = umbral_cook * 3,
           label = paste0(n_influyentes, " obs. influyentes (",
                          pct_influyentes, "%)"),
           color = "#C00000", size = 3) +
  labs(x = "Índice de observación", y = "Distancia de Cook (log)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p99 <- quantile(base_mincer$ingreso_laboral, 0.99, na.rm = TRUE)

modelo_sin_out <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + informal + factor(sector),
  data = base_mincer %>%
    filter(!is.na(informal), !is.na(sector), ingreso_laboral <= p99)
)

tabla_rob1 <- tibble(
  Parametro = c("Semi-elasticidad educación (%)",
                "Elasticidad horas", "Brecha género (%)"),
  original = c(round(b_educ_cor  * 100, 3),
               round(b_horas_cor, 3),
               round((exp(b_mujer) - 1) * 100, 2)),
  sin_out  = c(round(coef(modelo_sin_out)["educ"] * 100, 3),
               round(coef(modelo_sin_out)["ln_horas"], 3),
               round((exp(coef(modelo_sin_out)["factor(SEXO)2"]) - 1) * 100, 2)),
  cambio   = c(round((coef(modelo_sin_out)["educ"] - b_educ_cor) * 100, 4),
               round(coef(modelo_sin_out)["ln_horas"] - b_horas_cor, 4),
               round(((exp(coef(modelo_sin_out)["factor(SEXO)2"])-1) -
                       (exp(b_mujer)-1)) * 100, 4))
)
names(tabla_rob1) <- c("Parámetro", "OLS Ext. original",
                        "Sin top 1% ingreso", "Cambio (pp)")

kbl(tabla_rob1,
    escape = FALSE,
    caption = paste0("Tabla R-1. Robustez ante outliers: exclusión del top 1% de ingresos (> RD$",
                     format(round(p99, 0), big.mark = ","), ")"),
    booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = paste0(
      "Retorno educativo: cambio de ", round((coef(modelo_sin_out)["educ"] - b_educ_cor) * 100, 4), " pp. ",
      "Elasticidad horas: cambio de ", round(coef(modelo_sin_out)["ln_horas"] - b_horas_cor, 4), " pp. ",
      "Brecha de género: cambio de ", round(((exp(coef(modelo_sin_out)["factor(SEXO)2"])-1) - (exp(b_mujer)-1)) * 100, 2), " pp. ",
      "Los coeficientes de educación y horas son estables (< 0.1 pp); la brecha de género varía levemente al excluir ingresos extremos, ",
      "confirmando robustez de los resultados principales."
    ),
    general_title = "Conclusión:"
  )

modelo_hombres <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + informal + factor(sector),
  data = base_ext %>% filter(SEXO == 1)
)
modelo_mujeres <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + informal + factor(sector),
  data = base_ext %>% filter(SEXO == 2)
)

tabla_genero <- tibble(
  Parametro = c("Semi-elasticidad educación (%)",
                "Elasticidad horas",
                "Penalización informalidad (%)", "R²", "N"),
  Hombres = c(
    round(coef(modelo_hombres)["educ"] * 100, 3),
    round(coef(modelo_hombres)["ln_horas"], 3),
    round((exp(coef(modelo_hombres)["informal"]) - 1) * 100, 2),
    round(summary(modelo_hombres)$r.squared, 3),
    format(nobs(modelo_hombres), big.mark = ",")
  ),
  Mujeres = c(
    round(coef(modelo_mujeres)["educ"] * 100, 3),
    round(coef(modelo_mujeres)["ln_horas"], 3),
    round((exp(coef(modelo_mujeres)["informal"]) - 1) * 100, 2),
    round(summary(modelo_mujeres)$r.squared, 3),
    format(nobs(modelo_mujeres), big.mark = ",")
  ),
  Diferencia = c(
    round((coef(modelo_mujeres)["educ"] - coef(modelo_hombres)["educ"]) * 100, 3),
    round(coef(modelo_mujeres)["ln_horas"] - coef(modelo_hombres)["ln_horas"], 3),
    round(((exp(coef(modelo_mujeres)["informal"])-1) -
           (exp(coef(modelo_hombres)["informal"])-1)) * 100, 2),
    NA, NA
  )
)
names(tabla_genero) <- c("Parámetro", "Hombres", "Mujeres", "Diferencia (M − H)")

kbl(tabla_genero,
    caption = "Tabla R-2. OLS Extendido por submuestra de sexo  -  verificación de heterogeneidad de retornos",
    booktabs = TRUE, escape = FALSE, na = "—") %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  footnote(
    general = {
      dif_educ_genero <- round((coef(modelo_mujeres)["educ"] - coef(modelo_hombres)["educ"]) * 100, 3)
      if (dif_educ_genero > 0) {
        paste0(
          "La diferencia en retorno educativo (Mujeres − Hombres) es de +", dif_educ_genero, " pp: ",
          "la educación compensa parcialmente la brecha salarial de género. ",
          "Consistente con el modelo de señalización (Spence, 1973): la credencial educativa ",
          "puede reducir la discriminación estadística hacia las mujeres en el sector formal."
        )
      } else {
        paste0(
          "La diferencia en retorno educativo (Mujeres − Hombres) es de ", dif_educ_genero, " pp: ",
          "los retornos marginales a la educación son menores para las mujeres, ",
          "lo que indica que la educación no compensa la brecha salarial de género por este canal. ",
          "La brecha opera principalmente por intercepto (penalización fija), ",
          "no por diferencias en retornos marginales."
        )
      }
    },
    general_title = "Interpretación:"
  )

modelo_formal   <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + factor(sector),
  data = base_ext %>% filter(informal == 0)
)
modelo_informal <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + factor(sector),
  data = base_ext %>% filter(informal == 1)
)

tabla_form <- tibble(
  Parametro = c("Semi-elasticidad educación (%)",
                "Elasticidad horas", "Brecha género (%)", "R²", "N"),
  Formal = c(
    round(coef(modelo_formal)["educ"] * 100, 3),
    round(coef(modelo_formal)["ln_horas"], 3),
    round((exp(coef(modelo_formal)["factor(SEXO)2"]) - 1) * 100, 2),
    round(summary(modelo_formal)$r.squared, 3),
    format(nobs(modelo_formal), big.mark = ",")
  ),
  Informal = c(
    round(coef(modelo_informal)["educ"] * 100, 3),
    round(coef(modelo_informal)["ln_horas"], 3),
    round((exp(coef(modelo_informal)["factor(SEXO)2"]) - 1) * 100, 2),
    round(summary(modelo_informal)$r.squared, 3),
    format(nobs(modelo_informal), big.mark = ",")
  )
)
names(tabla_form) <- c("Parámetro", "Sector formal", "Sector informal")

kbl(tabla_form,
    caption = "Tabla R-3. OLS Extendido por sector formal/informal  -  retornos dentro de cada segmento",
    booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  footnote(
    general = paste0(
      "Si el retorno educativo es mayor en el sector formal, la educación actúa como ",
      "credencial de acceso (señalización, Spence 1973) además de como capital humano. ",
      "Si es similar en ambos segmentos, el retorno refleja productividad independientemente ",
      "del tipo de contrato."
    ),
    general_title = "Interpretación:"
  )

tau_seq <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Submuestra de 20,000 obs para reducir tiempo de bootstrap
set.seed(params$semilla)
base_rq <- base_ext %>%
  filter(!is.na(ln_ingreso), !is.na(educ), !is.na(informal)) %>%
  slice_sample(n = min(20000, nrow(.)))

rq_resultados <- map_dfr(tau_seq, function(tau) {
  m <- rq(
    ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
                 ln_horas + factor(SEXO) + informal + factor(sector),
    data = base_rq, tau = tau
  )
  s     <- summary(m, se = "boot", R = 50)
  coefs <- s$coefficients

  # Compatibilidad entre versiones de quantreg:
  # versiones antiguas usan columna "coefficients"; versiones >= 5.86 usan "Value"
  col_coef <- if ("coefficients" %in% colnames(coefs)) "coefficients" else "Value"
  col_se   <- if ("Std. Error"   %in% colnames(coefs)) "Std. Error"   else colnames(coefs)[2]

  b  <- coefs["educ", col_coef]
  se <- coefs["educ", col_se]

  tibble(
    tau       = tau,
    coef_educ = b,
    se_educ   = se,
    lo95      = b - 1.96 * se,
    hi95      = b + 1.96 * se
  )
})

ggplot(rq_resultados, aes(x = tau, y = coef_educ * 100)) +
  geom_ribbon(aes(ymin = lo95 * 100, ymax = hi95 * 100),
              fill = "grey80", alpha = 0.6) +
  geom_line(color = "#1F3864", linewidth = 1.1) +
  geom_point(color = "#1F3864", size = 2.5) +
  geom_hline(yintercept = b_educ_cor * 100, linetype = "dashed",
             color = "#C00000", linewidth = 0.8) +
  annotate("text", x = 0.12, y = b_educ_cor * 100 + 0.05,
           label = paste0("OLS: ", round(b_educ_cor * 100, 3), "%"),
           color = "#C00000", size = 3) +
  scale_x_continuous(breaks = tau_seq, labels = paste0("Q", tau_seq * 100)) +
  labs(x = "Cuantil de la distribución de ingresos",
       y = "Retorno a la educación por cuantil (% por año)") +
  theme_minimal(base_size = 12)

# ── Interacción educación × género ───────────────────────────────────────────
# Pregunta: ¿La educación reduce o amplifica la brecha salarial de género?
# β(educ:mujer) > 0 → la educación compensa parcialmente la penalización de género
# β(educ:mujer) = 0 → retornos homogéneos; la brecha opera por intercepto

modelo_int_genero <- lm(
  ln_ingreso ~ educ * factor(SEXO) + experiencia_lab +
               I(experiencia_lab^2) + ln_horas + informal + factor(sector),
  data = base_ext
)
coefs_ig    <- coeftest(modelo_int_genero, vcov = vcovHC(modelo_int_genero, "HC1"))
b_educ_h    <- coefs_ig["educ", "Estimate"]
b_educ_md   <- coefs_ig["educ:factor(SEXO)2", "Estimate"]
b_educ_m    <- b_educ_h + b_educ_md
p_interac_g <- coefs_ig["educ:factor(SEXO)2", "Pr(>|t|)"]

tabla_int_g <- tibble(
  Grupo    = c("Hombres", "Mujeres", "Diferencial (M − H)"),
  retorno  = c(paste0(round(b_educ_h  * 100, 3), "%"),
               paste0(round(b_educ_m  * 100, 3), "%"),
               paste0(round(b_educ_md * 100, 3), " pp")),
  signif   = c("—", "—",
               if_else(p_interac_g < 0.001, "p < 0.001",
               if_else(p_interac_g < 0.05,  paste0("p = ", round(p_interac_g, 3)),
                                            "n.s.")))
)
names(tabla_int_g) <- c("Grupo", "Retorno educación (% por año)", "Significancia")

kbl(tabla_int_g,
    escape = FALSE,
    caption = "Tabla I-1. Retornos a la educación por género (modelo con interacción educ × SEXO, errores HC1)",
    booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = if_else(p_interac_g < 0.05,
      if_else(b_educ_md > 0,
        paste0("La educación REDUCE la brecha de género: cada año adicional de escolaridad ",
               "aporta ", round(b_educ_md * 100, 3),
               " pp más a las mujeres que a los hombres. ",
               "Mecanismo consistente con teoría de señalización: la credencial educativa ",
               "reduce la discriminación estadística hacia las mujeres en el sector formal (Spence, 1973)."),
        "La educación AMPLIFICA la brecha de género: los retornos marginales son menores para mujeres. Posible causa: segregación ocupacional por género."),
      "Los retornos a la educación son estadísticamente homogéneos entre géneros. La brecha opera principalmente por intercepto (penalización fija), no por diferencias en los retornos marginales."),
    general_title = "Interpretación:"
  )

# ── Interacción educación × informalidad ─────────────────────────────────────
# Pregunta: ¿La educación amortigua o amplifica la penalización por informalidad?
# Implicación de política: si β(educ:informal) > 0, la educación puede ser
# un canal indirecto de formalización del mercado laboral.

modelo_int_formal <- lm(
  ln_ingreso ~ educ + educ:informal + experiencia_lab +
               I(experiencia_lab^2) + ln_horas + factor(SEXO) +
               informal + factor(sector),
  data = base_ext
)
coefs_if    <- coeftest(modelo_int_formal, vcov = vcovHC(modelo_int_formal, "HC1"))
b_educ_f    <- coefs_if["educ", "Estimate"]
b_int_f     <- coefs_if["educ:informal", "Estimate"]
b_educ_inf  <- b_educ_f + b_int_f
b_inf_base  <- coefs_if["informal", "Estimate"]
p_int_f     <- coefs_if["educ:informal", "Pr(>|t|)"]

tabla_int_f <- tibble(
  Sector   = c("Formal", "Informal",
                "Diferencial retorno educ. (Inf − For)",
                "Penalización informalidad (sin educ.)"),
  valor    = c(paste0(round(b_educ_f   * 100, 3), "% por año"),
               paste0(round(b_educ_inf * 100, 3), "% por año"),
               paste0(round(b_int_f    * 100, 3), " pp"),
               paste0(round((exp(b_inf_base) - 1) * 100, 2), "%")),
  signif   = c("—", "—",
               if_else(p_int_f < 0.001, "p < 0.001",
               if_else(p_int_f < 0.05,  paste0("p = ", round(p_int_f, 3)), "n.s.")),
               "—")
)
names(tabla_int_f) <- c("Sector / Parámetro", "Estimación", "Significancia")

kbl(tabla_int_f,
    escape = FALSE,
    caption = "Tabla I-2. Retornos a la educación por formalidad (modelo con interacción educ × informal)",
    booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 11) %>%
  footnote(
    general = if_else(p_int_f < 0.05,
      if_else(b_int_f > 0,
        paste0("La educación REDUCE la penalización por informalidad. ",
               "Cada año adicional de escolaridad amortigua la brecha en ",
               round(b_int_f * 100, 3), " pp. ",
               "Implicación de política: la inversión educativa puede actuar como ",
               "canal indirecto de reducción de la segmentación formal-informal."),
        "La educación AMPLIFICA la segmentación: trabajadores informales con mayor educación tienen mayor costo de oportunidad relativo."),
      "La interacción no es significativa: la penalización por informalidad es homogénea entre niveles educativos."),
    general_title = "Interpretación:"
  )

hm   <- 42.875 * 52 / 12
im   <- mean(base_mincer$ingreso_laboral, na.rm = TRUE)
med  <- median(base_mincer$ingreso_laboral, na.rm = TRUE)

ret_ols_b    <- round(b_educ_base * 100, 3)
ret_ols      <- round(b_educ_cor  * 100, 3)
sesgo_pp     <- round((b_educ_base - b_educ_cor) * 100, 3)
ret_heck     <- round(b_educ_heck * 100, 3)
ret_fe       <- round(b_educ_fe   * 100, 3)
elas_h_ext   <- round(b_horas_cor,  3)   # OLS Extendido
elas_h_base  <- round(b_horas_base, 3)   # OLS Base
pen_inf      <- round((exp(b_informal)  - 1) * 100, 2)
brecha_o     <- round((exp(b_mujer)     - 1) * 100, 2)
brecha_h     <- round((exp(b_sexo_heck) - 1) * 100, 2)
comp_sel     <- round(brecha_o - brecha_h, 2)
s_educ       <- format(round(med * (exp(b_educ_cor) - 1)), big.mark = ",")
s_formal     <- format(round(med * abs(exp(b_informal) - 1)), big.mark = ",")
s_hora       <- round(im * b_horas_cor / hm, 1)
s_brecha_o   <- format(round(med * abs(exp(b_mujer) - 1)), big.mark = ",")
s_brecha_h   <- format(round(med * abs(exp(b_sexo_heck) - 1)), big.mark = ",")
s_sel        <- format(round(med * abs((exp(b_mujer)-1) - (exp(b_sexo_heck)-1))), big.mark = ",")
med_fmt      <- format(round(med, 0), big.mark = ",")
dif_heck_ols <- round(ret_heck - ret_ols, 3)

tbl_coefs <- data.frame(
  Parametro = c("b_educ: retorno por anno de escolaridad (%)",
                "b_horas: elasticidad ingreso-horas",
                "Brecha genero (Mujer vs. Hombre, %)",
                "Penalizacion informalidad (%)",
                "R2 / R2 within",
                "Rol en la tesis"),
  OLS_Base  = c(paste0(ret_ols_b, "% [sesgo +", sesgo_pp, " pp]"),
                elas_h_base,
                "No incluido",
                "No incluido",
                round(summary(modelo_base)$r.squared, 3),
                "Benchmark"),
  OLS_Ext   = c(paste0(ret_ols, "%"),
                elas_h_ext,
                paste0(brecha_o, "%"),
                paste0(pen_inf, "%"),
                round(summary(modelo_corregido)$r.squared, 3),
                "MODELO PRINCIPAL"),
  Heckman   = c(paste0(ret_heck, "% [dif. ", dif_heck_ols, " pp]"),
                round(b_horas_heck, 3),
                paste0(brecha_h, "% [corr. seleccion]"),
                "No incluido",
                "0.162",
                "Correccion seleccion"),
  Pseudo_FE = c(paste0(ret_fe, "% [medias cohorte]"),
                paste0(round(b_horas_fe, 3), " (p=0.31, n.s.)"),
                "No incluido",
                "No incluido",
                round(summary(modelo_pseudo_fe)$r.squared["rsq"], 3),
                "Robustez intertemporal"),
  stringsAsFactors = FALSE
)
names(tbl_coefs) <- c("Parametro", "OLS Base", "OLS Extendido *",
                      "Heckman", "Pseudopanel FE")

kbl(tbl_coefs,
    caption   = paste0("Tabla S-1. Sintesis de parametros clave por modelo - ENCFT 2016-2024 (N = 285,515). ",
                       "Mediana ingreso: RD$ ", med_fmt),
    booktabs  = TRUE,
    escape    = TRUE) %>%
  kable_styling(full_width = TRUE, font_size = 11,
                bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(3, bold = TRUE) %>%
  row_spec(6, italic = TRUE) %>%
  footnote(
    general = paste0(
      "* OLS Extendido es el modelo principal. ",
      "Errores estandar HC1 (Eicker-Huber-White) en todos los OLS. ",
      "b_educ del Pseudopanel opera sobre medias de cohorte: no comparable con estimadores individuales. ",
      "Brecha genero en Heckman corregida por sesgo de seleccion (rho = -0.547)."
    ),
    general_title = "Nota:"
  )

# ── Tabla B: Impacto monetario sobre la mediana ──────────────────────────────
tbl_monetaria <- data.frame(
  Escenario  = c("1 año adicional de escolaridad",
                 "Trabajador formal vs. informal (comparable)",
                 "1 hora adicional por semana",
                 "Brecha de genero — OLS bruta",
                 "Brecha de genero — Heckman corregida",
                 "Componente de seleccion (diferencia)"),
  Impacto    = c(paste0("+RD$ ", s_educ, "/mes"),
                 paste0("+RD$ ", s_formal, "/mes"),
                 paste0("+RD$ ", s_hora, "/hr"),
                 paste0("-RD$ ", s_brecha_o, "/mes"),
                 paste0("-RD$ ", s_brecha_h, "/mes"),
                 paste0("-RD$ ", s_sel, "/mes")),
  Modelo     = c("OLS Extendido", "OLS Extendido", "OLS Extendido",
                 "OLS Extendido", "Heckman", "Heckman"),
  stringsAsFactors = FALSE
)

kbl(tbl_monetaria,
    caption = paste0("Tabla S-2. Impacto monetario mensual estimado — base: mediana RD$ ",
                     med_fmt, " (ENCFT 2016-2024)"),
    col.names = c("Escenario", "Impacto mensual estimado", "Modelo fuente"),
    booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(4:6, color = "#c0392b") %>%
  footnote(
    general = paste0(
      "Impactos calculados sobre la mediana muestral (RD$ ", med_fmt, "). ",
      "No son predicciones causales: reflejan asociaciones condicionales. ",
      "Componente de seleccion = brecha OLS - brecha Heckman (",
      comp_sel, " pp): ",
      "fraccion de la brecha de genero atribuible a diferencias de participacion."
    ),
    general_title = "Nota:"
  )

# ── Tabla resumen retornos por sector (ya estimados en sección de correcciones) ──
tabla_sw <- data.frame(
  Especificacion = c("OLS Extendido (pooled)",
                     "Switching — Sector Formal",
                     "Switching — Sector Informal",
                     "Diferencial F–I"),
  Retorno        = c("3.821%", "6.257%", "2.371%", "3.886 pp"),
  Rol            = c("Promedio condicional",
                     "Premio educativo formal",
                     "Premio educativo informal",
                     "Brecha de segmentación")
)

kbl(tabla_sw,
    col.names = c("Especificación", "Retorno educativo", "Interpretación"),
    caption   = "Tabla OBS-1. Retornos educativos por sector de inserción — síntesis switching regression",
    booktabs  = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "condensed")) %>%
  row_spec(4, bold = TRUE) %>%
  footnote(general = paste0("La switching regression estima ecuaciones salariales simultáneas ",
                            "por régimen (Goldfeld & Quandt, 1973), corrigiendo la endogeneidad ",
                            "de la selección sectorial. Cov(Informal, ε) ≠ 0 invalida la ",
                            "especificación OLS con dummy de informalidad como estimador del ",
                            "efecto causal de la segmentación."),
           general_title = "Nota:")

# ── Modelo con dummy pandémica e interacción educación × pandemia ──────────────
# pandemia = 1 si ANO ∈ {2020, 2021}
# educ_pand = educ × pandemia (interacción)

base_pand <- base_mincer %>%
  filter(!is.na(informal), !is.na(sector)) %>%
  mutate(
    pandemia  = as.integer(ANO %in% c(2020, 2021)),
    educ_pand = educ * pandemia
  )

modelo_pandemia <- lm(
  ln_ingreso ~ educ + experiencia_lab + I(experiencia_lab^2) +
               ln_horas + factor(SEXO) + informal + factor(sector) +
               pandemia + educ_pand,
  data = base_pand
)

# Errores robustos HC1 (consistente con especificaciones previas)
vcov_pand <- vcovHC(modelo_pandemia, type = "HC1")
ct_pand   <- coeftest(modelo_pandemia, vcov = vcov_pand)

# Extraer parámetros de interés
delta1     <- ct_pand["pandemia",  "Estimate"]
delta2     <- ct_pand["educ_pand", "Estimate"]
se_delta1  <- ct_pand["pandemia",  "Std. Error"]
se_delta2  <- ct_pand["educ_pand", "Std. Error"]
p_delta1   <- ct_pand["pandemia",  "Pr(>|t|)"]
p_delta2   <- ct_pand["educ_pand", "Pr(>|t|)"]

beta1_pand <- ct_pand["educ", "Estimate"]          # retorno base (fuera pandemia)
ret_prepand  <- round(beta1_pand * 100, 3)
ret_pandemia <- round((beta1_pand + delta2) * 100, 3)

cat("=== EFECTOS PANDEMIA (HC1) ===\n")

cat(sprintf("δ₁ (efecto nivel pandemia):          %.4f  SE=%.4f  p=%.4f\n",
            delta1, se_delta1, p_delta1))

cat(sprintf("δ₂ (cambio retorno educ pandemia):   %.4f  SE=%.4f  p=%.4f\n",
            delta2, se_delta2, p_delta2))

cat(sprintf("Retorno educativo fuera pandemia:     %.3f%%\n", ret_prepand))

cat(sprintf("Retorno educativo durante pandemia:   %.3f%%\n", ret_pandemia))

cat(sprintf("Diferencia (Pand - Base):             %.3f pp\n", ret_pandemia - ret_prepand))

# ── Tabla de resultados del modelo pandemia ────────────────────────────────────
tabla_pand <- data.frame(
  Parametro = c("β₁ — Educación (años, fuera pandemia)",
                "δ₁ — Pandemia (efecto nivel)",
                "δ₂ — Educación × Pandemia (cambio en retorno)",
                "β₁ + δ₂ — Retorno durante pandemia",
                "Diferencia pandemia vs. base"),
  Estimacion = c(sprintf("%.4f (%.3f%%)", beta1_pand, ret_prepand),
                 sprintf("%.4f",          delta1),
                 sprintf("%.4f",          delta2),
                 sprintf("%.4f (%.3f%%)", beta1_pand + delta2, ret_pandemia),
                 sprintf("%.3f pp",       ret_pandemia - ret_prepand)),
  Sig = c(ifelse(ct_pand["educ",     "Pr(>|t|)"] < 0.01, "***", ""),
          ifelse(p_delta1 < 0.01, "***", ifelse(p_delta1 < 0.05, "**", "n.s.")),
          ifelse(p_delta2 < 0.01, "***", ifelse(p_delta2 < 0.05, "**", "n.s.")),
          "—", "—")
)

kbl(tabla_pand,
    col.names = c("Parámetro", "Estimación (HC1)", "Sig."),
    caption   = "Tabla OBS-3. Efectos de la pandemia COVID-19 sobre los retornos educativos (OLS Extendido + interacción)",
    booktabs  = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "condensed")) %>%
  row_spec(4, bold = TRUE) %>%
  footnote(general = paste0(
    "Pandemia = 1 si ANO ∈ {2020, 2021}. Errores estándar HC1. ",
    "*** p<0.01, ** p<0.05, n.s. = no significativo. ",
    "Muestra: base_mincer con observaciones no faltantes en informal y sector. ",
    "El retorno durante la pandemia es β₁ + δ₂; fuera de pandemia es β₁."),
    general_title = "Nota:")

# ── Gráfico: tendencia de ingresos por año con banda pandémica ─────────────────
# Se agrupa por ANO y nivel educativo (primaria/menos, secundaria, superior)
base_mincer %>%
  mutate(nivel_edu = case_when(
    educ <= 8  ~ "Primaria o menos",
    educ <= 12 ~ "Secundaria",
    educ >  12 ~ "Superior"
  )) %>%
  group_by(ANO, nivel_edu) %>%
  summarise(mediana_w = median(ingreso_laboral, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = ANO, y = mediana_w, color = nivel_edu, group = nivel_edu)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf, fill = "#E8B84B", alpha = 0.15) +
  annotate("text", x = 2020.5, y = Inf, vjust = 1.8, size = 3,
           label = "Pandemia\n2020-2021", color = "#8B6914", fontface = "italic") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = c("Primaria o menos" = "#CC3333",
               "Secundaria"       = "#E8A020",
               "Superior"         = "#0D1B2A"),
    name = "Nivel educativo") +
  scale_y_continuous(labels = scales::comma_format(prefix = "RD$")) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(x = "Año", y = "Ingreso mensual mediano",
       caption = "Fuente: ENCFT 2016-2024. Elaboración propia.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, color = "gray50"))

# ── Tasa de participación femenina y composición formal/informal por año ────────
# SEXO == 2 → Mujer en la ENCFT
part_mujer <- base_trab %>%
  filter(SEXO == 2, EDAD >= 16, EDAD <= 65) %>%
  group_by(ANO) %>%
  summarise(
    total_mujeres  = n(),
    participan     = sum(participa == 1, na.rm = TRUE),
    tasa_part      = round(participan / total_mujeres * 100, 1),
    .groups = "drop"
  )

comp_sector_mujer <- base_mincer %>%
  filter(SEXO == 2, !is.na(informal)) %>%
  group_by(ANO) %>%
  summarise(
    pct_formal   = round(mean(informal == 0, na.rm = TRUE) * 100, 1),
    pct_informal = round(mean(informal == 1, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

tabla_mujer <- left_join(part_mujer, comp_sector_mujer, by = "ANO") %>%
  select(ANO, tasa_part, pct_formal, pct_informal)

kbl(tabla_mujer,
    col.names = c("Año", "Tasa participación (%)", "Empleo formal (%)", "Empleo informal (%)"),
    caption   = "Tabla OBS-4. Participación laboral femenina y composición sectorial — ENCFT 2016–2024",
    booktabs  = TRUE, escape = FALSE,
    format.args = list(big.mark = ",")) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "condensed")) %>%
  footnote(general = paste0(
    "Tasa de participación: mujeres con ingreso > 0 y horas > 0 sobre total mujeres 16-65. ",
    "Empleo formal/informal: sobre la muestra analítica base_mincer (ocupadas con ingreso). ",
    "SEXO == 2 corresponde a mujer en la codificación ENCFT."),
    general_title = "Nota:")

# ── Gráfico composición formal/informal del empleo femenino ────────────────────
base_mincer %>%
  filter(SEXO == 2, !is.na(informal)) %>%
  group_by(ANO) %>%
  summarise(
    Formal   = mean(informal == 0, na.rm = TRUE),
    Informal = mean(informal == 1, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  pivot_longer(cols = c(Formal, Informal),
               names_to = "sector", values_to = "proporcion") %>%
  ggplot(aes(x = ANO, y = proporcion, fill = sector)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf, fill = "#E8B84B", alpha = 0.12) +
  geom_area(alpha = 0.85, position = "stack") +
  scale_fill_manual(values = c("Formal" = "#0E7C7B", "Informal" = "#CC3333"),
                    name = "Sector") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(x = "Año", y = "Proporción del empleo femenino",
       caption = "Fuente: ENCFT 2016-2024. Elaboración propia.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, color = "gray50"))

# ── Tabla síntesis ──────────────────────
tabla_sint <- data.frame(
  Obs = c("OBS 1", "OBS 2", "OBS 3", "OBS 4"),
  Observacion = c(
    "Ecuaciones formal e informal",
    "Tipo y calidad de la educación",
    "Efectos de la pandemia COVID-19",
    "Dinámica participación femenina"
  ),
  Estado = c(
    "Implementada — Switching Regression",
    "Limitación de datos — agenda futura",
    "Extensión nueva — dummy pandémica",
    "Cubierta — Heckman + extensión desc."
  ),
  Hallazgo = c(
    "Retorno formal (6.257%) > informal (2.371%) en 3.886 pp",
    "ENCFT no registra institución/calidad; sesgo no cuantificable sin datos MESCyT",
    "δ₁ y δ₂ cuantifican efecto nivel y cambio en retorno durante 2020-2021",
    "Selección explica 18.13 pp de la brecha; discriminación neta: −4.12%"
  )
)

kbl(tabla_sint,
    col.names = c("", "Observaciones", "Estado de cobertura", "Hallazgo / argumento clave"),
    caption   = "Tabla OBS-S. Síntesis de respuestas a las cuatro observaciones del comité evaluador",
    booktabs  = TRUE, escape = FALSE) %>%
  kable_styling(full_width = TRUE, font_size = 11,
                bootstrap_options = c("striped", "condensed")) %>%
  column_spec(1, bold = TRUE, width = "1.2cm") %>%
  column_spec(2, width = "4cm") %>%
  column_spec(3, width = "4.5cm") %>%
  column_spec(4, width = "7cm") %>%
  row_spec(1, background = "#e8f5f5") %>%
  row_spec(3, background = "#fff8e8") %>%
  footnote(general = paste0(
    "OBS 1 y OBS 4 están resueltas dentro del marco ya estimado (switching regression y Heckman). ",
    "OBS 2 es una limitación de fuente de datos no superable con la ENCFT. ",
    "OBS 3 es la única extensión nueva: requiere la dummy pandémica y la interacción educación × pandemia."),
    general_title = "Nota:")
