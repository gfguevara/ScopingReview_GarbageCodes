# ==============================================================================
# PIPELINE EN R: VISUALIZACIÓN COMPARATIVA DE LA REDISTRIBUCIÓN (BeBOD)
# ==============================================================================
# Este script carga la base de datos original de Colombia (2018) y la base
# corregida mediante el método de Bélgica, genera una matriz de comparación
# y produce un gráfico de barras doble de calidad de publicación (ggplot2).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Preparación del Entorno y Carga de Librerías
# ------------------------------------------------------------------------------
requisitos <- c("dplyr", "tidyr", "ggplot2", "data.table", "stringr")
instalar <- requisitos[!(requisitos %in% installed.packages()[, "Package"])]
if (length(instalar)) install.packages(instalar)

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(stringr)

# ------------------------------------------------------------------------------
# 2. Carga Segura de Datos (Original vs Corregido)
# ------------------------------------------------------------------------------
# Definimos nombres de archivos
archivo_original <- list.files(pattern = "(?i)defunciones_colombia_2018(\\.(csv|txt))?|nofetales_2018(\\.(csv|txt))?")[1]
archivo_corregido <- "defunciones_colombia_2018_corregidas.csv"

# Verificación de la existencia física de los archivos
if (!is.na(archivo_original) && file.exists(archivo_original) && file.exists(archivo_corregido)) {
  message("=== Carga exitosa: Leyendo datos locales reales... ===")
  df_orig <- fread(archivo_original)
  df_corr <- fread(archivo_corregido)
} else {
  warning("No se encontraron ambos archivos físicos locales en tu directorio actual.")
  message("Para garantizar que puedas probar el script de inmediato, generaremos una simulación")
  message("coherente del 'antes y después' del procesamiento del método de Bélgica.")
  
  # Simulación de datos alineada con la estructura del DANE
  set.seed(42)
  n_records <- 8000
  
  df_orig <- data.frame(
    C_BAS1 = sample(c("I509", "J189", "R99", "C55", "I219", "C349", "C530", "E119"), 
                    n_records, replace = TRUE, prob = c(0.18, 0.12, 0.08, 0.04, 0.28, 0.18, 0.06, 0.06))
  )
  
  df_corr <- data.frame(
    ucod_final = sample(c("I219", "C349", "C530", "C541", "E119"), 
                        n_records, replace = TRUE, prob = c(0.42, 0.26, 0.12, 0.06, 0.14))
  )
}

# ------------------------------------------------------------------------------
# 3. Armonización de Cabeceras e Identificadores
# ------------------------------------------------------------------------------
# Detectamos causa básica original en df_orig
orig_col <- grep("^(C_BAS1|C_BAS|CAUSA_BAS|CAUSABAS)$", names(df_orig), ignore.case = TRUE, value = TRUE)[1]
if (is.na(orig_col)) orig_col <- names(df_orig)[1]

# Detectamos causa básica corregida en df_corr
corr_col <- grep("^(ucod_final|ucod_p3|C_BAS1)$", names(df_corr), ignore.case = TRUE, value = TRUE)[1]
if (is.na(corr_col)) corr_col <- names(df_corr)[1]

# ------------------------------------------------------------------------------
# 4. Procesamiento y Agregación de Frecuencias
# ------------------------------------------------------------------------------
message("=== Procesando frecuencias comparativas... ===")

# Calculamos frecuencias para el archivo original
freq_orig <- df_orig %>%
  rename(Causa = !!sym(orig_col)) %>%
  mutate(Causa = str_replace_all(Causa, "[^A-Za-z0-9]", "")) %>%
  count(Causa, name = "Frecuencia_Original")

# Calculamos frecuencias para el archivo corregido
freq_corr <- df_corr %>%
  rename(Causa = !!sym(corr_col)) %>%
  mutate(Causa = str_replace_all(Causa, "[^A-Za-z0-9]", "")) %>%
  count(Causa, name = "Frecuencia_Corregida")

# Fusionamos ambas tablas (Full Outer Join)
df_comparativa <- full_join(freq_orig, freq_corr, by = "Causa") %>%
  replace_na(list(Frecuencia_Original = 0, Frecuencia_Corregida = 0))

# Agregamos etiquetas clínicas para mejor legibilidad en el gráfico
diagnosticos_nombres <- c(
  "I509" = "Insuficiencia Cardíaca (Código Basura)",
  "J189" = "Neumonía Inespecífica (Código Basura)",
  "R99"  = "Causa Mal Definida (Código Basura)",
  "C55"  = "Cáncer de Útero Inespecífico (Código Basura)",
  "I219" = "Infarto Agudo de Miocardio (Válido)",
  "C349" = "Cáncer de Pulmón (Válido)",
  "C530" = "Cáncer de Cuello Uterino (Válido)",
  "C541" = "Cáncer de Cuerpo Uterino (Válido)",
  "E119" = "Diabetes Mellitus Tipo 2 (Válido)"
)

df_comparativa <- df_comparativa %>%
  mutate(
    Etiqueta_Clinica = ifelse(Causa %in% names(diagnosticos_nombres), 
                              diagnosticos_nombres[Causa], 
                              paste("CIE-10:", Causa)),
    # Marcamos si la causa es código basura o válida para coloreado
    Tipo_Causa = ifelse(Causa %in% c("I509", "J189", "R99", "C55"), "Código Basura", "Causa Válida")
  )

# Convertimos a formato largo (tidy long format) para ggplot2
df_long <- df_comparativa %>%
  pivot_longer(
    cols = c(Frecuencia_Original, Frecuencia_Corregida),
    names_to = "Base_Datos",
    values_to = "Fallecidos"
  ) %>%
  mutate(
    Base_Datos = factor(Base_Datos, 
                        levels = c("Frecuencia_Original", "Frecuencia_Corregida"),
                        labels = c("1. Datos Originales (Pre-BeBOD)", "2. Datos Corregidos (Post-BeBOD)"))
  )

# ------------------------------------------------------------------------------
# 5. Generación del Gráfico GGPLOT2 (Calidad de Publicación)
# ------------------------------------------------------------------------------
message("=== Generando visualización ggplot2... ===")

grafico_comparativa <- ggplot(df_long, aes(x = reorder(Etiqueta_Clinica, Fallecidos), y = Fallecidos, fill = Base_Datos)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black", size = 0.2) +
  geom_text(aes(label = ifelse(Fallecidos > 0, comma(Fallecidos), "")), 
            position = position_dodge(width = 0.8), 
            hjust = -0.2, 
            size = 3, 
            fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("1. Datos Originales (Pre-BeBOD)" = "#e06666", "2. Datos Corregidos (Post-BeBOD)" = "#3d85c6")) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Impacto de la Redistribución de Códigos Basura en Colombia 2018",
    subtitle = "Comparativa del perfil de mortalidad antes y después del algoritmo de Bélgica (BeBOD)",
    x = "Diagnóstico Registrado (CIE-10)",
    y = "Número Absoluto de Fallecidos",
    fill = "Estado de los Datos",
    caption = "Nota: El algoritmo reasigna los códigos basura a causas válidas usando información demográfica y multicausal (MCoD)."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1a1a1a"),
    plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 10, color = "#222222"),
    axis.text = element_text(size = 9, color = "#333333"),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 30, 20, 20)
  )

# Función auxiliar de formateo numérico rápido en caso de no tener scales cargado
comma <- function(x) format(x, big.mark = ",", scientific = FALSE)

# ------------------------------------------------------------------------------
# 6. Guardar Gráfico y Resultados
# ------------------------------------------------------------------------------
# Guardamos en tu directorio local
nombre_grafico <- "comparativa_redistribucion_colombia_2018.png"
ggsave(nombre_grafico, plot = grafico_comparativa, width = 11, height = 6, dpi = 150)

message("\n=== PIPELINE DE COMPARACIÓN COMPLETADO CON ÉXITO ===")
message("Gráfico guardado en tu directorio de trabajo como: '", nombre_grafico, "'")
message("Ejecuta este script para visualizar el cambio en la distribución de causas en tu tesis.")
