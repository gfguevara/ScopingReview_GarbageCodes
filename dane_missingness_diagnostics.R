# ==============================================================================
# PIPELINE DE DIAGNÓSTICO DE CALIDAD Y COMPLETITUD DE DATOS (DANE COLOMBIA 2018)
# ==============================================================================
# Este script carga la base de datos real del DANE "defunciones_colombia_2018",
# analiza de forma automática el porcentaje de datos faltantes (incompletitud)
# en cada columna clave para el análisis BeBOD y clasifica la calidad del dato 
# según el estándar internacional de Romero & Cunha (Excelente < 5%, Bueno 5-10%, 
# Regular 10-20%, Malo 20-50%, Muy Malo >= 50%).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Carga de Librerías
# ------------------------------------------------------------------------------
requisitos <- c("dplyr", "data.table", "ggplot2", "stringr")
instalar <- requisitos[!(requisitos %in% installed.packages()[, "Package"])]
if (length(instalar)) install.packages(instalar)

library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)

# ------------------------------------------------------------------------------
# 2. Carga Inteligente de los Datos Reales o Simulación
# ------------------------------------------------------------------------------
# Nombre de tu archivo descargado
nombre_archivo <- "defunciones_colombia_2018"

# Buscamos variaciones comunes de extensión (.csv, .txt) en el directorio
buscar_archivos <- list.files(pattern = paste0("^", nombre_archivo))

if (length(buscar_archivos) > 0) {
  archivo_path <- buscar_archivos[1]
  message("=== Archivo real detectado: ", archivo_path, " ===")
  message("Cargando microdatos del DANE con data.table...")
  
  # Cargamos seleccionando únicamente las columnas requeridas para el análisis
  df_raw <- fread(archivo_path, select = c("C_BAS1", "SEXO", "EDAD", "C_MUER1", "C_MUER2", "C_MUER3", "C_MUE_VI"))
} else {
  message("=== No se encontró el archivo real '", nombre_archivo, "' ===")
  message("Activando simulación estocástica de microdatos con el estándar del DANE para pruebas...")
  
  # Generamos una muestra simulada que introduce patrones de valores vacíos (missing) para diagnosticar
  set.seed(123)
  n_records <- 10000
  df_raw <- data.frame(
    C_BAS1 = sample(c("I509", "J189", "R99", "C55", "I219", "C349", "", NA), n_records, replace = TRUE, prob = c(0.16, 0.10, 0.08, 0.04, 0.25, 0.17, 0.15, 0.05)),
    SEXO = sample(c(1, 2, 9, NA), n_records, replace = TRUE, prob = c(0.48, 0.49, 0.01, 0.02)),
    EDAD = sample(c(1055, 1072, 1088, 2003, 3012, 9999, NA), n_records, replace = TRUE, prob = c(0.20, 0.30, 0.20, 0.03, 0.02, 0.20, 0.05)),
    C_MUER1 = sample(c("I219", "I10", "", NA), n_records, replace = TRUE, prob = c(0.30, 0.20, 0.40, 0.10)),
    C_MUER2 = sample(c("Ninguna", "I10", "I509", "", NA), n_records, replace = TRUE, prob = c(0.50, 0.15, 0.05, 0.20, 0.10)),
    C_MUER3 = sample(c("Ninguna", "A419", "", NA), n_records, replace = TRUE, prob = c(0.40, 0.10, 0.35, 0.15)),
    C_MUE_VI = sample(c("A419", "", "Ninguna", NA), n_records, replace = TRUE, prob = c(0.12, 0.50, 0.30, 0.08))
  )
}

# ------------------------------------------------------------------------------
# 3. Función de Diagnóstico de Completitud
# ------------------------------------------------------------------------------
diagnosticar_completitud <- function(data) {
  n_total <- nrow(data)
  
  # Analizamos cada columna
  reporte <- lapply(names(data), function(col_name) {
    vec <- data[[col_name]]
    
    # Identificamos qué se considera "Faltante / Incompleto" en el estándar del DANE:
    # 1. Valores NA nativos de R
    # 2. Cadenas vacías o con espacios en blanco ("" o " ")
    # 3. Códigos de ignorados específicos del DANE (ej. SEXO = 9 es "sin información", EDAD = 9999 es "ignorado")
    n_missing <- sum(
      is.na(vec) | 
      str_trim(as.character(vec)) == "" | 
      (col_name == "SEXO" & as.character(vec) == "9") |
      (col_name == "EDAD" & as.character(vec) == "9999")
    )
    
    pct_missing <- (n_missing / n_total) * 100
    
    # Clasificación de la calidad del dato (Romero & Cunha)
    clasificacion <- case_when(
      pct_missing < 5  ~ "Excelente (<5% faltante)",
      pct_missing >= 5  & pct_missing < 10  ~ "Bueno (5-10% faltante)",
      pct_missing >= 10 & pct_missing < 20  ~ "Regular (10-20% faltante)",
      pct_missing >= 20 & pct_missing < 50  ~ "Malo (20-50% faltante)",
      pct_missing >= 50 ~ "Muy Malo (>=50% faltante)"
    )
    
    # Color sugerido para visualización (Hexadecimales limpios)
    color_hex <- case_when(
      pct_missing < 5  ~ "#2ecc71", # Verde
      pct_missing >= 5  & pct_missing < 10  ~ "#27ae60", # Verde Oscuro
      pct_missing >= 10 & pct_missing < 20  ~ "#f1c40f", # Amarillo
      pct_missing >= 20 & pct_missing < 50  ~ "#e67e22", # Naranja
      pct_missing >= 50 ~ "#e74c3c"  # Rojo
    )
    
    data.frame(
      Variable = col_name,
      Total_Registros = n_total,
      Registros_Faltantes = n_missing,
      Porcentaje_Incompletitud = round(pct_missing, 2),
      Porcentaje_Completitud = round(100 - pct_missing, 2),
      Calidad_Dato = clasificacion,
      Color = color_hex,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  return(reporte)
}

# Ejecutamos el diagnóstico
reporte_calidad <- diagnosticar_completitud(df_raw)

# ------------------------------------------------------------------------------
# 4. Imprimir Reporte Consolidado en Consola
# ------------------------------------------------------------------------------
message("\n==============================================================================")
message("   REPORTE DE DIAGNÓSTICO DE CALIDAD DE DATOS (DANE COLOMBIA 2018)  ")
message("==============================================================================")
print(as.data.frame(reporte_calidad %>% select(Variable, Total_Registros, Registros_Faltantes, Porcentaje_Incompletitud, Calidad_Dato)))
message("==============================================================================\n")

# ------------------------------------------------------------------------------
# 5. Generar Visualización de Calidad (ggplot2)
# ------------------------------------------------------------------------------
message("Generando gráfico de diagnóstico de completitud de datos...")

grafico_diagnostico <- ggplot(reporte_calidad, aes(x = reorder(Variable, Porcentaje_Incompletitud), y = Porcentaje_Incompletitud, fill = Calidad_Dato)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = paste0(Porcentaje_Incompletitud, "%")), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(
    name = "Nivel de Calidad (Romero & Cunha)",
    values = c(
      "Excelente (<5% faltante)" = "#2ecc71",
      "Bueno (5-10% faltante)" = "#27ae60",
      "Regular (10-20% faltante)" = "#f1c40f",
      "Malo (20-50% faltante)" = "#e67e22",
      "Muy Malo (>=50% faltante)" = "#e74c3c"
    )
  ) +
  labs(
    title = "Diagnóstico de Incompletitud en Microdatos de Defunciones",
    subtitle = "Análisis automatizado previo a la aplicación del algoritmo de Bélgica (BeBOD)",
    x = "Variables de la Base de Datos",
    y = "Porcentaje de Datos Incompletos / Faltantes (%)",
    caption = "Clasificación basada en el estándar Romero & Cunha para evaluación de Sistemas de Información en Salud."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#7f8c8d", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank()
  ) +
  ylim(0, 100)

ggsave("C:/Users/gfguevara/OneDrive - UNIVERSIDAD DR. JOSE MATIAS DELGADO/Documentos/PhD/Tesis/Protocolo/Protocol/dane_incompletitud_reporte.png", 
       plot = grafico_diagnostico, 
       width = 9, 
       height = 5, 
       dpi = 150)

message("Gráfico guardado con éxito en tu carpeta de la tesis.")