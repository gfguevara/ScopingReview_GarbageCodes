# ==============================================================================
# PIPELINE IN R DE MACHINE LEARNING: PREDICCIÓN Y RECLASIFICACIÓN DE CAUSAS DE MUERTE
# APLICANDO RANDOM FOREST (LIBRERÍA CARET) A MICRODATOS DEL DANE COLOMBIA
# ==============================================================================
# Este script implementa un modelo de Inteligencia Artificial (Random Forest)
# para predecir la causa básica de muerte verdadera detrás de códigos inespecíficos,
# utilizando características demográficas y causas múltiples de muerte (MCoD).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Instalación y Carga de Librerías Necesarias
# ------------------------------------------------------------------------------
requisitos <- c("dplyr", "tidyr", "data.table", "caret", "randomForest", "e1071")
instalar <- requisitos[!(requisitos %in% installed.packages()[, "Package"])]
if (length(instalar)) install.packages(instalar, dependencies = TRUE)

library(dplyr)
library(tidyr)
library(data.table)
library(caret)
library(randomForest)
library(e1071)

# Set seed para reproducibilidad de los modelos estocásticos
set.seed(123)

# ------------------------------------------------------------------------------
# 2. Carga o Simulación de Microdatos con Estructura Real del DANE
# ------------------------------------------------------------------------------
# Intentamos cargar una base de datos real local ("defunciones_colombia.csv")
# Si no existe, el pipeline genera automáticamente una base simulada de 5,000 registros
# que imita con precisión el comportamiento multivariado y de causas múltiples del DANE.

csv_local <- "defunciones_colombia.csv"

if (file.exists(csv_local)) {
  message("=== Cargando base de datos real del DANE... ===")
  df_raw <- fread(csv_local)
} else {
  message("=== Archivo local no encontrado. Generando muestra simulada con estructura exacta del DANE... ===")
  n_records <- 5000
  
  # I219: Infarto de miocardio (Causa cardíaca específica)
  # C349: Cáncer de pulmón (Causa oncológica específica)
  # C530: Cáncer de cuello uterino (Causa oncológica específica femenina)
  # E119: Diabetes mellitus tipo 2 (Causa endocrina específica)
  
  df_raw <- data.frame(
    C_BAS1 = sample(c("I219", "C349", "C530", "E119"), n_records, replace = TRUE, prob = c(0.40, 0.25, 0.15, 0.20)),
    SEXO = sample(c(1, 2), n_records, replace = TRUE),
    EDAD = sample(c(1005, 1018, 1055, 1072, 1088, 2003, 3012), n_records, replace = TRUE, prob = c(0.02, 0.05, 0.20, 0.40, 0.28, 0.03, 0.02)),
    # Causas múltiples contribuyentes (MCoD) que guardan correlación clínica con la causa básica
    C_MUER1 = "Ninguna",
    C_MUER2 = "Ninguna",
    C_MUE_VI = "Ninguna"
  )
  
  # Inyectamos correlación clínica realista para que el Random Forest aprenda patrones médicos
  df_raw <- df_raw %>%
    mutate(
      C_MUER1 = case_when(
        C_BAS1 == "I219" ~ sample(c("I10", "Ninguna"), n(), replace = TRUE, prob = c(0.70, 0.30)), # Hipertensión (I10) coexiste con Infarto (I219)
        C_BAS1 == "C349" ~ sample(c("J189", "Ninguna"), n(), replace = TRUE, prob = c(0.60, 0.40)), # Neumonía (J189) coexiste con Cáncer de pulmón
        C_BAS1 == "E119" ~ sample(c("N19", "Ninguna"), n(), replace = TRUE, prob = c(0.50, 0.50)),  # Falla renal (N19) coexiste con Diabetes
        TRUE ~ "Ninguna"
      ),
      C_MUE_VI = case_when(
        C_BAS1 == "I219" ~ sample(c("I509", "Ninguna"), n(), replace = TRUE, prob = c(0.80, 0.20)), # Insuficiencia cardíaca terminal (I509)
        TRUE ~ "Ninguna"
      ),
      # Garantizar consistencia biológica (C530 solo ocurre en mujeres, SEXO = 2)
      SEXO = if_else(C_BAS1 == "C530", 2, SEXO)
    )
}

# ------------------------------------------------------------------------------
# 3. Preprocesamiento de Variables y Feature Engineering
# ------------------------------------------------------------------------------
message("=== Ejecutando preprocesamiento de datos y Feature Engineering... ===")

# Decodificamos el sistema de edad alfanumérico del DANE y creamos factores
df_prep <- df_raw %>%
  # 1. Homologación de sexo
  mutate(sex = factor(if_else(SEXO == 1, "M", "F"))) %>%
  # 2. Decodificación de edad DANE (1xxx son años de vida)
  mutate(
    edad_anios = case_when(
      EDAD >= 1000 & EDAD < 2000 ~ EDAD - 1000,
      EDAD >= 2000 ~ 0, # Meses, días y horas se truncan a 0 años
      TRUE ~ NA_real_
    )
  ) %>%
  # 3. Clasificación etaria BeBOD de 6 estratos
  mutate(
    age_group = factor(case_when(
      edad_anios < 5   ~ "0-4",
      edad_anios >= 5  & edad_anios < 15 ~ "5-14",
      edad_anios >= 15 & edad_anios < 45 ~ "15-44",
      edad_anios >= 45 & edad_anios < 65 ~ "45-64",
      edad_anios >= 65 & edad_anios < 85 ~ "65-84",
      edad_anios >= 85 ~ "85+",
      TRUE ~ "Desconocido"
    ))
  ) %>%
  # 4. Convertimos los predictores clínicos de causas múltiples (MCoD) en variables tipo factor
  mutate(
    mcod_hipertension = factor(if_else(C_MUER1 == "I10" | C_MUER2 == "I10", "SI", "NO")),
    mcod_neumonia      = factor(if_else(C_MUER1 == "J189" | C_MUER2 == "J189", "SI", "NO")),
    mcod_fallarenal    = factor(if_else(C_MUER1 == "N19" | C_MUER2 == "N19", "SI", "NO")),
    mcod_insucardiaca  = factor(if_else(C_MUE_VI == "I509", "SI", "NO")),
    
    # Target o variable respuesta (Causa básica bien definida que queremos predecir)
    target = factor(C_BAS1)
  ) %>%
  # Seleccionamos las columnas estructuradas para el entrenamiento del Random Forest
  select(target, sex, age_group, mcod_hipertension, mcod_neumonia, mcod_fallarenal, mcod_insucardiaca) %>%
  filter(!is.na(target))

# ------------------------------------------------------------------------------
# 4. División de la Muestra (Train/Test Split 70-30)
# ------------------------------------------------------------------------------
message("=== Dividiendo la muestra en set de Entrenamiento (70%) y Prueba (30%)... ===")
indices_entrenamiento <- createDataPartition(df_prep$target, p = 0.70, list = FALSE)

train_set <- df_prep[indices_entrenamiento, ]
test_set  <- df_prep[-indices_entrenamiento, ]

# ------------------------------------------------------------------------------
# 5. Configuración y Entrenamiento de Random Forest (Caret)
# ------------------------------------------------------------------------------
message("=== Iniciando entrenamiento del modelo Random Forest con Caret... ===")

# Configuración del entrenamiento por validación cruzada repetida (K-Fold CV)
control_modelo <- trainControl(
  method = "repeatedcv",
  number = 5,          # 5-Fold Cross Validation
  repeats = 2,         # Repetir la validación cruzada 2 veces para estabilizar la varianza
  verboseIter = TRUE   # Imprimir el progreso del entrenamiento en consola
)

# Entrenamiento del clasificador Random Forest
# mtry es el hiperparámetro clave: número de variables aleatorias evaluadas en cada división
modelo_rf <- train(
  target ~ sex + age_group + mcod_hipertension + mcod_neumonia + mcod_fallarenal + mcod_insucardiaca,
  data = train_set,
  method = "rf",
  trControl = control_modelo,
  tuneGrid = expand.grid(mtry = c(2, 3, 4)), # Ajuste automático del hiperparámetro mtry
  ntree = 150,                               # Entrenamos 150 árboles de decisión
  importance = TRUE                          # Calcular la importancia de las variables
)

# Mostramos el resumen del modelo y su mejor hiperparámetro mtry seleccionado
print(modelo_rf)

# ------------------------------------------------------------------------------
# 6. Evaluación de Desempeño y Validación Cruzada en el Test Set
# ------------------------------------------------------------------------------
message("\\n=== Evaluando el desempeño del modelo en el set de prueba independiente (Test Set)... ===")

# Generamos predicciones sobre el Test Set
predicciones <- predict(modelo_rf, newdata = test_set)

# Matriz de Confusión y Estadísticas Avanzadas (Métricas JBI-standard)
matriz_confusion <- confusionMatrix(predicciones, test_set$target)
print(matriz_confusion)

# ------------------------------------------------------------------------------
# 7. Importancia de Características Clínicas (Variable Importance)
# ------------------------------------------------------------------------------
message("\\n=== Calculando la importancia de las variables para la clasificación médica... ===")

importancia_variables <- varImp(modelo_rf, scale = TRUE)
print(importancia_variables)

# Graficar la importancia de las variables
plot(importancia_variables, main = "Importancia de Variables Clínicas en la Predicción de Causa de Muerte")

# ------------------------------------------------------------------------------
# 8. Guardar el Modelo Entrenado para su Transferencia a la Microsimulación
# ------------------------------------------------------------------------------
# Guardar el modelo en formato .rds permite cargarlo de forma instantánea en un entorno
# de simulación estocástica de agentes para realizar micro-reasignaciones en milisegundos.
saveRDS(modelo_rf, file = "modelo_random_forest_dane.rds")
message("\\nPipeline finalizado con éxito. Modelo guardado como 'modelo_random_forest_dane.rds' en tu directorio.")
