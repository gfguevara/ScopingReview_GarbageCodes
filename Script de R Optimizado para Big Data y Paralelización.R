library(data.table)
library(parallel)

# Configurar hilos automáticos para data.table
setDTthreads(percent = 80)

# ==========================================
# 1. GENERACIÓN EFICIENTE DE DATOS (500,000 registros)
# ==========================================
set.seed(2026)
n_registros <- 500000

# Usamos data.table para crear la base de datos masiva de forma instantánea
df_mortalidad <- data.table(
  id = 1:n_registros,
  edad_grupo = sample(c("18-64", "65+"), n_registros, replace = TRUE, prob = c(0.3, 0.7)),
  sexo = sample(c("Masculino", "Femenino"), n_registros, replace = TRUE),
  ucod_original = sample(
    c("Cardiopatia_Isquemica", "Diabetes", "EPOC", "Sintoma_Vago", "Falla_Cardiaca"), 
    n_registros, replace = TRUE, prob = c(0.30, 0.20, 0.15, 0.20, 0.15)
  ),
  causa_contribuyente = sample(
    c("Ninguna", "Infeccion", "Hipertension", "Diabetes"), 
    n_registros, replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.1)
  )
)

# ==========================================
# 2. SEPARACIÓN DE VÁLIDOS Y CÓDIGOS BASURA
# ==========================================
codigos_basura <- c("Sintoma_Vago", "Falla_Cardiaca")

df_validos <- df_mortalidad[!ucod_original %in% codigos_basura]
df_basura  <- df_mortalidad[ucod_original %in% codigos_basura]


# ==========================================
# 3. CÁLCULO DE PESOS DE TRANSICIÓN
# ==========================================
# Agregación ultrarrápida optimizada en C con data.table
pesos_transicion <- df_validos[, .N, by = .(edad_grupo, sexo, causa_contribuyente, ucod_original)]
pesos_transicion[, probabilidad := N / sum(N), by = .(edad_grupo, sexo, causa_contribuyente)]
setnames(pesos_transicion, "ucod_original", "causa_objetivo")


# ==========================================
# 4. SIMULACIÓN DE MONTE CARLO PARALELIZADA
# ==========================================
n_simulaciones <- 50 

# Detectar núcleos disponibles (dejando 1 libre para el sistema operativo)
num_cores <- max(1, detectCores() - 1)

# Función que ejecuta una iteración individual de Monte Carlo
ejecutar_simulacion <- function(i, dt_basura, dt_pesos, dt_validos) {
  dt_b <- copy(dt_basura)
  
  # Cruce optimizado por claves demográficas y clínicas
  setkey(dt_pesos, edad_grupo, sexo, causa_contribuyente)
  setkey(dt_b, edad_grupo, sexo, causa_contribuyente)
  dt_unido <- dt_pesos[dt_b, allow.cartesian = TRUE]
  
  # Muestreo estocástico ponderado por cada registro único (id)
  set.seed(2026 + i)
  dt_seleccionado <- dt_unido[, .SD[sample(.N, 1, prob = probabilidad)], by = id]
  
  # Estructura final de la iteración
  dt_redistribuido <- dt_seleccionado[, .(id, edad_grupo, sexo, ucod_final = causa_objetivo)]
  
  dt_total_iter <- rbind(
    dt_validos[, .(id, edad_grupo, sexo, ucod_final = ucod_original)],
    dt_redistribuido
  )
  
  # Conteo final de la iteración
  res_conteo <- dt_total_iter[, .(n = .N), by = ucod_final]
  res_conteo[, iteracion := i]
  return(res_conteo)
}

# Configuración del cluster en paralelo
cl <- makeCluster(num_cores)
clusterExport(cl, c("ejecutar_simulacion", "df_basura", "pesos_transicion", "df_validos"))
clusterEvalQ(cl, { library(data.table) })

# Ejecución paralela de las 50 simulaciones midiendo el tiempo
cat("Ejecutando simulaciones de Monte Carlo en paralelo...\n")
tiempo_total <- system.time({
  resultados_lista <- parLapply(cl, 1:n_simulaciones, function(i) {
    ejecutar_simulacion(i, df_basura, pesos_transicion, df_validos)
  })
})

stopCluster(cl)
print(tiempo_total)


# ==========================================
# 5. CONSOLIDACIÓN E INTERVALOS DE INCERTIDUMBRE
# ==========================================
df_monte_carlo_final <- rbindlist(resultados_lista)

resumen_final <- df_monte_carlo_final[, .(
  media_muertes = mean(n),
  ci_inf = quantile(n, 0.025),
  ci_sup = quantile(n, 0.975)
), by = ucod_final]

cat("\n--- Resultados consolidados (500k registros optimizados) ---\n")
print(resumen_final)