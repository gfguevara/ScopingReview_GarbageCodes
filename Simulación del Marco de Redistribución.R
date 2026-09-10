library(dplyr)
library(tidyr)

# ==========================================
# 1. GENERACIÓN DE DATOS SIMULADOS (MCOD)
# ==========================================
set.seed(2026)
n_registros <- 500000

# Simulamos microdatos con Causa Subyacente (UCoD) y Causas Contribuyentes (MCOD)
df_mortalidad <- data.frame(
  id = 1:n_registros,
  edad_grupo = sample(c("18-64", "65+"), n_registros, replace = TRUE, prob = c(0.3, 0.7)),
  sexo = sample(c("Masculino", "Femenino"), n_registros, replace = TRUE),
  
  # Causa subyacente reportada (incluye códigos basura como "Sintoma_Vago" o "Fallaseca")
  ucod_original = sample(
    c("Cardiopatia_Isquemica", "Diabetes", "EPOC", "Sintoma_Vago", "Falla_Cardiaca"), 
    n_registros, replace = TRUE, prob = c(0.30, 0.20, 0.15, 0.20, 0.15)
  ),
  
  # Causa contribuyente presente en las líneas superiores del certificado (MCOD)
  causa_contribuyente = sample(
    c("Ninguna", "Infeccion", "Hipertension", "Diabetes"), 
    n_registros, replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.1)
  )
)

# ==========================================
# 2. SEPARACIÓN DE CASOS VÁLIDOS Y BASURA
# ==========================================
# Definimos cuáles son códigos basura (Garbage Codes)
codigos_basura <- c("Sintoma_Vago", "Falla_Cardiaca")

df_validos <- df_mortalidad %>% filter(!ucod_original %in% codigos_basura)
df_basura  <- df_mortalidad %>% filter(ucod_original %in% codigos_basura)


# ==========================================
# 3. CÁLCULO DE PESOS DE TRANSICIÓN (MCOD + Estratos)
# ==========================================
# Scohy et al. utilizan la relación entre las causas válidas y el contexto clínico/demográfico
pesos_transicion <- df_validos %>%
  count(edad_grupo, sexo, causa_contribuyente, ucod_original) %>%
  group_by(edad_grupo, sexo, causa_contribuyente) %>%
  mutate(probabilidad = n / sum(n)) %>%
  ungroup() %>%
  select(edad_grupo, sexo, causa_contribuyente, causa_objetivo = ucod_original, probabilidad)


# ==========================================
# 4. SIMULACIÓN DE MONTE CARLO (Múltiples Iteraciones / Draws)
# ==========================================
# Para evaluar la incertidumbre, Scohy repite el proceso estocástico N veces
n_simulaciones <- 50 
resultados_acumulados <- list()

set.seed(42)
for (i in 1:n_simulaciones) {
  
  # Para cada simulación, hacemos un join y un muestreo estocástico ponderado
  df_redistribuido_sim <- df_basura %>%
    left_join(pesos_transicion, by = c("edad_grupo", "sexo", "causa_contribuyente"), relationship = "many-to-many") %>%
    group_by(id) %>%
    # Si hay coincidencia multicausa, usa los pesos; si no, asigna por distribución marginal del estrato
    slice_sample(n = 1, weight_by = if(all(is.na(probabilidad))) NULL else probabilidad) %>%
    ungroup() %>%
    mutate(ucod_final = coalesce(causa_objetivo, "Cardiopatia_Isquemica")) %>% # Fallback de seguridad
    select(id, edad_grupo, sexo, ucod_final)
  
  # Unimos los válidos originales con los basura redistribuidos en esta iteración
  df_iteracion <- bind_rows(
    df_validos %>% select(id, edad_grupo, sexo, ucod_final = ucod_original),
    df_redistribuido_sim
  )
  
  # Guardamos el conteo por causa de esta simulación
  conteo_sim <- df_iteracion %>%
    count(ucod_final) %>%
    mutate(iteracion = i)
  
  resultados_acumulados[[i]] <- conteo_sim
}

# Consolidamos todas las simulaciones en una sola tabla
df_monte_carlo_final <- bind_rows(resultados_acumulados)


# ==========================================
# 5. RESULTADOS FINALES: INTERVALOS DE INCERTIDUMBRE
# ==========================================
# Calculamos la media y los percentiles (ej. intervalo del 95%) para cada causa corregida
resumen_final <- df_monte_carlo_final %>%
  group_by(ucod_final) %>%
  summarise(
    media_muertes = mean(n),
    ci_inf = quantile(n, 0.025),
    ci_sup = quantile(n, 0.975)
  )

print("--- Resultados consolidados con Incertidumbre de Monte Carlo ---")
print(resumen_final)