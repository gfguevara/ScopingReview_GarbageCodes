library(dplyr)

# 1. Inspeccionar qué categorías existen realmente en la base
print(table(df_mortalidad$tipo_causa))

# 2. Separar usando un patrón de texto flexible ("Basura") para evitar errores de tildes
df_basura <- df_mortalidad %>%
  filter(grepl("Basura", tipo_causa))

df_validos <- df_mortalidad %>%
  filter(!grepl("Basura", tipo_causa))

# Imprimir cuántos detectó para confirmar que ya no está en 0
cat("Registros basura detectados:", nrow(df_basura), "\n")
cat("Registros válidos detectados:", nrow(df_validos), "\n")


# 3. Calcular los pesos de transición usando solo los válidos
pesos_transicion <- df_validos %>%
  count(edad_grupo, sexo, tipo_causa) %>%
  group_by(edad_grupo, sexo) %>%
  mutate(probabilidad = n / sum(n)) %>%
  ungroup()


# 4. Aplicar la redistribución probabilística de forma segura
set.seed(123)
df_basura_redistribuido <- df_basura %>%
  left_join(pesos_transicion, by = c("edad_grupo", "sexo"), relationship = "many-to-many") %>%
  group_by(id, edad_grupo, sexo) %>%
  slice_sample(n = 1, weight_by = probabilidad) %>%
  ungroup() %>%
  select(id, edad_grupo, sexo, tipo_causa = tipo_causa.y)


# 5. Unir la base corregida final
df_final_corregido <- bind_rows(df_validos, df_basura_redistribuido)

# 6. Ver el resultado final exitoso
cat("\n--- Distribución final de causas tras la redistribución ---\n")
print(prop.table(table(df_final_corregido$tipo_causa)) * 100)
