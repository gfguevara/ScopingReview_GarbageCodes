library(litsearchr)

# POBLACIÓN (Líneas 5, 6 y 7 del Appendix I)
poblacion <- c(
  "vital statistics", "vital statistics registration*", "registration of vital statistics", 
  "vital registration", "civil registration", "CRVS", "population register*", 
  "mortality regist*", "death regist*", "death certificat*", "death record*", 
  "cause of death", "causes of death", "death cause*", "underlying cause*", 
  "mortality data", "mortality statistic*", "mortality record*", 
  "mortality information system*", "mortality rate*", "death rate*", 
  "population surveillance", "public health surveillance", 
  "international classification of disease*", "ICD-9", "ICD-10", "ICD-11", 
  "clinical coding", "medical coding", "health information system*", "mortality"
)

# CONCEPTO A: Códigos basura y causas mal definidas (Líneas 10 y 11 del Appendix I)
# Nota: Las líneas 12 y 13 usan el operador de proximidad 'adj3'. litsearchr 
# genera búsquedas booleanas estándar (AND/OR), por lo que para bases de datos 
# sin operadores de adyacencia, es mejor expandir los términos principales.
codigos_basura <- c(
  "garbage code*", "garbage cause*", "garbage categor*", "garbage diagnos*", 
  "ill-defined cause*", "ill-defined death*", "ill-defined condition*", 
  "poorly defined cause*", "causas mal definidas", "codigos basura", 
  "codigos lixo", "codigos garbage", "R00-R99", "ill-defined", "unknown cause*", 
  "unspecified cause*", "nonspecific cause*", "non-specific cause*", 
  "unspecific cause*", "undetermined cause*", "vague code*", 
  "cause of death assignment", "underlying cause assignment", "misclassif*", 
  "miscod*", "misattribut*", "miscertif*"
)

# CONCEPTO B: Métodos de corrección / redistribución (Líneas 30, 31, 32, 33, 34 y 36)
metodos_correccion <- c(
  "proportional redistribution", "proportional distribution", "fractional algorithm*", 
  "Brass technique", "Brass technic", "reverse survival", "stable population", 
  "demographic analys*", "life table*", "statistical model*", "probabilistic model*", 
  "bayesian", "empirical bayes", "spatiotemporal", "spatio-temporal", 
  "spatial regression", "imput*", "CODEm", "CoDCorrect", "cause of death ensemble model*", 
  "coarsened exact matching", "reallocat*", "reassign*", "recod*", "algorithm*", 
  "multiple cause*", "linkage", "verbal autops*", "hospital discharge", 
  "discharge record*", "discharge data", "artificial intelligence", "machine learning", 
  "decision tree*", "data mining", "random forest*", "gradient boosting", 
  "deep learning", "neural network*", "microsimulat*", "micro-simulation*", 
  "agent-based", "synthetic population*", "global burden of disease", "GBD", 
  "InterVA", "InSilicoVA", "SmartVA", "tariff method", "bridge coding", 
  "comparability ratio*", "IRIS software", "automated coding", 
  "automated classification", "small area estimation", "correction factor*", 
  "redistribut*", "reclassif*"
)

# Agrupar los conceptos en una lista
conceptos_revision <- list(poblacion, codigos_basura, metodos_correccion)

# Generar la cadena de búsqueda (ejemplo para Scopus / Web of Science / PubMed)
cadena_traducida <- write_search(
  groupdata = conceptos_revision,
  languages = "English",
  exactphrase = TRUE,
  stemming = FALSE,
  closure = "left",
  writesearch = FALSE
)

cat(cadena_traducida)

writeLines(cadena_traducida, "estrategia_completa.txt")


library(litsearchr)
library(igraph) # Paquete necesario para procesar y dibujar la red

# 1. Importar los resultados de Ovid MEDLINE
mis_resultados <- import_results(file = "EstrategiaUnificada_151026.ris")

# 2. Extraer términos automáticamente de los títulos y resúmenes
terminos_extraidos <- extract_terms(text = paste(mis_resultados$title, mis_resultados$abstract), 
                                    method = "fakerake", min_freq = 3, min_n = 2)

# 3. Combinar los términos extraídos con los de la estrategia para forzar su análisis
mis_terminos_propios <- c(poblacion, codigos_basura, metodos_correccion) 
todos_los_terminos <- unique(c(terminos_extraidos, mis_terminos_propios))

# 4. Crear la matriz documento-término (DFM)
matriz_terminos <- create_dfm(elements = paste(mis_resultados$title, mis_resultados$abstract), 
                              features = todos_los_terminos)

# 5. Construir la red matemática de coocurrencia
red_terminos <- create_network(search_dfm = matriz_terminos, min_studies = 3)

# 6. Generar el gráfico
plot(red_terminos, 
     vertex.size = igraph::degree(red_terminos)*0.5, # El tamaño del nodo depende de sus conexiones
     vertex.label.cex = 0.7, # Tamaño del texto
     vertex.color = "lightblue", 
     edge.color = "gray80",
     main = "Red de Coocurrencia: Metodologías de Códigos Basura")

# 1. Encontrar el punto de corte (retiene el 80% de la fuerza acumulada de la red)
corte <- find_cutoff(red_terminos, method = "cumulative", percent = 0.80)

# 2. Reducir el gráfico eliminando los nodos irrelevantes (limpia la periferia)
red_reducida <- reduce_graph(red_terminos, cutoff_strength = corte)

# 3. Calcular el tamaño de los nodos según su grado (número de conexiones)
tamanio_nodos <- igraph::degree(red_reducida)

# 4. Condicionar las etiquetas: mostrar texto SOLO para nodos muy conectados
# Puedes subir o bajar el '5' dependiendo de qué tan limpia quieras la tipografía
etiquetas <- ifelse(tamanio_nodos > 5, igraph::V(red_reducida)$name, NA)

# 5. Aplicar el algoritmo Fruchterman-Reingold para separar los clústeres
set.seed(42)
disposicion <- igraph::layout_with_fr(red_reducida)

# 6. Dibujar el gráfico con parámetros estéticos de alto contraste
par(mar = c(1, 1, 3, 1), bg = "white") # Aumenta el espacio negativo alrededor del gráfico

plot(red_reducida,
     layout = disposicion,
     vertex.size = tamanio_nodos * 0.5,                  # Escala el tamaño de los círculos
     vertex.color = "#E0E0E0",                           # Fondo de nodo en gris claro
     vertex.frame.color = "#333333",                     # Borde oscuro para contraste
     vertex.label = etiquetas,
     vertex.label.color = "#000000",                     # Tipografía negra para legibilidad
     vertex.label.cex = 0.8,                             # Tamaño de fuente
     vertex.label.family = "sans",                       # Tipografía limpia y sin serifas
     edge.color = adjustcolor("#999999", alpha.f = 0.3), # Líneas de conexión semitransparentes
     edge.width = 0.5,
     main = "Red de Coocurrencia Filtrada: Metodologías de Códigos Basura")


install.packages(c("ggraph", "tidygraph"))
library(ggraph)
library(tidygraph)

# Convertir la red a un formato compatible con el ecosistema tidy
grafo_zen <- as_tbl_graph(red_reducida) %>%
  activate(nodes) %>%
  mutate(grado = centrality_degree()) %>%
  # Identificar el punto de corte para el top 30 de términos más conectados
  mutate(umbral_top = sort(grado, decreasing = TRUE)[30]) %>%
  # Crear una columna de etiquetas que solo tenga texto para el top 30, el resto queda vacío (NA)
  mutate(etiqueta_limpia = ifelse(grado >= umbral_top, name, NA))

# Dibujar la red
set.seed(42)
ggraph(grafo_zen, layout = "fr") +
  # 1. Líneas de conexión (aristas) muy sutiles y transparentes
  geom_edge_link(color = "#E0E0E0", alpha = 0.4, width = 0.5) +
  
  # 2. Nodos (círculos) escalados por importancia
  geom_node_point(aes(size = grado), color = "#4A90E2", alpha = 0.7) +
  
  # 3. Etiquetas de texto con repulsión automática (¡La clave para evitar la superposición!)
  geom_node_text(aes(label = etiqueta_limpia), 
                 repel = TRUE,              # Evita que los textos choquen
                 max.overlaps = Inf,        # Fuerza a dibujar todas las etiquetas del top 30
                 size = 3.5,                # Tamaño de la fuente
                 color = "#333333",         # Gris muy oscuro para alto contraste
                 family = "sans",           # Tipografía limpia
                 bg.color = "white",        # Un sutil halo blanco alrededor de las letras
                 bg.r = 0.15) +
  
  # 4. Estética minimalista (elimina ejes, fondos y leyendas innecesarias)
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(10, 10, 10, 10))