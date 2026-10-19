# Instalar y cargar el paquete si es necesario
if (!requireNamespace("rentrez", quietly = TRUE)) install.packages("rentrez")
library(rentrez)

# 1. Definir la cadena de búsqueda exacta del Apéndice I
query_pubmed <- paste0(
  "((Vital Statistics[Mesh] OR Death Certificates[Mesh] OR Cause of Death[Mesh] OR Mortality[Mesh]) OR ",
  '("vital statistics" OR "registration of vital statistics" OR "vital registration" OR "civil registration" OR CRVS OR "population register*" OR "death regist*" OR "death certificat*" OR "death record*" OR "cause of death" OR "causes of death" OR "death cause*" OR "underlying cause*" OR "death rate*" OR "population surveillance" OR "public health surveillance" OR "international classification of disease*" OR ICD-9 OR ICD-10 OR ICD-11 OR "clinical coding" OR "medical coding" OR "health information system*" OR mortality)[tiab])',
  " AND ",
  '("garbage code*" OR "garbage cause*" OR "garbage categor*" OR "garbage diagnos*" OR "poorly defined cause*" OR "causas mal definidas" OR "codigos basura" OR "codigos lixo" OR "codigos garbage" OR R00-R99 OR ill-defined OR "unknown cause*" OR "unspecified cause*" OR "nonspecific cause*" OR "non-specific cause*" OR "unspecific cause*" OR "undetermined cause*" OR "vague code*" OR "cause of death assignment" OR "underlying cause assignment" OR misclassif* OR miscod* OR misattribut* OR miscertif*)[tiab]',
  " AND ",
  "((Models, Statistical[Mesh] OR Data Interpretation, Statistical[Mesh] OR Bayes Theorem[Mesh] OR Spatio-Temporal Analysis[Mesh] OR Medical Record Linkage[Mesh] OR Autopsy[Mesh] OR Patient Discharge[Mesh] OR Artificial Intelligence[Mesh] OR Decision Trees[Mesh] OR Data Mining[Mesh] OR Random Forest[Mesh] OR Life Tables[Mesh] OR Demography[Mesh] OR Algorithms[Mesh] OR Computer Simulation[Mesh]) OR ",
  '("proportional redistribution" OR "proportional distribution" OR "fractional algorithm*" OR "Brass technique" OR "Brass technic" OR "reverse survival" OR "stable population" OR "demographic analys*" OR "life table*" OR "statistical model*" OR "probabilistic model*" OR bayesian OR "empirical bayes" OR spatiotemporal OR spatio-temporal OR "spatial regression" OR imput* OR CODEm OR CoDCorrect OR "cause of death ensemble model*" OR "coarsened exact matching" OR reallocat* OR reassign* OR recod* OR algorithm* OR "multiple cause*" OR linkage OR "verbal autops*" OR "hospital discharge" OR "discharge record*" OR "discharge data" OR "artificial intelligence" OR "machine learning" OR "decision tree*" OR "data mining" OR "random forest*" OR "gradient boosting" OR "deep learning" OR "neural network*" OR microsimulat* OR micro-simulation* OR agent-based OR "synthetic population*" OR "global burden of disease" OR GBD OR InterVA OR InSilicoVA OR SmartVA OR "tariff method" OR "bridge coding" OR "comparability ratio*" OR "IRIS software" OR "automated coding" OR "automated classification" OR "small area estimation" OR "correction factor*" OR redistribut* OR reclassif*)[tiab])'
)

# 2. Consultar a la API de PubMed
cat("Consultando a PubMed, por favor espera...\n")
resultado_prueba <- entrez_search(db = "pubmed", term = query_pubmed, retmax = 0)

# 3. Mostrar el resultado en consola
cat("--------------------------------------------------\n")
cat("Resultados encontrados en PubMed:", resultado_prueba$count, "\n")
cat("--------------------------------------------------\n")


library(rentrez)

# 1. Asegúrate de tener cargada tu variable query_pubmed con la ecuación completa
# (Es la misma que usamos en el paso anterior)

# 2. Ejecutar la búsqueda activando el historial (use_history = TRUE)
# Esto guarda tu búsqueda en los servidores del NCBI temporalmente para no saturar tu RAM
cat("Iniciando búsqueda en PubMed...\n")
busqueda_pubmed <- entrez_search(db = "pubmed", term = query_pubmed, use_history = TRUE)
total_registros <- busqueda_pubmed$count

# 3. Preparar el archivo de texto donde se guardarán (se creará en tu carpeta de trabajo)
nombre_archivo <- "resultados_pubmed.nbib"
if (file.exists(nombre_archivo)) file.remove(nombre_archivo) # Borra si ya existía uno viejo
file.create(nombre_archivo)

# 4. Bucle para descargar por lotes de 500
tamanio_lote <- 500
cat("Se encontraron", total_registros, "artículos. Iniciando descarga por lotes...\n")

for (inicio in seq(0, total_registros, by = tamanio_lote)) {
  cat("Descargando registros desde el", inicio + 1, "hasta el", min(inicio + tamanio_lote, total_registros), "...\n")
  
  # Descarga el lote en formato MEDLINE (texto estructurado similar al RIS)
  lote <- entrez_fetch(db = "pubmed", 
                       web_history = busqueda_pubmed$web_history, 
                       rettype = "medline", 
                       retmode = "text", 
                       retstart = inicio, 
                       retmax = tamanio_lote)
  
  # Añadir el lote descargado al archivo
  cat(lote, file = nombre_archivo, append = TRUE)
  
  # Pausa de 1 segundo para ser "educados" con el servidor y evitar bloqueos (Error 429)
  Sys.sleep(1)
}

cat("--------------------------------------------------\n")
cat("¡Descarga completada con éxito!\n")
cat("Tus", total_registros, "citas se guardaron en el archivo:", nombre_archivo, "\n")
cat("Ruta de guardado:", getwd(), "\n")
cat("--------------------------------------------------\n")