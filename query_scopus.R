# 1. Instalar y cargar el paquete rscopus (si no lo tienes)
if (!requireNamespace("rscopus", quietly = TRUE)) install.packages("rscopus")
library(rscopus)

# 2. Configurar tu API Key de Elsevier
set_api_key("7e1b065695590adc579a9572d57d2ba5")

# 3. Definir la cadena de búsqueda exacta (TITLE-ABS-KEY)
query_scopus <- 'TITLE-ABS-KEY ( "vital statistics" OR "registration of vital statistics" OR "vital registration" OR "civil registration" OR CRVS OR "population register*" OR "death regist*" OR "death certificat*" OR "death record*" OR "cause of death" OR "causes of death" OR "death cause*" OR "underlying cause*" OR "death rate*" OR "population surveillance" OR "public health surveillance" OR "international classification of disease*" OR ICD-9 OR ICD-10 OR ICD-11 OR "clinical coding" OR "medical coding" OR "health information system*" OR mortality ) AND TITLE-ABS-KEY ( "garbage code*" OR "garbage cause*" OR "garbage categor*" OR "garbage diagnos*" OR "poorly defined cause*" OR "causas mal definidas" OR "codigos basura" OR "codigos lixo" OR "codigos garbage" OR R00-R99 OR ill-defined OR "unknown cause*" OR "unspecified cause*" OR "nonspecific cause*" OR "non-specific cause*" OR "unspecific cause*" OR "undetermined cause*" OR "vague code*" OR "cause of death assignment" OR "underlying cause assignment" OR misclassif* OR miscod* OR misattribut* OR miscertif* ) AND TITLE-ABS-KEY ( "statistical model*" OR "statistical data interpretation" OR "Bayes theorem" OR "spatiotemporal analysis" OR "medical record linkage" OR autopsy OR "patient discharge" OR "artificial intelligence" OR "decision tree*" OR "data mining" OR "random forest*" OR "life table*" OR demography OR algorithm* OR "computer simulation" OR "proportional redistribution" OR "proportional distribution" OR "fractional algorithm*" OR "Brass technique" OR "Brass technic" OR "reverse survival" OR "stable population" OR "demographic analys*" OR "probabilistic model*" OR bayesian OR "empirical bayes" OR spatiotemporal OR spatio-temporal OR "spatial regression" OR imput* OR CODEm OR CoDCorrect OR "cause of death ensemble model*" OR "coarsened exact matching" OR reallocat* OR reassign* OR recod* OR "multiple cause*" OR linkage OR "verbal autops*" OR "hospital discharge" OR "discharge record*" OR "discharge data" OR "machine learning" OR "gradient boosting" OR "deep learning" OR "neural network*" OR microsimulat* OR micro-simulation* OR agent-based OR "synthetic population*" OR "global burden of disease" OR GBD OR InterVA OR InSilicoVA OR SmartVA OR "tariff method" OR "bridge coding" OR "comparability ratio*" OR "IRIS software" OR "automated coding" OR "automated classification" OR "small area estimation" OR "correction factor*" OR redistribut* OR reclassif* )'

# 4. Consultar a la API de Scopus
# count = 1 extrae solo el conteo inicial y los metadatos básicos del primer resultado para que veas el total
cat("Consultando a Scopus, por favor espera...\n")
resultado_scopus <- scopus_search(query = query_scopus, count = 1)

# 5. Imprimir el total de documentos encontrados
cat("--------------------------------------------------\n")
cat("Resultados encontrados en Scopus:", resultado_scopus$total_results, "\n")
cat("--------------------------------------------------\n")


scopus_search(query = 'TITLE-ABS-KEY("garbage code" AND mortality)', count = 1)
