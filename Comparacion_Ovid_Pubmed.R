# Instalar y cargar rentrez si no lo tienes
if (!requireNamespace("rentrez", quietly = TRUE)) install.packages("rentrez")
library(rentrez)

# 1. Definir los términos controlados (MeSH) adaptados a la sintaxis de PubMed
mesh_pob <- "(Vital Statistics[Mesh] OR Death Certificates[Mesh] OR Cause of Death[Mesh] OR Mortality[Mesh])"
mesh_metodos <- "(Models, Statistical[Mesh] OR Data Interpretation, Statistical[Mesh] OR Bayes Theorem[Mesh] OR Spatio-Temporal Analysis[Mesh] OR Medical Record Linkage[Mesh] OR Autopsy[Mesh] OR Patient Discharge[Mesh] OR Artificial Intelligence[Mesh] OR Decision Trees[Mesh] OR Data Mining[Mesh] OR Random Forest[Mesh] OR Life Tables[Mesh] OR Demography[Mesh] OR Algorithms[Mesh] OR Computer Simulation[Mesh])"

# 2. Bloques de texto libre extraídos de tu archivo estrategia_completa.txt (con etiqueta [tiab])
libre_pob <- '("vital statistics" OR "registration of vital statistics" OR "vital registration" OR "civil registration" OR CRVS OR "population register*" OR "death regist*" OR "death certificat*" OR "death record*" OR "cause of death" OR "causes of death" OR "death cause*" OR "underlying cause*" OR "death rate*" OR "population surveillance" OR "public health surveillance" OR "international classification of disease*" OR ICD-9 OR ICD-10 OR ICD-11 OR "clinical coding" OR "medical coding" OR "health information system*" OR mortality)[tiab]'

libre_basura <- '("garbage code*" OR "garbage cause*" OR "garbage categor*" OR "garbage diagnos*" OR "poorly defined cause*" OR "causas mal definidas" OR "codigos basura" OR "codigos lixo" OR "codigos garbage" OR R00-R99 OR ill-defined OR "unknown cause*" OR "unspecified cause*" OR "nonspecific cause*" OR "non-specific cause*" OR "unspecific cause*" OR "undetermined cause*" OR "vague code*" OR "cause of death assignment" OR "underlying cause assignment" OR misclassif* OR miscod* OR misattribut* OR miscertif*)[tiab]'

libre_metodos <- '("proportional redistribution" OR "proportional distribution" OR "fractional algorithm*" OR "Brass technique" OR "Brass technic" OR "reverse survival" OR "stable population" OR "demographic analys*" OR "life table*" OR "statistical model*" OR "probabilistic model*" OR bayesian OR "empirical bayes" OR spatiotemporal OR spatio-temporal OR "spatial regression" OR imput* OR CODEm OR CoDCorrect OR "cause of death ensemble model*" OR "coarsened exact matching" OR reallocat* OR reassign* OR recod* OR algorithm* OR "multiple cause*" OR linkage OR "verbal autops*" OR "hospital discharge" OR "discharge record*" OR "discharge data" OR "artificial intelligence" OR "machine learning" OR "decision tree*" OR "data mining" OR "random forest*" OR "gradient boosting" OR "deep learning" OR "neural network*" OR microsimulat* OR micro-simulation* OR agent-based OR "synthetic population*" OR "global burden of disease" OR GBD OR InterVA OR InSilicoVA OR SmartVA OR "tariff method" OR "bridge coding" OR "comparability ratio*" OR "IRIS software" OR "automated coding" OR "automated classification" OR "small area estimation" OR "correction factor*" OR redistribut* OR reclassif*)[tiab]'

# 3. Ensamblar la ecuación de búsqueda final replicando la estructura de Ovid
query_pubmed <- paste0("(", mesh_pob, " OR ", libre_pob, ") AND ", libre_basura, " AND (", mesh_metodos, " OR ", libre_metodos, ")")

# 4. Consultar a la API de PubMed (retmax = 0 devuelve solo el conteo de resultados)
cat("Consultando a PubMed, por favor espera...\n")
resultado_ejercicio <- entrez_search(db = "pubmed", term = query_pubmed, retmax = 0)

# 5. Imprimir los resultados
cat("--------------------------------------------------\n")
cat("Resultados originales en Ovid MEDLINE : 1952\n")
cat("Resultados encontrados en PubMed      :", resultado_ejercicio$count, "\n")
cat("--------------------------------------------------\n")