

################SECUENCIA DE RECOLECCION Y PROCESAMIENTO DE  DATOS BIBLIOMETRICOS SOBRE LA PRODUCCION CIENTIFICA DE IA EN MÉXICO EMPLEANDO LA LIBRERIA OPENALEXR######################
###Autora: Gabriela Sued########

############SE EXTRAJERON LOS DATOS POR SUBCAMPO DE LA IA SEGUN LOS SIGUIENTES CODIGOS#################################

# QUERY3= "machine learning", "deep learning" #TODO EL CODIGO SE EJEMPLIFICA CON ESTA QUERY PARA EVITAR SCRIPTS LARGOS CON FUNCIONES EQUIVALENTES

# QUERY4= "natural language processing", "NLP", "speech processing"
# QUERY5= "computer vision", "digital image processing"
# QUERY6= "neural network", "evolutionary computing", "evolutive algorithms"
# QUERY7= "robotics, robots"
# QUERY8= "artificial intelligence"
###Las bases de datos  se encuentran disponibles en https://zenodo.org/records/10642462####################
#Las claves se aplicaron a títulos, resúmenes y conceptos
library(openalexR)
library(tidyr)
library(dplyr)
library (rio)
library(tibble)


# EJEMPLO CON MACHINE LEARNING & DEEP LEARNING

query3 <- oa_fetch(
  entity = "works",
  title.search = c("machine learning", "deep learning"),
  #from_publication_date = "2021-01-01",
  #to_publication_date = "2021-12-31",
  institutions.country_code="mx",
  concepts.id="c41008148",
  options = list(sort = "from_publication_date:desc"),
  #count_only = TRUE,
  verbose = TRUE)

query3_abstract <- oa_fetch(
  entity = "works",
  abstract.search = c("machine learning", "deep learning"),
  #from_publication_date = "2021-01-01",
  #to_publication_date = "2021-12-31",
  institutions.country_code="mx",
  concepts.id="c41008148",
  options = list(sort = "from_publication_date:desc"),
  #count_only = TRUE,
  verbose = TRUE)

query3_concepts <- oa_fetch(
  entity = "works",
 display_name.search = c("machine learning", "deep learning"),
  #from_publication_date = "2021-01-01",
  #to_publication_date = "2021-12-31",
  institutions.country_code="mx",
  concepts.id="c41008148",
  options = list(sort = "from_publication_date:desc"),
  #count_only = TRUE,
  verbose = TRUE)

# UNIR RECOLECCIONES
query3_abstract_concept_title<-rbind(query3_abstract, query3_concepts, query3_title)
#DEPURAR DUPLICADOS
query3_abstract_concept_title_sin_duplicados <- distinct(query3_abstract_concept_title)
#CONTAR REGISTROS
num_query_3 <- nrow(query3_abstract_concept_title_sin_duplicados)
head(num_query_3)

######################### CONTROL Y REDISTRIBUCION DE REGISTROS REPETIDOS EN MÁS DE UN DF#####################

library(dplyr)
library(tidyr)
library(tibble)
library(rio)
relevancia_query34<-query3_abstract_concept_title_sin_duplicados_1%>%
    inner_join(query4_abstract_concept_title_sin_duplicados_1, by="doi")
relevancia_query34<-relevancia_query3%>%
  select(display_name.x, ab.x,doi, concepts.y)
relevancia_query34<-relevancia_query34%>%
  unnest_wider(concepts.y)
view(relevancia_query34)
export(relevancia_query34,"query34.csv")

relevancia_query35<-query3_abstract_concept_title_sin_duplicados%>%
  inner_join(query5_abstract_concept_title_sin_duplicados, by="doi")
relevancia_query35<-relevancia_query3%>%
  select(display_name.x, ab.x,doi, concepts.y)
relevancia_query35<-relevancia_query3%>%
  unnest_wider(concepts.y)
view(relevancia_query35)
#No hay duplicados entre q3 y q5

relevancia_query36<-query3_abstract_concept_title_sin_duplicados%>%
  inner_join(query6_abstract_concept_title_sin_duplicados, by="doi")
relevancia_query36<-relevancia_query36%>%
  select(display_name.x, ab.x,doi, concepts.y)
relevancia_query36<-relevancia_query36%>%
  unnest_wider(concepts.y)
view(relevancia_query36)

relevancia_query37<-query3_abstract_concept_title_sin_duplicados%>%
  inner_join(query7_abstract_concept_title_sin_duplicados, by="doi")
relevancia_query37<-relevancia_query37%>%
  select(display_name.x, ab.x,doi, concepts.y)
relevancia_query37<-relevancia_query37%>%
  unnest_wider(concepts.y)
view(relevancia_query37)

relevancia_query38<-query3_abstract_concept_title_sin_duplicados%>%
  inner_join(query8_abstract_concept_title_sin_duplicados, by="doi")
relevancia_query37<-relevancia_query38%>%
  select(display_name.x, ab.x,doi, concepts.y)
relevancia_query38<-relevancia_query37%>%
  unnest_wider(concepts.y)
view(relevancia_query38)

#Redistribuir registros duplicados según la relevancia que asigna OA a los conceptos que describen a cada artículo.
library(rio)
#Registros distribuidos entre q3 y q6
export(relevancia_query36, "query36.csv")
query3_abstract_concept_title_sin_duplicados_1<-query3_abstract_concept_title_sin_duplicados%>%
  anti_join(doi_quitar_3)
query6_abstract_concept_title_sin_duplicados_1<-query6_abstract_concept_title_sin_duplicados%>%
  anti_join(doi_quitar_6)
#Registros distribuidos entre q3 y q4
export(relevancia_query34, "query34.csv")
query3_abstract_concept_title_sin_duplicados_1<-query3_abstract_concept_title_sin_duplicados_1%>%
  anti_join(quitar_doi_34)
query4_abstract_concept_title_sin_duplicados_1<-query4_abstract_concept_title_sin_duplicados_1%>%
  anti_join(quitar_doi_43)
#Registros distribuidos entre q3 y q5
relevancia_query35<-query3_abstract_concept_title_sin_duplicados_1%>%
  inner_join(query5_abstract_concept_title_sin_duplicados, by="id")
relevancia_query35<-relevancia_query35%>%
  select(display_name.x, ab.x,doi.x, concepts.y)
relevancia_query35<-relevancia_query35%>%
  unnest_wider(concepts.y)
view(relevancia_query35)
#No parece haber coincidencias

#Registros distribuidos entre q3 y q7

relevancia_query37<-query3_abstract_concept_title_sin_duplicados_1%>%
  inner_join(query7_abstract_concept_title_sin_duplicados, by="doi")
relevancia_query37<-relevancia_query37%>%
  select(display_name.x, ab.x,doi, concepts.y)
relevancia_query37<-relevancia_query37%>%
  unnest_wider(concepts.y)
view(relevancia_query37)
export(relevancia_query37, "query_37.csv")

query3_abstract_concept_title_sin_duplicados_1<-query3_abstract_concept_title_sin_duplicados_1%>%
  anti_join(quitar_doi_37)

query7_abstract_concept_title_sin_duplicados_1<-query7_abstract_concept_title_sin_duplicados%>%
  anti_join(quitar_doi_73)

################CONVERSION DE DFS A FORMATO BIBLIOMETRIX (PARA USAR CON BIBLIOSHINY)##################
#Conversion dfs de subcampos a formato bibliometrix
library(bibliometrix)
query3_bibliometrix<-oa2bibliometrix(query3_abstract_concept_title)

################

####Desanidar las listas de los df en formato bibliometrix (Revisado por Carlos Brito)

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author, names_sep = "_")
## Al Desanidar "author" crean las siguientes columnas con listas que tambien debemos desanidar 
query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_au_id, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_au_orcid, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_au_affiliation_raw, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_institution_display_name, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_institution_lineage, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_author_position, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_institution_id, names_sep ="_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_institution_ror, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_institution_country_code, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_au_display_name, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(author_institution_type, names_sep = "_")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(concepts, names_sep = "_")
##Al Desanidar "concepts" crean las siguientes columnas con listas que tambien debemos desanidar 
query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(concepts_id, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(concepts_level, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(concepts_score, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(concepts_wikidata, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(concepts_display_name, names_sep = "_")
#:::::::::::::::::::::::::::::::::::::::::::::::::::

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(counts_by_year, names_sep = "_")
##Al Desanidar "counts_by_year" crean las siguientes columnas con listas que tambien debemos desanidar 
query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(counts_by_year_cited_by_count, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(counts_by_year_year, names_sep = "_")
#::::::::::::::::::::::::::::::::::::::::::::::::::::

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(ids, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(referenced_works, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%
  unnest_wider(related_works, names_sep = "_")

query3_bibliometrix<-query3_bibliometrix%>%                          # agregamos el argumento "names_repairde" para especificar una estrategia de reparación de nombres ya que en caso contrario marcara error porque generara columnas con nombres duplicados
  unnest_wider(grants, names_sep = "_", names_repair = "unique")   # "unique" renombrara las columnas duplicadas agregando sufijos numericos para hacer nombres unicos


#3c. Se vuelve hacer la consulta de las columas que sean listas para verificar que no tengamos ninguna
es_lista <- sapply(query3_bibliometrix, is.list)        
columnas_lista <- names(query3_bibliometrix)[es_lista]  
print(columnas_lista)

######A PARTIR DE AQUÍ SE DETALLAN LAS CONSULTAS USADAS EN EL ARTÍCULO. SIEMPRE SE EJEMPLIFICAN CON QUERY3="machine learning, deep learning"######

############CONSULTAS DE AUTORES, PAÍSES DE INSTITUCIONES, AUTORES POR UNIVERSIDAD, TIPO DE FUENTES #######################

#Recuento de autores para cada subcampo

library(openalexR)
library(tidyr)
library(openxlsx)
library(dplyr)
library(tibble)
library(ggplot2)
library(RColorBrewer)
library(tidytext)

#Seleccionar la variable AU 
authors_query3<-query3_bibliometrix%>%
  select(AU)
#Tokenizar los autores
autores_query3 <- authors_query3%>%
  unnest_tokens(AU,AU,token = stringr::str_split, pattern =";")
autores_query3<-data.frame(autores_query3)
autores_query3_freq<-autores_query3%>%
  count(AU, sort = TRUE)
  view(autores_query3_freq)

#Seleccionar la variable AU_CO
countries_query3<-query3_bibliometrix%>%
  select(AU_CO)
#Tokenizar los paises
paises_query3 <- countries_query3%>%
  unnest_tokens(AU_CO,AU_CO,token = stringr::str_split, pattern =";")
paises_query3<-data.frame(paises_query3)
paises_query3_freq<-paises_query3%>%
  count(AU_CO, sort = TRUE)
sum_paises_query3_freq<-sum(paises_query3_freq$n)
view(sum_paises_query3_freq)
view(paises_query3_freq)

#Seleccionar la variable AU_UN
universities_query3<-query3_bibliometrix%>%
  select(AU_UN)
#Tokenizar las universidades
universidades_query3 <- universities_query3%>%
  unnest_tokens(AU_UN,AU_UN,token = stringr::str_split, pattern =";")
universidades_query3<-data.frame(universidades_query3)
universidades_query3_freq<-universidades_query3%>%
  count(AU_UN, sort = TRUE)
view(universidades_query3_freq)

#Tipo de fuentes (artículos, capítulos, libros, disertaciones, etc)
sources_query3<-query3_abstract_concept_title_sin_duplicados%>%
select(type)
sources_query3_freq<-sources_query3%>%
  count(type, sort = TRUE)
view(sources_query3_freq)

##################CONSULTA DE PRIMEROS AUTORES MEXICANOS E INTERNACIONALES##################

library(tidyr)
library(dplyr)
library(tibble)

query3_autores_by_year<-query3_abstract_concept_title%>%
  select(author, publication_year)
query3_autores_by_year<-query3_autores_by_year%>%
  unnest_wider(author, names_sep = "_")
view(query3_autores_by_year)

query3_aut_by_year<-query3_autores_by_year%>%
  select(author_au_display_name, publication_year)

query3_paises_by_year<-query3_autores_by_year%>%
  select(author_institution_country_code,publication_year)
view(query3_paises_by_year)
query3_aut_by_year<-query3_aut_by_year%>%
  separate_rows(author_au_display_name, sep = ",")
view(query3_aut_by_year)
query3_paises_by_year<-query3_paises_by_year%>%
  separate_rows(author_institution_country_code, sep=";")
query3_autores_paises_by_year<-cbind(query3_aut_by_year, query3_paises_by_year)
view(query3_autores_paises_by_year)
#### 1 autor por fila
query3_unique_authors <- query3_autores_paises_by_year %>%
  select(author_au_display_name, author_institution_country_code) %>%
  distinct(author_au_display_name, .keep_all = TRUE)

query3_unique_authors_MX <- query3_unique_authors %>%
  count (author_institution_country_code=="MX")


##############################CANTIDAD DE CITAS POR TRABAJOS DE AUTORES MEXICANOS###############################
library(dplyr)
library(tidyr)
library(tibble)
library(rio)
#Listar citas por autor
query3_autores_citas<-query3_abstract_concept_title%>%
  select(author, cited_by_count)
query3_autores_citas<- query3_autores_citas%>%
  unnest_wider(author, names_sep = "_")
query3_autores_citas<-query3_autores_citas%>%
  select(author_au_display_name, cited_by_count)
query3_autores_citas<-query3_autores_citas%>%
  separate_rows(author_au_display_name, sep = ",")

#Listar paises
query3_paises<-query3_autores%>%
  select(author_institution_country_code)
query3_paises<-query3_paises%>%
  separate_rows(author_institution_country_code, sep=";")
#Juntar autores y citas con paises
query3_autores_citas_pais<-cbind(query3_autores_citas, query3_paises)
query3_autores_citas<-query3_autores_citas_pais%>%
  filter(author_institution_country_code=="MX")

query3_autores_citas<-query3_autores_citas%>%
  group_by(author_au_display_name)%>%
  summarise(total_citations = sum(cited_by_count, na.rm = TRUE))
#Juntar autores, nro publicaciones y citas

query3_autores_citas_join<-query3_autores_trabajos_count%>%
  inner_join(query3_autores_citas, by="author_au_display_name")
export(query3_autores_citas_join, "query3_autores_citas.csv")
view(query3_autores_citas_join)


query3_autores_trabajos_count<-query3_autores_citas_join%>%
  count(author_au_display_name, sort=TRUE)
view(query3_autores_trabajos_count)

#####################CANTIDAD DE AUTORES POR CANTIDAD DE PUBLICACIONES (EXPERTOS E INICIALES)#####################

query3_autores_trabajos_count <- query3_autores_citas_join%>%
  mutate(publication_level = cut(n,
                                 breaks = c(0, 9, 19, 30, Inf),
                                 #labels = c('0 a 9', '10 a 19, '20 a 30', 'más de 1000'),
                                 right = FALSE))

view(query3_autores_trabajos_count)

query3_count_by_level <- query3_autores_trabajos_count %>%
  group_by(publication_level) %>%
  summarise(count = n())%>%
  mutate(Subcampo="Machine Learning, Deep Learning")

view(query3_count_by_level)



########################## DISTRIBUCION ENTRE AUTORES MEXICANOS E INTERNACIONALES#####################################33

query3_autores_paises_total_by_year <- query3_autores_paises_by_year %>%
  group_by(author_au_display_name) %>%
  summarize(count = n()) %>%
  left_join(query3_autores_paises_by_year %>% select(author_au_display_name, author_institution_country_code, publication_year) %>% distinct(), by = "author_au_display_name")%>%
  arrange(desc(count))
view(query3_autores_paises_total_by_year)

# Definiendo la función
filtrar_autores_by_year <- function(query3_autores_paises_total_by_year) {
  # Primero filtramos para obtener solo las filas donde 'author_institution_country_code' es 'MX'
  autores_mx <-query3_autores_paises_total_by_year %>% filter(author_institution_country_code == "MX")
  
  # Luego filtramos el dataframe original para excluir aquellos que ya fueron seleccionados como 'MX'
  autores_no_mx <- query3_autores_paises_total_by_year%>% 
    filter(!(author_au_display_name %in%  autores_mx$author_au_display_name))
           autores_filtrados <- bind_rows(autores_mx, autores_no_mx) %>%
             group_by(author_au_display_name) %>%
             slice(1) # seleccionamos la primera aparición de cada autor
           
           return(autores_filtrados)
}

# Aplicando la función al dataframe
query3_filtrado <- filtrar_autores_by_year(query3_autores_paises_total_by_year)
view(query3_filtrado)

library(dplyr)
autores_mx <-query3_autores_paises_total_by_year %>% filter(author_institution_country_code == "MX")
autores_no_mx <- query3_filtrado%>% 
  filter(!author_institution_country_code=="MX")

query3_sumarize <- autores_no_mx %>%
  group_by(author_institution_country_code, publication_year) %>%
  summarize(total_count = sum(count, na.rm = TRUE))
              
              
 ### DF PARA EL GRAFICO DE DISPERSION 
  query3_sumarize_dispersion<-query3_sumarize%>%
  filter(publication_year>"2011")
  query3_sumarize_dispersion<-query3_sumarize_dispersion%>%
    filter(total_count>30)
  query3_sumarize_dispersion<-query3_sumarize_dispersion%>%
    mutate(subcampo= "Machine Learning")
  view(query3_sumarize_dispersion)
  query3_sumarize_dispersion<-query3_sumarize_dispersion%>%
    filter(publication_year<"2023")
  view(query3_sumarize_dispersion)

 #########################3#PUBLICACIONES CON PRIMEROS AUTORES MEXICANOS########################################
  library(dplyr)
  query3_pais_primer_autor_año<-query3_bibliometrix%>%
    select(author_institution_country_code_1, publication_date, Origen)%>%
    filter(author_institution_country_code_1=="MX")
  view(query3_pais_primer_autor_año)
  
  query3_grafico_autor<-query3_pais_primer_autor_año%>%
    reframe(author_institution_country_code_1)%>%
    group_by(year)

###############################MAPA DE COLABORACIONES (FIGURA 4)####################################################3
library(tibble)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rworldmap)
paises_query3_freq<-paises_query3_freq%>%
  mutate(Subárea="Machine Learning, Deep Learning")
paises_query4_freq<-paises_query4_freq%>%
  mutate(Subárea="Natural Language Processing, Speech Processing")
paises_query5_freq<-paises_query5_freq%>%
  mutate(Subárea="Computer Vision, Digital Image Processing")
paises_query6_freq<-paises_query6_freq%>%
  mutate(Subárea="Neural Networks, Genetic Algorithms, Evolutionary Computing")
paises_query7_freq<-paises_query7_freq%>%
  mutate(Subárea="Robots and Robotics")
paises_query8_freq<-paises_query8_freq%>%
  mutate(Subárea="Artificial Intelligence")

colaboraciones<-rbind(paises_query3_freq,paises_query4_freq,paises_query5_freq,paises_query6_freq, paises_query7_freq,paises_query8_freq)
print(colaboraciones)
colaboraciones<-colaboraciones%>%
  filter(!AU_CO=="mexico")
colaboraciones<-colaboraciones%>%
  filter(!AU_CO=="na")


colaboraciones_totales<-colaboraciones%>%
  group_by(AU_CO)%>%
  summarise(Total_Autores=sum(n))
view(colaboraciones_totales)


#################graficos##################
autores_por_pais<-colaboraciones_totales

library(ggplot2)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)

# Paso 1: Sumar autores por país

# Convertir nombres de países a códigos ISO3
autores_por_pais$iso3 <- countrycode(autores_por_pais$AU_CO, "country.name", "iso3c")

# Obtener los datos de los países con rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Unir los datos de autores con los datos espaciales de los países
world_data <- world %>% 
  left_join(autores_por_pais, by = c("iso_a3" = "iso3"))

# Generar el mapa de calor
mapa_calor <- ggplot(data = world_data) +
  geom_sf(aes(fill = Total_Autores), color = "gray") +
  scale_fill_gradientn(colors = brewer.pal(9, "Greys"), na.value = "white") +
  #scale_fill_gradient(low = "yellow", high = "red", na.value = "white") +
  labs(fill = "Total de autores") +
  theme_minimal()+
  theme(legend.position = "bottom",  # Posición de la leyenda
        legend.box = "horizontal",   # Orientación horizontal de los elementos de la leyenda
        legend.title = element_text(size = 10),  # Ajustes del texto del título de la leyenda
        legend.text = element_text(size = 8))
  
ggsave("figura4_byn.jpg", mapa_calor, height =altura_cm, width =ancho_cm, dpi=300, units = "cm")
# Imprimir el mapa de calor
print(mapa_calor)

########################### CONSULTA DE ORGANISMOS FINANCIADORES (COLABORACION DE JOSÉ GERARDO LÓPEZ)#################################################
library(httr)
library(jsonlite)
library(dplyr)
library(openalexR)
library(tibble)
library(rio)

pedir <- function(url) {
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    p <- data$group_by
    m <- data$meta
  } else {
    print(paste('La solicitud a la API falló. Código de estado:', status_code(response)))
  }
  
  return(list(p, m))
}

#resultados_grant<- pedir("https://api.openalex.org/works?filter=concepts.wikidata:https://www.wikidata.org/wiki/Q2539,authorships.institutions.country_code:mx&group-by=grants.funder")
#resultados_grant_ML <- pedir("https://api.openalex.org/works?filter=abstract.search:machine%20learning%20OR%20deep%20learning,authorships.institutions.country_code:mx&group-by=grants.funder")
#resultados_grant_ROB <- pedir("https://api.openalex.org/works?filter=abstract.search:robots%20OR%20Robotics,authorships.institutions.country_code:mx&group-by=grants.funder")
resultados_grant_NLP <- pedir("https://api.openalex.org/works?filter=abstract.search:natural%20language%20Processing%20OR%20Speech%20Processing,authorships.institutions.country_code:mx&group-by=grants.funder")

df_p <- resultados_grant_ML[[1]]
df_m <- resultados_grant_ML[[2]]
export(df_p, "grants_NLP.csv")
# Visualizar los data frames
View(df_p)
head(df_m)


####################POSICION DE MÉXICO EN LA ESCALA MUNDIAL. TRABAJOS PUBLICADOS CON UN AUTOR MEXICANO POR LO MENOS (COLABORACION DE JOSÉ GERARDO LÓPEZ)##############################

library(httr)
library(jsonlite)
library(dplyr)
library(openalexR)
library(tibble)
library(rio)
pedir <- function(url) {
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    p <- data$group_by
    m <- data$meta
  } else {
    print(paste('La solicitud a la API falló. Código de estado:', status_code(response)))
  }
  
  return(list(p, m))
}

#resultados_meta <- pedir("https://api.openalex.org/works?filter=concepts.wikidata:https://www.wikidata.org/wiki/Q2539&group-by=authorships.institutions.country_code")

#df_p <- resultados_meta[[1]]
#df_m <- resultados_meta[[2]]

# Visualizar los data frames
View(df_p)
head(df_m)
#https://api.openalex.org/works?filter=abstract.search:artificial%20intelligence

###################DESARROLLO DE CONCEPTOS A LO LARGO DE LOS AÑOS######################################3

library(tidyr)

query3_año_concepto<-query3_abstract_concept_title%>%
  select (publication_year, concepts)
query3_año_concepto<-query3_año_concepto%>%
  unnest_wider(concepts, names_sep = "_")
  query3_año_concepto<-query3_año_concepto%>%
  select(publication_year,concepts_display_name)
  query3_año_concepto$concepts_display_name <- sapply(query3_año_concepto$concepts_display_name, function(x) paste(x, collapse = ", "))
  

view(query3_año_concepto)
# Separar la columna 'concept_display_name' en tres nuevas columnas

query3_año_concepto_separado <- separate(query3_año_concepto, col = concepts_display_name, 
                                into = c("concepto_1", "concepto_2", "concepto_3", "concepto_4", "concepto_5"), 
                                sep = ",", extra = "drop", fill = "right")

# 'extra = "drop"' descarta cualquier valor después del tercero
# 'fill = "right"' asegura que si hay menos de 3 valores, las columnas restantes se llenen con NA
query3_año_concepto$concepts_display_name <- sapply(query3_año_concepto$concepts_display_name, function(x) paste(x, collapse = ", "))
query3_año_concepto_separado<-as.data.frame(query3_año_concepto_separado)
view(query3_año_concepto_separado)
  
################################FRECUENCIA DE PALABRAS EN LOS TITULOS DE LAS PUBLICACIONES####################################
####SE REALIZA UN ANALISIS DE FRECUENCIA DE PALABRAS EN LOS TÍTULOS Y SE CONECTAN LAS PALABRAS A TRAVÉS DEL ID DE CADA ARTICULO###
####SE MODELA LA RED CON GEPHI#########
library(dplyr)
library(tidytext)
library(tibble)
library(tidyr)

query3_title<-query3_abstract_concept_title_x%>%
  select(display_name, publication_year,Origen)
query4_title<-query4_abstract_concept_title%>%
  select(display_name, publication_year,Origen)
query5_title<-query5_abstract_concept_title%>%
  select(display_name, publication_year,Origen)
query6_title<-query6_abstract_concept_title%>%
  select(display_name, publication_year,Origen)
query7_title<-query7_abstract_concept_title%>%
  select(display_name, publication_year,Origen)
query8_title<-query8_abstract_concept_title%>%
  select(display_name, publication_year,Origen)
titles<-rbind(query3_title, query4_title, query5_title, query6_title, query7_title, query8_title)
titles <- titles %>%
  mutate(article_id = row_number())


titles_unnest<-titles%>%
  unnest_tokens(word, display_name)
view(titles_unnest)
data("stop_words")
titles_unnest<-titles_unnest%>%
  anti_join(stop_words, by="word")
titles_unnest <- titles_unnest %>%
  add_count(word, name = "Frecuencia") %>%
  arrange(desc(Frecuencia))
view(titles_unnest)
library(rio)
export(titles_unnest, "titles.csv")
}
################################GRÁFICO DE CUADRANTES##########################################3
#SE RETOMA LA TABLA TITLES.CSV, SE MIDE CENTRALIDAD Y DENSIDAD CON GEPHI, SE GENERA LA TABLA CUADRANTES, SE IMPORTA A RSTUDIO Y SE PRODUCE UN GRÁFICO DE CUADRANTES#

library(ggplot2)


library(ggplot2)
library(ggrepel)


plot<-ggplot(cuadrantes, aes(x = Centralidad, y = Densidad)) +
  geom_text_repel(
    aes(label = Label),
    max.overlaps = Inf # Esto permite superposiciones infinitas
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10)) + # Breaks cada 0.10
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10)) + # Breaks cada 0.10
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  xlab("Centralidad") +
  ylab("Densidad") +
  ggtitle("Gráfico de Cuadrantes") +
  theme_minimal() +
  theme(legend.position = "none")
plot
plot
library(ggplot2)
library(ggrepel)


plot<-ggplot(cuadrantes, aes(x = Centralidad, y = Densidad)) +
  geom_text_repel(
    aes(label = Label),
    max.overlaps = Inf,
    segment.color = NA  # Esto hace que las líneas sean invisibles
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10)) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  annotate("text", x = c(0.25, 0.25, 0.75, 0.75), y = c(0.75, 0.25, 0.75, 0.25), 
           label = c("Básicos", "Emergentes", "Motores", "Nichos"), 
           size = 5, fontface = "bold", color="grey", hjust = 0.1) +
  xlab("Centralidad") +
  ylab("Densidad") +
  ggtitle("Gráfico de Cuadrantes") +
  theme_minimal() +
  theme(legend.position = "none")

plot
