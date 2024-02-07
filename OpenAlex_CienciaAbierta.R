#                                     Minería de OpenAlex para Ciencia Abierta para AL+C
#                                                     13 OCTUBRE 2023
# Este ejemplo parte de la libreria OpenAlexR, revisada por Gabriela Sued y Carlos Brito para la consulta de datos en OpenAlexR:
#  En este caso nos interesa identificar la literatura sobre dos temas: 1. Ciencia Abierta y 2. Evaluación de la ciencia
# Colaboradores: Eduardo Robles Belmont, Carlos Brito y Gabriela Sued


#0. :::::::::::::::::::::::::::::::::::::Instalacion y ejecucion de Paqueterias::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#install.packages("tidyr")
#install.packages("openalexR")
#install.packages("openxlsx")
#install.packages("bibliometrix")

library(tidyr)
library(openalexR)
library(openxlsx)
library(bibliometrix)


#1.:::::::::::::::::::::::::::::::::::::::::::::::: EXTRACCION DE DATOS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Se define la variable "conceptos" para realizar la busqueda de estos
##Ejemplo con solo 5 conceptos
#conceptos <- c("c2778149293")

#Se define la variable "paises" para realizar la busqueda de estos
##Ejemplo para solo mexico
#paises <- "mx"
##Todos los paises de America latina
#paises <- c("mx","AG","AR","BS","BB","BZ","BR","CL","CO","CR","CU","DO","EC","SV","BO","GD","GT","GY","HT","HN","JM","MX","NI","PA","PY","PE","VE","DO","KN","VC","LC","SR","TT","UY")


#oa_fetches una funci?n compuesta que se utiliza para construir consultas, realizar solicitudes y convertir los resultados en un data frame.
#Esta funci?n se utiliza para interactuar con la API de OpenAlex y recuperar datos acad?micos y de investigaci?n.

query <- oa_fetch(
  #entity(Car?cter): Indica la entidad acad?mica o de investigaci?n de la b?squeda.
  #                  Puede ser uno de los siguientes: "works" (trabajos), "authors" (autores), "lugares" (lugares de publicaci?n), "institutions" (instituciones) o "concepts" (conceptos).
  entity = "works",
  
  #Se comentan las siguientes dos lines para no tener limites en la fecha de publicacion
  #from_publication_date = "2019-01-01",     #limite inferior de fecha de publicacion 
  #to_publication_date   = "2021-12-21",     #limite superior de fecha de publicacion
  
  institutions.country_code = c("MX","AG","AR","BS","BB","BZ","BR","CL","CO","CR","CU","DO","EC","SV","BO","GD","GT","GY","HT","HN","JM","MX","NI","PA","PY","PE","VE","DO","KN","VC","LC","SR","TT","UY"),
  concepts.id               = c("c2778149293"), #los conceptos usados son en temas de evaluación y ciencia abierta
  
  #Para hacer la búsqueda en títulos
  #title.search=c("open science"),
  #abstract.search=c("open science"),
  #title.search=c("research evaluation","indicator"),
  options = list(sort = "from_publication_date:desc"),  #Se especifica una opci?n adicional para ordenar los resultados en orden descendente (desc) seg?n la fecha de publicaci?n, de manera que los trabajos m?s recientes aparezcan primero
  
  #verbose(L?gico): Si es VERDADERO, imprime informaci?n sobre el proceso de consulta.
  verbose = TRUE)


#2. :::::::::::::::::::::::::::::::::::::TRANSFORMAR EL DF A FORMATO BILIOMETRIX:::::::::::::::::::::::::::::::::::::::::
#   La funcionoa2bibliometrixes se utiliza para convertir los resultados de la consulta con "oa_fetchen" 
#   a un formato compatible con bibliometrix. 

query_bibliometrix<-oa2bibliometrix(query)


#3.::::::::::::::::::: DESANIDAR TODAS LAS LISTAS Y VECTORES DENTRO DE "query_bibliometrix" ::::::::::::::::::::::::::::::::::::::
# (esto es necesario porque sino no se puede exportar el df a un archivo csv o xlsx, este es el que usa biblioshiny)

#3a. Listar todas las variables que son listas
es_lista <- sapply(query_bibliometrix, is.list)        #Se crea una lista con valores logicos de las columnas que son listas en "query_bibliometrix"
columnas_lista <- names(query_bibliometrix)[es_lista]  #Se crea una lista con los nombres de las columnas que son listas en "query_bibliometrix"
print(columnas_lista)                                  #Se visualizan el nombre de estas columnas 


#3b. Desanidar todas las variables que son listas con la funcion 'unnest' del paquete tidyr

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author, names_sep = "_")
## Al Desanidar "author" crean las siguientes columnas con listas que tambien debemos desanidar 
query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_au_id, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_au_orcid, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_au_affiliation_raw, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_institution_display_name, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_institution_lineage, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_author_position, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_institution_id, names_sep ="_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_institution_ror, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_institution_country_code, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_au_display_name, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(author_institution_type, names_sep = "_")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(concepts, names_sep = "_")
##Al Desanidar "concepts" crean las siguientes columnas con listas que tambien debemos desanidar 
query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(concepts_id, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(concepts_level, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(concepts_score, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(concepts_wikidata, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(concepts_display_name, names_sep = "_")
#:::::::::::::::::::::::::::::::::::::::::::::::::::

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(counts_by_year, names_sep = "_")
##Al Desanidar "counts_by_year" crean las siguientes columnas con listas que tambien debemos desanidar 
query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(counts_by_year_cited_by_count, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(counts_by_year_year, names_sep = "_")
#::::::::::::::::::::::::::::::::::::::::::::::::::::

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(ids, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(referenced_works, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%
  unnest_wider(related_works, names_sep = "_")

query_bibliometrix<-query_bibliometrix%>%                          # agregamos el argumento "names_repairde" para especificar una estrategia de reparaci?n de nombres ya que en caso contrario marcara error porque generara columnas con nombres duplicados
  unnest_wider(grants, names_sep = "_", names_repair = "unique")   # "unique" renombrara las columnas duplicadas agregando sufijos numericos para hacer nombres unicos


#3c. Se vuelve hacer la consulta de las columas que sean listas para verificar que no tengamos ninguna
es_lista <- sapply(query_bibliometrix, is.list)        
columnas_lista <- names(query_bibliometrix)[es_lista]  
print(columnas_lista)  


#4.:::::::::::::::::::::::::::::::::::::::::::EXPORTAR EL DF A UN .XLSX::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# En el parametro "file" poner toda la direccion donde se quiera guardar el archivo, agregando el nombre que se le desee
# poner y con terminacion .xlsx
write.xlsx(query_bibliometrix, file = "C:/Users/Eduardo Robles Belmo/Documents/Ciencia Abierta/OpenAlex/CienciaAbierta/Ciencia Abierta AL+C/CienciaAbierta_ALC_24enero24.xlsx")


#5.::::::::::::::::::::::::::::::::::::::::::::Utilizar BIBLIOMETRIX ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#  Una vez cargado el paquete, biblioshiny() abre la interfaz web donde se analizan los datos
biblioshiny() #abre una pagina web, vista 
# Ya dentro de biblioshiny en cargar datos se carga el archivo query3_bibliometrix.xlsx 
# y la aplicacion comienza el analisis














