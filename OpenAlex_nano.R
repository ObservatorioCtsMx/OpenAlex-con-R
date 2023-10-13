#                                        TRABAJO AGREGADO POR CARLOS BRITO MEJIA 
#                                                     12 OCTUBRE 2023
# Se reviso el codigo ejemplo de GabrielaSued para la consulta de datos en OpenAlexR:
#  corrigiendo errores al desanidar columnas con listas en ellas
#  se modifico dicho codico para que la consulta de datos fuera sobre conceptos relacionados a nano 
#  se agrego una extensa lista de comentarios para una mayor comprension del codigo 


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
conceptos <- c("c171250208","c512720949","c186187911","c155672457","c2780257685")
##Todos los conceptos dados
#concepttos <- c("c5752710","c91129048","c2778402822","c74214498","c155672457","c492672","c8078014","c51967427","c277600612","c27778078","c126201875","c159951928","c141795571","c278057685","c4185801","c14140026","c17585410","c8562591","c9288079","c18028147","c29097476","c1861740","c45206210","c77066764","c186187911","c140676511","c18791181","c48940184","c27289702","c4612889","c51720949","c6644492","c175616097","c2910849864","c2150","c2547985","c149986","c76110504","c190818770","c14676847","c20499865","c2777968448","c277888944","c278056986","c4508100","c64564810","c17767955","c2776540687","c2777715892","c1687964","c1508742","c26926545","c197981","c8242429","c8702908","c129275984","c148402106","c2990090","c127445965","c2777486477","c21946209","c18150654","c9708629","c108410000","c199529486","c140807948","c1285501","c277761969","c1267278","c277907274","c1442408","c5114156","c2987941056","c85255121","c2986665194","c277861005","c16598687","c90291627","c1740988","c1756080","c16211746","c4766710","c154267886","c278088067","c14642724","c12651998","c16588628","c11872197","c72045907","c2910607562","c186801447","c2777046567","c58916441","c18096280","c17260008","c2785018","c80086925","c17920168","c2908926650","c855076","c54887055","c10659712","c14904697","c2776801781","c277812052","c71191651","c20608485","c2978448")

#Se define la variable "paises" para realizar la busqueda de estos
##Ejemplo para solo mexico
paises <- "mx"
##Todos los paises de America latina
#paises <- c("mx","AG","AR","BS","BB","BZ","BR","CL","CO","CR","CU","DO","EC","SV","BO","GD","GT","GY","HT","HN","JM","MX","NI","PA","PY","PE","VE","DO","KN","VC","LC","SR","TT","UY")


#oa_fetches una función compuesta que se utiliza para construir consultas, realizar solicitudes y convertir los resultados en un data frame.
#Esta función se utiliza para interactuar con la API de OpenAlex y recuperar datos académicos y de investigación.

query <- oa_fetch(
  #entity(Carácter): Indica la entidad académica o de investigación de la búsqueda.
  #                  Puede ser uno de los siguientes: "works" (trabajos), "authors" (autores), "lugares" (lugares de publicación), "institutions" (instituciones) o "concepts" (conceptos).
  entity = "works",
  
  #Se comentan las siguientes dos lines para no tener limites en la fecha de publicacion
  from_publication_date = "2019-01-01",     #limite inferior de fecha de publicacion 
  to_publication_date   = "2021-12-21",     #limite superior de fecha de publicacion
  
  institutions.country_code = paises,       #cambiar la variable paises para tener todo America latina o solo Mexico
  concepts.id               = conceptos,    #cambiar la variable conceptos para tener solo 5 o todos los conceptos registrados
 
  #options(Lista): Parámetros adicionales que se pueden agregar a la consulta, como select, sort, sample, y seed.
  #                Estos parámetros permiten personalizar la consulta y los resultados.
  options = list(sort = "from_publication_date:desc"),  #Se especifica una opción adicional para ordenar los resultados en orden descendente (desc) según la fecha de publicación, de manera que los trabajos más recientes aparezcan primero
  
  #verbose(Lógico): Si es VERDADERO, imprime información sobre el proceso de consulta.
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

query_bibliometrix<-query_bibliometrix%>%                          # agregamos el argumento "names_repairde" para especificar una estrategia de reparación de nombres ya que en caso contrario marcara error porque generara columnas con nombres duplicados
  unnest_wider(grants, names_sep = "_", names_repair = "unique")   # "unique" renombrara las columnas duplicadas agregando sufijos numericos para hacer nombres unicos


#3c. Se vuelve hacer la consulta de las columas que sean listas para verificar que no tengamos ninguna
es_lista <- sapply(query_bibliometrix, is.list)        
columnas_lista <- names(query_bibliometrix)[es_lista]  
print(columnas_lista)  


#4.:::::::::::::::::::::::::::::::::::::::::::EXPORTAR EL DF A UN .XLSX::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# En el parametro "file" poner toda la direccion donde se quiera guardar el archivo, agregando el nombre que se le desee
# poner y con terminacion .xlsx
write.xlsx(query_bibliometrix, file = "D:/carli/Documents/Servicio Social/Trabajo/OpenAlex/prueba(mx,5concepts)_bibliometrix.xlsx")


#5.::::::::::::::::::::::::::::::::::::::::::::Utilizar BIBLIOMETRIX ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#  Una vez cargado el paquete, biblioshiny() abre la interfaz web donde se analizan los datos
biblioshiny() #abre una pagina web, vista 
# Ya dentro de biblioshiny en cargar datos se carga el archivo query3_bibliometrix.xlsx 
# y la aplicacion comienza el analisis














