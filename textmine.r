# Mineria de texto ; Mapa de Palabras
library(syuzhet) # Funciones get_
library(stringr) # Funciones str_
library(tm) # Funciones de text mining
library(wordcloud) # Crear mapa de nubes
library(tidyverse)

# Proyecto Gutenber; obra Niebla de Miguel de Unamuno
url <- "http://www.gutenberg.org/files/49836/49836-0.txt"

obra <- get_text_as_string(url) # importar todo el texto como una cadena

oraciones <- get_sentences(obra) # vector oraciones del texto

# Limpieza de texto

# Eliminamos primeras filas de notas, prólogo, post-prólogo
total_lineas <- length(oraciones)

linea_empieza <- 115
linea_final <- total_lineas - linea_empieza

texto_limpio <- oraciones[linea_empieza:linea_final]

# Detectar caracteres especiales
texto_limpio <- texto_limpio %>%
  str_replace_all(.,"[[:cntrl:]]", " ") %>% # saltos de line y tab
  str_to_lower() %>% # todo en minúsculas
  removePunctuation() %>%
  str_replace_all(.,"-", " ")
texto_limpio <- texto_limpio %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  str_to_lower() %>% 
  removePunctuation() %>% 
  str_replace_all(., "—", " ")

# Para el español
texto_limpio <- removeWords(texto_limpio, words = stopwords("spanish"))
# Espacios vacíos
texto_limpio <- stripWhitespace(texto_limpio)

# Creación del Corpus, colección de datos
coleccion <- texto_limpio %>%
  VectorSource() %>%
  Corpus()

# Creación del mapa de palabras
wordcloud(coleccion,
          min.freq = 5,
          max.words = 80,
          random.order = FALSE,
          colors = brewer.pal(name = "Dark2", n = 8)
          )

# 2da Lipieza de datos
a_retirar <-c("usted", "pues", "tal", "tan", "así", "dijo", "como",
              "sino", "entonces", "aunque", "don", "doña")
texto_limpio <- removeWords(texto_limpio, words = a_retirar)

coleccion <- texto_limpio %>%
  VectorSource() %>%
  Corpus()

wordcloud(coleccion,
          min.freq = 5,
          max.words =80,
          random.order = FALSE,
          colors = brewer.pal(name ="Dark2", n = 8)
          )
# Frecuencia de palabras
palabras <- coleccion %>%
  # coleccion a matriz
  TermDocumentMatrix() %>%
  as.matrix() %>%
  rowSums() %>%
  # se obtiene un vector con la frecuencia de palabras
  sort(decreasing =TRUE)

palabras %>%
  head(20)

# Con los valores de nombres y frecuencias, obtener un data frame
frecuencias <- data.frame(
  palabra = names(palabras),
  frecuencia =palabras
)

# Visualización de top 10 palabras
frecuencias[1:10,] %>%
  ggplot() +
  aes(frecuencia, y = reorder(palabra, frecuencia)) +
  geom_bar(stat = "identity", color = "white", fill = "blue") +
  geom_text(aes(label = frecuencia, hjust = 1.5), color = "white") +
  labs(x=NULL, y="Palabras mas usadas en la obra")


# 9.6 Minería de texto: Análisis de sentimientos
library(readxl)

url <- "https://dparedesi.github.io/DS-con-R/rmapalacios_user_tweets.xlsx"

# Creamos un nombre & ruta temporal para nuestro archivo.
archivo_temporal <- tempfile()

# Descargamos el archivo en nuestro temporal
download.file(url, archivo_temporal)

# Importamos el excel
publicaciones <- read_excel(archivo_temporal)

# Eliminamos el archivo temporal
file.remove(archivo_temporal)

# Hemos creado nuestro objeto publicaciones, el cual tiene 
# en la columna Text los diferentes tweets, retweets y replies,
# realizados.

tuits <- publicaciones %>% 
  filter(`Tweet Type` == "Tweet") %>% 
  #filter(`Tweet Id` == "Tweet") %>% 
  .$Text

 
 # Creación de un mapa de palabras 
 tuits_limpio <- tuits %>% 
   removePunctuation() %>% 
   str_to_lower() %>% 
   str_replace_all(., "[[:cntrl:]]", " ") %>% 
   removeWords(., words = stopwords("spanish")) %>% 
   removeWords(., words = c("usted", "pues", "tal", "tan",
                            "así", "dijo", "cómo", "sino", 
                            "entonces", "aunque", "que"))
 
 
 coleccion1 <- tuits_limpio %>%
   VectorSource() %>%
   Corpus()
 
 wordcloud(coleccion1,
           min.freq = 5,
           max.words = 80,
           random.order = FALSE,
           colors = brewer.pal(name="Dark2", n=8))

 # El Léxico de Emociones NRC es una lista de palabras
 # y sus asociaciones con ocho emociones básicas 
 # (ira, miedo, anticipación, confianza, asombro, tristeza, alegría
 # y aversión) y dos sentimientos (negativo y positivo).
 
 # Resultados
 resultado <- get_nrc_sentiment(tuits_limpio, language = "spanish")
 
 resultado %>%
   head(10)
 
 trad_emociones <- function(cadena){
   case_when(
     cadena =="anger" ~ "Ira",
     cadena == "anticipation" ~ "Anticipación",
     cadena == "disgust" ~ "Aversión",
     cadena == "fear" ~ "Miedo",
     cadena == "joy" ~ "Alegría",
     cadena == "sadness" ~ "Tristeza",
     cadena == "surprise" ~ "Asombro",
     cadena == "trust" ~ "Confianza",
     cadena == "negative" ~ "Negativo",
     cadena == "positive" ~ "Positivo",
     TRUE ~ cadena
   )
 }
 
 # Resumen de las emociones/sentimientos
 
 sentimientos <- resultado %>% 
   gather(sentimiento, cantidad) %>% 
   mutate(sentimiento = trad_emociones(sentimiento)) %>% 
   group_by(sentimiento) %>% 
   summarise(total = sum(cantidad))
 
 sentimientos
 
 # 8 sentimientos y dos emociones
 index <- sentimientos$sentimiento %in% c("Positivo", "Negativo") 
 
 # Visualizar emociones
 sentimientos[!index,] %>% 
   ggplot() +
   aes(sentimiento, total) +
   geom_bar(aes(fill=sentimiento),stat ="identity") +
   theme(axis.text.x=element_text(angle=45,hjust=1)) +
   xlab(NULL) +
   ylab("Total") +
   ggtitle("Emociones en los Tweets de RosaMaría Palacos")
 
 # Visualización de si son sentimientos positivos o negativos:
 sentimientos[index,] %>% 
   ggplot() +
   aes(sentimiento, total) +
   geom_bar(aes(fill = sentimiento), stat = "identity") +
   xlab(NULL) +
   ylab("Total") +
   ggtitle("Sentimientos de los Tweets de Rosa María Palacios")
         