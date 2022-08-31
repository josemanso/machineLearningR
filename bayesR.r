# Naïve Bayes with R; ext classifier
library(tidyverse) # Set of packages; ggplot, dplur, tydyr...
library(tidytext)  # text mining
library(naivebayes)
library(tm) # text mining
library(caret) # Training and plotting classification and regression models

#download.file(url = "https://raw.githubusercontent.com/jboscomendoza/rpubs/master/bayes_twitter/tuits_bayes.csv", 
 #             destfile = "/home/josemo/Rprogram/dataset/tuits.csv")

# read data
# para español con tildes y ñ 
tuits_df <-
  read.csv("/home/josemo/Rprogram/dataset/tuits.csv",
           stringsAsFactors = F, fileEncoding = "latin1") %>%
  tbl_df

# Preprocessing data
putoff_url <- function(texto) {
  gsub("\\<http\\S*\\>|[0-9]", " ", texto)
}

# Sparse matrix creation; matriz dispersa
# Row -> text
# colunm -> word
tuits_df
# Segmentar el tuit por palabra
# Contar cuantas veces aparece cada palabra
# dar formato de matriz

tuits_df %>%
  unnest_tokens(input="text", output="palabra") %>%
  count(screen_name, status_id, palabra) %>%
  spread(key = palabra, value = n)

#  A tabble: 1,348 (tuits) × 8571 columnas ( palabras)

crear_matriz <- function(tabla) {
  tabla %>%
    mutate(text = putoff_url(text)) %>%  # url
    unnest_tokens(input="text", output = "palabra") %>%
    count(screen_name, status_id, palabra) %>%
    spread(key=palabra, value = n) %>%
    select(-status_id)
}


# data prepared for analysis for naive bayes

# twit made for @MSFTMexico, or Otro
# Bayes

ejemplo_matriz <-
  tuits_df %>%
  mutate(screen_name = ifelse(screen_name=="MSFTMexico", screen_name, "Otro"),
         screen_name = as.factor(screen_name)) %>%
  crear_matriz

# for changing screen_name
elegir_usuario <- function(nombres, usuario) {
  as.factor(ifelse(nombres %in% usuario, nombres, "Otro"))
}

# Set for training and set for test
set.seed(2001)
training <- sample_frac(ejemplo_matriz, .7)
test <- setdiff(ejemplo_matriz, training)

# sets in lists
crear_sets <- function(tabla, prop = .7) {
  lista_sets <- list()
  lista_sets$train <- sample_frac(tabla, prop)
  lista_sets$test <- setdiff(tabla, lista_sets[["train"]])
  
  lista_sets
}

# Using naive_bayes
ejemplo_modelo <- naive_bayes(formula = screen_name ~ ., data = training)

# Prediction
prediccion <- predict(ejemplo_modelo, test)

head(prediccion, 25)

# Confusion matrix

confusionMatrix(prediccion,as.factor(test$screen_name))

# Analyzer

# Improving model performancde
obtener_bayes <- function(lista_sets, objetivo ="screen_name") {
  bayes_formula <- as.formula(paste0(objetivo, "~ ."))
  bayes <- list()
  
  bayes$modelo <- naive_bayes(formula=bayes_formula, data=lista_sets[["train"]])
  bayes$prediccion   <- predict(object = bayes$modelo, newdata = lista_sets[["test"]])
  
  bayes
}
mat_conf <- function(resultado, set_test) {
  confusionMatrix(resultado[["prediccion"]], set_test[["test"]][["screen_name"]])
}
ejemplo_conf <- confusionMatrix(prediccion,as.factor(test$screen_name))
plot(ejemplo_conf[["table"]])

# plot matrix
plot_conf <- function(resultados_bayes) {
  plot(resultados_bayes[["confusion"]][["table"]],
       col = c("#00BBFF", "#FF6A00"),
       main = resultados_bayes[["confusion"]][["positive"]])
}

# Analizer
hacer_bayes <- function(tabla, usuario) {
  ingenuo <- list()
  
  ingenuo[["matriz"]] <-
    tabla %>%
    mutate(screen_name=elegir_usuario(screen_name, usuario)) %>%
    crear_matriz()
  
  ingenuo[["sets"]] <- crear_sets(ingenuo[["matriz"]])
  ingenuo[["resultado"]] <- obtener_bayes(ingenuo[["sets"]])
  
  ingenuo[["confusion"]] <- list()
  
  ingenuo[["confusion"]] <- mat_conf(ingenuo[["resultado"]], ingenuo[["sets"]])
  ingenuo
}

# runing new classification "CMLL_OFICIAL"
set.seed(1988)
bayes_cmll <- hacer_bayes(tuits_df, "CMLL_OFICIAL")
# matrix
bayes_cmll[["mat"]]
# stets
bayes_cmll[["sets"]][["train"]]
bayes_cmll[["sets"]][["test"]]
# naive Bayes
bayes_cmll[["resultado"]][["modelo"]]

# Confusion matrix
head(bayes_cmll[["resultado"]][["prediccion"]], 25)

# with map
lista_usuarios <- list(lopezobrador_ = "lopezobrador_",
                       MSFTMexico = "MSFTMexico",
                       UNAM_MX  = "UNAM_MX",
                       CMLL_OFICIAL = "CMLL_OFICIAL")

lista_bayes <- map(lista_usuarios, hacer_bayes, tabla= tuits_df)

lista_bayes$lopezobrador_

