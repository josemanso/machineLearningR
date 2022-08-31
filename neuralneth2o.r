# Tratamiento de datos y gráficos
# ==============================================================================
library(tidyverse)
#library(ggthemes)
library(ggpubr)

# Modelado
# ==============================================================================

library(h2o)

# Creación de un cluster local con todos los cores disponibles.
h2o.init(
  ip = "localhost",
  # -1 indica que se empleen todos los cores disponibles.
  nthreads = -1,
  # Máxima memoria disponible para el cluster.
  max_mem_size = "6g"
)

# Se eliminan los datos del cluster por si ya había sido iniciado.
h2o.removeAll()
# Para que no se muestre la barra de progreso.
h2o.no_progress()

# shoe cluster h2o (http://localhost:54321/flow/index.html).
# Ejemplo de conexión a un cluster remoto
#h2o.init(ip = "123.45.67.89", port = 54321)


# Carga de datos en el cluster H2O desde url.
##url_path <- paste0("https://github.com/JoaquinAmatRodrigo/Estadistica-con-R/raw/",
##                   "/home/josemo/Rprogram/dataset/adult_custom.csv")
##
##datos_h2o <- h2o.importFile(
##  path = url_path, header = TRUE, sep = ",",
##  destination_frame = "datos_h2o"
##)
# Carga de datos en el cluster H2O desde local.
datos_h2o <- h2o.importFile(
  path   = "/home/josemo/Rprogram/dataset/adult_custom.csv",
  header = TRUE,
  sep    = ",",
  destination_frame = "datos_h2o"
)
###### Information #####
#datos_h2o <- as.h2o(datos)
h2o.ls()
##   key
##  1 datos_h2o

# Dimensiones del set de datos
h2o.dim(datos_h2o)

##  [1] 45222    15

# Nombre de las columnas
h2o.colnames(datos_h2o)
##[1] "age"              "workclass"        "final_weight"    
##[4] "education"        "education_number" "marital_status"  
##[7] "occupation"       "relationship"     "race"            
##[10] "sex"              "capital_gain"     "capital_loss"    
##[13] "hours_per_week"   "native_country"   "salario"      

h2o.describe(datos_h2o)
# Índices
indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric")
indices
# Nombres
h2o.colnames(x = datos_h2o)[indices]
indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric")
h2o.cor(x = datos_h2o[, indices], y = NULL, method = "Pearson", na.rm = TRUE)
####

# Se crea una tabla con el número de observaciones de cada tipo.
tabla_muestra <- as.data.frame((h2o.table(datos_h2o$salario)))
tabla_muestra
# plot tabla
ggplot(
  data = tabla_muestra,
  aes(x=salario,y=Count, fill=salario)) +
  geom_col() +
  scale_fill_manual(values = c("gray50","orangered2")) +
  theme_bw() +
  labs(
    x="Salario", y="Número de observaciones",
    title="Distribución de la variable Salario") +
  theme(legend.position = "none")

  

# Separación de las observaciones en conjunto de entrenamiento
# y test. 3 conjuntos
particiones <- h2o.splitFrame(datos_h2o, ratios=c(.8),seed=123)
datos_train_h2o <- h2o.assign(data=particiones[[1]],key="datas_train_H2o")
datos_test_h2o <- h2o.assign(data=particiones[[2]],key="datos_tesy_H2O")

# Observ.
h2o.table(datos_train_h2o$salario)
h2o.table(datos_test_h2o$salario)
# Porcentaje
h2o.table(datos_train_h2o$salario)/h2o.nrow(datos_train_h2o)

#### Modelo h2o
## Generalized linear models (GLMs)

# Se comprueba que la variable respuesta es de tipo factor.
datos_train_h2o$salario <- h2o.asfactor(datos_train_h2o$salario)
datos_test_h2o$salario  <- h2o.asfactor(datos_test_h2o$salario)
h2o.isfactor(datos_train_h2o$salario)
h2o.isfactor(datos_test_h2o$salario)
# [1] TRUE

# Se define la variable respuesta y los predictores.
var_respuesta <- "salario"
predictores <- setdiff(h2o.colnames(datos_h2o), var_respuesta)

# Ajuste del modelo y validación mediente 5-CV para estimar su error.
modelo_glm <- h2o.glm(
  y = var_respuesta,
  x = predictores,
  training_frame = datos_train_h2o,
  family = "binomial",
  link = "logit",
  standardize = TRUE,
  balance_classes = FALSE,
  ignore_const_cols = TRUE,
  # Se especifica que hacer con observaciones incompletas
  missing_values_handling = "Skip",
  # Se hace una búsqueda del hiperparámetro lamba
  lambda_search = TRUE,
  # Selección automática del solver adecuado
  solver = "AUTO",
  alpha  = 0.95,
  # Validación cruzada de 5 folds para estimar el error
  # del modelo.
  seed = 123,
  nfolds = 5,
  # Reparto estratificado de las observaciones en la creación
  # de las particiones.
  fold_assignment = "Stratified",
  keep_cross_validation_predictions = FALSE,
  model_id = "modelo_glm"
)
modelo_glm


# Coeficientes de regresión de cada uno de los predictores.
as.data.frame(modelo_glm@model$coefficients_table) %>% head()

# Predictores incluidos.
names(modelo_glm@model$coefficients[modelo_glm@model$coefficients != 0])

# Predictores
coeficientes <- as.data.frame(modelo_glm@model$coefficients_table)

# Se excluye el intercept.
coeficientes <- coeficientes %>% filter(names != "Intercept")

# Se calcula el valor absoluto.
coeficientes <- coeficientes %>%
  mutate(abs_stand_coef = abs(standardized_coefficients))

# Se añade una variable con el signo del coeficiente.
coeficientes <- coeficientes %>%
  mutate(signo = if_else(standardized_coefficients > 0,
                         "Positivo",
                         "Negativo"))

ggplot(data = coeficientes,
       aes(x = reorder(names, abs_stand_coef),
           y = abs_stand_coef,
           fill = signo)) +
  geom_col() +
  coord_flip() +
  labs(title = "Importancia de los predictores en el modelo GLM",
       x = "Predictor",
       y = "Valor absoluto coeficiente estandarizado") +
  theme_bw() +
  theme(legend.position = "bottom")


# Equivalente:
h2o.varimp(modelo_glm)
h2o.varimp_plot(modelo_glm)


# métricas de entrenemiento

# Área bajo la curva
h2o.auc(modelo_glm, train = TRUE)

# Mean Squared Error
h2o.mse(modelo_glm, train = TRUE)
# R2
h2o.r2(modelo_glm, train = TRUE)
# LogLoss
h2o.logloss(modelo_glm, train = TRUE)
# Coeficiente de Gini
h2o.giniCoef(modelo_glm, train = TRUE)
# Desviance del modelo nulo
h2o.null_deviance(modelo_glm, train = TRUE)
# Desviance del modelo final
h2o.residual_deviance(modelo_glm, train = TRUE)
# AIC
h2o.aic(modelo_glm, train = TRUE)

###  H2OBinomialMetrics: glm
###  ** Reported on training data. **
h2o.performance(model = modelo_glm, train = TRUE)

# Equivalente a:
modelo_glm@model$training_metrics


# Área bajo la curva
h2o.auc(modelo_glm, xval = TRUE)

h2o.performance(model = modelo_glm, xval = TRUE)


## Predicciones
predicciones <- h2o.predict(object = modelo_glm, newdata = datos_test_h2o)
predicciones


h2o.performance(model = modelo_glm, newdata = datos_test_h2o)

# Cálculo manual de accuracy
mean(as.vector(predicciones$predict) == as.vector(datos_test_h2o$salario))

# Valores de alpha que se van a comparar.
hiperparametros <- list(alpha = c(0, 0.1, 0.5, 0.95, 1))

grid_glm <- h2o.grid(
  # Algoritmo y parámetros
  algorithm      = "glm",
  family         = "binomial",
  link           = "logit",
  # Variable respuesta y predictores
  y              = var_respuesta,
  x              = predictores,
  # Datos de entrenamiento
  training_frame = datos_train_h2o,
  # Preprocesado
  standardize    = TRUE,
  missing_values_handling = "Skip",
  ignore_const_cols = TRUE,
  # Hiperparámetros
  hyper_params    = hiperparametros,
  # Tipo de búsqueda
  search_criteria = list(strategy = "Cartesian"),
  lambda_search   = TRUE,
  # Selección automática del solver adecuado
  solver          = "AUTO",
  # Estrategia de validación para seleccionar el mejor modelo
  seed            = 123,
  nfolds          = 10,
  # Reparto estratificado de las observaciones en la creación
  # de las particiones
  fold_assignment = "Stratified",
  keep_cross_validation_predictions = FALSE,
  grid_id         = "grid_glm"
)

# Se muestran los modelos ordenados de mayor a menor AUC.
resultados_grid <- h2o.getGrid(
  grid_id = "grid_glm",
  sort_by = "auc",
  decreasing = TRUE
)
print(resultados_grid)

resultados_grid@summary_table

# Identificador de los modelos creados por validación cruzada.
id_modelos <- unlist(resultados_grid@model_ids)

# Se crea una lista donde se almacenarán los resultados.
auc_xvalidacion <- vector(mode = "list", length = length(id_modelos))

# Se recorre cada modelo almacenado en el grid y se extraen la métrica (auc)
# obtenida en cada partición.
for (i in seq_along(id_modelos)) {
  modelo <- h2o.getModel(resultados_grid@model_ids[[i]])
  metricas_xvalidacion_modelo <- modelo@model$cross_validation_metrics_summary
  names(auc_xvalidacion)[i]   <- modelo@model$model_summary$regularization
  auc_xvalidacion[[i]] <- as.numeric(metricas_xvalidacion_modelo["auc", -c(1,2)])
}

# Se convierte la lista en dataframe.
auc_xvalidacion_df <- as.data.frame(auc_xvalidacion) %>%
  mutate(resample = row_number()) %>% 
  gather(key = "modelo", value = "auc", -resample) %>%
  mutate(modelo = str_replace_all(string = modelo,
                                  pattern = "_" ,
                                  replacement = " \n "))
# Gráfico
ggplot(data = auc_xvalidacion_df, aes(x = modelo, y = auc, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "point") +
  theme_bw() +
  labs(title = "Accuracy obtenido por 10-CV") +
  coord_flip() +
  theme(legend.position = "none")
