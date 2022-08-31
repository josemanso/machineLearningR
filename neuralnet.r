# Neuronal network; H2o package

# Tratamiento de datos y gráficos
# ==============================================================================
library(tidyverse)
library(ggthemes)
library(ggpubr)

# Modelado
# ==============================================================================

library(h2o)

# Creación de un cluster local con todos los cores disponibles.
h2o.init(
  ip       = "localhost",
  # -1 indica que se empleen todos los cores disponibles.
  nthreads = -1,
  # Máxima memoria disponible para el cluster.
  max_mem_size = "6g"
)
# Se eliminan los datos del cluster por si ya había sido iniciado.
h2o.removeAll()
# Para que no se muestre la barra de progreso.
h2o.no_progress()


#datos
# Número de observaciones por clase
N <- 300
# Número de dimensiones
D <- 2
# Número de clases
K <- 3
# Matriz para almacenar las observaciones
x_1 <- vector(mode="numeric")
x_2 <- vector(mode="numeric")
y <- vector(mode="numeric")

# Datos simulados
for(i in 1:K) {
  set.seed(123)
  r <- seq(from = 0, to = 1, length.out = N)
  t <- seq(from =  i*4, to = (i+1)*4, length.out = N) + rnorm(n = N) * 0.35
  x_1 <- c(x_1, r * sin(t))
  x_2 <- c(x_2, r * cos(t))
  y <- c(y, rep(letters[i],N))
}

datos_espiral <- data.frame(y=as.factor(y), x_1,x_2)

ggplot(data = datos_espiral, aes(x = x_1, y = x_2, color = y)) + 
  geom_point() +
  theme_bw() + 
  theme(legend.position = "none",
        text = element_blank(),
        axis.ticks =  element_blank())


datos_espiral_h2o <- as.h2o(datos_espiral)
h2o.ls()

# Modelos
# ==============================================================================
modelo_1 <- h2o.deeplearning(
  x = c("x_1","x_2"),
  y = "y",
  distribution = "multinomial",
  training_frame = datos_espiral_h2o,
  standardize = TRUE,
  activation = "Rectifier",
  adaptive_rate = FALSE,
  hidden = 5,
  stopping_rounds = 0,
  epochs = 500,
  seed = 123,
  model_id = "model_1"
)
modelo_2 <- h2o.deeplearning(
  x               = c("x_1", "x_2"),
  y               = "y",
  distribution    = "multinomial",
  training_frame  = datos_espiral_h2o,
  standardize     = TRUE,
  activation      = "Rectifier",
  adaptive_rate   = FALSE,
  hidden          = 20,
  stopping_rounds = 0,
  epochs          = 1000,
  seed            = 123,
  model_id        = "modelo_2"
)
modelo_3 <- h2o.deeplearning(
  x               = c("x_1", "x_2"),
  y               = "y",
  distribution    = "multinomial",
  training_frame  = datos_espiral_h2o,
  standardize     = TRUE,
  activation      = "Rectifier",
  adaptive_rate   = FALSE,
  hidden          = c(10, 10),
  stopping_rounds = 0,
  epochs          = 1000,
  seed            = 123,
  model_id        = "modelo_3"
)

modelo_4 <- h2o.deeplearning(
  x               = c("x_1", "x_2"),
  y               = "y",
  distribution    = "multinomial",
  training_frame  = datos_espiral_h2o,
  standardize     = TRUE,
  activation      = "Rectifier",
  adaptive_rate   = FALSE,
  hidden          = c(50, 50, 50),
  stopping_rounds = 0,
  epochs          = 1000,
  seed            = 123,
  model_id        = "modelo_4"
)

# Predicciones de cada modelo
# ==============================================================================
grid_predicciones <- expand.grid(
  x_1=seq(from = -1, to = 1, length = 75),
  x_2=seq(from = -1, to = 1, length = 75)
)
grid_predicciones_h2o <- as.h2o(grid_predicciones)

predicciones_1 <- h2o.predict(
  object = modelo_1,
  newdata = grid_predicciones_h2o
)
grid_predicciones$y_5 <- as.vector(predicciones_1$predict)


predicciones_2 <- h2o.predict(
  object = modelo_2,
  newdata = grid_predicciones_h2o
)
grid_predicciones$y_20 <- as.vector(predicciones_2$predict)

predicciones_3 <- h2o.predict(
  object = modelo_3,
  newdata = grid_predicciones_h2o
)
grid_predicciones$y_10_10 <- as.vector(predicciones_3$predict)

predicciones_4 <- h2o.predict(
  object = modelo_4,
  newdata = grid_predicciones_h2o
)
grid_predicciones$y_50_50_50 <- as.vector(predicciones_4$predict)


#grid_predicciones$modelo_1 <- as.vector(predicciones_1$preddict)

#grid_predicciones$modelo_2 <- as.vector(predicciones_2$preddict)

#grid_predicciones$modelo_3 <- as.vector(predicciones_3$preddict)

#grid_predicciones$modelo_4 <- as.vector(predicciones_4$preddict)


# Gráfico de predicciones
# ==============================================================================
p1 <- ggplot(
  data=grid_predicciones,aes(x=x_1,y=x_2,color= y_5)) +
  geom_point(size=0.5) +
  theme_fivethirtyeight() +
  labs(title="Arquitectura: (5)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 11),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

p2 <- ggplot(
  data=grid_predicciones,aes(x=x_1,y=x_2,color = y_20)) +
  geom_point(size=0.5) +
  theme_fivethirtyeight() +
  labs(title="Arquitectura: (20)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 11),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

p3 <- ggplot(
  data=grid_predicciones,aes(x=x_1,y=x_2,color= y_10_10)) +
  geom_point(size=0.5) +
  theme_fivethirtyeight() +
  labs(title="Arquitectura: (10, 10)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 11),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

p4 <- ggplot(
  data=grid_predicciones,aes(x=x_1,y=x_2,color=y_50_50_50)) +
  geom_point(size=0.5) +
  theme_fivethirtyeight() +
  labs(title="Arquitectura: (50, 50, 50)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 11),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
