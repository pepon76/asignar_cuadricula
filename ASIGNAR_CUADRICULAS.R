library(tidyverse)
library(ggmap)
library(readxl)
library(openxlsx)

register_stadiamaps("YOUR_STADIAMAPS_APIKEY")

# Definir los límites del área de zoom, puedes redefinir estos limites usando el boton exportar de la web
# https://www.openstreetmap.org/

xlim_min <- -25.71
xlim_max <- 43.27
ylim_min <- 21
ylim_max <- 51.02


# Crear una secuencia de coordenadas desde -180 hasta 180, con un paso de 5° , si lo ncesitas puedes modificar el grid
x_coords <- seq(-180, 180, by = 5)
y_coords <- seq(-180, 180, by = 5)

# Crear un data frame con todas las intersecciones de la cuadrícula
grid_data <- expand.grid(x = x_coords, y = y_coords) %>%
  mutate(
    left = x,
    right = x + 5,
    bottom = y,
    top = y + 5,
    cuadrante = case_when(
      x >= 0 & y >= 0 ~ 1,
      x >= 0 & y < 0 ~ 2,
      x < 0 & y < 0 ~ 3,
      x < 0 & y >= 0 ~ 4,
      TRUE ~ NA_integer_
    ),
    nombre = case_when(
      x >= 0 & y >= 0 ~ paste0(sprintf("%02d", abs(y)), sprintf("%02d", abs(x))),        # Cuadrante 1: límite inferior y izquierdo
      x >= 0 & y < 0 ~ paste0(sprintf("%02d", abs(y + 5)), sprintf("%02d", abs(x))), # Cuadrante 2: límite superior e izquierdo
      x < 0 & y < 0 ~ paste0(sprintf("%02d", abs(y + 5)), sprintf("%02d", abs(x + 5))),  # Cuadrante 3: límite superior y derecho
      x < 0 & y >= 0 ~ paste0(sprintf("%02d", abs(y)), sprintf("%02d", abs(x + 5)))  # Cuadrante 4: límite inferior y derecho
    )
  )

# Solo, si es necesario, filtrar los datos para que solo incluyan el área de interés
filtered_data <- grid_data %>%
  filter(left >= xlim_min & right <= xlim_max & bottom >= ylim_min & top <= ylim_max) %>%
  rename(lon = left, lat = bottom)

# Definir el bounding box para el área de zoom
bbox <- c(left = xlim_min, bottom = ylim_min, right = xlim_max, top = ylim_max)

# Obtener el mapa de fondo
map <- get_stadiamap(bbox, zoom = 5, maptype = "stamen_toner_lite")

# Crear el gráfico
ggmap(map) +
  geom_rect(data = filtered_data, aes(xmin = lon, xmax = lon + 5, ymin = lat, ymax = lat + 5, fill = factor(cuadrante)), color = "black", alpha = 0.5) +
  geom_text(data = filtered_data, aes(x = lon + 2.5, y = lat + 2.5, label = nombre), color = "black", size = 2) +
  labs(title = "Cuadrícula de 5x5 grados con nombres y cuadrantes (Área de Zoom)",
       x = "Longitud",
       y = "Latitud",
       fill = "Cuadrante") +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "lightgreen", "3" = "lightpink", "4" = "lightyellow"))

# ASIGNACION CUADRANTES Y CUADRICULAS -----------------------------------------------------------------------------

# Definir los límites de los cuadrantes y cuadrículas
cuadrante1 <- list(xmin = 0, xmax = 180, ymin = 0, ymax = 180)
cuadrante2 <- list(xmin = 0, xmax = 180, ymin = -180, ymax = 0)
cuadrante3 <- list(xmin = -180, xmax = 0, ymin = -180, ymax = 0)
cuadrante4 <- list(xmin = -180, xmax = 0, ymin = 0, ymax = 180)

# Función para determinar el cuadrante y la cuadrícula de un punto (x, y)
asignar_cuadrante_cuadricula <- function(x, y) {
  cuadrante <- case_when(
    x >= 0 & y >= 0 ~ 1,
    x >= 0 & y < 0 ~ 2,
    x < 0 & y < 0 ~ 3,
    x < 0 & y >= 0 ~ 4
  )
  
  # Determinar en qué cuadrícula está el punto dentro del cuadrante
  if (cuadrante == 1) {
    cuadricula_x <- floor(x / 5) * 5
    cuadricula_y <- floor(y / 5) * 5
  } else if (cuadrante == 2) {
    cuadricula_x <- floor(x / 5) * 5
    cuadricula_y <- ceiling(y / 5) * 5
  } else if (cuadrante == 3) {
    cuadricula_x <- ceiling(x / 5) * 5
    cuadricula_y <- ceiling(y / 5) * 5
  } else {
    cuadricula_x <- ceiling(x / 5) * 5
    cuadricula_y <- floor(y / 5) * 5
  }
  
  # Convertir las coordenadas de la cuadrícula a caracteres y asegurar que tengan cuatro dígitos
  cuadricula_x <- sprintf("%02d", abs(cuadricula_x))
  cuadricula_y <- sprintf("%02d", abs(cuadricula_y))
  
  cuadricula <- paste0(cuadricula_y, cuadricula_x)
  
  return(list(cuadrante = cuadrante, cuadricula = cuadricula))
}

# IMPORTAMOS DATOS REALES LLBASE ----------------------------------------------------------------------------------
# Aplicar la función a cada fila de la serie de puntos y agregar los resultados al dataframe
# Ahora cargas tus datos , tienen que tener un campo X(longitud) y un campo Y(latitud)

LLBASE <- read_excel("C:/Users/pepe/Desktop/ARCHIVOS_PROCESOS_ICCAT/2023/LLBASE_2023.xlsx")
names(LLBASE)

mis_puntos <- LLBASE %>% select(ID_DIARIO,X,Y) %>% rename("x" = "X", "y"="Y") %>% 
  rowwise() %>%
  mutate(
    cuadrante_cuadricula = list(asignar_cuadrante_cuadricula(x, y))
  ) %>%
  ungroup() %>%
  mutate(
    cuadrante = sapply(cuadrante_cuadricula, function(x) x$cuadrante),
    cuadricula = sapply(cuadrante_cuadricula, function(x) x$cuadricula)
  ) %>%
  select(-cuadrante_cuadricula)

View(mis_puntos)

# GRAFICO_COMPLETO ------------------------------------------------------------------------------------------------

ggmap(map) +
  geom_rect(data = filtered_data, aes(xmin = lon, xmax = lon + 5, ymin = lat, ymax = lat + 5, fill = factor(cuadrante)), color = "black", alpha = 0.5) +
  geom_text(data = filtered_data, aes(x = lon + 2.5, y = lat + 2.5, label = nombre), color = "black", size = 5) +
  labs(title = "Cuadrícula de 5x5 grados con nombres y cuadrantes (Área de Zoom)",
       x = "Longitud",
       y = "Latitud",
       fill = "Cuadrante") +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "lightgreen", "3" = "lightpink", "4" = "lightyellow"))+
  geom_point(data = mis_puntos, aes(x, y), size = 0.5)



write.xlsx(mis_puntos, "mis_puntos.xlsx")
getwd()
