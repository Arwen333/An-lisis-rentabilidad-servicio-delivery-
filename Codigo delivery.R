# ============================================================
# LIMPIEZA Y PREPARACIÓN DE DATOS - 
# ============================================================
# Autora: Arwen Yetzirah Ortiz Nuñez
# Proyecto: "Que la comida no se enfríe"
# Fecha: 2026-04-21
# ============================================================

# 1. Cargar librerías necesarias
library(tidyverse)      # manipulación y visualización
library(janitor)        # limpieza de nombres de columnas
library(geosphere)      # cálculo de distancias con coordenadas
library(naniar)         # visualización de valores perdidos

# 2. Cargar datos DIRECTAMENTE desde GitHub
url_data <- "https://raw.githubusercontent.com/Arwen333/Analisis-rentabilidad-servicio-delivery-/refs/heads/main/deliverytime.csv"

# Cargar SOLO las primeras 500 filas (evita duplicados masivos)
deliverytime <- read.csv(url_data) %>%
  clean_names() %>%           # nombres en minúsculas sin espacios
  slice(1:500)                # toma solo primeras 500 filas

# 3. Inspección inicial
cat("📊 Dimensiones originales:", nrow(deliverytime), "filas x", ncol(deliverytime), "columnas\n")
glimpse(deliverytime)
head(deliverytime, 3)

# 4. Limpieza de coordenadas inválidas (0,0)
# Estas coordenadas no existen geográficamente
deliverytime_clean <- deliverytime %>%
  filter(
    restaurant_latitude != 0,
    restaurant_longitude != 0,
    delivery_location_latitude != 0,
    delivery_location_longitude != 0
  )

cat("🗑️ Filas eliminadas por coordenadas (0,0):", nrow(deliverytime) - nrow(deliverytime_clean), "\n")

# 5. Verificar valores perdidos
missing_summary <- deliverytime_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nulos")

print("📋 Resumen de valores perdidos:")
print(missing_summary)

# 6. Limpiar espacios en blanco en variables categóricas
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    type_of_order = str_trim(type_of_order),
    type_of_vehicle = str_trim(type_of_vehicle)
  )

# 7. Detección de outliers en edad (solo informativo, no eliminamos)
Q1_edad <- quantile(deliverytime_clean$delivery_person_age, 0.25, na.rm = TRUE)
Q3_edad <- quantile(deliverytime_clean$delivery_person_age, 0.75, na.rm = TRUE)
IQR_edad <- Q3_edad - Q1_edad
limite_inf_edad <- Q1_edad - 1.5 * IQR_edad
limite_sup_edad <- Q3_edad + 1.5 * IQR_edad

outliers_edad <- deliverytime_clean %>%
  filter(delivery_person_age < limite_inf_edad | 
           delivery_person_age > limite_sup_edad)

cat("📊 Outliers detectados en edad:", nrow(outliers_edad), "\n")

# 8. Conversión de variables categóricas a factores
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    type_of_order = as.factor(type_of_order),
    type_of_vehicle = as.factor(type_of_vehicle),
    delivery_person_id = as.factor(delivery_person_id)
  )

# 9. Cálculo de distancia real (fórmula de Haversine)
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    distancia_km = distHaversine(
      cbind(restaurant_longitude, restaurant_latitude),
      cbind(delivery_location_longitude, delivery_location_latitude)
    ) / 1000
  )

# 10. Crear variable de velocidad (km/h)
deliverytime_clean <- deliverytime_clean %>%
  mutate(
    velocidad_kmh = (distancia_km / time_taken_min) * 60
  )

# 11. FILTRADO CRÍTICO: eliminar distancias y velocidades imposibles
cat("\n🚨 Antes de filtrar valores extremos:\n")
cat("   - Distancia máx:", round(max(deliverytime_clean$distancia_km, na.rm = TRUE), 2), "km\n")
cat("   - Velocidad máx:", round(max(deliverytime_clean$velocidad_kmh, na.rm = TRUE), 2), "km/h\n")

deliverytime_clean <- deliverytime_clean %>%
  filter(
    distancia_km < 50,           # distancia máxima realista: 50 km
    distancia_km > 0,            # distancia positiva
    velocidad_kmh < 80,          # velocidad máxima realista: 80 km/h
    velocidad_kmh > 5,           # velocidad mínima realista: 5 km/h
    delivery_person_age >= 18,   # edad mínima legal para trabajar
    delivery_person_age <= 70,   # edad máxima razonable
    delivery_person_ratings <= 5, # calificación máxima es 5
    delivery_person_ratings >= 1  # calificación mínima es 1
  )

cat("\n✅ Después de filtrar valores extremos:\n")
cat("   - Filas restantes:", nrow(deliverytime_clean), "\n")
cat("   - Distancia máx:", round(max(deliverytime_clean$distancia_km, na.rm = TRUE), 2), "km\n")
cat("   - Velocidad máx:", round(max(deliverytime_clean$velocidad_kmh, na.rm = TRUE), 2), "km/h\n")

# 12. Verificar resultado final
cat("\n📈 Resumen estadístico del dataset limpio:\n")
summary(deliverytime_clean)

# 13. GUARDAR CSV LIMPIO (importante para análisis posteriores)
write.csv(deliverytime_clean, "deliverytime_clean.csv", row.names = FALSE)
cat("\n💾 Archivo guardado: deliverytime_clean.csv\n")

# 14. Verificación rápida de categorías
cat("\n📌 Categorías únicas:\n")
cat("   - Tipo de pedido:", paste(levels(deliverytime_clean$type_of_order), collapse = ", "), "\n")
cat("   - Tipo de vehículo:", paste(levels(deliverytime_clean$type_of_vehicle), collapse = ", "), "\n")

# 15. Mensaje final de éxito
cat("\n🎉 ¡LIMPIEZA COMPLETADA CON ÉXITO!\n")
cat("   ✅ Dataset listo para análisis descriptivo\n")
cat("   ✅ Dataset listo para modelos de supervivencia\n")
cat("   ✅ CSV limpio guardado en tu directorio de trabajo\n")
 
#| echo: false
#| fig.cap: "Distribución del tiempo de entrega (minutos)"

ggplot(deliverytime_clean, aes(x = time_taken_min)) +
  geom_histogram(bins = 20, fill = "forestgreen", color = "white") +
  labs(title = "Distribución del tiempo de entrega",
       x = "Tiempo (minutos)", 
       y = "Frecuencia") +
  theme_minimal()

#| echo: false
#| fig.cap: "Tiempo de entrega según tipo de pedido"

ggplot(deliverytime_clean, aes(x = type_of_order, y = time_taken_min, fill = type_of_order)) +
  geom_boxplot() +
  labs(title = "Tiempo de entrega según tipo de pedido",
       x = "Tipo de pedido", 
       y = "Tiempo (minutos)") +
  theme_minimal() +
  theme(legend.position = "none")

#| echo: false
#| fig.cap: "Distribución del tiempo de entrega según tipo de vehículo"

ggplot(deliverytime_clean, aes(x = type_of_vehicle, y = time_taken_min, fill = type_of_vehicle)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  labs(title = "Distribución del tiempo por tipo de vehículo",
       x = "Tipo de vehículo", 
       y = "Tiempo (minutos)") +
  theme_minimal() +
  theme(legend.position = "none")
ggplot(deliverytime_clean, aes(x = delivery_person_ratings, y = time_taken_min)) +
  geom_point(alpha = 0.5, color = "forestgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Calificación del repartidor vs tiempo de entrega",
       x = "Calificación (1-5)", 
       y = "Tiempo (minutos)") +
  theme_minimal()
