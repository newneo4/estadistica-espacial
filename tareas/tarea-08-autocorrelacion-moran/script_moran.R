# ==============================================================================
# Práctica: Índice de Moran
# Entregable: Script de R reproducible
# ==============================================================================
library(spdep)

# ------------------------------------------------------------------------------
# Parte B - Reproducción en R de la Parte A
# ------------------------------------------------------------------------------
cat("\n======================================================\n")
cat("PARTE B: REPRODUCCIÓN EN R DE LA PARTE A (5 parcelas)\n")
cat("======================================================\n")

# 1. Datos: rendimiento (t/ha) de 5 parcelas en línea
z <- c(1.6, 1.8, 2.0, 1.5, 1.3)

# 2. Matriz de pesos por contigüidad lineal (vecinos adyacentes)
W_matrix <- matrix(0, 5, 5)
for (i in 1:4) { 
  W_matrix[i, i+1] <- 1
  W_matrix[i+1, i] <- 1 
}

# Convertir la matriz a lista de pesos espaciales
# style = "B" significa binaria, sin estandarizar
lw <- mat2listw(W_matrix, style = "B")

# 3. Índice de Moran (debe coincidir con I = 0.207)
cat("\n-> Cálculo del Índice de Moran:\n")
moran_index <- moran(z, lw, n = length(z), S0 = Szero(lw))
print(moran_index$I) 

# 4. Prueba de hipótesis
cat("\n-> Prueba analítica (moran.test):\n")
m_test_A <- moran.test(z, lw, randomisation=FALSE)
print(m_test_A)

cat("\n-> Prueba por Monte Carlo (moran.mc):\n")
set.seed(999) # Para reproducibilidad
m_mc_A <- moran.mc(z, lw, nsim = 999)
print(m_mc_A)


# ------------------------------------------------------------------------------
# Parte C - Ejercicios Propuestos
# ------------------------------------------------------------------------------
cat("\n======================================================\n")
cat("PARTE C: EJERCICIOS PROPUESTOS (Grilla 3x3)\n")
cat("======================================================\n")

# Datos de la grilla 3x3 (leídos fila a fila)
z_grilla <- c(2.1, 2.0, 1.6, 
              1.9, 1.8, 1.5, 
              1.7, 1.4, 1.3)

# Coordenadas de la grilla (para establecer vecindades)
# x va de 1 a 3, y va de 3 a 1 (para coincidir con las filas)
coords <- expand.grid(x = 1:3, y = 3:1)

# Ejercicio 1: Grilla 3x3, contigüidad tipo torre (Rook)
cat("\n--- 1. Ejercicio 1: Contigüidad tipo torre (Rook) ---\n")
# dnearneigh con d=1 encuentra vecinos ortogonales
nb_rook <- dnearneigh(as.matrix(coords), 0, 1)
lw_rook <- nb2listw(nb_rook, style = "B")

moran_rook <- moran(z_grilla, lw_rook, n = length(z_grilla), S0 = Szero(lw_rook))
cat("Índice de Moran (Rook) calculado manual en el documento: 0.4875\n")
cat("Índice de Moran (Rook) calculado en R:", moran_rook$I, "\n")

cat("\nContraste de significancia (Rook):\n")
test_rook <- moran.test(z_grilla, lw_rook, randomisation=FALSE)
print(test_rook)


# Ejercicio 2: Efecto de la vecindad (Queen)
cat("\n--- 2. Efecto de la vecindad (Queen) ---\n")
# d=1.5 incluye las diagonales (distancia sqrt(2) aprox 1.41)
nb_queen <- dnearneigh(as.matrix(coords), 0, 1.5)
lw_queen <- nb2listw(nb_queen, style = "B")

moran_queen <- moran(z_grilla, lw_queen, n = length(z_grilla), S0 = Szero(lw_queen))
cat("Índice de Moran (Queen):", moran_queen$I, "\n")
cat("Comentario: El índice disminuye al incluir las diagonales.\n")


# Ejercicio 3: Aleatorización
cat("\n--- 3. Aleatorización ---\n")
set.seed(123)
z_aleatorio <- sample(z_grilla)
moran_aleatorio <- moran(z_aleatorio, lw_rook, n = length(z_aleatorio), S0 = Szero(lw_rook))

cat("Índice de Moran (Aleatorio):", moran_aleatorio$I, "\n")
cat("Valor Esperado E[I]:", -1/(length(z_grilla)-1), "\n")
cat("Comentario: El valor aleatorio se aproxima al esperado bajo H0.\n")
