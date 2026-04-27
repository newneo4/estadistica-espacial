# =====================================================================
# SCRIPT COMPLETO: EJERCICIOS DE VARIABLE ALEATORIA GAUSSIANA
# =====================================================================

# ---------------------------------------------------------------------
# Ejercicio 1: Detección de anomalías en sensores IoT
# ---------------------------------------------------------------------
cat("--- EJERCICIO 1 ---\n")
mu_1 <- 70
sigma_1 <- 5

# (a) Intervalo numérico que define una lectura normal
limite_inf <- mu_1 - 3 * sigma_1
limite_sup <- mu_1 + 3 * sigma_1
cat("(a) Intervalo normal: [", limite_inf, ",", limite_sup, "]\n")

# (b) Calcular P(X > 82)
# Usamos lower.tail = FALSE para calcular la probabilidad de la cola derecha
prob_82 <- pnorm(82, mean = mu_1, sd = sigma_1, lower.tail = FALSE)
cat("(b) P(X > 82) =", prob_82, "\n")
# Decisión: Como 82 está dentro del intervalo normal [55, 85], no dispara alerta.

# (c) Porcentaje de anomalías esperado según la regla empírica (99.7% dentro de 3 sigma)
porcentaje_anomalo <- (1 - 0.997) * 100
cat("(c) Porcentaje esperado de anomalías:", porcentaje_anomalo, "%\n\n")


# ---------------------------------------------------------------------
# Ejercicio 2: Umbrales de clasificación en modelos de crédito
# ---------------------------------------------------------------------
cat("--- EJERCICIO 2 ---\n")
mu_2 <- 650
sigma_2 <- 80

# (a) Estandarizar el umbral 550 para obtener la puntuación Z
Z_550 <- (550 - mu_2) / sigma_2
cat("(a) Puntuación Z para 550 =", Z_550, "\n")

# (b) Calcular P(X < 550) y la proporción de alto riesgo
prob_alto_riesgo <- pnorm(550, mean = mu_2, sd = sigma_2)
cat("(b) Proporción de la cartera de alto riesgo =", prob_alto_riesgo, "(Aprox. 10.56%)\n")

# (c) Umbral para que solo el 5% sea de alto riesgo
# qnorm encuentra el valor 'x' dado el percentil (0.05)
umbral_5pct <- qnorm(0.05, mean = mu_2, sd = sigma_2)
cat("(c) Nuevo umbral para el 5% de alto riesgo =", umbral_5pct, "\n\n")


# ---------------------------------------------------------------------
# Ejercicio 3: Control de calidad en predicciones
# ---------------------------------------------------------------------
cat("--- EJERCICIO 3 ---\n")
mu_3 <- 0
sigma_3 <- 200

# (a) Calcular P(|e| > 400)
# Por simetría, la probabilidad de exceder 400 o ser menor a -400 es el doble de ser menor a -400.
prob_inaceptable <- 2 * pnorm(-400, mean = mu_3, sd = sigma_3)
cat("(a) Probabilidad de error inaceptable =", prob_inaceptable, "\n")

# (b) Predicciones inaceptables en un mes de 30 días
dias <- 30
errores_esperados <- dias * prob_inaceptable
cat("(b) Predicciones inaceptables esperadas en 30 días =", errores_esperados, "\n")

# (c) Cambio en P(|e| > 400) si sigma baja a 120
sigma_3_nueva <- 120
prob_inaceptable_nueva <- 2 * pnorm(-400, mean = mu_3, sd = sigma_3_nueva)
cat("(c) Nueva probabilidad de error (sigma=120) =", prob_inaceptable_nueva, "\n\n")


# ---------------------------------------------------------------------
# Ejercicio 4: Muestreo y representatividad en conjuntos de datos
# ---------------------------------------------------------------------
cat("--- EJERCICIO 4 ---\n")
mu_4 <- 34
sigma_4 <- 9

# (a) Probabilidad de que el usuario tenga entre 25 y 43 años P(25 <= X <= 43)
prob_25_43 <- pnorm(43, mean = mu_4, sd = sigma_4) - pnorm(25, mean = mu_4, sd = sigma_4)
cat("(a) P(25 <= X <= 43) =", prob_25_43, "\n")

# (b) Puntuación Z para un usuario de 55 años
Z_55 <- (55 - mu_4) / sigma_4
cat("(b) Puntuación Z para 55 años =", Z_55, "\n")

# (c) Edad por encima de la cual se encuentra el 10% más mayor (percentil 90)
edad_10pct_superior <- qnorm(0.90, mean = mu_4, sd = sigma_4)
cat("(c) El 10% de usuarios más mayores supera los", edad_10pct_superior, "años\n\n")


# ---------------------------------------------------------------------
# Ejercicio 5: Evaluación de tiempos de respuesta en sistemas
# ---------------------------------------------------------------------
cat("--- EJERCICIO 5 ---\n")
mu_5 <- 120
sigma_5 <- 25

# (a) Calcular P(X > 170) y decidir si es raro
prob_170 <- pnorm(170, mean = mu_5, sd = sigma_5, lower.tail = FALSE)
cat("(a) P(X > 170) =", prob_170, "(Es raro, ~2.2%)\n")

# (b) Determinar el valor T que cumple que el 99% sea menor a T
T_99 <- qnorm(0.99, mean = mu_5, sd = sigma_5)
cat("(b) El valor T para el SLA (99%) es", T_99, "ms\n")

# (c) Nuevo T si el tráfico pico duplica la varianza
nueva_varianza <- 2 * (sigma_5^2)
nueva_sigma <- sqrt(nueva_varianza) # Se extrae la raíz para obtener la nueva desviación estándar
T_99_pico <- qnorm(0.99, mean = mu_5, sd = nueva_sigma)
cat("(c) Si la varianza se duplica, el nuevo valor T necesario es", T_99_pico, "ms\n")

