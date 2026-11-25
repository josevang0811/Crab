# Tabla de contingencia {#tabcontingencia}

Las tablas de contingencia permiten estudiar la relación entre dos variables cualitativas a partir de sus frecuencias. En este capítulo se aplican las dos pruebas vistas en las notas de clase, ambas basadas en el estadístico $\chi^2$:

- La **prueba de homogeneidad**, para comparar si la distribución de una variable categórica es la misma en varias poblaciones.
- La **prueba de independencia**, para evaluar si dos variables categóricas medidas en una misma población están asociadas.

En la base de cangrejos la única variable categórica es el sexo, así que primero es necesario construir otras variables categóricas a partir de las numéricas.

Carguemos los datos con la misma limpieza de los capítulos anteriores:


```r
library(tidyverse)

df <- read.csv("datos/train.csv") %>%
  select(-id) %>%
  rename(
    sexo           = Sex,
    largo          = Length,
    diametro       = Diameter,
    altura         = Height,
    peso           = Weight,
    peso_carne     = Shucked.Weight,
    peso_visceras  = Viscera.Weight,
    peso_caparazon = Shell.Weight,
    edad           = Age
  ) %>%
  mutate(
    altura = ifelse(altura == 0, NA, altura),
    altura = ifelse(is.na(altura), mean(altura, na.rm = TRUE), altura),
    sexo = factor(sexo,
                  levels = c("F", "M", "I"),
                  labels = c("Hembra", "Macho", "Indeterminado"))
  )
```

## Creación de las variables categóricas

**Grupo de edad.** Para clasificar a los cangrejos según su edad usamos los cuartiles de la variable `edad`:


```r
quantile(df$edad, c(0.25, 0.75))
```

```
## 25% 75% 
##   8  11
```

Con estos valores se definen tres grupos: *Joven* para los cangrejos de hasta $8$ meses (hasta el primer cuartil), *Adulto* para los que tienen entre $9$ y $11$ meses (entre el primer y el tercer cuartil) y *Mayor* para los de $12$ meses o más (por encima del tercer cuartil).

**Grupo de tamaño.** Para el tamaño usamos el largo del cangrejo dividido en terciles, de forma que cada grupo tenga aproximadamente la tercera parte de los datos:


```r
quantile(df$largo, c(0, 1/3, 2/3, 1))
```

```
##        0% 33.33333% 66.66667%      100% 
##  0.187500  1.250000  1.487500  2.012815
```

Así, los cangrejos con largo hasta $1{,}25$ pies son *Pequeños*, los que están entre $1{,}25$ y $1{,}4875$ pies son *Medianos* y los de más de $1{,}4875$ pies son *Grandes*. Creemos las dos variables:


```r
df <- df %>%
  mutate(
    grupo_edad = cut(edad,
                     breaks = c(-Inf, 8, 11, Inf),
                     labels = c("Joven", "Adulto", "Mayor")),
    grupo_tamano = cut(largo,
                       breaks = quantile(largo, c(0, 1/3, 2/3, 1)),
                       include.lowest = TRUE,
                       labels = c("Pequeño", "Mediano", "Grande"))
  )
```

Veamos la frecuencia de cada grupo:


```r
df %>%
  count(grupo_edad, name = "Frecuencia") %>%
  mutate(Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 2))
```

```
##   grupo_edad Frecuencia Porcentaje
## 1      Joven      24176      32.65
## 2     Adulto      32699      44.16
## 3      Mayor      17176      23.19
```


```r
df %>%
  count(grupo_tamano, name = "Frecuencia") %>%
  mutate(Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 2))
```

```
##     grupo_tamano Frecuencia Porcentaje
## 1 Peque<U+00F1>o      25914      34.99
## 2        Mediano      23987      32.39
## 3         Grande      24150      32.61
```

El grupo *Adulto* es el más grande, con el $44{,}16\%$ de los cangrejos, mientras que los *Jóvenes* son el $32{,}65\%$ y los *Mayores* el $23{,}19\%$. Los grupos no quedan exactamente en $25\%$, $50\%$ y $25\%$ porque la edad solo toma valores enteros y hay muchos cangrejos con la misma edad justo en los puntos de corte. Por la misma razón, los grupos de tamaño tampoco tienen exactamente un tercio cada uno ($34{,}99\%$, $32{,}39\%$ y $32{,}61\%$), aunque están bastante balanceados.

## Prueba de homogeneidad

Se quiere saber si la distribución de los grupos de edad es la misma en las hembras, los machos y los indeterminados. En este caso, cada sexo se considera una población distinta ($I = 3$) y el grupo de edad es la variable categórica común ($J = 3$). Si $p_{ij}$ es la proporción de cangrejos de la población $i$ que caen en el grupo de edad $j$, las hipótesis son:

$$
H_0: p_{1j} = p_{2j} = p_{3j}, \quad \text{para cada } j = 1, 2, 3
$$

$$
H_1: \text{al menos una de las proporciones es diferente}
$$

<div class="admon-box teorema">
<div class="admon-title"><span class="icon"></span> Teorema (prueba de homogeneidad)</div>

Bajo $H_0$, el número esperado en cada celda es

$$
\hat{E}_{ij} = \frac{R_i C_j}{n},
$$

donde $R_i$ y $C_j$ son los totales de la fila $i$ y la columna $j$. Si todos los $\hat{E}_{ij} \geq 5$, se rechaza $H_0$ cuando

$$
\chi^2 = \sum_{i=1}^{I} \sum_{j=1}^{J} \frac{(O_{ij} - \hat{E}_{ij})^2}{\hat{E}_{ij}} > \chi^2_\alpha,
$$

donde $\chi^2_\alpha$ es el valor crítico de la distribución chi-cuadrado con $(I - 1)(J - 1)$ grados de libertad.

</div>

### Solución paso a paso

Construyamos la tabla de contingencia con las frecuencias observadas:


```r
observados <- table(Sexo = df$sexo, Edad = df$grupo_edad)
observados
```

```
##                Edad
## Sexo            Joven Adulto Mayor
##   Hembra         2242  12562  8206
##   Macho          3885  15314  7885
##   Indeterminado 18049   4823  1085
```

Calculemos los totales por fila, columna y el total general:


```r
fila_totales <- rowSums(observados)
col_totales <- colSums(observados)
total_general <- sum(observados)

fila_totales
```

```
##        Hembra         Macho Indeterminado 
##         23010         27084         23957
```

```r
col_totales
```

```
##  Joven Adulto  Mayor 
##  24176  32699  17176
```

```r
total_general
```

```
## [1] 74051
```

Ahora, hallemos las frecuencias esperadas bajo $H_0$:


```r
esperados <- outer(fila_totales, col_totales, FUN = function(f, c) f * c / total_general)
dimnames(esperados) <- dimnames(observados)
round(esperados, 2)
```

```
##                Edad
## Sexo              Joven   Adulto   Mayor
##   Hembra        7512.25 10160.62 5337.13
##   Macho         8842.32 11959.59 6282.09
##   Indeterminado 7821.43 10578.79 5556.78
```

Verifiquemos que todas las frecuencias esperadas son mayores o iguales a $5$:


```r
all(esperados >= 5)
```

```
## [1] TRUE
```

La condición se cumple, pues la frecuencia esperada más pequeña es de $5337{,}13$. Seguido calculemos el estadístico $\chi^2$:


```r
chi_cuadrado <- sum((observados - esperados)^2 / esperados)
chi_cuadrado
```

```
## [1] 30040.34
```

Determinemos los grados de libertad:


```r
gl <- (nrow(observados) - 1) * (ncol(observados) - 1)
gl
```

```
## [1] 4
```

Ahora, el valor crítico y la decisión al $5\%$:


```r
alfa <- 0.05
valor_critico <- qchisq(1 - alfa, gl)
valor_critico
```

```
## [1] 9.487729
```


```r
rechaza_H0 <- chi_cuadrado > valor_critico
rechaza_H0
```

```
## [1] TRUE
```

Concluimos que


```r
if (rechaza_H0) {
  conclusion <- "Se rechaza H0: la distribución de los grupos de edad no es la misma en los tres sexos."
} else {
  conclusion <- "No se rechaza H0: no hay evidencia suficiente para afirmar que la distribución de los grupos de edad cambia según el sexo."
}
conclusion
```

```
## [1] "Se rechaza H0: la distribuci<U+00F3>n de los grupos de edad no es la misma en los tres sexos."
```

Si calculamos el p-valor


```r
valor_p <- pchisq(chi_cuadrado, gl, lower.tail = FALSE)
valor_p
```

```
## [1] 0
```

El p-valor es tan pequeño que R lo muestra como cero.

### Prueba con la función `chisq.test()`


```r
chisq.test(observados)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  observados
## X-squared = 30040, df = 4, p-value < 2.2e-16
```

La prueba chi-cuadrado indica que, con una confianza del $95\%$, la distribución de los grupos de edad **no es homogénea** entre hembras, machos e indeterminados ($\chi^2_{(4)} = 30040,\ p\text{-valor} < 0{,}001$). El resultado coincide con el cálculo manual.

Si se mira la tabla de frecuencias observadas, se nota de dónde viene la diferencia: en las hembras y en los machos la mayoría de los cangrejos son adultos ($12.562$ de $23.010$ y $15.314$ de $27.084$, respectivamente), mientras que en los indeterminados la gran mayoría son jóvenes ($18.049$ de $23.957$). Esto es consistente con lo encontrado en el capítulo \@ref(estparynopar), donde los indeterminados resultaron ser el grupo más joven.

## Prueba de independencia

Ahora se quiere saber si el grupo de edad de un cangrejo está asociado con su tamaño. En este caso se tiene una sola población (todos los cangrejos de la base) y cada uno se clasifica según dos atributos: el grupo de edad ($A$, con $I = 3$ categorías) y el grupo de tamaño ($B$, con $J = 3$ categorías). Las hipótesis son:

$$
H_0: p_{ij} = p_{i\cdot} \, p_{\cdot j} \quad \text{para todo } i, j \quad \text{(el grupo de edad y el tamaño son independientes)}
$$

$$
H_1: \text{el grupo de edad y el tamaño no son independientes}
$$

<div class="admon-box teorema">
<div class="admon-title"><span class="icon"></span> Teorema (prueba de independencia)</div>

Bajo $H_0$, el número esperado de observaciones en cada celda es

$$
\hat{E}_{ij} = n \, \hat{p}_{i\cdot} \, \hat{p}_{\cdot j} = n \cdot \frac{R_i}{n} \cdot \frac{C_j}{n} = \frac{R_i C_j}{n}.
$$

Si todos los $\hat{E}_{ij} \geq 5$, se rechaza $H_0$ al nivel $\alpha$ cuando $\chi^2 > \chi^2_\alpha$, con $(I-1)(J-1)$ grados de libertad.

</div>

### Solución paso a paso

Construyamos la tabla de contingencia:


```r
observados2 <- table(Edad = df$grupo_edad, Tamano = df$grupo_tamano)
observados2
```

```
##         Tamano
## Edad     Peque<U+00F1>o Mediano Grande
##   Joven           19166    4239    771
##   Adulto           4996   13548  14155
##   Mayor            1752    6200   9224
```

Hallemos los totales y los valores esperados:


```r
fila_totales2 <- rowSums(observados2)
col_totales2 <- colSums(observados2)
n <- sum(observados2)

esperados2 <- outer(fila_totales2, col_totales2) / n
round(esperados2, 2)
```

```
##        Peque<U+00F1>o  Mediano   Grande
## Joven         8460.34  7831.22  7884.44
## Adulto       11442.95 10592.04 10664.01
## Mayor         6010.71  5563.74  5601.55
```


```r
all(esperados2 >= 5)
```

```
## [1] TRUE
```

Todas las frecuencias esperadas son mayores a $5$ (la menor es $5563{,}74$). Calculemos el estadístico chi-cuadrado y los grados de libertad:


```r
chi_cuadrado2 <- sum((observados2 - esperados2)^2 / esperados2)
chi_cuadrado2
```

```
## [1] 32645.15
```


```r
gl2 <- (nrow(observados2) - 1) * (ncol(observados2) - 1)
gl2
```

```
## [1] 4
```

Ahora, el valor crítico y la decisión al $5\%$:


```r
valor_critico2 <- qchisq(1 - alfa, gl2)
valor_critico2
```

```
## [1] 9.487729
```


```r
rechaza_H0_2 <- chi_cuadrado2 > valor_critico2
rechaza_H0_2
```

```
## [1] TRUE
```

Si calculamos el p-valor


```r
valor_p2 <- pchisq(chi_cuadrado2, gl2, lower.tail = FALSE)
valor_p2
```

```
## [1] 0
```

### Prueba con la función `chisq.test()`


```r
chisq.test(observados2)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  observados2
## X-squared = 32645, df = 4, p-value < 2.2e-16
```

La prueba chi-cuadrado nos indica que el grupo de edad y el grupo de tamaño **no son independientes** ($\chi^2_{(4)} = 32645,\ p\text{-valor} < 0{,}001$). Es decir, el tamaño de un cangrejo está asociado con su grupo de edad.

En la tabla de frecuencias observadas se ve la asociación: de los $24.176$ cangrejos jóvenes, $19.166$ son pequeños y solo $771$ son grandes, mientras que de los $17.176$ cangrejos mayores, $9.224$ son grandes. Sin embargo, entre los mayores también hay $1.752$ cangrejos pequeños y $6.200$ medianos, lo que muestra que el tamaño por sí solo no determina la edad. Esto coincide con lo visto en los diagramas de dispersión del capítulo \@ref(eda), donde cangrejos del mismo largo podían tener edades muy distintas.
