# Análisis de estadística paramétrica y no paramétrica {#estparynopar}

En el capítulo anterior se vio que la edad de los cangrejos cambia según el sexo: las hembras y los machos tienen edades parecidas, mientras que los indeterminados son claramente más jóvenes. En este capítulo se verifica si esas diferencias son estadísticamente significativas. Primero se comparan dos grupos (hembras y machos) con la prueba *t* y con la U de Mann-Whitney, y luego se comparan los tres grupos con el ANOVA de un factor y con la prueba de Kruskal-Wallis.

En todas las pruebas se trabaja con un nivel de significancia de $\alpha = 0{,}05$. Carguemos las librerías y preparemos los datos con la misma limpieza del capítulo \@ref(eda):


```r
library(tidyverse)
library(nortest)
library(car)
library(effsize)
library(rstatix)

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

head(df)
```

```
##            sexo  largo diametro altura     peso peso_carne peso_visceras
## 1 Indeterminado 1.5250   1.1750 0.3750 28.97319  12.728926      6.647958
## 2 Indeterminado 1.1000   0.8250 0.2750 10.41844   4.521745      2.324659
## 3         Macho 1.3875   1.1125 0.3750 24.77746  11.339800      5.556502
## 4        Hembra 1.7000   1.4125 0.5000 50.66056  20.354941     10.991839
## 5 Indeterminado 1.2500   1.0125 0.3375 23.28911  11.977664      4.507570
## 6         Macho 1.5000   1.1750 0.4125 28.84562  13.409313      6.789705
##   peso_caparazon edad
## 1       8.348928    9
## 2       3.401940    8
## 3       6.662133    9
## 4      14.996885   11
## 5       5.953395    8
## 6       7.937860   10
```

## Comparación de la edad entre hembras y machos con estadística paramétrica

Se quiere verificar si existe una diferencia significativa en la edad promedio de los cangrejos hembra y macho, y si esa diferencia es relevante desde el punto de vista práctico. Sean $\mu_1$ la edad promedio de las hembras y $\mu_2$ la edad promedio de los machos. Las hipótesis son:

$$
H_0: \mu_1 = \mu_2 \quad \text{vs} \quad H_a: \mu_1 \ne \mu_2
$$

Separemos las edades de cada grupo:


```r
# edades de las hembras
hembras <- df %>%
  filter(sexo == "Hembra") %>%
  pull(edad)

# edades de los machos
machos <- df %>%
  filter(sexo == "Macho") %>%
  pull(edad)

length(hembras)
```

```
## [1] 23010
```

```r
length(machos)
```

```
## [1] 27084
```

### Verificación de supuestos

La **independencia** se cumple porque cada registro corresponde a un cangrejo distinto y un cangrejo no puede ser hembra y macho a la vez.

Para la **normalidad** no se puede usar la prueba de Shapiro-Wilk, ya que cada grupo tiene más de $5.000$ observaciones. Por eso usamos las pruebas de Kolmogorov-Smirnov y de Lilliefors:


```r
df %>%
  filter(sexo %in% c("Hembra", "Macho")) %>%
  group_by(sexo) %>%
  summarise(n = length(edad),
            est_ks = ks.test(scale(edad), "pnorm")$statistic,
            p_ks = ks.test(scale(edad), "pnorm")$p.value,
            est_lt = lillie.test(edad)$statistic,
            p_lt = lillie.test(edad)$p.value)
```

```
## # A tibble: 2 x 6
##   sexo       n est_ks  p_ks est_lt  p_lt
##   <fct>  <int>  <dbl> <dbl>  <dbl> <dbl>
## 1 Hembra 23010  0.189     0  0.189     0
## 2 Macho  27084  0.194     0  0.194     0
```

Con una confianza del $95\%$, las pruebas de Kolmogorov-Smirnov y de Lilliefors indican que la edad no sigue una distribución normal ni en las hembras ($D = 0{,}189,\ p\text{-valor} < 0{,}001$) ni en los machos ($D = 0{,}194,\ p\text{-valor} < 0{,}001$). El estadístico $D$ es el mismo en ambas pruebas porque las dos miden la máxima distancia entre la distribución empírica y la normal; la diferencia está en que Lilliefors corrige el p-valor por haber estimado la media y la desviación estándar con la misma muestra. Además, R advierte que hay empates, ya que la edad solo toma valores enteros.

Aunque la normalidad no se cumple, cada grupo tiene más de $23.000$ observaciones, así que por el Teorema del Límite Central la prueba *t* sigue siendo válida para comparar las medias. De todas formas, en la sección \@ref(mw-hembras-machos) se hace también la comparación no paramétrica.

Verifiquemos la **igualdad de varianzas** (homocedasticidad):


```r
var.test(hembras, machos)
```

```
## 
## 	F test to compare two variances
## 
## data:  hembras and machos
## F = 1.0524, num df = 23009, denom df = 27083, p-value = 5.539e-05
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  1.026591 1.078906
## sample estimates:
## ratio of variances 
##            1.05241
```

Con una confianza del $95\%$, hay diferencias estadísticamente significativas en las varianzas de la edad de hembras y machos ($F_{(23009,\,27083)} = 1{,}0524,\ p\text{-valor} < 0{,}001$). Hay que notar que la razón de varianzas es muy cercana a $1$ y que la diferencia sale significativa principalmente por el tamaño tan grande de las muestras.

<div class="admon-box teorema">
<div class="admon-title"><span class="icon"></span> Teorema (razón de varianzas)</div>

Si $s_1^2$ y $s_2^2$ son las varianzas de muestras aleatorias independientes de tamaño $n_1$ y $n_2$, tomadas de poblaciones normales con varianzas $\sigma_1^2$ y $\sigma_2^2$, entonces el estadístico

$$
F = \frac{s_1^2}{s_2^2}
$$

sigue una distribución $F$ con $v_1 = n_1 - 1$ y $v_2 = n_2 - 1$ grados de libertad, y un intervalo de confianza al nivel $1 - \alpha$ para $\sigma_1^2 / \sigma_2^2$ es

$$
\left( \frac{s_1^2}{s_2^2} \cdot \frac{1}{F_{1 - \alpha/2}(v_1, v_2)},\ \frac{s_1^2}{s_2^2} \cdot \frac{1}{F_{\alpha/2}(v_1, v_2)} \right)
$$

</div>

Verifiquemos la salida de `var.test()` paso a paso con este teorema:


```r
n1 <- length(hembras)
n2 <- length(machos)
s1_2 <- var(hembras)
s2_2 <- var(machos)

# estadístico F
F_calc <- s1_2 / s2_2

# grados de libertad
v1 <- n1 - 1
v2 <- n2 - 1

# intervalo de confianza del 95%
alpha <- 0.05
IC_F <- c(F_calc / qf(1 - alpha/2, v1, v2),
          F_calc / qf(alpha/2, v1, v2))

# p-valor bilateral
p_F <- 2 * min(pf(F_calc, v1, v2), 1 - pf(F_calc, v1, v2))

data.frame(F_calc = F_calc, v1 = v1, v2 = v2,
           IC_inf = IC_F[1], IC_sup = IC_F[2], p_valor = p_F)
```

```
##    F_calc    v1    v2   IC_inf   IC_sup      p_valor
## 1 1.05241 23009 27083 1.026591 1.078906 5.538777e-05
```

Los valores coinciden con los de `var.test()`: $F = 1{,}0524$, intervalo de confianza $(1{,}0266;\ 1{,}0789)$ y p-valor menor a $0{,}001$. Como el intervalo no contiene al $1$, se rechaza la igualdad de varianzas.

### Prueba *t* de Welch

Como no se cumple la homocedasticidad, se usa la versión de Welch de la prueba *t*, que no asume varianzas iguales. En este caso, el error estándar de la diferencia de medias y los grados de libertad son:

$$
SE = \sqrt{ \frac{s_1^2}{n_1} + \frac{s_2^2}{n_2} }, \qquad
gl \approx \frac{ \left( \frac{s_1^2}{n_1} + \frac{s_2^2}{n_2} \right)^2 }{ \frac{ \left( \frac{s_1^2}{n_1} \right)^2 }{n_1 - 1} + \frac{ \left( \frac{s_2^2}{n_2} \right)^2 }{n_2 - 1} }
$$

y el estadístico de prueba es $t = \dfrac{\bar{x}_1 - \bar{x}_2}{SE}$. Hagamos el cálculo paso a paso:


```r
xbar_1 <- mean(hembras)
xbar_2 <- mean(machos)

# error estándar de la diferencia de medias
SE <- sqrt(s1_2/n1 + s2_2/n2)

# estadístico t
t_calc <- (xbar_1 - xbar_2) / SE

# grados de libertad de Welch
gl <- (s1_2/n1 + s2_2/n2)^2 /
      ((s1_2/n1)^2/(n1 - 1) + (s2_2/n2)^2/(n2 - 1))

# valor crítico, p-valor e intervalo de confianza
t_critico <- qt(1 - alpha/2, df = gl)
p_t <- 2 * pt(-abs(t_calc), df = gl)
IC_t <- (xbar_1 - xbar_2) + c(-1, 1) * t_critico * SE

data.frame(media_hembras = xbar_1, media_machos = xbar_2, SE = SE,
           t_calc = t_calc, gl = gl, t_critico = t_critico,
           p_valor = p_t, IC_inf = IC_t[1], IC_sup = IC_t[2])
```

```
##   media_hembras media_machos         SE   t_calc       gl t_critico
## 1      11.34155     10.89595 0.02620881 17.00168 48371.49  1.960013
##        p_valor    IC_inf    IC_sup
## 1 1.230463e-64 0.3942242 0.4969634
```


```r
decision <- ifelse(abs(t_calc) > t_critico, "Se rechaza H_0", "No se rechaza H_0")
decision
```

```
## [1] "Se rechaza H_0"
```

Ahora comparemos con la función `t.test()`:


```r
t.test(hembras,
       machos,
       paired = FALSE,         # muestras independientes
       var.equal = FALSE       # varianzas diferentes (Welch)
       )
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  hembras and machos
## t = 17.002, df = 48371, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.3942242 0.4969634
## sample estimates:
## mean of x mean of y 
##  11.34155  10.89595
```

La prueba de Welch para muestras independientes nos indica que, con una confianza del $95\%$, existe una diferencia estadísticamente significativa entre la edad promedio de las hembras y la de los machos ($t_{(48371{,}49)} = 17{,}002,\ p\text{-valor} < 0{,}001$). Las hembras tienen en promedio $11{,}34$ meses y los machos $10{,}90$ meses, y el intervalo de confianza indica que la diferencia real está entre $0{,}39$ y $0{,}50$ meses. Los resultados del cálculo manual coinciden con los de la función.

### Tamaño del efecto

Con muestras tan grandes era de esperarse que la diferencia fuera significativa, así que lo importante es saber si esa diferencia tiene relevancia práctica. Veamos el cálculo del tamaño del efecto:


```r
cohen.d(hembras,
        machos,
        paired = FALSE)
```

```
## 
## Cohen's d
## 
## d estimate: 0.1527468 (negligible)
## 95 percent confidence interval:
##     lower     upper 
## 0.1351487 0.1703448
```

Hagamos también el cálculo del tamaño del efecto $r$ a partir del estadístico $t$, usando $r = \sqrt{\dfrac{t^2}{t^2 + gl}}$:


```r
r_efecto <- sqrt(t_calc^2 / (t_calc^2 + gl))
r_efecto
```

```
## [1] 0.07707321
```

Con base en el índice de Cohen's d, se obtiene un valor estimado de $0{,}15$ ($IC_{95\%} = (0{,}14;\ 0{,}17)$), que corresponde a un tamaño del efecto **trivial o muy pequeño** ($|d| < 0{,}20$). El coeficiente $r = 0{,}077$ lleva a la misma conclusión, porque es menor a $0{,}10$. Esto quiere decir que, aunque la diferencia de edad entre hembras y machos es estadísticamente significativa, en la práctica es muy pequeña: equivale a menos de medio mes y a apenas $0{,}15$ desviaciones estándar. La significancia se explica sobre todo por el gran tamaño de las muestras.

## Comparación de la edad entre hembras y machos con estadística no paramétrica {#mw-hembras-machos}

Como la edad no sigue una distribución normal, es conveniente hacer también la comparación con la prueba de Mann-Whitney-Wilcoxon, que trabaja con los rangos de los datos. Las hipótesis son:

$$
H_0: P(X > Y) = 0{,}5 \quad \text{vs} \quad H_1: P(X > Y) \neq 0{,}5
$$

donde $X$ es la edad de una hembra y $Y$ la edad de un macho, ambos elegidos al azar.

Los supuestos de la prueba se cumplen: las observaciones son independientes, la edad es una variable que se puede ordenar y no se requiere normalidad. Para poder interpretar el resultado como una comparación de medianas, las dos distribuciones deben tener una forma parecida. Como no hay normalidad, la igualdad de varianzas se revisa con la prueba de Levene centrada en la mediana:


```r
df_hm <- df %>%
  filter(sexo %in% c("Hembra", "Macho")) %>%
  droplevels()

leveneTest(edad ~ sexo, data = df_hm, center = "median")
```

```
## Levene's Test for Homogeneity of Variance (center = "median")
##          Df F value    Pr(>F)    
## group     1  27.133 1.907e-07 ***
##       50092                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

La prueba de Levene indica que hay diferencias significativas en la varianza de la edad entre hembras y machos ($F_{(1,\,50092)} = 27{,}133,\ p\text{-valor} < 0{,}001$). Por lo tanto, el resultado de Mann-Whitney se debe leer como una diferencia en la distribución de la edad y no estrictamente como una diferencia de medianas.

Veamos si hay diferencia entre los dos grupos:


```r
df_hm %>%
  wilcox_test(edad ~ sexo,
              paired = FALSE)
```

```
## # A tibble: 1 x 7
##   .y.   group1 group2    n1    n2  statistic        p
## * <chr> <chr>  <chr>  <int> <int>      <dbl>    <dbl>
## 1 edad  Hembra Macho  23010 27084 341462630. 3.63e-78
```

La prueba U de Mann-Whitney muestra que hay evidencia estadísticamente significativa de una diferencia en la edad de los cangrejos hembra y macho ($U = 341.462.630,\ p\text{-valor} < 0{,}001$). Veamos el tamaño del efecto:


```r
df_hm %>%
  wilcox_effsize(edad ~ sexo, paired = FALSE)
```

```
## # A tibble: 1 x 7
##   .y.   group1 group2 effsize    n1    n2 magnitude
## * <chr> <chr>  <chr>    <dbl> <int> <int> <ord>    
## 1 edad  Hembra Macho   0.0836 23010 27084 small
```

El tamaño del efecto es $r = 0{,}084$, que la función clasifica como pequeño, aunque está por debajo de $0{,}10$ y en la escala de las notas de clase corresponde a un efecto trivial. Así, la prueba no paramétrica confirma lo encontrado con la prueba *t*: las hembras tienden a ser un poco mayores que los machos, pero la diferencia es muy pequeña en términos prácticos.

## ANOVA de la edad según el sexo

Ahora comparemos la edad promedio de los tres grupos. Sean $\mu_1$, $\mu_2$ y $\mu_3$ las edades promedio de las hembras, los machos y los indeterminados, respectivamente. Las hipótesis son:

$$
\begin{aligned}
H_0: &\quad \mu_1 = \mu_2 = \mu_3 \quad \text{(todas las medias son iguales)} \\
H_1: &\quad \text{Al menos una media difiere de las demás}
\end{aligned}
$$

### Verificación de supuestos

Verifiquemos la normalidad en cada grupo:


```r
df %>%
  group_by(sexo) %>%
  summarise(n = length(edad),
            est_ks = ks.test(scale(edad), "pnorm")$statistic,
            p_ks = ks.test(scale(edad), "pnorm")$p.value,
            est_lt = lillie.test(edad)$statistic,
            p_lt = lillie.test(edad)$p.value)
```

```
## # A tibble: 3 x 6
##   sexo              n est_ks  p_ks est_lt  p_lt
##   <fct>         <int>  <dbl> <dbl>  <dbl> <dbl>
## 1 Hembra        23010  0.189     0  0.189     0
## 2 Macho         27084  0.194     0  0.194     0
## 3 Indeterminado 23957  0.182     0  0.182     0
```

La prueba de Lilliefors muestra que la edad no tiene comportamiento normal en ninguno de los tres grupos ($p\text{-valor} < 0{,}001$ en todos los casos). Probemos la homogeneidad de varianzas con las pruebas de Bartlett y de Levene:


```r
bartlett.test(edad ~ sexo, data = df)
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  edad by sexo
## Bartlett's K-squared = 2195.3, df = 2, p-value < 2.2e-16
```


```r
leveneTest(edad ~ sexo, data = df, center = "median")
```

```
## Levene's Test for Homogeneity of Variance (center = "median")
##          Df F value    Pr(>F)    
## group     2  501.64 < 2.2e-16 ***
##       74048                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Tanto la prueba de Bartlett ($K^2_{(2)} = 2195{,}3,\ p\text{-valor} < 0{,}001$) como la de Levene ($F_{(2,\,74048)} = 501{,}64,\ p\text{-valor} < 0{,}001$) indican que las varianzas no son iguales. La prueba de Bartlett es sensible a la falta de normalidad, por lo que en este caso es más confiable la de Levene, pero ambas llegan a la misma conclusión. Esto se ve también en el análisis exploratorio, donde los indeterminados tienen una dispersión mucho menor que los otros dos grupos.

<div class="admon-box advertencia">
<div class="admon-title"><span class="icon"></span> Advertencia</div>

Los supuestos de normalidad y homocedasticidad no se cumplen. El ANOVA es bastante robusto a la falta de normalidad cuando los grupos son grandes, pero la diferencia de varianzas puede afectar el resultado. Por eso el ANOVA se desarrolla a continuación con fines de comparación, y la conclusión principal se apoya en la prueba de Kruskal-Wallis de la sección \@ref(kw-sexo).

</div>

### Cálculo paso a paso

Usando la notación de las notas de clase, $y_{ij}$ es la $i$-ésima observación del grupo $j$, $n_j$ el tamaño del grupo $j$, $N$ el total de observaciones, $\bar{y}_j$ la media del grupo $j$ y $\bar{y}$ la media global. Hallemos los tamaños y los promedios de cada muestra:


```r
edad_h <- df %>% filter(sexo == "Hembra") %>% pull(edad)
edad_m <- df %>% filter(sexo == "Macho") %>% pull(edad)
edad_i <- df %>% filter(sexo == "Indeterminado") %>% pull(edad)

n1 <- length(edad_h)         # tamaño de muestra n_1
n2 <- length(edad_m)         # tamaño de muestra n_2
n3 <- length(edad_i)         # tamaño de muestra n_3
N <- n1 + n2 + n3            # tamaño muestral total
k <- nlevels(df$sexo)        # número de poblaciones

ybar_1 <- mean(edad_h)
ybar_2 <- mean(edad_m)
ybar_3 <- mean(edad_i)

ybar_total <- (sum(edad_h) + sum(edad_m) + sum(edad_i)) / N

data.frame(n1, n2, n3, N, k, ybar_1, ybar_2, ybar_3, ybar_total)
```

```
##      n1    n2    n3     N k   ybar_1   ybar_2   ybar_3 ybar_total
## 1 23010 27084 23957 74051 3 11.34155 10.89595 7.599073   9.967806
```

Hallemos la suma de cuadrados total (SST), la suma de cuadrados entre grupos (SSA) y la suma de cuadrados dentro de los grupos (SSE):

$$
SST = \sum_{j=1}^{k} \sum_{i=1}^{n_j} (y_{ij} - \bar{y})^2, \qquad
SSA = \sum_{j=1}^{k} n_j (\bar{y}_j - \bar{y})^2, \qquad
SSE = \sum_{j=1}^{k} \sum_{i=1}^{n_j} (y_{ij} - \bar{y}_j)^2
$$


```r
SST <- sum((edad_h - ybar_total)^2) +
       sum((edad_m - ybar_total)^2) +
       sum((edad_i - ybar_total)^2)

SSA <- n1 * (ybar_1 - ybar_total)^2 +
       n2 * (ybar_2 - ybar_total)^2 +
       n3 * (ybar_3 - ybar_total)^2

SSE <- SST - SSA

data.frame(SST, SSA, SSE)
```

```
##        SST      SSA      SSE
## 1 746559.2 201175.6 545383.7
```

Por el teorema de descomposición, $SST = SSE + SSA$. Comprobemos que la $SSE$ obtenida por diferencia es igual a la que se obtiene con su definición, que también se puede escribir como $SSE = \sum_{j=1}^{k}(n_j - 1)S_j^2$:


```r
SSE_definicion <- sum((edad_h - ybar_1)^2) +
                  sum((edad_m - ybar_2)^2) +
                  sum((edad_i - ybar_3)^2)

SSE_varianzas <- (n1 - 1) * var(edad_h) +
                 (n2 - 1) * var(edad_m) +
                 (n3 - 1) * var(edad_i)

data.frame(SSE, SSE_definicion, SSE_varianzas)
```

```
##        SSE SSE_definicion SSE_varianzas
## 1 545383.7       545383.7      545383.7
```

Los tres valores son iguales, lo que confirma el teorema de descomposición. Calculemos los cuadrados medios y el estadístico de Fisher:

$$
MSA = \frac{SSA}{k - 1}, \qquad MSE = \frac{SSE}{N - k}, \qquad F = \frac{MSA}{MSE}
$$


```r
MSA <- SSA / (k - 1)
MSE <- SSE / (N - k)

F_anova <- MSA / MSE

data.frame(MSA, MSE, F_anova)
```

```
##        MSA      MSE  F_anova
## 1 100587.8 7.365272 13657.04
```

Hallemos el $F$ crítico y veamos si se rechaza o no la hipótesis nula:


```r
alpha <- 0.05
v1 <- k - 1
v2 <- N - k

F_critico <- qf(p = 1 - alpha, df1 = v1, df2 = v2)
F_critico
```

```
## [1] 2.995853
```

```r
decision <- ifelse(F_anova > F_critico, "Se rechaza H_0", "No se rechaza H_0")
decision
```

```
## [1] "Se rechaza H_0"
```

Otra forma de realizarlo es con el p-valor:


```r
pvalor <- pf(F_anova, v1, v2, lower.tail = FALSE)
pvalor
```

```
## [1] 0
```

```r
decision <- ifelse(pvalor < alpha, "Se rechaza H_0", "No se rechaza H_0")
decision
```

```
## [1] "Se rechaza H_0"
```

Organicemos los resultados en la tabla de ANOVA de un factor:


```r
data.frame(
  Fuente = c("Entre grupos", "Dentro de grupos", "Total"),
  gl = c(v1, v2, N - 1),
  SC = c(SSA, SSE, SST),
  CM = c(MSA, MSE, NA),
  F = c(F_anova, NA, NA)
)
```

```
##             Fuente    gl       SC           CM        F
## 1     Entre grupos     2 201175.6 1.005878e+05 13657.04
## 2 Dentro de grupos 74048 545383.7 7.365272e+00       NA
## 3            Total 74050 746559.2           NA       NA
```

### ANOVA con la función `aov()`


```r
modelo <- aov(edad ~ sexo, data = df)
summary(modelo)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)    
## sexo            2 201176  100588   13657 <2e-16 ***
## Residuals   74048 545384       7                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

La función `aov()` da los mismos resultados que el cálculo manual. El ANOVA muestra que existen diferencias significativas en la edad promedio de al menos uno de los tres grupos ($F_{(2,\,74048)} = 13657{,}04,\ p\text{-valor} < 0{,}001$). El estadístico $F$ es enorme comparado con el valor crítico ($3{,}00$), lo que indica que la variabilidad entre grupos es mucho mayor que la variabilidad dentro de los grupos. De hecho, la suma de cuadrados entre grupos representa el $26{,}9\%$ de la variabilidad total de la edad ($SSA/SST = 201175{,}6/746559{,}2$).

### Comparaciones múltiples con el método de Tukey

El ANOVA no dice cuáles grupos son diferentes, así que aplicamos el método de Tukey. Como los tamaños de muestra son distintos, la diferencia honestamente significativa para cada par de grupos $(i, j)$ es:

$$
HSD = q_{\alpha}(k, N - k) \cdot \sqrt{CME \left( \frac{1}{n_i} + \frac{1}{n_j} \right)}
$$

y dos medias son significativamente diferentes si la diferencia entre ellas es mayor que el $HSD$. Hagamos el cálculo:


```r
q <- qtukey(p = 1 - alpha, nmeans = k, df = N - k)
q
```

```
## [1] 3.314493
```

```r
medias <- c(ybar_1, ybar_2, ybar_3)
tamanos <- c(n1, n2, n3)
grupos <- c("Hembra", "Macho", "Indeterminado")

# Matriz de comparaciones por pares
comparaciones <- combn(1:k, 2, function(idx) {
  i <- idx[1]
  j <- idx[2]
  dif <- medias[i] - medias[j]
  HSD <- q * sqrt(MSE * (1 / tamanos[i] + 1 / tamanos[j]))
  c(Grupo_1 = grupos[i],
    Grupo_2 = grupos[j],
    Diferencia = round(dif, 4),
    HSD = round(HSD, 4),
    Significativa = ifelse(abs(dif) > HSD, "Sí", "No"))
})

comparacion_tukey <- as.data.frame(t(comparaciones), stringsAsFactors = FALSE)
names(comparacion_tukey) <- c("Grupo 1", "Grupo 2", "Diferencia", "HSD", "¿Significativa?")

comparacion_tukey
```

```
##   Grupo 1       Grupo 2 Diferencia    HSD <U+00BF>Significativa?
## 1  Hembra         Macho     0.4456 0.0806              S<U+00ED>
## 2  Hembra Indeterminado     3.7425  0.083              S<U+00ED>
## 3   Macho Indeterminado     3.2969 0.0798              S<U+00ED>
```

Ahora con la función `TukeyHSD()`:


```r
TukeyHSD(modelo)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = edad ~ sexo, data = df)
## 
## $sexo
##                            diff        lwr        upr p adj
## Macho-Hembra         -0.4455938 -0.5026201 -0.3885676     0
## Indeterminado-Hembra -3.7424738 -3.8011847 -3.6837630     0
## Indeterminado-Macho  -3.2968800 -3.3532935 -3.2404664     0
```

En los tres casos la diferencia de medias es mucho mayor que el $HSD$, y la función `TukeyHSD()` lleva a la misma conclusión, ya que ninguno de los intervalos contiene el cero. Por lo tanto, **los tres grupos difieren entre sí**. Los indeterminados son en promedio $3{,}74$ meses más jóvenes que las hembras ($IC_{95\%} = (-3{,}80;\ -3{,}68)$) y $3{,}30$ meses más jóvenes que los machos ($IC_{95\%} = (-3{,}35;\ -3{,}24)$). Entre machos y hembras la diferencia es de apenas $0{,}45$ meses ($IC_{95\%} = (-0{,}50;\ -0{,}39)$), que es significativa pero pequeña, como ya se había visto con el tamaño del efecto.

## Kruskal-Wallis de la edad según el sexo {#kw-sexo}

Como no se cumplen los supuestos del ANOVA, la prueba adecuada para comparar los tres grupos es la de Kruskal-Wallis. Las hipótesis son:

- $H_0$: la edad tiene la misma distribución en los tres grupos (hembra, macho e indeterminado).
- $H_A$: al menos un grupo tiene una distribución de edad distinta.

<div class="admon-box teorema">
<div class="admon-title"><span class="icon"></span> Estadístico de Kruskal-Wallis</div>

Se ordenan todas las observaciones de menor a mayor, se les asignan rangos y se calcula

$$
H = \frac{12}{N(N+1)} \sum_{i=1}^{k} \frac{R_i^2}{n_i} - 3(N+1)
$$

donde $N$ es el número total de observaciones, $k$ el número de grupos, $R_i$ la suma de rangos del grupo $i$ y $n_i$ el tamaño del grupo $i$. El estadístico $H$ se compara con una distribución $\chi^2$ con $k - 1$ grados de libertad.

</div>

Veamos si hay diferencias en la edad de los tres grupos:


```r
kruskal.test(edad ~ sexo, data = df)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  edad by sexo
## Kruskal-Wallis chi-squared = 27724, df = 2, p-value < 2.2e-16
```

La prueba de Kruskal-Wallis indica que existen diferencias estadísticamente significativas en la edad entre al menos dos de los grupos ($\chi^2_{(2)} = 27724,\ p\text{-valor} < 0{,}001$).

### Comparaciones post-hoc

Apliquemos una prueba post-hoc para ver qué grupos son diferentes:


```r
pairwise.wilcox.test(x = df$edad,
                     g = df$sexo,
                     exact = FALSE,
                     p.adjust.method = "bonferroni")
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
## 
## data:  df$edad and df$sexo 
## 
##               Hembra Macho 
## Macho         <2e-16 -     
## Indeterminado <2e-16 <2e-16
## 
## P value adjustment method: bonferroni
```

Con base en el ajuste de Bonferroni, se encontraron diferencias estadísticamente significativas en los tres pares de grupos: hembra vs macho, hembra vs indeterminado y macho vs indeterminado ($p < 0{,}001$ en todos los casos). Hagamos también el ajuste de Holm, que es menos conservador:


```r
pairwise.wilcox.test(x = df$edad,
                     g = df$sexo,
                     exact = FALSE,
                     p.adjust.method = "holm")
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
## 
## data:  df$edad and df$sexo 
## 
##               Hembra Macho 
## Macho         <2e-16 -     
## Indeterminado <2e-16 <2e-16
## 
## P value adjustment method: holm
```

Con el ajuste de Holm se llega a la misma conclusión. En resumen, la edad de los cangrejos es distinta en los tres grupos, pero la diferencia importante está en los indeterminados, que son claramente más jóvenes. Entre hembras y machos la diferencia es significativa solo por el tamaño de la muestra, ya que su magnitud es muy pequeña.
