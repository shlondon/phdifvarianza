Prueba de hipotesis para la diferencia de medias $\mu_1 - \mu_2$

Suponga que se tienen dos muestras aleatorias asi:

$x_1, x_2, \ldots, x_m$ proveniente de una poblacion I con media $\mu_1$ y varianza $\sigma^2_1$.
$y_1, y_2, \ldots, y_n$ proveniente de una poblacion II con media $\mu_2$ y varianza $\sigma^2_2$.
Ambas muestras son independientes entre si.
Se quiere estudiar la hipotesis nula $H_0: \mu_1 - \mu_2 = \Delta_0$ y se sospecha que:

$H_0: \mu_1 - \mu_2 < \Delta_0$
$H_0: \mu_1 - \mu_2 \neq \Delta_0$
$H_0: \mu_1 - \mu_2 > \Delta_0$
El estadistico para realizar la prueba es

$$T=\frac{\bar{x} - \bar{y} - \Delta_0}{\sqrt{\frac{s_1^2}{m} + \frac{s_2^2}{n}}}$$

El estadistico bajo la suposicion de que $H_0$ es verdadera tiene distribucion $t$-student con $v$ grados de libertad calculados como:

$$v=\frac{ \left( \frac{s_1^2}{m} + \frac{s_2^2}{n} \right)^2 }{ \frac{(s_1^2/m)^2}{m-1} + \frac{(s_2^2/n)^2}{n-1}}$$