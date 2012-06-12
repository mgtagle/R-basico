> n <- 100
> datos <- data.frame( mi.var = rnorm(n),
                    sexo    = sample( c("H", "M"), n, replace = T),
                    edad    = sample( 20:50, n, replace = T))

> str(datos)
'data.frame':	100 obs. of  3 variables:
 $ mi.var: num  0.5328 -0.1828 -0.7796 0.0171 -0.9623 ...
 $ sexo  : Factor w/ 2 levels "H","M": 1 2 1 2 1 1 1 2 2 1 ...
 $ edad  : int  23 44 43 28 27 37 37 31 26 44 ...

> boxplot(sexo ~ edad, data = datos, ylab = "Sexo")
Error en boxplot.default(split(mf[[response]], mf[-response]), ...) : 
  se agregó una clase"factor" a un objeto no válido

> boxplot(edad ~ sexo, data = datos, ylab = "Edad")

