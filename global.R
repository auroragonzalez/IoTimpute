ver <- "1.5.10" 
options( shiny.sanitize.errors = FALSE )


#--------------CARGAMOS PAQUETES-----------------------------------------###

library(shiny)
library(nplr)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(gstat)
library(geoR)
library(sp)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)


# rutaEjemplo <- "/srv/shiny-server/impBME/DatosEjemplo"
# rutaCode <- "/srv/shiny-server/impBME/code"

rutaEjemplo <-  paste0(getwd(),"/DatosEjemplo")
rutaCode <- paste0(getwd(),"/code")

# Copiado del examrandomizeR 
rutaIni <- getwd()
wd   <- tempdir()      # pasa ruta temporal por politica de sguridad de servidor
setwd( wd )
res.path <- paste0( wd, "/OUT/" ) #Para guardar los resultados en este directorio
dir.create( res.path ) #Se crea una vez al ppo
par(mar = rep(2,4))

#FunciÃ³n que crea y borra el directorio
directorios <- function(){
  if ( dir.exists( res.path ) == FALSE) {
    dir.create( res.path )  # si no existe el directorio de soluciones lo creamos
  }
  else{
    file.remove( paste0( res.path, dir( res.path ) ) ) #Si existe, borramos lo que haya dentro
  }
}


f.readfileauto <- function(file){
  header <- TRUE
  colsep <- "\t" 
  decsep <- ","
  
  char <- readChar(file, nchars = 20)
  
  if( length( grep( "^[0-9]", char ) ) == 1 ) header <- FALSE
  if( length( grep( ";", char ) ) == 1 ){      colsep <- ";"  }
  else if( length( grep( ",", char ) ) == 1 ) {     colsep <- ","}
  if( length( grep( "\\.", char ) ) == 1 )    decsep <- "."
  
  list( header = header, colsep = colsep, decsep = decsep )
  
}

####----------------------------------------PLOT's y TEXTO---------------------------------------------------####

#GrÃ¡ficos para mÃ©todos 1,2,3,4
f.plot <- function( a, b, t, fit, s, m, IC50, num_met ){
  n <- num_met
  if ( n == 1 ){
    plot( a, b, pch = 19, col = "aquamarine3", xlab = "ConcentraciÃ³n", ylab = "InhibiciÃ³n" )
  }
  if ( n == 2 ){
    plot( a, b, pch = 19, col = "aquamarine3", xlab = expression( Log[10]( conc ) ), ylab = "InhibiciÃ³n" )
  }
  if ( n == 3 || n == 4 ){
    plot( a, b, pch = 19, col = "aquamarine3", xlab = expression( Log[10]( conc ) ), ylab =  expression( Log[10]( Inhib / (100 - Inhib) ) ) )
  }
  abline( fit, col = "red3", lwd = 2 )
  coordx = ( min( a[ complete.cases(a) ] ) + max( a[ complete.cases(a) ] ) )/2
  coordy = ( min( b[ complete.cases(b) ] ) + max( b[ complete.cases(b) ] ) )/2
  segments( min( a[ complete.cases(a) ] ) - 1000, t, s, t, col = "slategray", lwd = 2)  
  segments( s, -1000, s, t, col = "slategray", lwd = 2)
}


#Para descarga
f.plot2 <- function( res, a, b, t, fit, s, m, IC50, num_met ) {
  myfile <- res
  png( paste0( res.path, res ) )
  f.plot( a, b, t, fit, s, m, IC50, num_met )
  dev.off()
}

#GrÃ¡ficos para mÃ©todo 5 y 6
f.plotm5 <- function( df ) {
  np1 <- nplr( x = df$x, y = df$y, npars = 4 )
  # plot( np1, showEstim = FALSE, unit = "nM", showGOF = FALSE,
  #       xlab = expression( Log[10]( conc ) ),
  #       ylab = "O.D. (% Control)" )
  xc <- getXcurve( np1 )
  yc <- getYcurve( np1 )
  dff <- data.frame( xc , yc )
  gg <- ggplot( data = dff, aes( x = xc, y = yc ) ) +
    geom_point( size = 0.000005, colour = "red" ) + geom_line( colour = "red", size = 0.5 ) +
    geom_point( data = df, aes(x = getX( np1 ), y = getY( np1 ) ), colour = "aquamarine3", size = 2.5 ) +
    scale_x_discrete( limit = c(getX( np1 ), 0, 1, 2, 3), labels = c(10^{getX( np1 )}, 1, 10, 100, 1000) ) +
    xlab( "ConcentraciÃ³n (Escala logarÃ­tmica)") +
    ylab( "O.D. (% Control)" ) +
    theme_bw()
  plot( gg )
}

f.plotm5ii <- function( df ) {
  np1 <- nplr( x = df$x, y = df$y, npars = 4, useLog = FALSE )
  plot( np1, showEstim = FALSE, unit = "nM", showGOF = FALSE , 
        xlab = "Conc.",
        ylab = "O.D. (% Control)" )
}

#Para descarga
f.plot3 <- function( res, df ) {
  myfile <- res
  png( paste0( res.path, res ) )
  f.plotm5( df )
  dev.off()
}

#Texto que se imprime en el .txt
f.texto <- function( l, res, a, num_metod ){
  n <- num_metod
  myfile <- res
  write( paste( "SAIC50. ", a, ", ", date(), sep = "" ), 
         file = paste( res.path, myfile, sep = "" ), append = TRUE )
  write( "----------------------------------", file = paste (res.path, myfile, sep = ""), append = TRUE ) 
  write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  write( l[[6]], file = paste( res.path, myfile, sep = "" ), append = TRUE )
  # write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  write( l[[5]], file = paste( res.path, myfile, sep = "" ), append = TRUE )
  #write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  #write( l[[1]], file = paste( res.path, myfile, sep = "" ), append = TRUE )
  #write( "----------------------------------", file = paste( res.path, myfile, sep = "" ), append = TRUE ) 
  # write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  if( n != 5 ){
    write( paste( "a = ", l[[2]]$coefficients[2], sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
    write( paste( "b = ", l[[2]]$coefficients[1], sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
  }
  if ( n == 5 ){
    write( paste( "a = ", l[[2]]$bottom, sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
    write( paste( "b = ", l[[2]]$scal, sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
    write( paste( "c = ", l[[2]]$xmid, sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
    write( paste( "d = ", l[[2]]$top, sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
  }
  write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  write( paste( "IC50 = ", l[[3]], sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
  # write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  # write( "\r\n", file = paste( res.path, myfile, sep = "" ), append = TRUE )
  write( paste( "RMSE/MAE = ", l[[4]], sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
  # write( "--", file = paste( res.path, myfile, sep = "" ), append = TRUE ) 
  # write( paste( "SAIC50. ", a, ", ", date(), sep = "" ), file = paste( res.path, myfile, sep = "" ), append = TRUE )
  write( "\r\n--
         Sec. Apoyo EstadÃ­stico.
         Servicio de Apoyo a la InvestigaciÃ³n (SAI)
         Vicerrectorado de InvestigaciÃ³n e InternacionalizaciÃ³n.
         Universidad de Murcia
         www.um.es/sai www.um.es/ae
         ---", 
          file = paste( res.path, myfile, sep = "" ), append = TRUE )
}#cierra f.texto



###------------------------------FUNCIONES QUE CALCULAN LA IC50 PARA CADA MÃTODO-----------------------------------------###

f.ic50m01 <- function(df){
  text1 <- "El primer mÃ©todo aplica a los datos el modelo de regresiÃ³n lineal simple para estudiar cÃ³mo los cambios en la concentraciÃ³n de una sustancia afectan a la inhibiciÃ³n del proceso biolÃ³gico que nos concierne de manera abrupta."
  a <- df$x
  b <- df$y
  fit <- lm( b ~ a, data = df )
  
  ic50 <- ( 50 - fit$coefficients[1] )/ ( fit$coefficients[2] )
  RMSE <- sqrt( mean( ( fit$residuals) ^ 2 ) )
  MAE <- mean( abs( fit$residuals ) )
  
  text2 <- "El ajuste realizado viene dado por la recta y = ax + b, donde"
  metodo <- "MÃTODO 1: RegresiÃ³n lineal directa y=f(x)"
  
  return( list( text1, fit, ic50, RMSE_MAE <- RMSE / MAE, text2, metodo ) )
}

f.ic50m02 <- function(df){
  text1 <- "Se observa que los datos (puntos) se acumulan para menores valores de x luego en el se hace una transformaciÃ³n logarÃ­tmica para que el modelo se asemeje mÃ¡s a una recta y asÃ­ ser mÃ¡s precisos."
  df <- df[ which( df$x!=0 ), ]
  df$logx <- log( df$x )
  a <- df$logx
  b <- df$y
  fit <- lm( b ~ a, data = df )
  ic50 <- exp( ( 50-fit$coefficients[1] )/( fit$coefficients[2] ) )
  RMSE <- sqrt( mean( ( fit$residuals )^2 ) )
  MAE <- mean( abs( fit$residuals ) )
  # text2 <- "El cÃ¡lculo del IC50 se harÃ¡ de forma similar al anterior pero teniendo en cuenta que el eje de coordenadas es z=logx (logaritmo de las concentraciones). AsÃ­, en la curva obtenida mediante regresiÃ³n y=az+b se da a y el valor 50: 50=az+b lo que implica z=(50-b)/a y de este se calcula la exponencial quedando la exponencial quedando IC50=e^z."
  text2 <- "El ajuste realizado viene dado por la recta y = ax + b, donde"
  metodo <- "MÃTODO 2: Cambio logarÃ­tmico en las x: z=log(x), y=f(z)"
  return( list( text1, fit, ic50, RMSE_MAE <- RMSE/MAE, text2, metodo ) )
}

f.ic50m03 <- function( df ){
  text1 <- "En el tercer mÃ©todo se hace una transformaciÃ³n logarÃ­tmica en el eje de ordenadas: t = log[y/(100-y)] y el de coordenadas serÃ¡ z=log(x)."
  df <- df[which(df$x != 0), ]
  df$logx <- log(df$x)
  df$calcy <- log(df$y / (100 - df$y))
  df <- df[which(df$calcy != Inf), ]
  a <- df$logx
  b <- df$calcy
  fit <- lm(b ~ a, data = df)
  ic50 <- exp(-fit$coefficients[1] / fit$coefficients[2])
  RMSE <- sqrt(mean((fit$residuals) ^ 2))
  MAE <- mean(abs(fit$residuals))
  #text2 <- "Para el cÃ¡lculo del IC50, en la curva obtenida mediante regresiÃ³n t=az+b se da a t el valor 0 porque t = log[y/(100-y)] y el objetivo es y=50, es decir, t = log[50/(100-50)]=log(1)=0. Ahora tenemos: 0 = az +b lo que implica z=-b/a finalmente IC50 =e^{z}."
  text2 <- "El ajuste realizado viene dado por la recta y = ax + b, donde"
  metodo <- "MÃTODO 3: Cambios en los ejes: z=log(x), t=g(y), t=f(z))"
  return( list( text1, fit, ic50, RMSE_MAE <- RMSE/MAE, text2, metodo ) )  
}

#Para mÃ©todo 4. (Creadas por AGV o AJPO)
fdeletemax <- function(df, names) {
  df1 <- df[which(df$x != names[length(names)]), ]
  return (df1)
}

fdeletemin <- function(df, names) {
  df1 <- df[which(df$x != names[1]), ]
  return (df1)
}

fdeleteTot <- function(df, names) {
  if (length(names) > 4) {
    minY <- min((df$y)[complete.cases(df$y)])
    maxY <- max((df$y)[complete.cases(df$y)])
    if (abs(50 - minY) > abs(50 - maxY)) {
      df <- fdeletemin(df, names)
    }
    else{
      df <- fdeletemax(df, names)
    }
  }
  else{
    df
  }
}

errorR <- function(fit) {
  fits <- summary(fit)
  a <- fits$r.squared
  return (a)
}

f.ic50m04 <- function(df){
  text1 <- "En el cuarto mÃ©todo vamos borrando sucesivamente todas las muestras de cada concentraciÃ³n y 
  calculando la recta de regresiÃ³n para quedarnos con la que comete el menor error. "
  df <- df[which(df$x != 0), ]
  df$logx <- log(df$x)
  df$calcy <- log(df$y / (100 - df$y))
  df <- df[which(df$calcy != Inf), ]
  t <- table(df$x)
  names <- attributes(t)$dimnames[[1]]
  e <- NULL
  fit <- lm(df$logx ~ df$calcy)
  e <- errorR(fit)
  df <- na.omit(df)
  l <- list(fit)
  datos <- list(df)
  for (i in (1):(length(names))) {
    df <- fdeleteTot(df, names)
    fit <- lm(df$logx ~ df$calcy)
    df <- na.omit(df)
    e <- c(e, errorR(fit))
    l <- c(l, list(fit))
    datos <- c(datos, list(df))
    t <- table(df$x)
    names <- attributes(t)$dimnames[[1]]
  }
  
  m <- which.max(e)
  DF <- datos[[m]]
  #  f.ic50m03(DF)
  a <- DF$logx
  b <- DF$calcy
  fit <- lm(b ~ a, data = DF)
  
  ic50 <- exp(-fit$coefficients[1] / fit$coefficients[2])
  
  RMSE <- sqrt(mean((fit$residuals) ^ 2))
  MAE <- mean(abs(fit$residuals))
  
  # text2 <- "Para el cÃ¡lculo del IC50, en la curva obtenida mediante regresiÃ³n t=az+b se da a t el valor 0 porque t = log[y/(100-y)] y el objetivo es y=50, es decir, t = log[50/(100-50)]=log(1)=0. Ahora tenemos: 0 = az +b lo que implica z=-b/a finalmente IC50 =e^{z}."
  text2 <- "El ajuste realizado viene dado por la recta y = ax + b, donde"
  metodo <- "MÃTODO 4: MÃ©todo 3 eliminando observaciones para reducir el error. Cambios en los ejes: z=log(x), t=g(y), t=f(z))"
  return( list( text1, fit, ic50, RMSE_MAE <- RMSE/MAE, text2, metodo ) )  
}

#MÃ©todos 5 (lg = TRUE) y 6 (lg = FALSE)
f.ic50m05 <- function(df ){
  text1 <- "En el quinto mÃ©todo se estiman los parÃ¡metros del modelo no lineal: y ~ d + (d-a)/(1+(c/x)^b)"
  np1 <- nplr(x = df$x, y = df$y, npars = 4 )
  estim <- getEstimates( np1, 50 )
  ic50 <- format( estim$x, digits = 6, scientific = FALSE )
  fit <- getPar( np1 )$param
  infl <- getInflexion( np1 )
  stD <- getStdErr( np1 )
  # text2 <- "Sustituyendo los valores obtenidos en el model no lineal, con y = 50, se obtiene la concentraciÃ³n x para la cual  el porcentaje de inhibiciÃ³n es 50 %"
  text2 <- "La curva ajustada viene dada por la funciÃ³n y = d + (d-a)/(1+(c/x)^b), donde"
  metodo <- "MÃTODO 5: RegresiÃ³n logÃ­stica de 4 parÃ¡metros"
  return( list( text1, fit, ic50, stD, text2, metodo ) )
}

# f.ic50m06 <- function( df ){
#   text1 <- "En el quinto mÃ©todo se estiman los parÃ¡metros del modelo no lineal: y ~ d + (d-a)/(1+(c/x)^b)"
#   np1 <- nplr(x = df$x, y = df$y, npars = 4, useLog = FALSE )
#   estim <- getEstimates(np1, 50)
#   ic50 <- format(estim$x, digits = 6, scientific = FALSE)
#   fit <- getFitValues(np1)
#   infl <- getInflexion(np1)
#   text2 <- "Sustituyendo los valores obtenidos en el model no lineal, con y = 50, se obtiene la concentraciÃ³n x para la cual  el porcentaje de inhibiciÃ³n es 50 %"
#   metodo <- "MÃTODO 6: RegresiÃ³n logÃ­stica de 4 parÃ¡metros cuando el eje x ya es logarÃ­tmico"
#   return( list( text1, fit, ic50, infl, text2, metodo ) )
# }

#Para calcular las IC50
f.method <- function( df, num_metodo, a ) {
  directorios()
  n <- num_metodo
  df$logx <- log(df$x)
  df$calcy <- log(df$y / (100 - df$y))
  myplot <- paste0( "plot0", n, ".png" )
  l <- eval( parse( text = paste0("f.ic50m0", n, "(df)" ) ) )
  txtname <- paste0( "Resultado-ajuste", n, "-ic50-", Sys.Date(), '.txt') #AquÃ­ crea resultado-ajuste5-ic50-xxxx-xx-xx.txt
  f.texto( l, txtname, a, n )
  pngname <- paste0( "grafico", n, "-", Sys.Date(), '.png')
  if ( n == 1  ){
    f.plot2( pngname, df$x, df$y, 50, l[[2]], l[[3]], 10, l[[3]], n )
  }
  else if ( n == 2 ){
    f.plot2( pngname, df$logx, df$y, 50, l[[2]], log( l[[3]] ), 10, l[[3]], n )
  }
  else if ( n == 3 ){
    f.plot2( pngname, df$logx, df$calcy, 0, l[[2]], log( l[[3]] ), 1, l[[3]], n )
  }
  else if ( n == 4 ){
    f.plot2( pngname, df$logx, df$calcy, 0, l[[2]], log( l[[3]] ), 10, l[[3]], n )
  }
  else if (n == 5 ){
    np1 <- nplr(x = df$x, y = df$y, npars = 4)
    estim <- getEstimates(np1, 50)
    f.plot3( pngname, df)
  }  
  else if (n == 6 ){
    np1 <- nplr(x = df$x, y = df$y, npars = 4, useLog = FALSE)
    estim <- getEstimates(np1, 50)
    f.plot3( pngname, df)
  }
}#cierra f.method

###------------------------------IMPRIME LAS IC50 SÃLO----------------------------------------------------###


f.method3 <- function(df, num_metodo ) {
  n <- num_metodo
  if (n == 1 ) {
    l <- f.ic50m01(df)
    print(paste("IC50 = ", l[[3]], sep = ""))
  }
  
  if (n == 2 ) {
    l <- f.ic50m02(df)
    print(paste("IC50 = ", l[[3]], sep = ""))
  }
  
  if (n == 3 ) {
    l <- f.ic50m03(df)
    print(paste("IC50 = ", l[[3]], sep = ""))
  }
  
  if (n == 4 ) {
    l <- f.ic50m04(df)
    print(paste("IC50 = ", l[[3]], sep = ""))
  }
  
  if (n == 5 ) {
    l <- f.ic50m05(df)
    print(paste("IC50 = ", l[[3]], sep = ""))
  }
  if (n == 6 ) {
    l <- f.ic50m06(df )
    print(paste("IC50 = ", l[[3]], sep = ""))
  }
}

###-------------------------------LLAMA A LAS FUNCIONES DEL PLOT'S--------------------------------------------###

f.method2 <- function(df, num_metodo  ) {
  n <- num_metodo
  df$logx <- log(df$x)
  df$calcy <- log(df$y / (100 - df$y))
  
  if (n == 1 ) {
    l <- f.ic50m01(df)
    plt <- f.plot(df$x, df$y, 50, l[[2]], l[[3]], 10, l[[3]], n)
  }
  
  if (n == 2 ) {
    l <- f.ic50m02(df)
    plt <- f.plot(df$logx, df$y, 50, l[[2]], log(l[[3]]), 10, l[[3]], n )
  }
  
  if (n == 3 ) {
    l <- f.ic50m03(df)
    plt <- f.plot(df$logx, df$calcy, 0, l[[2]], log(l[[3]]), 1, l[[3]], n )
  }
  
  if (n == 4 ) {
    l <- f.ic50m04(df)
    plt <- f.plot(df$logx, df$calcy, 0, l[[2]], log(l[[3]]), 1, l[[3]], n )
  }
  if (n == 5 ) {
    l <- f.ic50m05(df)
    plt <- f.plotm5(df)
  }
  if (n == 6 ){
    l <- f.ic50m06( df)
    plt <- f.plotm5ii(df) 
  }
  if (n > 6) {
    return('Solo hay 6 mÃ©todos.Introduce # de mÃ©todo asÃ­: f.method(df,#)')
  }
  return( plt )
}


