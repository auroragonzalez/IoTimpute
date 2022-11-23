#https://juno.inf.um.es/missingValues/
# ################################################################################
#
# server.r
# 2022.09.06
# Jose ()
# version:
# Notas:
# 
# ################################################################################

shinyServer( function( input, output ){

  # Variables auxiliares para evitar calculos repetidos
  tablaParametros <- reactiveVal(NULL)
  resultadosHard <- reactiveVal(NULL)
  resultadosSoft <- reactiveVal(NULL)
  resultadosPMF <- reactiveVal(NULL)
  ImputHard <- reactiveVal(NULL)
  ImputSoft <- reactiveVal(NULL)
  ImputPMF <- reactiveVal(NULL)
  
  unlink(paste(rutaCode, "/tmp", sep = ""), recursive = TRUE)
  ifelse(!dir.exists(file.path(rutaCode, "/tmp")), dir.create(file.path(rutaCode, "/tmp")), FALSE)
  
  # Devuelve el dataframe creado con el fichero de localizaciones seleccionado o el de ejemplo (OK)
  dataframe <- reactive({
    if(input$usarEjemplo){
      opt <- f.readfileauto(paste(rutaEjemplo, "/beachLoc.txt", sep = ""))
      ejemplo <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = opt$header, dec = opt$decsep, sep = opt$colsep )
      attr(ejemplo, "name") <- "beachLoc.txt"
      return (ejemplo)
    } else{
        if(!is.null(input$file1)){
          inFile <- input$file1
          opt <- f.readfileauto(inFile$datapath)
          rto <- read.csv(inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
          attr(rto, "name") <- inFile$nam
          return (rto)
        } else{
            return (NULL)
        }
    }
  })

  # Devuelve la tabla de localizaciones, si es demasiado grande se acorta (OK)
  output$vistacsv <- renderTable({
    rto <- dataframe()
    if (!is.null(rto)){
      names(rto) <- c(" \\ ", "X-axis", "Y-axis")
      
      if(nrow(rto) > 12){
        rto[11,1:3] <- c("...", "...", "...")
        rto[12,1:3] <- rto[nrow(rto),1:3]
      }
      
      return (rto[1:min(nrow(rto),12),1:3])
    }
  }, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")

  # Devuelve el dataframe creado con el fichero de datos seleccionado o el de ejemplo (OK)
  dataframe1 <- reactive({
    if(input$usarEjemplo){
      opt <- f.readfileauto(paste(rutaEjemplo, "/Rgt.csv", sep = ""))
      ejemplo <- read.csv(paste(rutaEjemplo, "/Rgt.csv", sep = ""), header = opt$header, dec = opt$decsep, sep = opt$colsep )
      attr(ejemplo, "name") <- "Rgt.csv"
      return (ejemplo)
    } else{
        if(!is.null(input$file2)){
          inFile <- input$file2
          opt <- f.readfileauto(inFile$datapath)
          rto <- read.csv(inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
          attr(rto, "name") <- inFile$nam
          return (rto)
        } else{
            return (NULL)
        }
    }
  })
  
  # Devuelve la tabla de valores, si es demasiado grande se acorta (OK)
  output$vistacsv1 <- renderTable({
    rto <- dataframe1()
    if (!is.null(rto)){
      col1 <- 1:nrow(rto)
      rto <- cbind(col1, rto)
      names(rto)[1] <- "\\"
      
      if(nrow(rto) > 13){
        rto[11,1:ncol(rto)] <- rep("...", ncol(rto))
        rto[12,1:ncol(rto)] <- rto[nrow(rto),1:ncol(rto)]
      }
      
      if(ncol(rto) > 15){
        rto[1:nrow(rto),14] <- rep("...", nrow(rto))
        rto[1:nrow(rto),15] <- rto[1:nrow(rto), ncol(rto)]
        names(rto)[14] <- "..."
        names(rto)[15] <- paste("V", ncol(rto)-1, sep = "")
      }
      
      return (rto[1:min(nrow(rto), 12),1:min(ncol(rto),15)])
    }
  }, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  # Devuelve el sliderImput con los valores ajustados segun el numero de observaciones temporales del fichero seleccionado o del de ejemplo (OK)
  output$observations <- renderUI({
    if(input$usarEjemplo){
      opt <- f.readfileauto(paste(rutaEjemplo, "/Rgt.csv", sep = ""))
      ejemplo <- read.csv(paste(rutaEjemplo, "/Rgt.csv", sep = ""), header = opt$header, dec = opt$decsep, sep = opt$colsep )
      
      sliderInput("observaciones", "Observaciones temporales",
                  min = 1, max = ncol(ejemplo),
                  value = ncol(ejemplo)/2, step = 1)
      
    } else if(!is.null(input$file2)){
      inFile1 <- input$file2
      opt1 <- f.readfileauto(inFile1$datapath)
      datos <- read.csv(inFile1$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep)
      
      sliderInput("observaciones", "Observaciones temporales",
                  min = 1, max = ncol(datos),
                  value = ncol(datos)/2, step = 1)
    }
  })
  
  # Devuelve los parametros del variograma (quitando los outliers) asociado a los datos seleccionados o a los datos de ejemplo (OK)
  tab <- function(flag){
    input$observaciones
    input$integer
    
    if(flag == 0)
      metodo <- input$Met
    else if (flag == 1)
      metodo <- input$Met2
    else 
      metodo <- input$Met5

    modelo <- switch(metodo, "1" = "linear", "2" = "pure.nugget", "3" = "exponential", "4" = "exponential", "5" = "gaussian")

    if((!is.null(input$file1) & !is.null(input$file2)) | input$usarEjemplo){
      if(input$usarEjemplo){
        loc <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = FALSE, dec = ".", sep = "\t")
        datos <- read.csv(paste(rutaEjemplo, "/Rgt.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      } else{
        inFile <- input$file1
        opt <- f.readfileauto(inFile$datapath)
        loc <- read.csv(inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
        inFile1 <- input$file2
        opt1 <- f.readfileauto(inFile1$datapath)
        datos <- read.csv(inFile1$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep)
      }
      
      loc <- loc[1:nrow(datos),2:3]
      if(!is.null(input$observaciones)) 
        datos <- datos[,1:max(2,input$observaciones)]
      tablaRes <- matrix(0, nrow = ncol(datos), ncol = 3)
      
      distancias <- dist(loc)
      ordenados <- sort(distancias)
      
      diametro = 0
      for (j in 1:(nrow(loc)-1)) {
        diametro = max(diametro, ordenados[j+1]-ordenados[j])
      }
      
      diametro = round(diametro+0.5)
      
      limiteSup = diametro
      cont = 1;
      while (limiteSup < max(ordenados)){
        limiteSup = diametro*cont;
        cont = cont+1;
      }
      
      for(i in 1:ncol(datos)){
        tabla <- data.frame(cbind(loc$V2,loc$V3))
        tabla <- data.frame(cbind(tabla, datos[i]))
        
        df = as.geodata(obj = tabla, coords.col = c(1,2), data.col = 3)
        
        # distancias <- dist(loc)
        # ordenados <- sort(distancias)
        # 
        # diametro = 0
        # for (j in 1:(nrow(loc)-1)) {
        #   diametro = max(diametro, ordenados[j+1]-ordenados[j])
        # }
        # 
        # diametro = round(diametro+0.5)
        # 
        # limiteSup = diametro
        # cont = 1;
        # while (limiteSup < max(ordenados)){
        #   limiteSup = diametro*cont;
        #   cont = cont+1;
        # }
        
        # geodat.v1 <- variog(df, breaks = seq(0, limiteSup, diametro)[1:round(3/4*length( seq(0, limiteSup, diametro)))], option = 'bin', messages = FALSE)
        # summary(df)$distances.summary[2]
        geodat.v1 <- variog(df, max.dist = (summary(df)$distances.summary[2])/2, breaks = seq(0, limiteSup, diametro)[1:round(3/4*length( seq(0, limiteSup, diametro)))], option = 'bin', messages = FALSE)
        
        geoExp <- variofit(geodat.v1, nugget = 0, fix.nugget = FALSE, cov.model = modelo, messages = FALSE)
        
        tablaRes[i,1] = geoExp$nugget+geoExp[[2]][1]
        tablaRes[i,2] = geoExp$practicalRange
        tablaRes[i,3] = geoExp$nugget
      }
      
      bp <- boxplot(tablaRes[,3])
      outliers <-bp$out
      index3 <- which(tablaRes[,3] %in% outliers)

      bp <- boxplot(tablaRes[,2])
      outliers <-bp$out
      index2 <- which(tablaRes[,2] %in% outliers)

      bp <- boxplot(tablaRes[,1])
      outliers <-bp$out
      index1 <- which(tablaRes[,1] %in% outliers)

      indices <- c(index1, index2, index3)
      indices <- indices[!duplicated(indices)]

      buenos <- setdiff(1:nrow(tablaRes),indices)
      finales <- tablaRes[buenos,1:3]
      
      return (finales)
      
      # return (tablaRes)
      
    } else{
      return (NULL)
    }
  }
  
  # Activa el boton para descargar el fichero con los resultados de los parametros del varigrama (OK)
  output$download <- renderUI({
    input$Met
    if((!is.null(input$file1) & !is.null(input$file2)) | input$usarEjemplo){
      downloadButton('downFile',"Descargar parámetros")
    }
  })
  
  # Devuelve la tabla de los parametros del variograma y ademas la almacena en la variable para no tener que volver a calcular (OK)
  output$res <- renderTable({
    rto <- tab(0)
    tablaParametros(rto)
    if (!is.null(rto)){
      colnames(rto) <- c("Sill", "Range", "Nugget")
      
      rto <- round(rto, digits = 5)
      rto <- format(rto, digits = 5)
      
      if(nrow(rto) > 13){
        rto[11,1:ncol(rto)] <- rep("...", ncol(rto))
        rto[12,1:ncol(rto)] <- rto[nrow(rto),1:ncol(rto)]
      }
      
      return (rto[1:min(nrow(rto),12),1:ncol(rto)])
    }
  }, digits = 5, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  # Devuelve la tabla con las medias de cada uno de los parametros del variograma (Mira la variable en vez de volver a calcular) (OK)
  output$medios <- renderTable({
    input$file1
    input$file2
    input$usarEjemplo
    input$observaciones
    input$Met
    input$integer
    rto <- tablaParametros()
    if (!is.null(rto)){
      promedios <- matrix(0, nrow = 1, ncol = 3)
      colnames(promedios) <- c("Sill", "Range", "Nugget")
      promedios[1,1] <- mean(rto[,1])
      promedios[1,2] <- mean(rto[,2])
      promedios[1,3] <- mean(rto[,3])
      return (promedios)
    }
  }, digits = 5, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  # Funcion que guarda el fichero con los parametros del variograma (Mira la variable antes de clacular de nuevo, separador decimal ",") (OK)
  output$downFile <- downloadHandler(
    filename = function() {
      paste0(input$downFile, "parametrosVariograma.csv")
    },
    content = function(file) {
      resultados <- tablaParametros()
      if(!is.null(resultados)){
        write.table(resultados[,1:2], file, row.names = FALSE, na = "", col.names = FALSE, sep = ",", dec = ".")
      }
    }
  )
  
  # Muestra el numero de neighbours para hard de acuerdo al fichero groups seleccionado (OK)
  output$neighs <- renderUI({
    if(input$usarEjemplo){
      opt <- f.readfileauto(paste(rutaEjemplo, "/beachLoc.txt", sep = ""))
      loc <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = opt$header, dec = opt$decsep, sep = opt$colsep)
      
      sliderInput("nei", "Número de neighbours",
                  min = 3, max = nrow(loc),
                  value = 3, step = 1)
      
    } else if(!is.null(input$file1)){
      inFile <- input$file1
      opt <- f.readfileauto(inFile$datapath)
      loc <- read.csv(inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
      
      sliderInput("nei", "Número de neighbours",
                  min = 3, max = nrow(loc),
                  value = 3, step = 1)
    }
  })
  
  # Activa el boton para aplicar el metodo HardBME (OK)
  output$downloadHard <- renderUI({
    if(((!is.null(input$file1) & !is.null(input$file2)) | input$usarEjemplo)){
      actionButton('doHard2', "Aplicar")
    }
  })
  
  # Activa el boton para aplicar el metodo SoftBME (OK)
  output$downloadSoft <- renderUI({
    if(((!is.null(input$file1) & !is.null(input$file2) & !is.null(input$hardFile1) & !is.null(input$softFile1)) | input$usarEjemplo) & !is.na(input$deltaValue1) & input$deltaValue1 >= 1 & input$deltaValue1 <= 100){
      actionButton("doSoftNei", "Aplicar")
    }
  })
  
  # Activa el boton para aplicar el metodo PMF (OK)
  output$downloadPMF <- renderUI({
    if(((!is.null(input$file1) & !is.null(input$file2)) | input$usarEjemplo) & !is.na(input$epocs) & input$epocs >= 10){
      actionButton("doPMF", "Aplicar")
    }
  })
  
  # Muestra el numero de neighbours para soft de acuerdo al fichero groups seleccionado (OK)
  output$neighs1 <- renderUI({
    if(input$usarEjemplo){
      opt <- f.readfileauto(paste(rutaEjemplo, "/beachLoc.txt", sep = ""))
      loc <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = opt$header, dec = opt$decsep, sep = opt$colsep)
      
      sliderInput("nei1", "Número de neighbours",
                  min = 3, max = nrow(loc),
                  value = 3, step = 1)
      
    } else if(!is.null(input$file1)){
      inFile <- input$file1
      opt <- f.readfileauto(inFile$datapath)
      loc <- read.csv( inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
      
      sliderInput("nei1", "Número de neighbours",
                  min = 3, max = nrow(loc),
                  value = 3, step = 1)
    }
  })
  
  # Ejecuta el script de HardBME segun los datos de entrada (OK)
  observeEvent(input$doHard2, {
    rutaIni <- "./tmp"
    metodo <- input$Met2
    modelo <- switch(metodo, "1" = "linearV", "2" = "nuggetC", "3" = "sphericalC", "4" = "exponentialC", "5" = "gaussianC")
    
    if(((!is.null(input$file1) & !is.null(input$file2)) | input$usarEjemplo)){
      
      if(input$usarEjemplo){
        loc <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = FALSE, dec = ".", sep = "\t")
        datos <- read.csv(paste(rutaEjemplo, "/Rgt.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      } else{
        inFile <- input$file1
        opt <- f.readfileauto(inFile$datapath)
        loc <- read.csv(inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
        inFile1 <- input$file2
        opt1 <- f.readfileauto(inFile1$datapath)
        datos <- read.csv(inFile1$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep)
      }
      
      if(!is.null(input$clustFile2)){
        inFile2 <- input$clustFile2
        opt2 <- f.readfileauto( inFile2$datapath )
        clus <- read.csv(inFile2$datapath, header = opt2$header, dec = opt2$decsep, sep = " ")
      } else{
        clus <- data.frame(matrix(NA, nrow = nrow(datos), ncol = 2))
        clus[,1] <- 1:nrow(datos)
        clus[,2] <- rep(1,nrow(datos))
      }
      
      write.table(datos, paste(rutaCode, "/tmp/Rgt.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
      write.table(loc, paste(rutaCode, "/tmp/locations.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(clus, paste(rutaCode, "/tmp/groupsk.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = " ")
      write.table(input$nei, paste(rutaCode, "/tmp/nei.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(input$observaciones, paste(rutaCode, "/tmp/N.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(input$integer, paste(rutaCode, "/tmp/NAS.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      cat(modelo, file = paste(rutaCode, "/tmp/modeloVariograma.txt", sep = ""))
      
      if(!is.null(input$paramsFile2)){
        paramsf <- input$paramsFile2
        opt1 <- f.readfileauto(paramsf$datapath)
        paramsfile <- read.csv( paramsf$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep)
        write.table(paramsfile, paste(rutaCode, "/tmp/parametrosVariograma.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",", dec = ".")

      } else{
        resultados <- tablaParametros()
        if(is.null(resultados) || input$Met != input$Met2){
          resultados <- tab(1)
        }
        if(!is.null(resultados)){
          write.table(resultados[,1:2], paste(rutaCode, "/tmp/parametrosVariograma.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",", dec = ".")
        }
      }
      
      system('octave "code/hardBME_mejorado.m"')
      
      if(file.exists(paste(rutaCode, "/tmp/informacionImputacionesHard.csv", sep = ""))){
        predFinal <- read.csv(paste(rutaCode, "/tmp/informacionImputacionesHard.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        ImputHard(predFinal)
      }
      
      if(file.exists(paste(rutaCode, "/tmp/RMSEhardNeighbours.csv", sep = ""))){
        error1 <- read.csv(paste(rutaCode, "/tmp/RMSEhardNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mse1 <- read.csv(paste(rutaCode, "/tmp/MSEhardNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mae1 <- read.csv(paste(rutaCode, "/tmp/MAEhardNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mape1 <- read.csv(paste(rutaCode, "/tmp/MAPEhardNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        cvrmse1 <- read.csv(paste(rutaCode, "/tmp/CVRMSEhardNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        tiempo1 <- read.csv(paste(rutaCode, "/tmp/timehardNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        resultadosHard(rbind(c("RMSE", error1), c("MSE", mse1), c("MAE", mae1), c("MAPE", mape1), c("CVRMSE", cvrmse1), c("Tiempo", tiempo1)))
      }
    }
  })

  # Muestra los resultados de aplicar el metodo HardBME a los datos de entrada (OK)
  output$tablaFinal <- renderTable({
    input$doHard2
    input$integer
    resultado <- resultadosHard()
    if(!is.null(resultado) && input$integer != 0){
      colnames(resultado) <- c("Métrica", "Valor")
      return (resultado)
    } else{
      return(NULL)
    }
  }, digits = 3, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  # Ejecuta el script de SoftBME segun los datos de entrada (OK)
  observeEvent(input$doSoftNei, {
    rutaIni <- "./tmp"
    metodo <- input$Met5
    modelo <- switch(metodo, "1" = "linearV", "2" = "nuggetC", "3" = "sphericalC", "4" = "exponentialC", "5" = "gaussianC")
    
    if(((!is.null(input$file1) & !is.null(input$file2) & !is.null(input$datosSoft) & !is.null(input$hardFile1) & !is.null(input$softFile1)) | input$usarEjemplo) & !is.na(input$deltaValue1) & input$deltaValue1 >= 1 & input$deltaValue1 <= 100){
      
      if(input$usarEjemplo){
        loc <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = FALSE, dec = ".", sep = "\t")
        datos <- read.csv(paste(rutaEjemplo, "/Rgt.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        datosSoft <- read.csv(paste(rutaEjemplo, "/R.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        hard <- read.csv(paste(rutaEjemplo, "/hard.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        soft <- read.csv(paste(rutaEjemplo, "/soft.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      } else{
        inFile <- input$file1
        opt <- f.readfileauto(inFile$datapath )
        loc <- read.csv(inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
        
        inFile1 <- input$file2
        opt1 <- f.readfileauto( inFile1$datapath )
        datos <- read.csv(inFile1$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep)
        
        inFile0 <- input$datosSoft
        opt0 <- f.readfileauto(inFile0$datapath)
        datosSoft <- read.csv(inFile0$datapath, header = opt0$header, dec = opt0$decsep, sep = opt0$colsep)
        
        inFile3 <- input$hardFile1
        opt3 <- f.readfileauto( inFile3$datapath )
        hard <- read.csv( inFile3$datapath, header = opt3$header, dec = opt3$decsep, sep = "," )
        
        inFile4 <- input$softFile1
        opt4 <- f.readfileauto( inFile4$datapath )
        soft <- read.csv( inFile4$datapath, header = opt4$header, dec = opt4$decsep, sep = "," )
      }
      
      if(!is.null(input$clustFile5)){
        inFile2 <- input$clustFile5
        opt2 <- f.readfileauto( inFile2$datapath )
        clus <- read.csv( inFile2$datapath, header = opt2$header, dec = opt2$decsep, sep = " " )
      } else{
        clus <- data.frame(matrix(NA, nrow = nrow(datos), ncol = 2))
        clus[,1] <- 1:nrow(datos)
        clus[,2] <- rep(1,nrow(datos))
      }
      
      write.table(datos, paste(rutaCode, "/tmp/Rgt.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
      write.table(datosSoft, paste(rutaCode, "/tmp/R.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
      write.table(loc, paste(rutaCode, "/tmp/locations.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(clus, paste(rutaCode, "/tmp/groupsk.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = " ")
      
      write.table(hard, paste(rutaCode, "/tmp/hard.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
      write.table(soft, paste(rutaCode, "/tmp/soft.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
      write.table(input$deltaValue1, file = paste(rutaCode, "/tmp/delta.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = " ")
      
      write.table(input$nei1, paste(rutaCode, "/tmp/nei.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(input$observaciones, paste(rutaCode, "/tmp/N.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(input$integer, paste(rutaCode, "/tmp/NAS.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      cat(modelo, file = paste(rutaCode, "/tmp/modeloVariograma.txt", sep = ""))
      
      if(!is.null(input$paramsFile5)){
        paramsf <- input$paramsFile5
        opt1 <- f.readfileauto(paramsf$datapath )
        paramsfile <- read.csv( paramsf$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep)
        write.table(paramsfile, paste(rutaCode, "/tmp/parametrosVariograma.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
        
      } else{
        resultados <- tablaParametros()
        if(is.null(resultados) || input$Met != input$Met5){
          resultados <- tab(2)
        }
        if(!is.null(resultados)){
          write.table(resultados[,1:2], paste(rutaCode, "/tmp/parametrosVariograma.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",", dec = ".")
        }
      }
      
      system('octave "code/softBME_mejorado.m"')
      
      if(file.exists(paste(rutaCode, "/tmp/informacionImputacionesSoft.csv", sep = ""))){
        predFinal <- read.csv(paste(rutaCode, "/tmp/informacionImputacionesSoft.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        ImputSoft(predFinal)
      }
      
      if(file.exists(paste(rutaCode, "/tmp/RMSEsoftNeighbours.csv", sep = ""))){
        error2 <- read.csv(paste(rutaCode, "/tmp/RMSEsoftNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mse2 <- read.csv(paste(rutaCode, "/tmp/MSEsoftNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mae2 <- read.csv(paste(rutaCode, "/tmp/MAEsoftNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mape2 <- read.csv(paste(rutaCode, "/tmp/MAPEsoftNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        cvrmse2 <- read.csv(paste(rutaCode, "/tmp/CVRMSEsoftNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        tiempo2 <- read.csv(paste(rutaCode, "/tmp/timesoftNeighbours.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        resultadosSoft(rbind(c("RMSE", error2), c("MSE", mse2), c("MAE", mae2), c("MAPE", mape2), c("CVRMSE", cvrmse2), c("Tiempo", tiempo2)))
      }
    }
  })
  
  # Muestra los resultados de aplicar el metodo SoftBME a los datos de entrada (OK)
  output$tablaFinal2 <- renderTable({
    input$doSoftNei
    input$integer
    resultado <- resultadosSoft()
    if(!is.null(resultado) && input$integer != 0){
      colnames(resultado) <- c("Métrica", "Valor")
      return (resultado)
    } else 
        return(NULL)
  }, digits = 3, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  # Ejecuta el script de PMF segun los datos de entrada (OK)
  observeEvent(input$doPMF, {
    rutaIni <- "./tmp"
    
    if(((!is.null(input$file1) & !is.null(input$file2)) | input$usarEjemplo) & !is.na(input$epocs) & input$epocs >= 10){
      
      if(input$usarEjemplo){
        loc <- read.csv(paste(rutaEjemplo, "/beachLoc.txt", sep = ""), header = FALSE, dec = ".", sep = "\t")
        datos <- read.csv(paste(rutaEjemplo, "/Rgt.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      } else{
        inFile <- input$file1
        opt <- f.readfileauto(inFile$datapath )
        loc <- read.csv( inFile$datapath, header = opt$header, dec = opt$decsep, sep = opt$colsep)
        
        inFile1 <- input$file2
        opt1 <- f.readfileauto( inFile1$datapath )
        datos <- read.csv( inFile1$datapath, header = opt1$header, dec = opt1$decsep, sep = opt1$colsep )
      }
      
      if(!is.null(input$clustFile3)){
        inFile2 <- input$clustFile3
        opt2 <- f.readfileauto( inFile2$datapath )
        clus <- read.csv( inFile2$datapath, header = opt2$header, dec = opt2$decsep, sep = " " )
      } else{
        clus <- data.frame(matrix(NA, nrow = nrow(datos), ncol = 2))
        clus[,1] <- 1:nrow(datos)
        clus[,2] <- rep(1,nrow(datos))
      }
      
      write.table(datos, paste(rutaCode, "/tmp/Rgt.csv", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = ",")
      write.table(loc, paste(rutaCode, "/tmp/locations.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(clus, paste(rutaCode, "/tmp/groupsk.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = " ")
      
      write.table(input$epocs, paste(rutaCode, "/tmp/epochs.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(input$observaciones, paste(rutaCode, "/tmp/N.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      write.table(input$integer, paste(rutaCode, "/tmp/NAS.txt", sep = ""), row.names = FALSE, na = "", col.names = FALSE, sep = "\t")
      
      system('octave "code/pmf_mejorado.m"')
      
      if(file.exists(paste(rutaCode, "/tmp/informacionImputacionesPMF.csv", sep = ""))){
        predFinal <- read.csv(paste(rutaCode, "/tmp/informacionImputacionesPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        ImputPMF(predFinal)
      }
      
      if(file.exists(paste(rutaCode, "/tmp/RMSEhardPMF.csv", sep = ""))){
        error3 <- read.csv(paste(rutaCode, "/tmp/RMSEhardPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mse3 <- read.csv(paste(rutaCode, "/tmp/MSEhardPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mae3 <- read.csv(paste(rutaCode, "/tmp/MAEhardPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        mape3 <- read.csv(paste(rutaCode, "/tmp/MAPEhardPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        cvrmse3 <- read.csv(paste(rutaCode, "/tmp/CVRMSEhardPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        tiempo3 <- read.csv(paste(rutaCode, "/tmp/timeshardPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
        resultadosPMF(rbind(c("RMSE", error3), c("MSE", mse3), c("MAE", mae3), c("MAPE", mape3), c("CVRMSE", cvrmse3), c("Tiempo", tiempo3)))
      }
      
    }
  })
  
  # Muestra los resultados de aplicar el metodo PMF a los datos de entrada (OK)
  output$tablaFinal3 <- renderTable({
    input$doPMF
    input$integer
    resultado <- resultadosPMF()
    if(!is.null(resultado) && input$integer != 0){
      colnames(resultado) <- c("Métrica", "Valor")
      return (resultado)
    } else
        return(NULL)
  }, digits = 3, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  ################## Muestra los resultados de aplicar el metodo Hard a los datos de entrada
  output$tablaImputHard <- renderTable({
    input$doHard2
    resultado <- ImputHard()
    if(!is.null(resultado))
      colnames(resultado) <- c("X", "Y", "Obs.", "Valor")
    return (resultado[1:10,])
  }, digits = 2, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  ############# Muestra el boton de descargar resultados del metodo Hard
  output$downloadImputHard <- renderUI({
    input$doHard2
    resultado <- ImputHard()
    if(!is.null(resultado)){
      downloadButton('downFile8',"Descargar imputaciones")
    }
  })
  
  output$downloadNAsHard <- renderUI({
    input$doHard2
    resultado <- ImputHard()
    if(!is.null(resultado)){
      downloadButton('downFile21',"Descargar datos con NA's")
    }
  })

  output$downFile21 <- downloadHandler(
    filename = function() {
      paste0(input$downFile21, "DatosNAHard.csv")
    },
    content = function(file) {
      resultados <- read.csv(paste(rutaCode, "/tmp/dataWithNA.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      if(!is.null(resultados)){
        write.table(resultados, file, row.names = FALSE, col.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  ############# Funcion que guarda el fichero con los resultados de Soft
  output$downFile8 <- downloadHandler(
    filename = function() {
      paste0(input$downFile8, "ImputacionesHard.csv")
    },
    content = function(file) {
      resultados <- read.csv(paste(rutaCode, "/tmp/datosImputadosHard.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      if(!is.null(resultados)){
        write.table(resultados, file, row.names = FALSE, col.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  ################## Muestra los resultados de aplicar el metodo Soft a los datos de entrada
  output$tablaImputSoft <- renderTable({
    input$doSoftNei
    resultado <- ImputSoft()
    if(!is.null(resultado))
      colnames(resultado) <- c("X", "Y", "Obs.", "Valor")
    return (resultado[1:10,])
  }, digits = 2, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  ############# Muestra el boton de descargar resultados del metodo Soft
  output$downloadImputSoft <- renderUI({
    input$doSoftNei
    resultado <- ImputSoft()
    if(!is.null(resultado)){
      downloadButton('downFile9',"Descargar imputaciones")
    }
  })
  
  output$downloadNAsSoft <- renderUI({
    input$doSoftNei
    resultado <- ImputSoft()
    if(!is.null(resultado)){
      downloadButton('downFile20',"Descargar datos con NA's")
    }
  })
  
  ############# Funcion que guarda el fichero con los resultados de Soft
  output$downFile9 <- downloadHandler(
    filename = function() {
      paste0(input$downFile9, "ImputacionesSoft.csv")
    },
    content = function(file) {
      resultados <- read.csv(paste(rutaCode, "/tmp/datosImputadosSoft.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      if(!is.null(resultados)){
        write.table(resultados, file, row.names = FALSE, col.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  output$downFile20 <- downloadHandler(
    filename = function() {
      paste0(input$downFile20, "DatosNASoft.csv")
    },
    content = function(file) {
      resultados <- read.csv(paste(rutaCode, "/tmp/dataWithNA.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      if(!is.null(resultados)){
        write.table(resultados, file, row.names = FALSE, col.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  ################## Muestra los resultados de aplicar el metodo PMF a los datos de entrada
  output$tablaImputPMF <- renderTable({
    input$doPMF
    resultado <- ImputPMF()
    if(!is.null(resultado))
      colnames(resultado) <- c("X", "Y", "Obs.", "Valor")
    return (resultado[1:10,])
  }, digits = 2, spacing = 'm', bordered = TRUE, align = 'c', width = "auto", na = "NA")
  
  ############# Muestra el boton de descargar resultados del metodo PMF
  output$downloadImputPMF <- renderUI({
    input$doPMF
    resultado <- ImputPMF()
    if(!is.null(resultado)){
      downloadButton('downFile7',"Descargar imputaciones")
    }
  })
  
  ############# Funcion que guarda el fichero con los resultados de PMF
  output$downFile7 <- downloadHandler(
    filename = function() {
      paste0(input$downFile7, "ImputacionesPMF.csv")
    },
    content = function(file) {
      resultados <- read.csv(paste(rutaCode, "/tmp/datosImputadosPMF.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      if(!is.null(resultados)){
        write.table(resultados, file, row.names = FALSE, col.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  # Muestra el boton de descargar resultados del metodo HardBME (OK)
  output$download1 <- renderUI({
    input$doHard2
    input$integer
    resultado <- resultadosHard()
    if(!is.null(resultado) && input$integer != 0){
      downloadButton('downFile1',"Descargar resultados")
    }
  })
  
  output$downloadNAsPMF <- renderUI({
    input$doPMF
    resultado <- ImputPMF()
    if(!is.null(resultado)){
      downloadButton('downFile22',"Descargar datos con NA's")
    }
  })

  output$downFile22 <- downloadHandler(
    filename = function() {
      paste0(input$downFile22, "DatosNAPMF.csv")
    },
    content = function(file) {
      resultados <- read.csv(paste(rutaCode, "/tmp/dataWithNA.csv", sep = ""), header = FALSE, dec = ".", sep = ",")
      if(!is.null(resultados)){
        write.table(resultados, file, row.names = FALSE, col.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  # Funcion que guarda el fichero con los resultados de Hard (OK)
  output$downFile1 <- downloadHandler(
    filename = function() {
      paste0(input$downFile1, "ResultadosHard.csv")
    },
    content = function(file) {
      resultados <- resultadosHard()
      if(!is.null(resultados) && input$integer != 0){
        colnames(resultados) <- c("Métrica", "Valor")
        write.table(resultados, file, row.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  # Muestra el boton de descargar resultados del metodo SoftBME (OK)
  output$download2 <- renderUI({
    input$doSoftNei
    input$integer
    resultado <- resultadosSoft()
    if(!is.null(resultado) && input$integer != 0){
      downloadButton('downFile2',"Descargar resultados")
    }
  })
  
  # Funcion que guarda el fichero con los resultados de Soft (OK)
  output$downFile2 <- downloadHandler(
    filename = function() {
      paste0(input$downFile2, "ResultadosSoft.csv")
    },
    content = function(file) {
      resultados <- resultadosSoft()
      if(!is.null(resultados) && input$integer != 0){
        colnames(resultados) <- c("Métrica", "Valor")
        write.table(resultados, file, row.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  # Muestra el boton de descargar resultados del metodo PMF (OK)
  output$download3 <- renderUI({
    input$doPMF
    input$integer
    resultado <- resultadosPMF()
    if(!is.null(resultado) && input$integer != 0){
      downloadButton('downFile3',"Descargar resultados")
    }
  })
  
  # Funcion que guarda el fichero con los resultados de PMF (OK)
  output$downFile3 <- downloadHandler(
    filename = function() {
      paste0(input$downFile3, "ResultadosPMF.csv")
    },
    content = function(file) {
      resultados <- resultadosPMF()
      if(!is.null(resultados) && input$integer != 0){
        colnames(resultados) <- c("Métrica", "Valor")
        write.table(resultados, file, row.names = FALSE, na = "", sep = ",")
      }
    }
  )
  
  # output$indicaciones <- renderTable({
  #   df_ini <- data.frame( Indicaciones = c( "1. Cargue un fichero csv con el formato especificado en la página de ayuda.",
  #                                            "2. Seleccione el método que quiera aplicar en el panel lateral.",
  #                                            "3. Muévase por las pestañas de para ver los resultados y/o descargarlos."))
  #   return( df_ini )
  # })
  # 
  # output$summary <- renderPrint({
  #   rto <- dataframe()
  #   if(  !is.null( rto ) ){
  #     f.method( rto, num_metodo = input$Met, ver)
  #     f.method3( rto, num_metodo = input$Met )
  #     print( paste( "Nombre del archivo de datos analizado: ", attr(rto, "name" ), sep = "" ) )
  #     print( paste( "El método seleccionado es: ", input$Met, sep = ""))
  #   }
  # })

  
})

  
