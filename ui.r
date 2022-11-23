
ver <- "2.0" 

fluidPage( theme = shinytheme( "journal" ),
           
           div( h1( paste( "Missing Data Imputation with Bayesian Maximun Entropy. V:", ver ), style = "color: #ffffff; margin-left: 1em;" ),
                style = "background-color: #222222; height: '100%'; padding-top:1px; border-style: solid;
                border-radius:5px; border-color:#ae1b2d ; margin-bottom:10px" ),
           
           sidebarLayout(
             
             sidebarPanel(width = 3,
                          br(),
                          fileInput('file2', 'Seleccionar el fichero de valores',
                                     accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                          br(),
                          fileInput('file1', 'Seleccionar el fichero de localizaciones',
                                     accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                          checkboxInput("usarEjemplo", "Usar datos de ejemplo", value = FALSE),
                          tags$hr(),
                          br(),
                          chooseSliderSkin("Round"),
                          uiOutput("observations"),
                          br(),
                          sliderInput("integer", "Porcentaje de NAs",
                                      min = 0, max = 30,
                                      value = 5, step = 1),
                          
                          tags$head(
                            tags$style(HTML('[type$="submit"] { background-color:#222222; border-color:#222222 ;}
                                              .btn {border-style: solid;
                                              border-radius:5px; background-color:#222222;}
                                              .btn:hover{background:black; border-color:black}
                                              .form-control {border-style: solid;
                                              border-radius:5px;}
                                              .well {border-style: solid;
                                              border-radius:5px;}
                                              a {color: #ae1b2d;}
                                              style="margin-bottom:50px;"
                                              a:hover {color: #76121C;}
                                              #header:hover {background:red;}'))
                            )
                        ),  
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Parámetros",
                           br(),
                           radioButtons('Met', 'Seleccione el modelo de variograma:',
                                         c( #'Modelo lineal' = "1",
                                            'Efecto pepita' = "2",
                                            'Modelo esférico' = "3",
                                            'Modelo exponencial' = "4",
                                            'Modelo gaussiano' = "5"),
                                         selected = '5'),
                           
                           splitLayout(
                             mainPanel(h4("Parámetros del variograma"), style = "overflow:hidden;",
                                       br(),
                                       tableOutput(outputId = 'res') %>% withSpinner(color="#0dc5c1"),
                                       br(),
                                       uiOutput("download"),
                                       br(),
                                       br()),
                             
                             mainPanel(h4("Valores promedios"), style = "overflow:hidden;",
                                       br(),
                                       tableOutput( 'medios') %>% withSpinner(color="#0dc5c1"))
                           )
                 ),
                 
                 ########################
                 ### Panel HARD_nei #####
                 ########################
                 tabPanel("Hard BME",
                           
                           splitLayout(
                             mainPanel(br(),
                                       uiOutput("neighs"),
                                       br(),
                                       h4(' '),
                                       fileInput("clustFile2", "Seleccionar el fichero de clusters", 
                                                  accept = c("text/plain", " ", ".txt") ),
                                       br(),
                                       h4(' '),
                                       fileInput('paramsFile2', 'Seleccionar el fichero de parámetros (opcional)', 
                                                  accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv' ) ),
                                       br(),
                                       radioButtons('Met2', 'Seleccione el modelo de variograma:',
                                                     c(#'Modelo lineal' = "1",
                                                        'Efecto pepita' = "2",
                                                        'Modelo esférico' = "3",
                                                        'Modelo exponencial' = "4",
                                                        'Modelo gaussiano' = "5"),
                                                     selected = '5'),
                                       br(),
                                       uiOutput("downloadHard"),
                                       br(),
                                       br()),
                                       # actionButton('doHard2', "Aplicar")),
                               mainPanel(br(), style = "overflow:hidden;",
                                        h4('Resultados'),
                                        tableOutput('tablaFinal') %>% withSpinner(color="#0dc5c1"),
                                        br(),
                                        uiOutput("download1"),
                                        br(),
                                        br()),
                             mainPanel(br(), style = "overflow:hidden;",
                                       h4('Imputaciones'),
                                       tableOutput('tablaImputHard') %>% withSpinner(color="#0dc5c1"),
                                       br(),
                                       uiOutput("downloadImputHard"),
                                       br(),
                                       br(),
                                       uiOutput("downloadNAsHard"),
                                       br()))
                 ),
                 
                 ########################
                 #### Panel Soft_nei ####
                 ########################
                 tabPanel( "Soft BME",
                           
                           splitLayout(
                             mainPanel(br(),
                                       uiOutput("neighs1"),
                                       
                                       br(),
                                       fileInput('datosSoft', 'Seleccionar el fichero de valores soft',
                                                 accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                       
                                       # br(),
                                       fileInput("clustFile5", "Seleccionar el fichero de clusters", 
                                                  accept = c("text/plain", " ", ".txt")),
                                       
                                       # br(),
                                       fileInput("hardFile1", "Seleccionar el fichero hard", 
                                                  accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                       
                                       # br(),
                                       fileInput("softFile1", "Seleccionar el fichero soft", 
                                                  accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                       
                                       # br(),
                                       # radioButtons('delta1', 'Seleccionar una opcion:',
                                       #               c('Porcentaje' = "1",
                                       #                  'Numero' = "2"),
                                       #               selected = '1'),
                                       numericInput("deltaValue1", "Delta", 1.1, min = 1, max = 100, width = '100px'),
                                       
                                       # br(),
                                       fileInput('paramsFile5', 'Seleccionar el fichero de parámetros (opcional)', 
                                                  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                       # br(),
                                       radioButtons('Met5', 'Seleccione el modelo de variograma:',
                                                     c(#'Modelo lineal' = "1",
                                                       'Efecto pepita' = "2",
                                                       'Modelo esférico' = "3",
                                                       'Modelo exponencial' = "4",
                                                       'Modelo gaussiano' = "5"),
                                                     selected = '5'),
                                       br(),
                                       # actionButton("doSoftNei", "Aplicar"),
                                       uiOutput("downloadSoft"),
                                       br(),
                                       br(),
                                       br()),
                 mainPanel(br(), style = "overflow:hidden;",
                           h4('Resultados'),
                           tableOutput( 'tablaFinal2') %>% withSpinner(color="#0dc5c1"),
                           br(),
                           uiOutput("download2"),
                           br(),
                           br()),
                 mainPanel(br(), style = "overflow:hidden;",
                           h4('Imputaciones'),
                           tableOutput('tablaImputSoft') %>% withSpinner(color="#0dc5c1"),
                           br(),
                           uiOutput("downloadImputSoft"),
                           br(),
                           br(),
                           uiOutput("downloadNAsSoft"),
                           br()))
                 ),
                 
                 ########################
                 ###### Panel PMF #######
                 ########################
                 tabPanel("PMF",
                           
                  splitLayout(
                    mainPanel(br(),
                              numericInput("epocs", "Número de épocas: ", 100, min = 10, max = 300),
                              h4(' '),
                              fileInput("clustFile3", "Seleccionar el fichero de clusters", 
                                accept = c("text/plain", " ", ".txt")),
                              uiOutput("downloadPMF"),
                              br(),
                              br(),
                              br()),
                              # actionButton("doPMF", "Aplicar")),
                    mainPanel(br(), style = "overflow:hidden;",
                             h4('Resultados'),
                             tableOutput( 'tablaFinal3') %>% withSpinner(color="#0dc5c1"),
                             br(),
                             uiOutput("download3"),
                             br(),
                             br()),
                    mainPanel(br(), style = "overflow:hidden;",
                              h4('Imputaciones'),
                              tableOutput('tablaImputPMF') %>% withSpinner(color="#0dc5c1"),
                              br(),
                              uiOutput("downloadImputPMF"),
                              br(),
                              br(),
                              uiOutput("downloadNAsPMF"),
                              br()))
                 ),
                 
                 ########################
                 ###### Panel Soft ######
                 ########################
                 # tabPanel( "Soft",
                 #           
                 #           h4(' '),
                 #           fileInput( "clustFile4", "Seleccionar el fichero de clusters", 
                 #                      accept = c("text/plain", " ", ".txt") ),
                 #           
                 #           h4(' '),
                 #           fileInput( "hardFile", "Seleccionar el fichero hard", 
                 #                      accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv' ) ),
                 #           
                 #           h4(' '),
                 #           fileInput( "softFile", "Seleccionar el fichero soft", 
                 #                      accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv' ) ),
                 #           
                 #           h4('Delta'),
                 #           radioButtons( 'delta', 'Seleccionar una opcion:',
                 #                         c( 'Porcentaje' = "1",
                 #                            'Numero' = "2"),
                 #                         selected = '1'),
                 #           numericInput("deltaValue", "", 5, min = 1, max = 100, width = '100px'),
                 #           
                 #           h4(' '),
                 #           fileInput( 'paramsFile4', 'Seleccionar el fichero de parametros (opcional)', 
                 #                      accept = c( 'text/csv', 'text/comma-separated-values,text/plain', '.csv' ) ),
                 #           
                 #           radioButtons( 'Met4', 'Seleccione el modelo de variograma:',
                 #                         c( 'Modelo lineal' = "1",
                 #                            'Efecto pepita' = "2",
                 #                            'Modelo esferico' = "3",
                 #                            'Modelo exponencial' = "4",
                 #                            'Modelo gaussiano' = "5"),
                 #                         selected = '5'),
                 #           
                 #           actionButton("doSoft", "Aplicar"),
                 # ),
                 
                 tabPanel("Localizaciones", style = "overflow:hidden;",
                           br(),
                           h4("Tabla de localizaciones"),
                           tableOutput( 'vistacsv') %>% withSpinner(color="#0dc5c1"),
                           br(),
                           br()
                 ),
                 
                 tabPanel( "Datos",
                           br(),
                           h4("Tabla de valores"),
                           tableOutput('vistacsv1') %>% withSpinner(color="#0dc5c1"),
                           br(),
                           br()
                 )#,
                 
                 # tabPanel( "Variograma",
                 #           h4( "Pintando el variograma" ),
                 #           plotOutput("grafica")
                 # ),
                 
                 # tabPanel("Ayuda",
                           # h4( "Panel de ayuda" ),
                           # HTML( " <ol><li>El fichero que contiene las localizaciones debe estar formado por tres columnas. La primera es un número entero que hace referencia al número de sensor (comenzando en 1), y las dos restantes se corresponden con las coordenas de cada sensor.</li>
                           #       <li>El fichero de datos debe ser una matriz de tamañoo NxM donde cada fila se corresponde con un sensor, y cada columna con ua medicion temporal.</li>
                           #       </ol>")
                 # )

            )# ...tabsetpanel
               
        )#...mainpanel
    )

)