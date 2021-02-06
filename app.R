# Colegio Universitario de Estudios Financieros
# Máster en Data Science para Finanzas
# Técnicas de visualicación para Data Science
# Madrid, diciembre de 2020

# Trabajo final de la asignatura
# Tema: aplicación web con Shiny para editar gráficos de ggplot2.

# Integrantes del grupo:
  # Blanco García, Gabriel 
  # Ferrín Meilán, Michelle
  # Piqueras Córcoles, Lucía
  # Ruedas Burgos, Marta

# Librerías  ------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ISLR)
library(DT)

# Generamos esta lista porque la utilizaremos después para que el 
# usuario pueda seleccionar los temas de los gráficos.
listaTemas <- list("Clásico" = theme_classic(),
                   "Minimal" = theme_minimal(),
                   "Five ThirtyEight" = theme_fivethirtyeight(),
                   "Gris" = theme_igray(),
                   "Pander" = theme_pander(),
                   "Highchart JS" = theme_hc(),
                   "Solarized" = theme_solarized(),
                   "Solarized 2" = theme_solarized_2(),
                   "Wall Street Journal" = theme_wsj(),
                   "The economist" = theme_economist(),
                   "The economist white" = theme_economist_white(),
                   "Blanco y negro" = theme_bw(),
                   "Light" = theme_light(),
                   "Oscuro" = theme_dark(),
                   "Google Docs" = theme_gdocs(),
                   "Libre Office" = theme_calc(),
                   "Excel" = theme_excel_new(),
                   "Stata" = theme_stata(),
                   "Tufte" = theme_tufte(),
                   "Few" = theme_few())

# Links de las documentaciones de los dataset de demo
helpCoches <- 'https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/mpg'
helpCarseats <- 'https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Carseats'
helpDiamantes <- 'https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/diamonds'

# User interface --------------------------------------------------------------

ui <- fluidPage(
  
  theme = shinytheme("sandstone"), # el tema del shiny
  
  h1( # titulo de la página, en h1 y negrita
    titlePanel(
      strong("Interfaz de visualización: paquete ggplot2"))),
  
  fluidRow(
    
    column(12, 
           wellPanel(
             h4(
               strong("Próposito")))) # lo utilizamos como título de sección
    
    ), 

  
  fluidRow( # en esta fila proporcionamos información de la aplicación
    
      column(12,
             wellPanel(
             p("Esta apliación permite visualizar datos de manera sencilla utilizando
               el paquete", code("ggplot2()"), "de manera intuitiva."),
             
             span(), # separación entre párrafos
             
             p("En primer lugar, se muestra
             una vista previa de los datos con los que se va a trabajar. 
             Contiene tres datasets de muestra: ",
               
               a(href = helpCoches, # con esto insertamos los link de las 
                                    # documentaciones de cada dataset
                 'coches',
                 .noWS = "outside"), ", ",
               
               a(href = helpCarseats,
                 'venta de sillas de bebé',
                 .noWS = "outside"), " y ",
               
               a(href = helpDiamantes,
                 'diamantes.',
                 .noWS = "outside")),
             
             span(),
             
             p("Adicionalmente, el usuario puede subir sus
             propios datos guardados en formato csv. Es posible elegir el tipo
             de visualización y las variables que se representan en cada eje.
             También es posible seleccionar las variables que se utilizan como
             color y tamaño del gráfico."),
             
             span(),
             
             p("Para mejorar la experienca, al final de la página se muestra el código
             de R empleado para generar la visualización, y es posible guardar
             el gráfico.")
             
             ) # cierre del wellPanel
             
             ) # cierre de la columna
      
  ), # cierre del fluidRow

  fluidRow( # en esta sección dejamos enlaces a nuestros LinkedIn y Github
    
    # Titulo de autores
    column(12, 
           wellPanel(
             h4(
               strong("Autores")))),
     
    # Gabriel
    column(3,
           wellPanel(
             h5(
               strong("Blanco García, Gabriel:")),
             
             tags$ul( # ul (unordered list) permite crear una lista con 
                      # bullet points
               
               tags$li( # primer bullet con el enlace a LinkedIn
                 a(href = 'https://www.linkedin.com/in/gabriel-blanco-garc%C3%ADa-75a197184/',
                   ' linkedIn/gabrielblanco',
                   .noWS = "outside")),
        
               tags$li( # segundo bullet, enlace a Git Hub
                 a(href = 'https://github.com/gabrielblancogarcia',
                  ' github.com/gabrielblancogarcia',
                  .noWS = "outside"))
               
               ) # cierre ul
        
        ) # cierre wellPanel
        
        ), # cierre columna
    
    # Michelle
    column(3,
           wellPanel(
             h5(
               strong("Ferrín Meilán, Michelle:")),
             
             tags$ul(
               
               tags$li(
                 a(href = 'https://www.linkedin.com/in/michelle-ferr%C3%ADn-meil%C3%A1n-464974184/',
                   ' linkedIn/michelleferrinmeilan',
                   .noWS = "outside")),
        
               tags$li(
                 a(href = 'https://github.com/MichelleFerrin',
                  ' github.com/michelleferrinmeilan',
                  .noWS = "outside"))
               ) # cierre ul
             
             ) # cierre wellPanel
           
           ), # cierre columna

    
    column(3,
           wellPanel(
             h5(
               strong("Piqueras Córcoles, Lucía:")),
             
             tags$ul(
               
               tags$li(
                 a(href = 'https://www.linkedin.com/in/lucía-piqueras-córcoles-56b84b187/',
                  ' linkedIn/luciapiquerascorcoles',
                  .noWS = "outside")),
               
               tags$li(
                 a(href = 'https://github.com/luciapiquerascorcoles', 
                  ' github.com/luciapiquerascorcoles',
                  .noWS = "outside"))
               
               ) # cierrre ul
             
             ) # cierre wellPanel
           
           ), # cierre columna
    
    column(3, 
           wellPanel(
             h5(
               strong("Ruedas Burgos, Marta:")),
             
             tags$ul(
               
               tags$li(
                a(href = 'https://www.linkedin.com/in/martaruedas/',
                  ' linkedIn/martaruedas',
                  .noWS = "outside")),
               
               tags$li(
                 a(href = 'https://github.com/martaruedas',
                  ' github.com/martaruedas',
                  .noWS = "outside"))
               
               ) # cierre ul
             
             ) # cierre wellPanel
           
           ) # cierre columna
    
    ), # cierre del fluidrow
  
  fluidRow(
    
    column(12,
           wellPanel(
             h3(
               strong("Vista previa de los datos")
               )
             )
           )
    
    ),
  
  fluidRow(
    
    column(2,  # con esto el usuario el dataset de demo 
           selectInput("seleccionDataset",
                       label = "Elige un dataset",
                       
                       choices = list("Coches" = "mpg",
                                      "Venta de sillas" = "Carseats",
                                      "Diamantes" = "diamonds"),
                       
                       selected = "mpg"),
           
           # con este input, se sube el archivo csv
           fileInput("userDataset", 
                      "O sube un archivo csv",
                      accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           uiOutput("parametrosLectura") # lo creamos como renderUI abajo para 
                                         # que los paráemtros de parseo solo 
                                         # aparezcan cuando el usuario suba un 
                                         # csv
           
    ), # cierre de la primera columna
    
    column(10,
           dataTableOutput("previewDataset")) # preview del dataset
  
  ), # cierre del fluidrow
  
  fluidRow(
    column(12,
           wellPanel(
             h3(
               strong("Visualizaciones")
               )
             )
           )
    
  ),
  
  fluidRow(
    
    column(2,
           selectInput("tipoGrafico", # con este input se selecciona el tipo de
                                      # gráfico
                       label = "Tipo de gráfico",
                       
                       choices = list("Dispersión" = "geom_jitter",
                                      "Grafico de conteo" = "geom_bar",
                                      "Grafico de barras" = "geom_col",
                                      "Caja y bigotes" = "geom_boxplot"),
                       
                       selected = "geom_jitter"), # tipo de grafico
           
           textInput("tituloGrafico", # con este input el usuario puede poner 
                                      # título al gr´´afico
                     label = "Título del gráfico",
                     value = "Título"),
           
           selectInput("temaGrafico", # y con este, selecciona el tema del 
                                      # gráfico
                       label = "Diseño del gráfico",
                       choices = names(listaTemas),
                       selected = "Pander"),
           
           # boton para descargar el gráfico
           downloadButton("descargarPlot",
                          label = "Descargar gráfico")
           
           ), # cierre de la columna  
    
    column(2, 
           uiOutput("elementosGraficos")), # variables de los graficos
    
    column(8, 
           plotOutput("graficos")) # el output del plot
    
  ), # cierre del fluidrow
  
  fluidRow(
    
    column(12,
           wellPanel(
             h3(
               strong("Código de las visualizaciones")
             )
           ))
    
  ),
  
  fluidRow(
    
    column(12,
           wellPanel(
             "Todo el código de los gráficos se genera utilizando el paquete ",
             code("ggplot2()"),
             ", pero algunos de los temas que se incluyen provienen del paquete ",
             code("ggthemes()"),
             ". El dataset de Carseats se encuentra incluido en el paquete",
             code("ISLR"), ", del libro", 
             em("An Introduction to Statistical Learning.")
             )
           )
    
  ),
  
  fluidRow(
    
    column(12, 
           verbatimTextOutput("codigoGrafico")))
  
)



# Server ----------------------------------------------------------------------
server <- function(input, output) {
  
  # Antes de nada, creamos un reactive con los datos del usuario. Solo se 
  # ejecuta si el usuario sube archivos
  datosSubidos <- reactive({
    
    if (is.null(input$userDataset)) {
      
    # si no se han subido datos, no se ejecuta nada
    
  } else if (!is.null(input$userDataset)) {
    
    # Si se suben datos, se crea el dataframe datosSubidos() (reactive)
    # haciendo read.csv con los parámetros de parseo que ha especificado 
    # el usuario
      
      req(input$userDataset)
       
      read.csv(file = input$userDataset$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote,
               dec = input$dec, 
               encoding = "UTF-8") 
      }
  }) # cierre del reactive
  
  # Ahora, creamos un renderUI para que se muestren las opciones del CSV solo si el usuario
  # decide subirlo. Si el usuario no sube los datos, el valor del input
  # es NULL. En todos los ifelses, programamos primero la parte del código
  # de los dataset de R, y no el código de los gráficos del usuario. Seguimos 
  # esta estructura porque consideramos que lo habitual es no subir los datos, 
  # y de esta manera, se ejecuta menos código. 
  
  output$parametrosLectura <- renderUI({
    
    if ((is.null(input$userDataset))) {

      # no hacer nada 
      
    } else if (!is.null(input$userDataset)) {
      
      tagList(

        # Por si tiene encabezado 
        checkboxInput("header", 
                      label = "¿Tienen encabezado?",
                      value = TRUE),
        
        # Para el separador
        selectInput("sep",
                    label = "Separador",
                    choices = c(Coma = ",",
                                `Punto y coma` = ";",
                                Tab = "\t"),
                    selected = ","),
        
        # Para el tipo de comillas 
        selectInput("quote",
                    label = "Tipo de comillas",
                    choices = c(Ningunas = "",
                                `Comillas dobles` = '"',
                                `Comillas simples` = "'"),
                    selected = '"'),
        
        # Para el separador decimal
        selectInput("dec",
                    label = "Separador decimal",
                    choices = c(Punto = ".",
                                Coma = ","),
                    selected = "."))
    }
    
  }) # cierre output parámetros

  # Output con una preview de los datos
  output$previewDataset <- renderDataTable({ 

    if (is.null(input$userDataset)) { 
      
      get(input$seleccionDataset) %>%
        datatable(style = "bootstrap4",
                  options = list("pageLength" = 5))
      
    } else if (!is.null(input$userDataset)) {
      
        datosSubidos() %>%
          head() %>%
          datatable(style = "bootstrap4",
                    options = list("pageLength" = 5))
      
    }

  }) # cierre del output de la preview
  
  # Output para seleccionar los distintos tipos de gráficos
  output$seleccionGrafico <- renderUI({
    
    
    
  }) # cierre del output de seleccion de gráficos

  #  Output para seleccionar las variables de cada eje. Dependerá del dataset
  # seleccionado
  output$elementosGraficos <- renderUI({
    
  # Primero creamos la lista de variables de cada dataset. Utilizamos 
  # colnames para no tener que escribir a mano cada variable
  
  # Como no tiene sentido utilizar variables categóricas en el eje Y, 
  # creamos 3 listas de cada dataset: una con  todas las variables, otra 
  # con las variables numérocas y otras con las categoricas. De este modo,
  # restringiremos las variables del eje y a la lista de las numéricas. 
  # El razonamiento es el mismo para la variable size.
  
  # Creamos dos funciones para sacar las numéricas y las categoricas, y así
  # no tener que repetir el codigo 

  # Funcion para las numericas: solo las tipo numeric
    variablesNumericas <- function(datos) {
      
      datos %>%
        select(where(is.numeric)) %>%
        select(2, everything()) %>% # para que coja la segunda y la ponga como 
                                    # primera. De este modo se arregla la 
                                    # coinidencia del primer elemento de la 
                                    # lista de todas las variables, y las 
                                    # variables numericas. Hacemos esto porque 
                                    # "selected" dentro de selectInput, 
                                    # por defecto, toma el primer valor de la
                                    # lista, y no queremos que coincidan. 
                                    # Cambiando los valores de selected nos daba
                                    # problemas
        colnames()
      
      }
  
  # Función para las categóricas: strings y factores
    variablesCategoricas <- function(datos) {
      
      datos %>%
        select(where(is.character),
               where(is.factor)) %>%
        colnames()
      
      }  
    
  # Creamos un objeto que contenga la opción para no usar mapeo
    opcionNulo <- c("Sin valor" = "NULL")
  
  # Variables de coches -------------------------------------------------------
    
    # Todas
    if (is.null(input$userDataset)) { # si no se ha subido un dataset
      
      variablesCoches <- colnames(mpg)
      
      opcionesCoches <- list("Fabricante" = variablesCoches[1],
                             "Modelo" = variablesCoches[2],
                             "Tamaño del motor" = variablesCoches[3],
                             "Año" = variablesCoches[4],
                             "Cilindros" = variablesCoches[5],
                             "Tipo de transmision" = variablesCoches[6],
                             "Tipo de traccion" = variablesCoches[7],
                             "Consumo por ciudad" = variablesCoches[8],
                             "Consumo por autopista" = variablesCoches[9],
                             "Tipo de combustible" = variablesCoches[10],
                             "Modelo" = variablesCoches[11])
     
      # Añadimos la opción de nulo: lo hacemos así porque esta opción se 
      # incluyó posteriormente a la creación de las listas, por mantener 
      # el flujo de trabajo y no cambiar el código
      opcionesCoches <- c(opcionesCoches, opcionNulo)
     
    # Variables numericas
      variablesNumericasCoches <- variablesNumericas(mpg)
      
      opcionesNumericasCoches <- list("Año" = variablesNumericasCoches[1],
                                      "Tamaño del motor" = variablesNumericasCoches[2],
                                      "Cilindros" = variablesNumericasCoches[3],
                                      "Consumo por ciudad" = variablesNumericasCoches[4],
                                      "Consumo por autopista" = variablesNumericasCoches[5])
    
      opcionesNumericasCoches <- c(opcionesNumericasCoches, opcionNulo)
  
    # Variables categoricas
      variablesCategoricasCoches <- variablesCategoricas(mpg)
    
      opcionesCategoricasCoches <- list("Fabricante" = variablesCategoricasCoches[1],
                                        "Modelo" = variablesCategoricasCoches[2],
                                        "Tipo de transmisión" = variablesCategoricasCoches[3],
                                        "Tipo de tracción" = variablesCategoricasCoches[4],
                                        "Tipo de combustible" = variablesCategoricasCoches[5],
                                        "Modelo" = variablesCategoricasCoches[6])
      
      opcionesCategoricasCoches <- c(opcionesCategoricasCoches, opcionNulo)
    
  
    # Variables de sillas -----------------------------------------------------
     variablesSillas <- colnames(Carseats)
      
     opcionesSillas <- list("Ventas" = variablesSillas[1],
                            "Precio del competidor" = variablesSillas[2],
                            "Ingresos" = variablesSillas[3],
                            "Publicidad" = variablesSillas[4],
                            "Población" = variablesSillas[5],
                            "Precio" = variablesSillas[6],
                            "Localización en tienda" = variablesSillas[7],
                            "Edad" = variablesSillas[8],
                            "Educación" = variablesSillas[9],
                            "Zona" = variablesSillas[10],
                            "País" = variablesSillas[11])
     
     opcionesSillas <- c(opcionesSillas, opcionNulo)
    
    # Variables numericas de sillas 
      variablesNumericasSillas <- variablesNumericas(Carseats)
      
      opcionesNumericasSillas <- list("Precio de la competencia" = variablesNumericasSillas[1],
                                      "Ventas" = variablesNumericasSillas[2],
                                      "Ingresos" = variablesNumericasSillas[3],
                                      "Publicidad" = variablesNumericasSillas[4],
                                      "Poblacion" = variablesNumericasSillas[5],
                                      "Precio" = variablesNumericasSillas[6],
                                      "Edad" = variablesNumericasSillas[7])
      
      opcionesNumericasSillas <- c(opcionesNumericasSillas, opcionNulo)
    
    # Variables categoricas de sillas 
      variablesCategoricasSillas <- variablesCategoricas(Carseats)
      
      opcionesCategoricasSillas <- list("Localización en tienda" = variablesCategoricasSillas[1],
                                        "Zona" = variablesCategoricasSillas[2],
                                        "País" = variablesCategoricasSillas[3])
      
      opcionesCategoricasSillas <- c(opcionesCategoricasSillas, opcionNulo)
  
    # Variables de diamantes -----------------------------------------------------
      variablesDiamantes <- colnames(diamonds)
      
      opcionesDiamantes <- list("Peso" = variablesDiamantes[1],
                                "Calidad del corte" = variablesDiamantes[2],
                                "Color" = variablesDiamantes[3],
                                "Claridad" = variablesDiamantes[4],
                                "Profundidad (ratio)" = variablesDiamantes[5],
                                "Diametro mayor" = variablesDiamantes[6],
                                "Precio" = variablesDiamantes[7],
                                "Longitud (mm)" = variablesDiamantes[8],
                                "Ancho (mm)" = variablesDiamantes[9],
                                "Profundidad (mm)" = variablesDiamantes[10])
      
      opcionesDiamantes <- c(opcionesDiamantes, opcionNulo)
    
    
    # Variables numericas diamantes
      variablesNumericasDiamantes <- variablesNumericas(diamonds)
      
      opcionesNumericasDiamantes <- list("Profundidad (ratio)" = variablesNumericasDiamantes[1],
                                         "Peso" = variablesNumericasDiamantes[2],
                                         "Diámetro mayor" = variablesNumericasDiamantes[3],
                                         "Precio" = variablesNumericasDiamantes[4],
                                         "Longitud (mm)" = variablesNumericasDiamantes[5],
                                         "Ancho (mm)" = variablesNumericasDiamantes[6],
                                         "Profundidad (mm)" = variablesNumericasDiamantes[7])
      
      opcionesNumericasDiamantes <- c(opcionesNumericasDiamantes, opcionNulo)
    
    # Variables categóricas diamantes
      variablesCategoricasDiamantes <- variablesCategoricas(diamonds)
      
      opcionesCategoricasDiamantes <- list("Calidad del corte" = variablesCategoricasDiamantes[1],
                                           "Color" = variablesCategoricasDiamantes[2],
                                           "Claridad" = variablesCategoricasDiamantes[3])
      
      opcionesCategoricasDiamantes <- c(opcionesCategoricasDiamantes, opcionNulo)
    
    # Construcción del taglist() ----------------------------------------------
    # Creamos las tres listas, a las que se accederá con la sintaxis:
    # listaXXXX[input$seleccionDataset]
      
    # Lista de todas las listas en 3: sin distinción, numéricas y categóricas
      listaOpciones <- list("mpg" = opcionesCoches,
                            "Carseats" = opcionesSillas,
                            "diamonds" = opcionesDiamantes)
    
    # Lista de las numéricas
      listaOpcionesNumericas <- list("mpg" = opcionesNumericasCoches,
                                     "Carseats" = opcionesNumericasSillas,
                                     "diamonds" = opcionesNumericasDiamantes)
    
    # Lista de las categóricas
      listaOpcionesCategoricas <- list("mpg" = opcionesCategoricasCoches,
                                       "Carseats" = opcionesCategoricasSillas,
                                       "diamonds" = opcionesCategoricasDiamantes)
      
      tagList(
      
      # La del eje X
        selectInput("ejeX",
                  label = "Variable del eje X",
                  choices = listaOpciones[input$seleccionDataset]),
      
      # La del eje y
        selectInput("ejeY",
                    label = "Variable del eje y",
                    choices = listaOpcionesNumericas[input$seleccionDataset]),
      
      # La variable color 
        selectInput("variableColor",
                    label = "Variable del color",
                    choices = listaOpciones[input$seleccionDataset],
                    selected = opcionNulo),
      
      # La variable del tamaño
      selectInput("variableSize", 
                  label = "Variable del tamaño",
                  choices = listaOpcionesNumericas[input$seleccionDataset],
                  selected = opcionNulo)
      
      ) # cierre del taglist
    
    } else if (!is.null(input$userDataset)) {
      
      # Las opciones con las variables del dataset del usuario
      
      # Sin distinción
      variablesDatosSubidos <- colnames(datosSubidos())
      
      # Solo nunércias
      variablesNumericasDatosSubidos <- variablesNumericas(datosSubidos())
      
      # Solo categóricas
      variablesCategoricasDatosSubidos <- variablesCategoricas(datosSubidos())
      
      # Añadimos la opcion del mapeo a nulo en las tres 
      variablesDatosSubidos <- c(variablesDatosSubidos, opcionNulo)
      variablesNumericasDatosSubidos <- c(variablesNumericasDatosSubidos, opcionNulo)
      variablesCategoricasDatosSubidos <- c(variablesCategoricasDatosSubidos, opcionNulo)
      
      # Creamos el taglist que verá el usuario en caso de que suba sus datos
      tagList(
        
        selectInput("ejeX",
                  label = "Variable del eje x",
                  choices = variablesDatosSubidos,
                  selected = variablesDatosSubidos[1]),
        
        selectInput("ejeY",
                    label = "Variable del eje y",
                    choices = variablesNumericasDatosSubidos,
                    selected = variablesNumericasDatosSubidos[1]),
        
        selectInput("variableColor", 
                    label = "Variable color",
                    choices = variablesDatosSubidos,
                    selected = opcionNulo),
        
        selectInput("variableSize", 
                    label = "Variable de tamaño",
                    choices  = variablesNumericasDatosSubidos,
                    selected = opcionNulo)
        
        ) # cierre del taglist
    
      
    } # cierre del ifelse
  
  }) # cierre del renderUI de las variables de mapeo
  
  # Creamos un reactive con el tema que elija el usario. La sintaxis de 
  # acceso es la misma que se usa en las otras listas
  tema <- reactive({
    
    listaTemas[[input$temaGrafico]]
    
  })
  
  # Output con los distintos gráficos que verá el usuario
  output$graficos <- renderPlot({
    
    # Definimos los parámetros fuera
    
    # Utilizamos get para eliminar las comillas del nombre del dataset
    datos <- get(input$seleccionDataset)
    
    # Definimos el resto de inputs
    x <- input$ejeX
    y <- input$ejeY
    color <- input$variableColor
    size <- input$variableSize
    titulo <- input$tituloGrafico
    
    
    if (is.null(input$userDataset)) {# Creamos un gráfico base para no tener que repetir el código
        
      graficoBase <- ggplot(data = datos) +
        aes_string(x = x,
                   y = y,
                   fill = color,
                   size = size) + # fill es la que mas se repite, lo 
                                  # dejamos como global.
        
        tema() + # tema que elija, entre paréntesis por ser reactivo
        
        theme(axis.text.x = element_text(angle = 45, 
                                         vjust = 0.5), # evitar overlapping de 
                                                       # las xticks labels
              
              legend.position = "bottom") + # leyenda bajo el gráfico
        
        labs(title = titulo) # el título que introduzca el usuario
          
          if (input$tipoGrafico == "geom_jitter") {
            
            graficoBase +
              
              # Usamos geom_jitter con alpha para evitar que los puntos se 
              # solapen. 
              geom_jitter(aes_string(color = color), # aqui el color se introduce como
                                                     # color, no como fill
                          alpha = 0.5) +
            
              labs(title = titulo) 
          
        } else if (input$tipoGrafico == "geom_bar") {
            
          # En el caso de geom_bar, no podemos usar el gráfico base, porque 
          # geom_bar solo puede usar un eje de mapeo.
          
            ggplot(data = datos) +
              geom_bar(aes_string(x = x,
                                  fill = color), # color de relleno
                       
                       color = "black") + # color del borde de las barras
              
              tema() + 
            
              theme(axis.text.x = element_text(angle = 45),
                    legend.position = "bottom") +
            
              labs(title = titulo)
          
        } else if (input$tipoGrafico == "geom_col") {
          
          graficoBase +
            
            geom_col() 
          
        } else if (input$tipoGrafico == "geom_boxplot") { 
          
          graficoBase +
            
            # Modificamos los aesthetics de tal manera que el boxplot se 
            # muestre ordenado. Ordena la variable del eje x, es decir, cada
            # box, en función de la variable del eje y
            geom_boxplot(aes(x = reorder(get(input$ejeX), 
                                         get(input$ejeY)),
                             
                             y = get(input$ejeY))) 
          
          }
        
    } else if (!is.null(input$userDataset)) {
      
      # Igual que antes, definimos un gráfico base. El código es basicamente el
      # mismo pero cambiando los datos, así que no lo comentamos.
      graficoBaseUser <- ggplot(datosSubidos(), # los datos del usario
                                aes_string(x = x,
                                           y = y,
                                           fill = color,
                                           size = size)) +
        
        tema() + 
        
        theme(axis.text.x = element_text(angle = 45),
              legend.position = "bottom") +
        
        labs(title = titulo)
      
        if (input$tipoGrafico == "geom_jitter") {
          
          graficoBaseUser +
            geom_jitter(alpha = 0.5, 
                        aes_string(color = color))
          
        } else if (input$tipoGrafico == "geom_col") { 
            
          graficoBaseUser +
            
            geom_col() 
          
        } else if (input$tipoGrafico == "geom_bar") {
          
          ggplot(datosSubidos(), 
                 aes_string(x = x,
                            fill = color)) +
            
            geom_bar(color = "black")  +
            
            tema() +
            
            theme(axis.text.x = element_text(angle = 45),
                  legend.position = "bottom") +
            
            labs(title = titulo)
          
        } else if (input$tipoGrafico == "geom_boxplot") {
          
          graficoBaseUser +
            
            geom_boxplot(aes(x = reorder(get(input$ejeX), 
                                         get(input$ejeY)),
                             
                             y = get(input$ejeY)))
          
          }
      
    } # cierre del ifelse de si sube datos o no
    
  }) # cierre del output de los plots
  
  # Output que muestra el código que se utiliza para cada gráfico
  output$codigoGrafico <- renderText({
    
    # Dos versiones: si el usuario no mete su dataset, el argumento "data" se 
    # muestra exactamente con el nombre que esos datos tienen en R.
    # Si el usuario sube un dataset, como desconocemos el nombre, el argumento
    # data toma el valor "datos"
    
    x <- input$ejeX
    y <- input$ejeY
    color <- input$variableColor
    size <- input$variableSize
    titulo <- input$tituloGrafico
    datos <- input$seleccionDataset
    bottom <- "\"bottom\"" # de esta manera, bottom aparece con comillas. De 
                           # lo contrario, el código en R no funcionaría.
    
    # Necesitamos generar esta lista para que la parte del código donde se 
    # especifican los temas funcione. Ahora la parte de los temas está en string
    temasString <- list("Clásico" = "theme_classic()",
                        "Minimal" = "theme_minimal()",
                        "Five ThirtyEight" = "theme_fivethirtyeight()",
                        "Gris" = "theme_igray()",
                        "Pander" = "theme_pander()",
                        "Highchart JS" = "theme_hc()",
                        "Solarized" = "theme_solarized()",
                        "Solarized 2" = "theme_solarized_2()",
                        "Wall Street Journal" = "theme_wsj()",
                        "The economist" = "theme_economist()",
                        "The economist white" = "theme_economist_white()",
                        "Blanco y negro" = "theme_bw()",
                        "Light" = "theme_light()",
                        "Oscuro" = "theme_dark()",
                        "Google Docs" = "theme_gdocs()",
                        "Libre Office" = "theme_calc()",
                        "Excel" = "theme_excel_new()",
                        "Stata" = "theme_stata()",
                        "Tufte" = "theme_tufte()",
                        "Few" = "theme_few()")
    
    tema <- temasString[[input$temaGrafico]] # acceso al que ha seleccionado el 
                                             # usuario
    
    if (is.null(input$userDataset)) { # primero si no lo ha subido
      
      if (input$tipoGrafico == "geom_jitter") { # se hace un if para cada
                                                # posible gráfico
        
        paste0("ggplot(data = ", datos, ", #  datos en formato Data Frame
              
              # aquí van los mapeos del gráfico 
              mapping = aes(x = ", x, ", # variable del eje x 
              
                            y = ", y, ", # variable del eje y
                            
                            color = ", color, ",  # variable para colorear (numérica o categórica)
                            
                            size = ", size, ")) + # variable de tamaño (numérica)
                            
        # geom_jitter añade ruido aleatorio a los puntos para evitar el solapamiento de las observaciones
        
        geom_jitter(alpha = 0.5) + # alpha es la transparencia, también para evitar el solapamiento 
        
        ", tema, " + # el tema del gráfico
        
        labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
        
        theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
              legend.position = ", bottom,") # la posición de la leyenda  
            
               ")
        
      } else if (input$tipoGrafico == "geom_bar") {
        
        paste0("ggplot(data = ", datos, ", #  datos en formato Data Frame
              
              # aquí van los mapeos del gráfico 
              mapping = aes(x = ", x, ", # variable del eje X
              
                            # No hay variable en el eje Y porque geom_bar() cuenta automáticamente.
                            # En el eje y se representa la cuenta de la variable del eje X
                            
                            fill = ", y, ")) +  # variable para colorear (numérica o categórica)
        
        geom_bar() + # el gráfico de columnas 
        
        ", tema, " + # el tema del gráfico
        
        labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
        
        theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
              legend.position = ", bottom,") # la posición de la leyenda  
            
               ")
        
      } else if (input$tipoGrafico == "geom_col") {
        
        paste0("ggplot(data = ", datos, ", #  datos en formato Data Frame
              
              # aquí van los mapeos del gráfico 
              mapping = aes(x = ", x, ", # variable del eje x 
              
                            y = ", y, ", # variable del eje y
                            
                            fill = ", color, ")) +  # variable para colorear (numérica o categórica)
        
        geom_col() + # el gráfico de columnas 
        
        ", tema, " + # el tema del gráfico
        
        labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
        
        theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
              legend.position = ", bottom,") # la posición de la leyenda  
            
               ")
        
      } else if (input$tipoGrafico == "geom_boxplot") {
        
        paste0("ggplot(data = ", datos, ") + #  datos en formato Data Frame
                      
                     
        # El gráfico de caja y bigotes: se ordena el eje X
        # en función de la variable del eje Y
        geom_boxplot(mapping = aes(x = reorder(", x,", ", y,"),
                                   y = ", y,",
                                   fill = ", color,")) +  
        
        ", tema, " + # el tema del gráfico
        
        labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
        
        theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
              legend.position = ", bottom,") # la posición de la leyenda  
            
               ")

      }
    } else if (!is.null(input$userDataset)) {
      
        if (input$tipoGrafico == "geom_jitter") {
          
          paste0("ggplot(data = datos, #  datos en formato Data Frame
                
                # aquí van los mapeos del gráfico 
                mapping = aes(x = ", x, ", # variable del eje x 
                
                              y = ", y, ", # variable del eje y
                              
                              color = ", color, ",  # variable para colorear (numérica o categórica)
                              
                              size = ", size, ")) + # variable de tamaño (numérica)
                              
          # geom_jitter añade ruido aleatorio a los puntos para evitar el solapamiento de las observaciones
          
          geom_jitter(alpha = 0.5) + # alpha es la transparencia, también para evitar el solapamiento 
          
          ", tema, " + # el tema del gráfico
        
          labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
          
          theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
                legend.position = ", bottom,") # la posición de la leyenda  
              
                 ")
            
        } else if (input$tipoGrafico == "geom_bar") {
          
          paste0("ggplot(data = datos, #  datos en formato Data Frame
                
                # aquí van los mapeos del gráfico 
                mapping = aes(x = ", x, ", # variable del eje X
                
                              # No hay variable en el eje Y porque geom_bar() cuenta automáticamente.
                              # En el eje y se representa la cuenta de la variable del eje X
                              
                              fill = ", color, ")) +  # variable para colorear (numérica o categórica)
          
          geom_bar() + # el gráfico de columnas 
          
          ", tema, " + # el tema del gráfico
        
          labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
          
          theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
                legend.position = ", bottom,") # la posición de la leyenda  
              
                 ")
          
        } else if (input$tipoGrafico == "geom_col") {
          
          paste0("ggplot(data = datos, #  datos en formato Data Frame
                
                # aquí van los mapeos del gráfico 
                mapping = aes(x = ", x, ", # variable del eje x 
                
                              y = ", y, ", # variable del eje y
                              
                              fill = ", color, ")) +  # variable para colorear (numérica o categórica)
          
          geom_col() + # el gráfico de columnas 
          
          ", tema, " + # el tema del gráfico
        
          labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
          
          theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
                legend.position = ", bottom,") # la posición de la leyenda  
              
                 ")
            
        } else if (input$tipoGrafico == "geom_boxplot") {
          
          paste0("ggplot(data = datos) + #  datos en formato Data Frame
                        
                       
          # El gráfico de caja y bigotes: se ordena el eje X
          # en función de la variable del eje Y
          geom_boxplot(mapping = aes(x = reorder(", x,", ", y,"),
                                     y = ", y,",
                                     fill = ", color,")) +  
          
          ", tema, " + # el tema del gráfico
        
          labs(title = ", "\"", titulo, "\"", ") + # el título del gráfico
          
          theme(axis.text.x = element_text(angle = 45), # rotación de las etiquetas del eje x
                legend.position = ", bottom,") # la posición de la leyenda  
              
                 ")
          
        }
      
    } # cierre del ifelse de si ha subido gráfico o no 
    
  }) # cierre del output del código fuente
  
  # Output para descargar el plot.
  output$descargarPlot <- downloadHandler(
    
      filename = function() { # con esta parte se configura el nombred del
                              # archivo que se guardará
        
      paste(input$tituloGrafico, # el título que haya usado en el plot 
            ".png", # formato png 
            sep = "") 
      },
      
    content = function(file) { # y con esto se especifica lo que se guarda
      ggsave(file, 
             last_plot()) # se guarda el útlimo plot que haya hecho el usuario 
      
      }
    
  ) # cierre del output para descargrlo

} # cierre del server

# App -------------------------------------------------------------------------
shinyApp(ui = ui, server = server)



