#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinyWidgets) 
library(bslib)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(broom)
library(fit.models)
library(dplyr)
library(tidyverse)
library(MASS)
library(DT)
library(lmtest)
library(nortest)
library(olsrr)
library(gridExtra)
library(goftest)
library(extrafont)
library(performance)
library(RcmdrMisc)
library(car)
library(rsconnect)
library(kableExtra)
rsconnect::setAccountInfo(name='sindy-jessenia-higuera-agudelo',
                          token='DCB42406B56C119BB3C189C6B5F9CF57',
                          secret='e49T0SuGturv6wYSNL4y1kDQJBpaDXgW4hQ3Sb9B')
# Define UI for application that draws a histogram
ui <- fluidPage(
theme = bs_theme(version = 4, bootswatch = "sandstone"),
#############################################################################
    # Application title
    titlePanel("Análisis de Curvas de Ajuste"),
    h4(tags$a(href ="Sindy J. Higuera")),
    withMathJax(),
###########################################################################  
#SELECCIONAR ARCHIVO CSV
    sidebarLayout(
        sidebarPanel(   
          fileInput(
          inputId = "filedata",
          label = "Upload data. csv",
          multiple = FALSE,
          accept = c(".csv"),
          buttonLabel = "Choosing ...",
          placeholder = "No files selected yet"
        ),
        fileInput("filedata_last_year", "Upload data for Last Year.csv"),  # Nuevo input para el archivo del año anterior
        uiOutput("xvariable"),
        uiOutput("yvariable"),
        uiOutput("xvar_select_last_year"),
        uiOutput("yvar_select_last_year"),
###############################################################################            
#INCLUIR BANDA DE CONFIANZA      
          
            tags$b("Plot:"),
            checkboxInput("se", "Add confidence interval around the regression line", TRUE),
           # tags$b("Plot1:"),
           #checkboxInput("se", "Add confidence interval around the regression line", TRUE),
####################################################################################


################################################################            
actionButton("wls_button", "Realizar ajuste WLS"),        
        
            hr(),
numericInput("weight_term", "Término de peso adicional:", value = 0),
            radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
                         inline = TRUE
            ),
            checkboxInput("echo", "Show code in report?", FALSE),
            downloadButton("downloadReport"),
            hr(),
            #HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/statistics-202/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/statistics-202">code</a>. Back to <a href="https://www.antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>'),
            hr(),
           # HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/" target="_blank"><img alt="Licence Creative Commons" style="border-width:0"
        #src="http://i.creativecommons.org/l/by/2.0/be/80x15.png"/></a> This work of <span xmlns:cc="http://creativecommons.org/ns#"
        #property="cc:attributionName"><font face="Courier">RShiny@UCLouvain</font></span> is made available under the terms of the <a rel="license"
        #href="http://creativecommons.org/licenses/by/2.0/be/" target="_blank">Creative Commons Attribution 2.0 Belgium license</a>. Details on the use of this resource on <a href="http://sites.uclouvain.be/RShiny"
        #target="_blank"><font face="Courier">RShiny@UCLouvain</font></a>. Source code available on <a href="https://github.com/AntoineSoetewey/statistics-202" target="_blank">GitHub</a>.')
        ),
###############################################################################################################################################################################3333333333      
        
mainPanel(
  
tabsetPanel(type="tabs",
    tabPanel("Sus Datos", # Plots of distributions
    div(
   style = "border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
  tags$b("Esta es una aplicación que explora el análisis de correlación.
Carga el archivo filedata.csv e inspecciona la correlación de sus datos en este panel.
Aquí encontrará un análisis a través de Mínimos Cuadrados Ordinarios.
Nos enfocaremos en el procedimiento (gráficos, interpretaciones, hipótesis, etc)")),
    tags$b("Sus Datos:"),
    DT::dataTableOutput("tbl"),
                       uiOutput("data"),
                     ),
              verbatimTextOutput("Resumen"),  
###############################################################################################################              
    tabPanel("Curva de Ajuste de orden 1 OLS",
    div(
    style = "border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
    tags$b("Gráfica de Regresión")),
                       uiOutput("lmSummary"),
                       plotlyOutput("plot"),
                       br(),
                       br(),
                       div(
                      style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
                      tags$b("Asegúrese de que se cumplan las suposiciones para la regresión lineal (independencia, linealidad, normalidad y homocedasticidad) antes de interpretar los coeficientes.")),
                      
                      tags$b("Intervalo de confianza del 95% alrededor de la línea de regresión ")),
  
############################################################################################################################           
              tabPanel("Parámetros OLS Primer Grado",
              div(
              style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
              tags$b("Compute parameters in R:")),
              verbatimTextOutput("summary"),
              div(
                         style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                       tags$b("La salida del resumen muestra 6 componentes, que incluyen
* Call. Muestra la función utilizada para calcular el modelo de regresión.
* Residuos. Proporciona una vista rápida de la distribución de los residuos, que por definición tienen una media cero.
Por lo tanto, la mediana no debería alejarse mucho de cero, y el valor mínimo y máximo deberían ser aproximadamente iguales en
valor absoluto. 
  
* Coeficientes. Muestra los coeficientes beta de la regresión y su significancia estadística,
el error estándar residual (RSE), el coeficiente de determinación (R-cuadrado) y la estadística F son métricas que se utilizan 
para verificar qué tan bien se ajusta el modelo a nuestros datos. Las variables significativamente asociadas a la variable de resultado
están marcadas con asteriscos.")), br(),
                       
                       tags$b("Outlier and Leverage Diagnostics"),
                       plotOutput("Outlier", height = 700, width = 700),
                       br(), 
div(
style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
tags$b("Visualización de los residuos y los puntos con alta influencia (leverage points) en un modelo de regresión lineal.
           Esta gráfica que muestre residuos y puntos de alta influencia puede ayudar a evaluar la calidad del ajuste del modelo y 
           detectar posibles problemas, como la presencia de outliers o la influencia de puntos atípicos en la estimación del modelo."))),
###############################################################################################################################################################            


tabPanel("Cálculo de parámetros a mano", # Plots of distributions
          div(
          style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
          tags$b("Cálculo de parámetros:")),
                       uiOutput("by_hand"),
                       br(),
                       div(
                       style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                       tags$b("Resultados de la regresión:")),
########################################################################################                      
                       uiOutput("results"),
                       br(),
                       br(),
                       verbatimTextOutput("coefficients"),
                       div(
                       style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                       tags$b("Significancia de los coeficientes")),
                       
                       tags$b("La tabla de coeficientes, en el resumen estadístico del modelo, muestra::
*Las estimaciones de los coeficientes beta,
*Los errores estándar (SE), que definen la precisión de los coeficientes beta. Para un coeficiente beta dado,, 
el Error estándar refleja cómo varía el coeficiente en muestreos repetidos.
*El estadístico t y el valor p asociado, que define la significancia estadística de los coeficientes beta."),
                       br(),
                       br(),
                       div(
                       style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                       tags$b("Estadístico t y los p-valor:")),
                  
                       
                       tags$b("Para un predictor dado, el estadístico t de Student (y su valor p asociado) verifica si hay o no una relación estadísticamente significativa entre 
                              un predictor dado y la variable resultado, es decir, si el coeficiente beta del predictor es significativamente diferente de cero"),
                       
                       br(),
                       br(),
                       div(
                         style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",   
                      tags$b("Hipótesis EStadísticas:")),
                      
                       tags$b("*Hipótesis Nula (H_0):Los coeficientes son iguales a cero (i.e., no existe relación entre la variables x e y)
    
*Hipótesis Alternativa (Ha): Los coeficientes no son iguales a cero (i.e., existe alguna relación entre x e y)"),
                      
                      tags$b("El estadístico t es una guía muy útil para decidir si incluir o no un predictor en un modelo.
                             Los valores altos del estadístico t (que corresponden a valores bajos de p cercanos a 0) indican que un predictor
                             debe conservarse en el modelo, mientras que valores muy bajos del estadístico t indican que un predictor podría ser eliminado (P. Bruce y Bruce 2017)..")),
            
              tabPanel("Exactitud del Modelo",
                       br(),
                       
                       div(
                         style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",     
                       tags$b("Vista Previa")), 
                       
                       tags$b("Una vez que haya identificado que al menos una variable predictora está significativamente asociada con el resultado, debes continuar con 
                       el diagnóstico para verificar qué tan bien se ajusta el modelo a los datos. Este proceso también se conoce como bondad de ajuste.
La calidad general del ajuste de la regresión lineal se puede evaluar utilizando las siguientes tres cantidades, que se muestran en el resumen del modelo."),
                      
                        br(),
                        br(),
                       
                       div(
                         style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                       tags$b("Error Residual Estándar (RSE).")),
                       br(),
                       
                       tags$b("El RSE (también conocido como modelo sigma) es la varianza residual, que representa la variación promedio de 
                       los puntos de observación alrededor de la línea de regresión ajustada. Es la desviación estándar de los errores residuales. 
                       El RSE proporciona una medida absoluta de los patrones en los datos que no pueden ser explicados por el modelo.
                      Cuando se comparan dos modelos, el modelo con el RSE más pequeño indica que este modelo se ajusta mejor a los datos.."),
                       
                       verbatimTextOutput("RSE"),
                       br(),
                     
                       
                       div(
                        style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                        tags$b("R-cuadrado y R-cuadrado ajustado:")),
                       br(),
                       
                       tags$b("El R-cuadrado (R2) varía de 0 a 1 y representa la proporción de información (es decir, variación) en los datos que 
                              puede ser explicada por el modelo. El R-cuadrado ajustado tiene en cuenta los grados de libertad."),
                       
                     tags$b("Un valor alto de R2 es una buena indicación. Sin embargo, como el valor de R2 tiende a aumentar cuando se agregan
                            más predictores en el modelo, como en el caso del modelo de regresión lineal múltiple, deberías considerar principalmente
                            el R-cuadrado ajustado, que es un R2 penalizado para un mayor número de predictores."),
                       
                       br(),
                       verbatimTextOutput("Rsquared"),
                       br(),
                       
                       div(
                         style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
                       tags$b("Estadístico F:")),
                       
                       tags$b("El estadístico F proporciona la significancia general del modelo. Evalúa si al menos una variable predictora tiene un coeficiente 
                              distinto de cero."),
                       
                       tags$b("Un valor significativamente grande de F describirá la significancia general del modelo p-value (p < 0.05)"),
                       br(),
                       verbatimTextOutput("Fstatistics"),
              ),
#######################################################################################################################              
tabPanel("Coeficiente de deformación",
         pre("ESta gráfica muestra si los residuales presentan algún patrón de linealidad. Podría existir 
                       una relación no lineal entre la variable predictora y la variable de salida, en tanto que 
                       There could be a non-linear relationship 
between predictor variables and an outcome variable and the pattern could show up in this plot if the
model doesn’t capture the non-linear relationship. If you find equally spread residuals around a 
horizontal line without distinct patterns, that is a good indication you don’t have non-linear
relationships."), ),          
############################################################################################################              
              
                       tabPanel("Análisis Residual", 
                       tags$b("Residuals vs Fitted"),         
                       pre("ESta gráfica muestra si los residuales presentan algún patrón de linealidad. Podría existir 
                       una relación no lineal entre la variable predictora y la variable de salida, en tanto que 
                       There could be a non-linear relationship 
between predictor variables and an outcome variable and the pattern could show up in this plot if the
model doesn’t capture the non-linear relationship. If you find equally spread residuals around a 
horizontal line without distinct patterns, that is a good indication you don’t have non-linear
relationships."), 
                       br(),
                       tags$b("Normal Q-Q"),
                       pre("This plot shows if residuals are normally distributed. Do residuals follow a straight line well
or do they deviate severely?"),
                       br(),
                       tags$b("Residual Plots:"),
                       uiOutput("Residuals"),
                       plotOutput("Res", height = 900, width = 900),
                       br(),
                       tags$b(" Scale-Location:"),
                       pre("It’s also called Spread-Location plot. This plot shows if residuals are spread equally along the ranges
of predictors. This is how you can check the assumption of equal variance (homoscedasticity). It’s good if
you see a horizontal line with equally (randomly) spread points."),
                       br(),
                       tags$b(" Residuals vs Leverage"),
                       pre("This plot helps us to find influential cases (i.e., subjects) if any. Not all outliers are influential 
in linear regression analysis (whatever outliers mean). Even though data have extreme values, they might
not be influential to determine a regression line. That means, the results wouldn’t be much different if
we either include or exclude them from analysis. They follow the trend in the majority of cases and they 
don’t really matter; they are not influential. On the other hand, some cases could be very influential 
even if they look to be within a reasonable range of the values. They could be extreme cases against a 
regression line and can alter the results if we exclude them from analysis. Another way to put it is that
they don’t get along with the trend in the majority of the cases."),
              ),
             
              
              tabPanel("Normality Test", 
                       tags$b("Normality test:"),
                       uiOutput("Normality"),
                       plotOutput("Normal", height = 500, width = 500),
                       br(),
                       tags$b("Residual Normality test:"),
                       pre("Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S.
It is based on the correlation between the data and the corresponding normal scores."),
                       uiOutput("NormalityR"),
                       br(),
                       tags$b("Correlation between observed residuals and expected residuals under normality:"),
                       uiOutput("Correlation"),
                       br(),
                       
                       br(),
                       tags$b("Residuals Homocedasticity:"),
                       pre("The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression model 
(by default the same explanatory variables are taken as in the main regression model) and rejects if too much
of the variance is explained by the additional explanatory variables."),
                       uiOutput("Homocedasticity"),
                       br(),
              ),
######################################################################################################
tabPanel("Cálculo de Incertidumbre Tipo A",
         div(
           style = "border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
           tags$b("Cálculo de Incertidumbre Tipo A según la norma GUM")
         ),
         uiOutput("mathjax_formula"),  # Espacio para la ecuación
         verbatimTextOutput("uncertainty_calculation")
),
#######################################################################################################
#Análisis quitando Outliers
######################################################################################################
tabPanel("Outliers",
        tags$b("Regression plot:"),
       plotlyOutput("plot2"),
       
      br(),
      
      tags$b("Cálculo de Outliers:"),
      verbatimTextOutput("out"),
      br(),
      tags$b("Residual Analysis"),
      br(),
        tags$b("Compute parameters in R:"),
       verbatimTextOutput("summarytres"),
       br(),
      tags$b("Residual Analysis"),
      br(),
         
         plotOutput("Resy", height = 400, width = 900),
         br(),
      
     tags$b("Normality test:"),
      #uiOutput("Normality"),
      plotOutput("Normal1", height = 500, width = 500),
      br(),     
      
),
#####################################################################################################              
              tabPanel("Area constante",
                       tags$b("Regression plot:"),
                       plotlyOutput("plot1"),
                       br(),
                       tags$b("Compute parameters in R:"),
                       verbatimTextOutput("summarydos"),
                       br(),
                       tags$b("Residual Analysis"),
                       
                       plotOutput("Resx", height = 900, width = 900),
                       br(),
                       
              ),
######################################################################################################            
              tabPanel("Weighted Least Square",
                       tags$b("Cálculo de los pesos"),
                       verbatimTextOutput("pesos"),
                       div(
                         style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
                         tags$b("Calcula los pesos para el modelo ponderado. Los pesos son inversamente proporcionales a los residuales absolutos
                                al cuadrado del modelo ajustado. Esto se hace para asignar un mayor peso a las observaciones con residuales más pequeños, 
                                lo que ayuda a manejar valores atípicos y mejorar la precisión del ajuste.")),
                       br(),
                       tags$b("Compute parameters in R:"),
                       verbatimTextOutput("summaryg"),
                       br(),
                       tags$b("Residual Plots:"),
                       uiOutput("Residualsg"),
                       plotOutput("Resg", height = 400, width = 800),
                       br(),
                       plotOutput("plotwls"),
                       verbatimTextOutput("wls_output"),  
                       verbatimTextOutput("coefficientswls"),
                       verbatimTextOutput("RSEwls"),
                        ),
##########################################################################################################################################################
### Comparación
########################################################################################################################################################
tabPanel("Comparación OLS WLS",
         tags$b("Regression plot:"),
         plotOutput("ComparisonPlot"),
         div(
           style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
           tags$b("En esta gráfica, los puntos azules representan los valores observados.
                  Los puntos rojos son los valores ajustados por el modelo OLS y la línea punteada 
                  roja es la línea de regresión ajustada por OLS. Los puntos verdes son los valores
                  ajustados por el modelo WLS y la línea punteada verde es la línea de regresión 
                  ajustada por WLS.")),  
         div(
           style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
           tags$b("Esta gráfica  permitirá visualizar la comparación directa entre los resultados 
                  de ambos modelos y cómo se ajustan a los datos observados.Se Podrá observar si el
                  modelo WLS con pesos ofrece una mejor aproximación a los datos en comparación con 
                  el modelo OLS estándar. Además, es una forma eficaz de visualizar si los pesos
                  aplicados a las observaciones influencian el ajuste del modelo WLS en comparación 
                  con el OLS.")),),
###################################################################################################
tabPanel("Comparación OLS Outliers",
         tags$b("Regression plot:"),
         plotOutput("ComparisonPlot1"),
         div(
           style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
           tags$b("En esta gráfica, los puntos azules representan los valores observados. 
                  Los puntos rojos son los valores ajustados por el modelo OLS, y los puntos 
                  verdes son los valores ajustados por el modelo que excluye los outliers.
                  Ambos modelos tienen líneas de regresión punteadas, en rojo para el modelo OLS 
                  y en verde para el modelo que excluye los outliers.")),  
         div(
           style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
           tags$b("Esta gráfica  permitirá visualizar la comparación directa entre los resultados
                  de ambos modelos y cómo se ajustan a los datos observados, considerando y excluyendo
                  los outliers. De esta manera, se podrá evaluar cómo la exclusión de outliers afecta los
                  resultados y el ajuste del modelo."))),
########################################################################################################
tabPanel("Tabla Comparativa", 
         tags$b("Comparaciones:"),  
         tableOutput("CoefficientsTable")),
#######################################################################################################
tabPanel("Comparando coeficientes",
         br(),
         div(
           style ="border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;", 
         tags$b("Sin puntos de baja presión:"),  ),
plotOutput("coef_comparison", height = 400, width = 800),
),

#############################################################################################################
tabPanel("Comparación de Años", plotOutput("comparisonPlot")), 

############################################################################################################
tabPanel("Ajuste Polinómico",
         div(
           style = "border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
           tags$b("Ajuste Polinómico de Grado 2 y 3: Esta pestaña contiene un ajuste polinómico de grado 2 y grado 3, con la visualización de las curvas de ajuste y un resumen de los modelos ajustados.
          Curvas en gráficos: Las líneas rojas muestran el ajuste polinómico de grado 2 y las líneas verdes (dashed) muestran el ajuste polinómico de grado 3.")
         ),
      
         plotlyOutput("poly_plot"),
         verbatimTextOutput("poly_summary")
         
),
  )
)
    ))

server <- function(input, output) {
  
  
  data <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  output$tb1 <- renderDT(data())
###########################################################################  
  # Datos del año anterior
  data_last_year <- reactive({
    req(input$filedata_last_year)
    read.csv(input$filedata_last_year$datapath)
  })
  
##########################################################################  
  output$xvariable <- renderUI({
    req(data())
    xa<-colnames(data())
    pickerInput(inputId = 'xvar',
                label = 'Select x-axis variable',
                choices = c(xa[1:length(xa)]), selected=xa[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(data())
    ya<-colnames(data()) 
    pickerInput(inputId = 'yvar',
                label = 'Select y-axis variable',
                choices = c(ya[1:length(ya)]), selected=ya[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })  
  
##############################################################################
  # Selector de variables x e y para el año anterior
  output$xvar_select_last_year <- renderUI({
    req(data_last_year())
    selectInput("xvar_last_year", "Variable X (año anterior):", choices = names(data_last_year()))
  })
  
  output$yvar_select_last_year <- renderUI({
    req(data_last_year())
    selectInput("yvar_last_year", "Variable Y (año anterior):", choices = names(data_last_year()))
  })
#############################################################################3
  
    output$tbl <- DT::renderDataTable({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
        DT::datatable(data(),
                      extensions = "Buttons",
                      options = list(
                          lengthChange = FALSE,
                          dom = "Blfrtip",
                          buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
    })
    
   output$data <- renderUI({
     x <- as.numeric(data()[[as.name(input$xvar)]])
     y <- as.numeric(data()[[as.name(input$yvar)]])

          withMathJax(
            div(
              style = "border: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #eaf2f8; font-family: 'Arial', sans-serif; font-size: 12px;",
              tags$b("Esta tabla proporciona una descripción general de las estadísticas descriptivas de las variables que planea estudiar.")),
            output$summarydata <- renderPrint({
              x <- as.numeric(data()[[as.name(input$xvar)]])
              y <- as.numeric(data()[[as.name(input$yvar)]])
          
              summary(data())}))
          
          })
    
    output$by_hand <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
        fit <- lm(y ~ x)
        withMathJax(
            paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 20)),
            br(),
            paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 7)),
            br(),
            br(),
            paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 7), " + ", round(fit$coef[[2]], 20), "\\( x \\)")
        )
    })
    
    output$summary <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model<- lm(y ~ x)
      summary(model)
      
    })
##############################################################################################    
    output$Outlier<- renderPlot({
      
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      fit <- lm(y ~ x)     
      ols_plot_resid_lev(fit) #rvsr_plot_shiny(fit, dat, 'drat')
      #par(mfrow = c(2,1 )) # Change the panel layout to 2 x 2
      # plot(hist(residuals(fit)), col = 'darkgray', border = 'white')
      #par(mfrow = c(1, 1)) # Change back to 1 x 1
      
    })
#######################################################################################################    
    output$coefficients <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      summary(fit)$coefficients
    })
    
    output$RSE <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      summary(fit)$sigma
    })
    
    output$Rsquared <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      summary(fit)$r.squared
    })
    
    output$Fstatistics <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      summary(fit)$fstatistic
    })
    
    output$results <- renderUI({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
        model<- lm(y ~ x)
        withMathJax(
            paste0( " \\( R^2 = \\) ", round(summary(model)$r.squared, 7)),
            br(),
            paste0(    " \\( \\beta_0 = \\) ", round(model$coef[[1]], 7)),
            br(),
        
            paste0(    " \\( \\beta_1 = \\) ", round(model$coef[[2]], 15)),
            br(),    
            paste0(   " Std.Error ", "\\(\\beta_0 = \\) ",   summary(model)$coefficients[1,2]),
            br(),
            paste0(     " Std.Error ", "\\(\\beta_1 = \\) ",   summary(model)$coefficients[2,2]),
            br(),  
            paste0(   " P-value ", "\\( = \\) ", signif(summary(model)$coef[2, 4], 5))
            
        )
    })
    
    output$interpretation <- renderUI({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
        fit <- lm(y ~ x)
        if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, ".")
            )
        } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
                br(),
                paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
            )
        }
    })
    
    
    output$Res <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
       
        fit <- lm(y ~ x)
        
        #rvsr_plot_shiny(fit, dat, 'drat')
        par(mfrow = c(2,2 )) # Change the panel layout to 2 x 2
        plot(fit)
       par(mfrow = c(1, 1)) # Change back to 1 x 1
        
    })
    
    output$Normal <- renderPlot({
      
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      fit <- lm(y ~ x)     
      ols_plot_resid_hist(fit) #rvsr_plot_shiny(fit, dat, 'drat')
      #par(mfrow = c(2,1 )) # Change the panel layout to 2 x 2
      # plot(hist(residuals(fit)), col = 'darkgray', border = 'white')
      #par(mfrow = c(1, 1)) # Change back to 1 x 1
      
    })
    output$NormalityR <- renderPrint({
    
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
        fit <- lm(y ~ x)
    prueba<- shapiro.test(residuals(fit))
     prueba 
    
    })
    
    output$Correlation <- renderPrint({
      
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      corr<- ols_test_correlation(fit)
      corr
      
    })
    
    
    output$Homocedasticity <- renderPrint({
      
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      homo<-  bptest(fit)
      homo
      
    })
########################################################################################
    # Cálculo de incertidumbre tipo A
#############################################################################################
output$uncertainty_calculation <- renderPrint({
  req(input$xvar, input$yvar)
  
  # Extraer los valores de las variables x e y
  x <- as.numeric(data()[[input$xvar]])
  y <- as.numeric(data()[[input$yvar]])
  
  # Ajustar el modelo lineal
  fit <- lm(y ~ x)
  
  # Obtener los residuos
  residuals <- residuals(fit)
  
  # Calcular la desviación estándar de los residuos
  std_dev_residuals <- sd(residuals)
  
  # Número de observaciones
  n <- length(residuals)
  
  # Calcular la incertidumbre tipo A
  uncertainty_A <- std_dev_residuals / sqrt(n)
  
  # Imprimir el cálculo paso a paso
  cat("Cálculo detallado de la Incertidumbre Tipo A:\n")
  cat("------------------------------------------------------------\n")
  
  # Mostrar los residuos en una tabla
  cat("Paso 1: Obtener los residuos del modelo de regresión lineal.\n")
  cat("Tabla de residuos:\n")
  
  # Crear una tabla en formato simple
  print(data.frame(Observación = 1:n, Residuo = format(residuals, scientific = TRUE)))
  
  # Calcular e imprimir la desviación estándar de los residuos
  cat("\nPaso 2: Calcular la desviación estándar de los residuos.\n")
  cat("Desviación estándar de los residuos:", format(std_dev_residuals, scientific = TRUE), "\n")
  
  # Contar el número de observaciones
  cat("\nPaso 3: Contar el número de observaciones.\n")
  cat("Número de observaciones:", n, "\n")
  
  # Calcular e imprimir la incertidumbre tipo A
  cat("\nPaso 4: Calcular la incertidumbre tipo A.\n")
  cat("Incertidumbre Tipo A:", format(uncertainty_A, scientific = TRUE), "\n")
})
############################################################################################
#Comparando los coeficientes
#############################################################################################
    output$coef_comparison <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      # Modelo lineal ordinario (OLS)
      fit_ols <- lm(y ~ x)
      coef_ols <- coef(fit_ols)
      se_ols <- summary(fit_ols)$coefficients[, "Std. Error"]
      
      # Modelo lineal excluyendo outliers
      std_residuals <- rstandard(fit_ols)
      threshold <- 2
      data_no_outliers <- data()[abs(std_residuals) <= threshold, ]
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      model_no_outliers <- lm(y_no_outliers ~ x_no_outliers)
      coef_no_outliers <- coef(model_no_outliers)
      se_no_outliers <- summary(model_no_outliers)$coefficients[, "Std. Error"]
      
      # Modelo lineal ponderado (WLS)
      wt <- 1 / lm(abs(fit_ols$residuals) ~ fit_ols$fitted.values)$fitted.values^2
      model_wls <- lm(y ~ x, weights = wt)
      coef_wls <- coef(model_wls)
      se_wls <- summary(model_wls)$coefficients[, "Std. Error"]
      
      # Crear un dataframe para los coeficientes y errores estándar
      comparison_df <- data.frame(
        Model = rep(c("OLS", "Modelo sin Outliers", "WLS"), each = 2),
        Parameter = rep(c("Intercepto", "Pendiente"), times = 3),
        Coefficient = c(coef_ols, coef_no_outliers, coef_wls),
        Standard_Error = c(se_ols, se_no_outliers, se_wls)
      )
      
      # Crear dos gráficos independientes, uno para el intercepto y otro para la pendiente
      plot_intercept <- ggplot(subset(comparison_df, Parameter == "Intercepto"), aes(x = Model, y = Coefficient, fill = Model)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.4, color = "black") +
        geom_errorbar(aes(ymin = Coefficient - Standard_Error, ymax = Coefficient + 2*Standard_Error),
                      position = position_dodge(width = 0.5), width = 0.25) +
        labs(title = "Comparación del Intercepto",
             y = "Valor",
             x = "Modelo") +
        theme_minimal() +
        theme(axis.title.y = element_text(margin = margin(r = 20)))  # Ajustar margen del eje y del intercepto
      
      plot_slope <- ggplot(subset(comparison_df, Parameter == "Pendiente"), aes(x = Model, y = Coefficient, fill = Model)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "darkgreen") +
        geom_errorbar(aes(ymin = Coefficient - 2*Standard_Error, ymax = Coefficient + 2*Standard_Error),
                      position = position_dodge(width = 0.7), width = 0.25) +
        labs(title = "Comparación de la Pendiente",
             y = "Valor",
             x = "Modelo") +
        theme_minimal()
      
      # Mostrar los dos gráficos
      grid.arrange(plot_intercept, plot_slope, ncol = 1)
    })
#######################################################################################    
#Método wls
##########################################################################################################    
    output$pesos<-renderPrint({
    x <- as.numeric(data()[[as.name(input$xvar)]])
    y <- as.numeric(data()[[as.name(input$yvar)]])
    model<- lm(y ~ x)
    wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
    wt})
    
    output$summaryg <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model<- lm(y ~ x)
      wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model1<- lm(y ~ x,weights=wt)
      summary(model1)
    })
    
    output$Resg <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      # Modelo lineal normal sin pesos
      fit_normal <- lm(y ~ x)
      
      # Modelo lineal ponderado con pesos
      wt <- 1 / lm(abs(fit_normal$residuals) ~ fit_normal$fitted.values)$fitted.values^2
      model_weighted <- lm(y ~ x, weights = wt)
      
      par(mfrow = c(1, 2)) # Cambia el diseño del panel a 1 fila x 2 columnas
      
      # Gráfico de residuales del modelo lineal normal
      plot(fit_normal$residuals, pch = 16, col = "blue", main = "Residuales Modelo Lineal Normal",
           xlab = "Índice de observación", ylab = "Residuales")
      
      # Agregar línea punteada en cero
      abline(h = 0, lty = 2, col = "red")
      
      # Gráfico de residuales del modelo lineal ponderado
      plot(model_weighted$residuals, pch = 16, col = "green", main = "Residuales Modelo Lineal Ponderado",
           xlab = "Índice de observación", ylab = "Residuales")
      
      # Agregar línea punteada en cero
      abline(h = 0, lty = 2, col = "red")
      
      par(mfrow = c(1, 1)) # Restaura el diseño del panel a 1 fila x 1 columna
    })
    
    
    # Realizar ajuste WLS
    wls_fit <- eventReactive(input$wls_button, {
      weights <- 1 / (wt + input$weight_term)
      # Ajustar los pesos según el término adicional
      weights <- weights + input$additional_weight
      
      lm(y ~ x, data = data, weights = weights)
    })
    
    # Mostrar resultados de la regresión WLS
    output$wls_output <- renderPrint({
      wls <- wls_fit()
      summary(wls)
    })
    
    output$coefficientswls <- renderPrint({
      options(digits = 10)
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model<- lm(y ~ x)
      wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model1<- lm(y ~ x,weights=wt)
      # Restaurar las opciones por defecto para el número de dígitos decimales
      on.exit(options(digits = getOption("digits")))
      summary(model1)$coefficients
    })
    
    output$RSEwls <- renderPrint({
      options(digits = 10)
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model<- lm(y ~ x)
      wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model1<- lm(y ~ x,weights=wt)
      # Restaurar las opciones por defecto para el número de dígitos decimales
      on.exit(options(digits = getOption("digits")))
      summary(model1)$sigma
    })
###########################################################################    
# pendiente 0#################################   
################################################################################    
    output$summarydos <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      model<- lm(y ~ 1)
      summary(model)
    })
    
    output$Resx <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      fit <- lm(y ~ 1)
      
      #rvsr_plot_shiny(fit, dat, 'drat')
      par(mfrow = c(2,2 )) # Change the panel layout to 2 x 2
      plot(fit)
      par(mfrow = c(1, 1)) # Change back to 1 x 1
      
    })
    
#Pendiente 0   
 ####################################################################################   
    output$plot1 <- renderPlotly({
      xvar <- as.name(input$xvar)
      yvar <- as.name(input$yvar)
      
      x <- as.numeric(data()[[xvar]])
      y <- as.numeric(data()[[yvar]])
      
      fit <- lm(y ~ 1)  # Ajuste constante, sin la variable x
      
      q <- ggplot(data(), aes(x = x, y = y)) +
        geom_point(shape = 2, colour = "violet", fill = "white", size = 1, stroke = 1) +
        geom_abline(intercept = coef(fit), slope = 0, color = "red", linetype = "dashed") +  # Línea horizontal para el ajuste constante
        ylab(input$yvar) +
        xlab(input$xvar) +
        theme_minimal()
      
      ggplotly(q)
    })
    
#############################################################################################
# SIN PUNTOS DE OUTLIER
    
#################################################################################################
    
    output$plot2 <- renderPlotly({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      
      # Calculating standardized residuals
      std_residuals <- rstandard(fit)
      
      # Set the threshold for outliers removal (e.g., 2 standard deviations)
      threshold <- 2
      
      # Filter the data to exclude outliers
      data_no_outliers <- data()[abs(std_residuals) <= threshold, ]
      data_no_outliers
      
      # Extract x and y values without outliers
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      
      K <- ggplot(data_no_outliers, aes(x = x_no_outliers, y = y_no_outliers)) +
        geom_point(shape = 2, colour = "red", fill = "white", size = 1, stroke = 1) +
        stat_smooth(method = "lm", se = input$se,  n = 44, fullrange = FALSE,
                    level = 0.95, na.rm = FALSE) +
        ylab(input$yvar) +
        xlab(input$xvar) +
        theme_minimal()
      
      ggplotly(K)
    })
    
    output$out<- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      # Calculating standardized residuals
      std_residuals <- rstandard(fit)
      
      # Set the threshold for outliers removal (e.g., 2 standard deviations)
      threshold <- 2
      
      # Filter the data to exclude outliers
      data_no_outliers <- data()[abs(std_residuals) <= threshold, ]
      data_no_outliers}) 
    
    output$summarytres <- renderPrint({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      # Calculating standardized residuals
      std_residuals <- rstandard(fit)
      
      # Set the threshold for outliers removal (e.g., 2 standard deviations)
      threshold <- 2
      
      # Filter the data to exclude outliers
      data_no_outliers <- data()[abs(std_residuals) <= threshold, ]
      
      # Extract x and y values without outliers
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      model <- lm(y_no_outliers ~ x_no_outliers)
      summary(model)
    }) 
    
    output$Resy <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      # Modelo lineal normal con todos los datos
      fit_normal <- lm(y ~ x)
      
      # Calculating standardized residuals
      std_residuals <- rstandard(fit_normal)
      threshold <- 2
      
      # Modelo lineal sin outliers (filtrar los datos para excluir outliers)
      data_no_outliers <- data()[abs(std_residuals) <= threshold, ]
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      model_without_outliers <- lm(y_no_outliers ~ x_no_outliers)
      
      par(mfrow = c(1, 2)) # Cambia el diseño del panel a 1 fila x 2 columnas
      
      # Gráfico de residuales del modelo lineal normal
      plot(fit_normal$residuals, pch = 16, col = "blue", main = "Residuales Modelo Lineal Normal",
           xlab = "Índice de observación", ylab = "Residuales")
      
      # Agregar línea punteada en cero
      abline(h = 0, lty = 2, col = "red")
      
      # Gráfico de residuales del modelo lineal sin outliers
      plot(model_without_outliers$residuals, pch = 16, col = "green", main = "Residuales Modelo Lineal sin Outliers",
           xlab = "Índice de observación", ylab = "Residuales")
      
      # Agregar línea punteada en cero
      abline(h = 0, lty = 2, col = "red")
      
      par(mfrow = c(1, 1)) # Restaura el diseño del panel a 1 fila x 1 columna
    })
    
    
    
    output$Normal1<- renderPlot({
      
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)
      # Calculating standardized residuals
      std_residuals <- rstandard(fit)
      threshold <- 2
      # Filter the data to exclude outliers
      data_no_outliers <- data()[abs(std_residuals) <= threshold, ]
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      model2 <- lm(y_no_outliers ~ x_no_outliers)
      
      ols_plot_resid_hist(model2) 
      
    })
    
################################################################################################
    ##############################################################################
    #### Gráficos de comparación OLS-WLS
    output$ComparisonPlot <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      # Modelo lineal ordinario (OLS)
      fit_ols <- lm(y ~ x)
      
      # Calculating standardized residuals for OLS
      std_residuals_ols <- rstandard(fit_ols)
      threshold <- 2
      
      # Modelo lineal ponderado (WLS)
      wt <- 1 / lm(abs(fit_ols$residuals) ~ fit_ols$fitted.values)$fitted.values^2
      model_wls <- lm(y ~ x, weights = wt)
      
      # Plotting the comparison
      s <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_point(shape = 16, size = 3, color = "blue", alpha = 0.6) +  # Observations in blue
        geom_point(data = data.frame(x = x, y = fitted(fit_ols)), shape = 1, size = 3, color = "red", alpha = 0.6) +  # OLS fitted values in red
        geom_point(data = data.frame(x = x, y = fitted(model_wls)), shape = 1, size = 3, color = "green", alpha = 0.6) +  # WLS fitted values in green
        geom_abline(intercept = coef(fit_ols)[1], slope = coef(fit_ols)[2], linetype = "dashed", color = "red") +  # OLS regression line in red
        geom_abline(intercept = coef(model_wls)[1], slope = coef(model_wls)[2], linetype = "dashed", color = "green") +  # WLS regression line in green
        labs(title = "Comparación OLS vs. WLS", x = "Presión (Pa)", y = "Área (mm^2)") +
        theme_minimal()
      
      print(s)
    })
########################################################################################################
#### gRÁFICOS DE cOMPARACIÓN OLS-OUTLIERS
    output$ComparisonPlot1 <- renderPlot({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      # Modelo lineal ordinario (OLS)
      fit_ols <- lm(y ~ x)
      
      # Calculating standardized residuals for OLS
      std_residuals_ols <- rstandard(fit_ols)
      threshold <- 2
      
      # Modelo lineal excluyendo outliers
      data_no_outliers <- data()[abs(std_residuals_ols) <= threshold, ]
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      model_no_outliers <- lm(y_no_outliers ~ x_no_outliers)
      
      # Plotting the comparison
      x <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_point(shape = 16, size = 3, color = "blue", alpha = 0.6) +  # Observations in blue
        geom_point(data = data.frame(x = x, y = fitted(fit_ols)), shape = 1, size = 3, color = "red", alpha = 0.6) +  # OLS fitted values in red
        geom_point(data = data.frame(x = x_no_outliers, y = fitted(model_no_outliers)), shape = 1, size = 3, color = "green", alpha = 0.6) +  # Model without outliers fitted values in green
        geom_abline(intercept = coef(fit_ols)[1], slope = coef(fit_ols)[2], linetype = "dashed", color = "red") +  # OLS regression line in red
        geom_abline(intercept = coef(model_no_outliers)[1], slope = coef(model_no_outliers)[2], linetype = "dashed", color = "green") +  # Model without outliers regression line in green
        labs(title = "Comparación OLS vs. Modelo sin Outliers", x = "Presión (Pa)", y = "Área (mm^2)") +
        theme_minimal()
      
      print(x)
    })
##########################################################################################
#### TABLA COMPARATIVA####################################################################
    output$CoefficientsTable <- renderTable({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      
      # Modelo lineal ordinario (OLS)
      fit_ols <- lm(y ~ x)
      
      # Calculating standardized residuals for OLS
      std_residuals_ols <- rstandard(fit_ols)
      threshold <- 2
      
      # Modelo lineal excluyendo outliers
      data_no_outliers <- data()[abs(std_residuals_ols) <= threshold, ]
      x_no_outliers <- as.numeric(data_no_outliers[[as.name(input$xvar)]])
      y_no_outliers <- as.numeric(data_no_outliers[[as.name(input$yvar)]])
      model_no_outliers <- lm(y_no_outliers ~ x_no_outliers)
      
      # Modelo lineal ponderado (WLS)
      wt <- 1 / lm(abs(fit_ols$residuals) ~ fit_ols$fitted.values)$fitted.values^2
      model_wls <- lm(y ~ x, weights = wt)
      
      # Crear un dataframe para almacenar los resultados
      coefficients_df <- data.frame(
        Model = c("OLS_Intercepto", "OLS_Pendiente", "Modelo sin Outliers_Intercepto", "Modelo sin Outliers_Pendiente", "WLS_Intercepto", "WLS_Pendiente"),
        Coefficient = NA,
        Standard_Error = NA
      )
      
      # Llenar los coeficientes y errores estándar para OLS
      coefficients_df$Coefficient[1:2] <- format(coef(fit_ols), digits = 7, scientific = TRUE)
      coefficients_df$Standard_Error[1:2] <- format(summary(fit_ols)$coefficients[, "Std. Error"], digits = 7, scientific = TRUE)
      
      # Llenar los coeficientes y errores estándar para el modelo sin outliers (si existe)
      if (!is.null(coef(model_no_outliers))) {
        coefficients_df$Coefficient[3:4] <- format(coef(model_no_outliers), digits = 7, scientific = TRUE)
        coefficients_df$Standard_Error[3:4] <- format(summary(model_no_outliers)$coefficients[, "Std. Error"], digits = 7, scientific = TRUE)
      }
      
      # Llenar los coeficientes y errores estándar para WLS (si existe)
      if (!is.null(coef(model_wls))) {
        coefficients_df$Coefficient[5:6] <- format(coef(model_wls), digits = 7, scientific = TRUE)
        coefficients_df$Standard_Error[5:6] <- format(summary(model_wls)$coefficients[, "Std. Error"], digits = 7, scientific = TRUE)
      }
      
      # Mostrar solo las columnas relevantes
      coefficients_df <- coefficients_df[, c("Model", "Coefficient", "Standard_Error")]
      
      # Mostrar la tabla
      coefficients_df
    })
####################################################################################################
    
    output$comparisonPlot <- renderPlot({
      req(data(), data_last_year(), input$xvar, input$yvar, input$xvar_last_year, input$yvar_last_year)
      df1 <- data()
      df2 <- data_last_year()
      
      # Modelo OLS para el año actual
      fit1 <- lm(as.formula(paste(input$yvar, "~", input$xvar)), data = df1)
      
      # Modelo OLS para el año anterior
      fit2 <- lm(as.formula(paste(input$yvar_last_year, "~", input$xvar_last_year)), data = df2)
      
      # Gráfico con ggplot2
      ggplot() +
        geom_point(data = df1, aes(x = .data[[input$xvar]], y = .data[[input$yvar]]), color = "blue", alpha = 0.5) +
        geom_smooth(data = df1, aes(x = .data[[input$xvar]], y = .data[[input$yvar]]), method = "lm", formula = y ~ x, color = "blue", se = FALSE, linetype = "solid") +
        geom_point(data = df2, aes(x = .data[[input$xvar_last_year]], y = .data[[input$yvar_last_year]]), color = "red", alpha = 0.5) +
        geom_smooth(data = df2, aes(x = .data[[input$xvar_last_year]], y = .data[[input$yvar_last_year]]), method = "lm", formula = y ~ x, color = "red", se = FALSE, linetype = "dashed") +
        labs(title = "Comparación de Regresiones: Año Actual vs Año Anterior",
             x = "Variable X",
             y = "Variable Y") +
        theme_minimal()
    })
###########################################################################################################
    # Ajuste Polinómico (Grado 2 y 3)
    output$poly_plot <- renderPlotly({
      req(input$xvar, input$yvar)
      x <- as.numeric(data()[[input$xvar]])
      y <- as.numeric(data()[[input$yvar]])
      
      poly2 <- lm(y ~ poly(x, 2))
      poly3 <- lm(y ~ poly(x, 3))
      
      p <- ggplot(data(), aes(x = x, y = y)) +
        geom_point(shape = 2, colour = "blue") +
        stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = input$se, colour = "red") +
        stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = input$se, colour = "green", linetype = "dashed") +
        theme_minimal() +
        labs(title = "Ajuste Polinómico Grado 2 y 3", x = input$xvar, y = input$yvar)
      
      ggplotly(p)
    })
    
    output$poly_summary <- renderPrint({
      req(input$xvar, input$yvar)
      x <- as.numeric(data()[[input$xvar]])
      y <- as.numeric(data()[[input$yvar]])
      
      poly2 <- lm(y ~ poly(x, 2))
      poly3 <- lm(y ~ poly(x, 3))
      
      list(
        "Resumen Ajuste Polinómico Grado 2" = summary(poly2),
        "Resumen Ajuste Polinómico Grado 3" = summary(poly3)
      )
    })

# MÉTODO OLS
###################################################################################################
    output$plot <- renderPlotly({
      x <- as.numeric(data()[[as.name(input$xvar)]])
      y <- as.numeric(data()[[as.name(input$yvar)]])
      fit <- lm(y ~ x)

      p <- ggplot(data(), aes(x = x, y = y)) +
        
        geom_point(shape = 2, colour = "red", fill = "white", size = 1, stroke = 1) +
        stat_smooth(method = "lm", se = input$se,  n = 44, fullrange = FALSE,
                    level = 0.95, na.rm = FALSE) +
        ylab(input$yvar) +
        xlab(input$xvar) +
        theme_minimal()
      ggplotly(p)
    })
    
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste("my-report", sep = ".", switch(
                input$format, PDF = "pdf", HTML = "html", Word = "docx"
            ))
        },
        
        content = function(file) {
            src <- normalizePath("report.Rmd")
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, "report.Rmd", overwrite = TRUE)
            
            library(rmarkdown)
            out <- render("report.Rmd", switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)