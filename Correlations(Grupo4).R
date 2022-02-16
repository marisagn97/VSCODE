library(shiny)
library(shinydashboard)
library(ggplot2)
library(mvtnorm)
library(dplyr)
library(plotly)
library(purrr)

# UI ----------------------------------------------------------------------
#Creamos la estructura de dashboard con las diferentes pestañas
ui <- dashboardPage(
  dashboardHeader(title="Correlaciones"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Guess the correlation", tabName = "Guess"),
      menuItem("Paint the correlation", tabName = "Paint"),
      menuItem("Change the correlation", tabName = "Change"),
      menuItem("Guess the median", tabName = "Median")
    )
  ),
  
#Definimos el UI para cada una de las pestañas
  dashboardBody(
    tabItems(
# Minijuego 1 -------------------------------------------------------------
      tabItem(tabName = "Guess",
              h2("Guess the correlation"),
              fluidRow(
                column(actionButton("NP", "New Plot"),width=1),
                box(plotOutput("plot1"),width=6)
              ),
              fluidRow(
                column(strong("Your Guess:"),width=1),
                column(textInput("yourgue",label = NULL),width=1),
                column(actionButton("LSB", "Let's see"),width=2),
                column(textOutput("result"),width=8)
              )),
      
      
# Minijuego 2 -------------------------------------------------------------
      tabItem(tabName = "Paint",
              h2("Paint the correlation"),
              fluidRow(
                column(actionButton("NP_2", "Reset"), width=1),
                box(plotOutput('plot2', click = 'addclick'), 
                    width=6)
              ),
              fluidRow(
                column(textOutput('objective2'), width=8)
              ),
              fluidRow(
                column(actionButton('check2', 'Check it'), width=1)
              ),
              fluidRow(
                column(textOutput('result2'), width=8)
              )),
      
# Minijuego 3 -------------------------------------------------------------
      tabItem(tabName = "Change",
              h2("Change the correlation"),
              fluidRow(
                column(actionButton('NP_3', "New Plot"), width=1),
                column(plotlyOutput("plot3"),width=6)
              ), #en este caso definimos un output de plotly
              fluidRow(
                column(textOutput('objective3'), width=8)
              ),
              fluidRow(
                column(actionButton('check3', 'Check it'), width=1)
              ),
              fluidRow(
                column(textOutput('result3'), width=8)
              )),
      
      
# Minijuego 4 -------------------------------------------------------------
      tabItem(tabName = "Median",
              h2("Guess the medians"),
              fluidRow(
                column(actionButton("NP_4", "New Plot"),width=1),
                box(plotOutput("plot4"),width=6)
              ),
              fluidRow(
                column(strong("Guess the medians(Integer nº)"),width=7)
              ),
              fluidRow(
                column(strong("Median X (X1):"),width=2),
                column(textInput("medianx",label = NULL),width=1)
              ),
              fluidRow(
                column(strong("Median Y (X2):"),width=2),
                column(textInput("mediany",label = NULL),width=1)
              ),
              fluidRow(
                column(actionButton("check4", "Check"),width=2)
              ),
              fluidRow(
                column(textOutput("value_x"),width=8),
                column(textOutput("value_y"),width=8)
              ))
      
    )
  )
)



# SERVER ------------------------------------------------------------------
server <- function(input, output) {
#Creamos un dataframe de primeras con una correlacion aleatoria
#Nos servira para varios minijuegos
  df <- reactiveValues()
  cor <- round(runif(1,-1,1),2)
  sigma <- matrix(c(1,cor,cor,1),nrow=2)
  df$df <- data.frame(rmvnorm(50,sigma=sigma, method="chol"))

# Minijuego 1 -------------------------------------------------------------
#Cada vez que pulsemos sobre el botón de NewPlot se generara un nuevo dataframe
#Y por tanto un nuevo plot
  observeEvent(input$NP,{
    cor <- runif(1,-1,1)
    sigma <- matrix(c(1,cor,cor,1),nrow=2)
    df$df <- data.frame(rmvnorm(50,sigma=sigma, method="chol"))
    output$result <- renderText({
      paste0("")#"Vaciamos" la salida cada vez que se pulsa new plot
    })
  })
  observeEvent(input$yourgue,{
    output$result <- renderText({
      paste0("")#"Vaciamos" la salida por defecto, asi se vacia cada vez que se introduce un nuevo guess
    })
    })
#Ploteamos el diagrama de puntos
  output$plot1 <- renderPlot({
      ggplot(df$df) + geom_point(aes(x=X1, y=X2))+
      scale_x_continuous(limits = c(-2, 2))+
      scale_y_continuous(limits = c(-2, 2))
    })

#Cada vez que se pulsa el boton lets see button
#Se calcula la diferencia entre la correlacion real y la estimada por el jugador
  observeEvent(input$LSB,{
    z <- cor(df$df$X1,df$df$X2)
    c <- round(z,2)
    e <- round(as.numeric(input$yourgue)-c,2)
    output$result <- renderText({
      req(-1<=as.numeric(input$yourgue),
          as.numeric(input$yourgue)<=1,
          is.na(as.numeric(input$yourgue))==FALSE)#requerimos que la entrada sea un num entre -1 y 1
      paste0("La correlación real es: ",c," y el error cometido: ",e)
    })
  })
  
#Minijuego 2 -------------------------------------------------------------
#Creamos un df con un unico valor y una correlacion que será la correlacion objetivo
  df2 <- reactiveValues()
  df2$df<-data.frame(X1=c(0),X2=c(0))
  df2$corr <- round(runif(1, -1, 1), 2)
#Cada vez que pulsamos Reset obtenemos un df como el inicial y una nueva correlacion objetivo
#Eliminamos a su vez el mensaje de salida
observeEvent(input$NP_2,{
  df2$df<-data.frame(X1=c(0),X2=c(0))
  df2$corr <- round(runif(1, -1, 1), 2)
    output$result2 <- renderText({
      paste0("")
    })
  })
observeEvent(input$addclick,{
  df2$df <- rbind(df2$df,c(input$addclick$x,input$addclick$y))
  output$result2 <- renderText({
    paste0("") #reseteo salida cada vez que añada un punto
})
})
output$objective2 <- renderText({
    paste('Consigue una correlación de:', df2$corr)
  })

#Ploteamos el diagrama de puntos
output$plot2 <- renderPlot({
  req(df2$df)
  ggplot(df2$df) + geom_point(aes(x=X1, y=X2))+
    scale_x_continuous(limits = c(-2, 2))+
    scale_y_continuous(limits = c(-2, 2))
})
#Cada vez que se pulsa el boton de check calcula la dif entre la correlacion pintada y la real
observeEvent(input$check2, {
  current_corr <- round(cor(df2$df$X1, df2$df$X2), 2)
  dif_corr <- current_corr - df2$corr
    output$result2 <- renderText({
      paste0("La correlación actual es: ", current_corr," y el error cometido: ", dif_corr)
    })
  })

#Minijuego 3 -------------------------------------------------------------
#Creamos correlacion inicial objetivo
df$corr <- round(runif(1, -1, 1), 2)
#Cada vez que pulse el boton NP generamos un nuevo df inicial y una nueva correlacion objetivo
observeEvent(input$NP_3,{
  cor <- round(runif(1,-1,1),2)
  sigma<- matrix(c(1,cor,cor,1),nrow=2)
  df$df <- data.frame(rmvnorm(50,sigma=sigma, method="chol"))
  df$corr <- round(runif(1, -1, 1), 2)
  output$result3 <- renderText({
    paste0("")#reseteo salida cada vez que pulso
  })
})

#Creamos el plot con plotly
output$plot3 <- renderPlotly({
  #Creamos circulos negros de dos pixeles para cada punto
  circles <- map2(df$df$X1, df$df$X2, 
                  ~list(
                    type = "circle",
                    xanchor = .x,
                    yanchor = .y,
                    #Diametro 2pixel
                    x0 = -4, x1 = 4,
                    y0 = -4, y1 = 4,
                    xsizemode = "pixel", 
                    ysizemode = "pixel",
                    #color
                    fillcolor = "black",
                    line = list(color = "transparent")
                  )
  )
  
  #Ploteamos estos puntos
  plot_ly() %>%
    layout(shapes = circles) %>%
    config(edits = list(shapePosition = TRUE))
})
#Correlacion objetivo, se modificara gracias al reactive
output$objective3 <- renderText({
  paste('Consigue una correlación de:', df$corr)
})
#Al pulsar el boton check calcula la correlacion de los datos pintados
#Y el error respecto a la real
observeEvent(input$check3, {
  current_corr <- round(cor(df$df$X1, df$df$X2), 2)
  dif_corr <- current_corr - df$corr
  output$result3 <- renderText({
    paste0("La correlacion actual es: ", current_corr," y el error cometido: ", dif_corr)
  })
})
#utilizamos plotly_relayout para modificar el df,
#Compara los valores anteriores con las modificaciones para así crear nuevos puntos
observeEvent(event_data('plotly_relayout'),{
  ed <- event_data("plotly_relayout")
  shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
  if (length(shape_anchors) != 2) return()
  row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
  pts <- as.numeric(shape_anchors)
  df$df$X1[row_index] <- pts[1]
  df$df$X2[row_index] <- pts[2]
  output$result3 <- renderText({
    paste0("") #reseteo salida cada vez que mueva punto
  })
})

#Minijuego 4 -------------------------------------------------------------
#Creamos un df sencillo de n enteros y tamaño impar para que la mediana sea unica y entera
#Y por tanto, mas facil de adivinar
reactive <- reactiveValues()
reactive$df <- data.frame(X1=sample(c(-20:20),21),X2=sample(c(-20:20),21))

#Creamos un nuevo df y reseteamos el output cada vez que pulsamos New plot
observeEvent(input$NP_4,{
  reactive$df <- data.frame(X1=sample(c(-20:20),21),X2=sample(c(-20:20),21))
  output$value_x <- renderText({
    paste0("")
  })
  output$value_y <- renderText({
    paste0("")
  })
})
observeEvent(input$medianx,{
  output$value_x <- renderText({
    paste0("")#"Vaciamos" la salida por defecto, asi se vacia cada vez que se introduce un nuevo guess
  })
})
observeEvent(input$mediany,{
  output$value_y <- renderText({
    paste0("")#"Vaciamos" la salida por defecto, asi se vacia cada vez que se introduce un nuevo guess
  })
})

#Ploteamos el diagrama
output$plot4 <- renderPlot({
  ggplot(reactive$df)+geom_point(aes(x=X1, y=X2))
})
#Cada vez que pulsamos check calculamos los valores reales de la mediana
observeEvent(input$check4,{
  medianx <- round(median(array(reactive$df$X1)), 2)
  mediany <- round(median(array(reactive$df$X2)), 2)
  output$value_x <- renderText({
    req(input$medianx,is.na(as.numeric(input$medianx))==FALSE)
    #solo saca la mediana de x si se da un guess de x
    #requerimos tb que sea un num
    paste0("Your guess is: ",as.numeric(input$medianx), " and the real median for X (X1) is: ",
           medianx)
  })
  output$value_y <- renderText({
    req(input$mediany,is.na(as.numeric(input$mediany))==FALSE)
    #solo saca la mediana de x si se da un guess de y
    #de esta manera se puede tratar de adivinar solo x o solo y
    #requerimos tambien que sea un num
    paste0("Your guess is: ",as.numeric(input$mediany)," and the real median for Y (X2) is: ",
           mediany)
  })
})
}
# -------------------------------------------------------------------------


shinyApp(ui, server)

