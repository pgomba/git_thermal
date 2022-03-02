#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(readxl)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Thermal plate app"),
  sidebarPanel(
    fileInput("file", "Upload data*", accept = c(".xlsx")),
    p("Upload .xlsx file. Other formats to be included in due course", style = "font-family: 'times'; font-si16pt"),
    br(),
    p("Enter corner temperatures. Usually this is an average of the temperatures recorded via temperature logger", style = "font-family: 'times'; font-si16pt"),
    p("Day corner temperatures", style = "font-family: 'times'; font-si16pt"),
    splitLayout(numericInput("dTL","TopLeft",40,min=0,max=45),
                numericInput("dTR","TopRight",40,min=0,max=45)),
    splitLayout(numericInput("dBL","BottomLeft",0,min=0,max=45),
                numericInput("dBR","BottomRight",0,min=0,max=45)),
    p("Night corner temperatures", style = "font-family: 'times'; font-si16pt"),
    splitLayout(numericInput("nTL","TopLeft",40,min=0,max=45),
                numericInput("nTR","TopRight",40,min=0,max=45)),
    splitLayout(numericInput("nBL","BottomLeft",0,min=0,max=45),
                numericInput("nBR","BottomRight",0,min=0,max=45))
    
  ),
  
  mainPanel(
    p("Thermal plate Petri dish labels scheme.", style = "font-family: 'times'; font-si16pt"),
    img(src="thermal.png"),
    br(),
    br(),
    p(HTML(paste0("Upload information following this template. Respect column names. Leave cell in blank if T",tags$sub("50")," is not known")), style = "font-family: 'times'; font-si16pt"),
    img(src="thermal2.png"),
    tableOutput("table"),
    plotOutput("plot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table<-renderTable({
    file_to_read<-input$file
    if(is.null(file_to_read)){
      return()
    }
    read_xlsx(file_to_read$datapath)%>%head()
  })
  
  bb<-reactive({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    bb<-read_xlsx(file_to_plot$datapath)
  })
  
  output$plot<-renderPlot({
    ggplot(bb(),aes(x=a,y=b))+
      geom_point()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
