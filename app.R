
library(shiny)
library(readxl)
library(tidyverse)

# UI

ui <- fluidPage(

  titlePanel(title=div("ThermalPlate App",img(src="headimage.png"))), 
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
    div(img(src="thermal.png"),img(src="thermal2.png")),
    p(HTML(paste0("Left. Thermal plate Petri dish expected labelling. Right. Spreadsheet template. Keep same headings and leave blank unknown or not calculable T",tags$sub("50")," values")), style = "font-family: 'times'; font-si16pt"),
    tableOutput("table"),
    plotOutput("plot"),
    textOutput("selected_var")
  )
)

# SERVER STARTS HERE
server <- function(input, output) {

  # Outputs user template 
  output$table<-renderTable({
    file_to_read<-input$file
    if(is.null(file_to_read)){
      return()
    }
    read_xlsx(file_to_read$datapath)%>%head()
  })

# Creates bb(), a data frame wth your template 
  
  bb<-reactive({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    bb<-read_xlsx(file_to_plot$datapath)
  })
  
  output$selected_var <- renderText({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    paste0("Your Petri dish grid is ",nrow(bb())," by ",ncol(bb()))
  })
  
  output$plot<-renderPlot({
    if(is.null(bb())){
      return()
    }
    ggplot(bb(),aes(x=a,y=b))+
      geom_point()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
