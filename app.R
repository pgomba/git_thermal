
library(shiny)
library(readxl)
library(tidyverse)

# UI

ui <- fluidPage(

  titlePanel(title=div("ThermalPlate App",img(src="headimage.png"))), 
  sidebarPanel(
    
    # Upload file input
    fileInput("file", "Upload data*", accept = c(".xlsx")),
    p("Upload .xlsx file. Other formats to be included in due course", style = "font-family: 'times'; font-si16pt"),
    br(),
    p("Enter corner temperatures. Usually this is an average of the temperatures recorded via temperature logger", style = "font-family: 'times'; font-si16pt"),
    p("Day corner temperatures", style = "font-family: 'times'; font-si16pt"),
    # Upload temperature inputs
    splitLayout(numericInput("dTL","TopLeft",40,min=0,max=45),
                numericInput("dTR","TopRight",40,min=0,max=45)),
    splitLayout(numericInput("dBL","BottomLeft",0,min=0,max=45),
                numericInput("dBR","BottomRight",0,min=0,max=45)),
    p("Night corner temperatures", style = "font-family: 'times'; font-si16pt"),
    splitLayout(numericInput("nTL","TopLeft",0,min=0,max=45),
                numericInput("nTR","TopRight",40,min=0,max=45)),
    splitLayout(numericInput("nBL","BottomLeft",0,min=0,max=45),
                numericInput("nBR","BottomRight",40,min=0,max=45))
    
  ),
  
  mainPanel(
    div(img(src="thermal.png"),img(src="thermal2.png")),
    p(HTML(paste0("Left. Thermal plate Petri dish expected labelling. Right. Spreadsheet template. Keep same headings and leave blank unknown or not calculable T",tags$sub("50")," values")), style = "font-family: 'times'; font-si16pt"),
    tableOutput("table"),
    tableOutput("temperatures"),
    plotOutput("plot"),
    textOutput("selected_var")
  )
)

# SERVER STARTS HERE
server <- function(input, output) {

# OUTPUTS  
  # Outputs user template 
  output$table<-renderTable({
    file_to_read<-input$file
    if(is.null(file_to_read)){
      return()
    }
    read_xlsx(file_to_read$datapath)%>%head()
  })
  
  # Outputs thermal plate grid in use (i.e. 8x8 or 13x13)
  output$selected_var <- renderText({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    paste0("Your Petri dish grid is ",sqrt(nrow(bb()))," by ",sqrt(nrow(bb())))
  })

  # Outputs germination % vs day night temperature
  output$plot<-renderPlot({
    if(is.null(bb())){
      return()
    }
    ggplot(bb(),aes(x=germ,y=viab))+
      geom_point()
  })
  
  
# REACTIVE  
  
# Creates bb(), a data frame to be feed into ggplot and analysis 
  
  bb<-reactive({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    read_xlsx(file_to_plot$datapath)
  })
  
  output$temperatures<-renderTable({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    day_temp<-seq((input$dTL+input$dTR)/2,(input$dBL+input$dBR)/2,length.out=sqrt(nrow(bb())))
    night_temp<-seq((input$nTL+input$nBL)/2,(input$nTR+input$nBR)/2,length.out=sqrt(nrow(bb())))
    data.frame(day_temp,night_temp)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
