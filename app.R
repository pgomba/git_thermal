
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
    p(HTML(paste0("Use ",tags$code("germ"), "column to input the number of germinated seeds and" ,tags$code("viab"), " column for the total of viable seeds on your Petri dish (germinated+mouldy, but no empties)")), style = "font-family: 'times'; font-si16pt"),
    helpText(a("Click to download example/template.",href="https://github.com/pgomba/temp/raw/main/set.xlsx")),
    #tableOutput("table"),
    #tableOutput("mergetable"),
    plotOutput("plot2",width="600px",height = "590px"),
    plotOutput("plot3",width="600px",height = "590px"),
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
    day_temp_ext<-rep(day_temp,each=13)
    night_temp_ext<-rep(day_temp,times=13)
    data.frame(day_temp_ext,night_temp_ext)
    })

  output$mergetable<-renderTable({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    day_temp<-seq((input$dTL+input$dTR)/2,(input$dBL+input$dBR)/2,length.out=sqrt(nrow(bb())))
    night_temp<-seq((input$nTL+input$nBL)/2,(input$nTR+input$nBR)/2,length.out=sqrt(nrow(bb())))
    day_temp_ext<-rep(day_temp,each=13)
    night_temp_ext<-rep(day_temp,times=13)
    data.frame(day_temp_ext,night_temp_ext)%>%cbind(bb())
  })
  
  cc<-reactive({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    day_temp<-seq((input$dTL+input$dTR)/2,(input$dBL+input$dBR)/2,length.out=sqrt(nrow(bb())))
    night_temp<-seq((input$nTL+input$nBL)/2,(input$nTR+input$nBR)/2,length.out=sqrt(nrow(bb())))
    day_temp_ext<-rep(day_temp,each=13)
    night_temp_ext<-rep(night_temp,times=13)
    data.frame(day_temp_ext,night_temp_ext)%>%cbind(bb())
  })
  
  output$plot2<-renderPlot({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    ggplot(cc(),aes(day_temp_ext,night_temp_ext,z=(germ/viab)*100,label=round((germ/viab)*100,digits = 0)))+
      geom_segment(x=0,xend=40,y=0,yend=40,size=2,linetype="dashed")+
      geom_point(aes(fill=germ),size=16,shape=21,stroke=1.1)+
      theme_dark()+
      geom_text(size=6)+
      scale_fill_distiller(palette = "Spectral", direction = -1)+
      labs(title="Final Germination (%)",x="Day Temperature",y="Night Temperature")+
      theme(plot.title =element_text(size=16),
            legend.title = element_blank(),
            axis.text.x = element_text(size=20),
            axis.text.y= element_text(size=20),
            axis.title = element_text(size=20))+
      theme(legend.position = "right")
  })
  
  output$plot3<-renderPlot({
    file_to_plot<-input$file
    if(is.null(file_to_plot)){
      return()
    }
    ggplot(cc(),aes(day_temp_ext,night_temp_ext,z=1/t50,label=round(1/t50,2)))+
      geom_segment(x=0,xend=40,y=0,yend=40,size=2,linetype="dashed")+
      geom_point(aes(fill=1/t50),size=16,shape=21,stroke=1.1)+
      theme_dark()+
      geom_text(size=5)+
      scale_fill_distiller(palette = "YlGn", direction = -1)+
      labs(title=expression(Germination~Rate~(1/T[50])),x="Day Temperature",y="Night Temperature")+
      theme(plot.title =element_text(size=16),
            legend.title = element_blank(),
            axis.text.x = element_text(size=20),
            axis.text.y= element_text(size=20),
            axis.title = element_text(size=20)
            )+
      theme(legend.position = "right")
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
