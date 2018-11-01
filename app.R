#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rsconnect)
library(ggplot2)
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #sidebarPanel(
  
  # In case you want to let the graph interactive,
  # you can create a side bar + allows users input
  # and then on server.R you make a reactive variable
  # when you call the graph, so it keeps updating itself
  
  # dateRangeInput("daterange", "Choice the date",
  #                start = min(dataset$approval.date),
  #                end = max(dataset$approval.date),
  #                min = min(dataset$approval.date),
  #                max = max(dataset$approval.date),
  #                separator = " - ", format = "dd/mm/yy",
  #                startview = 'Week', language = 'us', weekstart = 1)
  
  #),
  
  mainPanel(
    # calling the output 'barplot' on server.R
    plotOutput('barplot')
  )
)
   
server <- function(input, output) {
  
  
  output$barplot <- renderPlot({
   
    
    
    ggplot(data=df, aes(x=dose, y=len, group=1)) +
      geom_line()+
      geom_point()
    
  }, height=700)
}

# Run the application 
shinyApp(ui = ui, server = server)

