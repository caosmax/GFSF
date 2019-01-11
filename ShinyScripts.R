# scripts shiny
library(shiny)

ui<- fluidPage(

#* Input() functions,
#* Output() functions,

)
server<- function(input, output){
  
  output$hist<- renderPlot({
    hist(rnorm(100))
    })
}

shinyApp(ui=ui, server=server)



