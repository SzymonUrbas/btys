library(shiny)

ui <- fluidPage(

  actionButton('switchtab',"Click this"),
  textOutput('code_ran')

)

server <- function(input, output, session){

  print("Initializing")

  observeEvent(input$switchtab,{
    aggg_result = -1
    if(aggg_result == -1)
    {
      session$reload()
      return()
      print("session reload not working")
    }

    print("Code running this line")

    output$code_ran <- renderText("code Ran this line without refreshing")

  })

}
shinyApp(ui = ui, server = server)