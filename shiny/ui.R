library(shiny)

fluidPage(
  titlePanel("Spreminjanje števila potovanj v državah EU"),
  column(3,
         selectInput("select", label = "Izberi državo", 
                     choices = list("Slovenia", "Austria", "Spain", "France", "Germany", "Luxembourg", "Denmark", "Belgium",
                                    "Bulgaria", "Czechia", "Estonia", "Ireland", "Greece", "Croatia", "Cyprus", "Italy", "Latvia",
                                    "Lithuania", "Malta", "Hungary", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Finland",
                                    "Sweden"), 
                     selected = "Slovenia")
         ))
  column(9,
         renderPlot({
           narisi.graf(input$drzava)
         })
  )
  
  
  