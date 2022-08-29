
shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_graf(input$drzava)
  })
})



narisi_graf <- function(drzava){
  graf <- ggplot(tabela_shiny %>% filter(Drzava == "Slovenia"))+
    aes(x = Leto)+
    geom_line(aes(y= Potovanja.znotraj.drzave), color="green")+
    geom_line(aes(y= Potovanja.v.tujino), color = "blue")+
    theme_classic() +
    labs(
      x = "Leto",
      y = "Število izletov (v tisočih)",
      title = "Spreminjanje števila izletov med elti 2012 in 2019 v državah EU",
      color = "Legenda"
    ) + theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5)
    )
  ggplotly(graf)
}
