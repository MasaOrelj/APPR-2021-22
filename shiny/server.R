
shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_graf(input$drzava)
  })
})



narisi_graf <- function(drzava){
  graf <- ggplot(tabela_shiny %>% filter(Drzava == drzava))+
    aes(x = Leto)+
    geom_line(aes(y=Stevilo.potovanj.znotraj.drzave))+
    geom_line(aes(y= Stevilo.potovanj.v.tujino))+
    theme_classic() +
    labs(
      x = "Leto",
      y = "Število izletov",
      title = "Spreminjanje števila izletov med elti 2012 in 2019 v državah EU"
    ) + theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5)
    )
  print(graf)
}
