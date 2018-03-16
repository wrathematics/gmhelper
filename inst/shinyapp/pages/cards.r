output$main_cards <- renderUI({
  mainPanel(
    tabsetPanel(
      
      tabPanel("Tarot", uiOutput("cards_tarot_ui"))
    )
  )
})



output$cards_tarot_ui = renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        actionButton("cards_button_tarot_draw", "Draw")
      ),
      
      mainPanel(
        renderUI({
          localstate$cards_tarot_draw
        })
      )
    )
  )
})



cards_tarot_draw = function(input)
{
  observeEvent(input$cards_button_tarot_draw, {
    cards = dir("www/tarot", full.names=FALSE)
    card = paste0("<img src='tarot/", sample(cards, size=1), "' width=200px>")
    
    localstate$cards_tarot_draw = HTML(card)
  })
  
  invisible()
}
