output$main_cards <- renderUI({
  mainPanel(
    tabsetPanel(
      
      tabPanel("Tarot", uiOutput("cards_tarot_ui")),
      tabPanel("Playing", uiOutput("cards_playing_ui"))
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
    card = paste0("<img src='tarot/", sample(tarot_cards, size=1), "' width=200px>")
    
    localstate$cards_tarot_draw = HTML(card)
  })
  
  invisible()
}



output$cards_playing_ui = renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        checkboxInput(inputId="cards_playing_joker", label="Include jokers?", value=FALSE, width=NULL),
        actionButton("cards_button_playing_draw", "Draw")
      ),
      
      mainPanel(
        renderUI({
          localstate$cards_playing_draw
        })
      )
    )
  )
})



cards_playing_draw = function(input)
{
  observeEvent(input$cards_button_playing_draw, {
    if (input$cards_playing_joker)
      draw = sample(playing_cards, size=1)
    else
      draw = sample(playing_cards_nojoker, size=1)
    
    card = paste0("<img src='playing/", draw, "' width=200px>")
    
    localstate$cards_playing_draw = HTML(card)
  })
  
  invisible()
}
