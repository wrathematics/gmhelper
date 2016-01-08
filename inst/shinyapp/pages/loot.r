output$main_loot <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Pockets", uiOutput("names_pockets_ui")),
      tabPanel("Potions", uiOutput("names_potions_ui"))
    )
  )
})



### NPC
output$names_pockets_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="loot_pockets_wealth", 
                     label="Wealth of the Mark", 
                     c("RANDOM", "Pauper", "Commoner", "Merchant", "Noble", "King"), 
                     selected="RANDOM", 
                     inline=FALSE),
        
        # selectInput(inputId="names_npc_race",
        #             label="Race", 
        #             c("Human", "Elf", "Dwarf", "Halfling", "Gnome", "Orc", "Troll"),
        #             selected="Human"),
        
        actionButton("loot_pockets_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$loot_out
        })
      )
    )
  )
})



loot_pockets <- function(input)
{
  observeEvent(input$loot_pockets_fit, {
    loot <- gmhelper:::pocketloot(input$loot_pockets_wealth)
    
    localstate$loot_out <- HTML(paste0(loot, "<br>"))
  })
  
  invisible()
}
