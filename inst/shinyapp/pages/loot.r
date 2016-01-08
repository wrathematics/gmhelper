output$main_loot <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Pockets", uiOutput("loot_pockets_ui")),
      tabPanel("Potions", uiOutput("loot_potions_ui"))
    )
  )
})



### NPC
output$loot_pockets_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="loot_pockets_wealth", 
                     label="Wealth of the Mark", 
                     c("RANDOM", "Pauper", "Commoner", "Merchant", "Noble", "King"), 
                     selected="RANDOM", 
                     inline=FALSE),
        
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
    
    localstate$loot_out <- HTML(list_to_bullets(loot))
  })
  
  invisible()
}



### Potions
potion_quality_labels <- c("RANDOM", "Very Low", "Low", "Average", "High", "Very High")
potion_accuarcy_labels <- c("RANDOM", "None", "Correct", "Incorrect")
potion_language_labels <- c("RANDOM", "None", "Correct", "Incorrect")

output$loot_potions_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="loot_potions_quality", 
                     label="Wealth of the Mark", 
                     potion_quality_labels, 
                     selected="RANDOM", 
                     inline=FALSE),
        
        radioButtons(inputId="loot_potions_label",
                    label="Label", 
                    potion_accuarcy_labels,
                    selected="RANDOM"),
        
        radioButtons(inputId="loot_potions_language",
                    label="Label Language", 
                    potion_language_labels,
                    selected="RANDOM"),
        
        actionButton("loot_potions_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$potions_out
        })
      )
    )
  )
})



loot_potions <- function(input)
{
  observeEvent(input$loot_potions_fit, {
    
    quality <- input$loot_potions_quality
    label <- input$loot_potions_label
    language <- input$loot_potions_language
    
    if (quality == "RANDOM")
      quality <- 'rand'
    else 
      quality <- which(potion_quality_labels == quality) - 1L
    
    if (label == "RANDOM")
      label <- 'rand'
    else 
      label <- which(potion_accuracy_labels == quality) - 1L
    
    if (language == "RANDOM")
      language <- 'rand'
    else 
      language <- which(potion_language_labels == quality) - 1L
    
    loot <- gmhelper:::gen_pot_desc(qual=quality, label=label, labellang=language)
    
    loot <- strsplit(loot, split="\n")[[1]]
    loot <- paste("<b>", loot)
    loot <- gsub(loot, pattern="*:", replacement="</b>:")
    
    localstate$potions_out <- HTML(list_to_bullets(loot))
  })
  
  invisible()
}
