output$main_misc <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Critical & Fumbles", uiOutput("misc_crits_ui"))
    )
  )
})



### NPC
output$misc_crits_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="misc_crits_hitmiss", 
                     label="Crit or Fumble", 
                     c("Crit", "Fumble"), 
                     selected="Crit", 
                     inline=FALSE),
        
        radioButtons(inputId="misc_crits_type", 
                     label="Weapon Type", 
                     c("Melee", "Ranged"), 
                     selected="Melee", 
                     inline=FALSE),
        
        actionButton("misc_crits_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$misc_crits
        })
      )
    )
  )
})



misc_crits <- function(input)
{
  observeEvent(input$misc_crits_fit, {
    hitmiss <- input$misc_crits_hitmiss
    type <- input$misc_crits_type
    
    if (hitmiss == "Crit")
      crit <- gmhelper:::gencrit(type)
    else if (hitmiss == "Fumble")
      crit <- gmhelper:::genmiss(type)
    
    crit <- sub(paste0("<b>", crit), pattern="!", replacement="!</b>")
    
    localstate$misc_crits <- HTML(crit)
  })
  
  invisible()
}
