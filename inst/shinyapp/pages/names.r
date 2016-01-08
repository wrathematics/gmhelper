output$main_names <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("NPC", uiOutput("names_npc_ui")),
      tabPanel("Dungeon", uiOutput("names_dungeon_ui")),
      tabPanel("Old School Adventure", uiOutput("names_adventure_ui")),
      tabPanel("Tavern & Inn", uiOutput("names_tavern_ui")),
      tabPanel("Town", uiOutput("names_town_ui"))
    )
  )
})



### NPC
output$names_npc_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="names_npc_sex", 
                     label="Sex", 
                     c("M", "F"), 
                     selected="M", 
                     inline=FALSE),
        
        checkboxGroupInput(inputId="names_npc_whichnames",
                      label="Name", 
                      c("First", "Middle", "Last"),
                      selected=c("First", "Last"), 
                      inline=FALSE),
        
        selectInput(inputId="names_npc_race",
                    label="Race", 
                    c("Human", "Elf", "Dwarf", "Halfling", "Gnome", "Orc", "Troll"),
                    selected="Human"),
        
        actionButton("names_npc_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$npc_out
        })
      )
    )
  )
})



names_npc <- function(input)
{
  observeEvent(input$names_npc_fit, {
    sex <- input$names_npc_sex
    
    whichnames <- input$names_npc_whichnames
    first <- ifelse("First"%in%whichnames, 1, 0)
    middle <- ifelse("Middle"%in%whichnames, 1, 0)
    last <- ifelse("Last"%in%whichnames, 1, 0)
    
    race <- input$names_npc_race
    
    if (race=="Human")  
      name <- lapply(ngen, function(.) gmhelper:::human_name(first=first, middle=middle, last=last, sex=sex) )
    else if (race=="Elf")  
      name <- lapply(ngen, function(.) gmhelper:::elf_name(first=first, middle=middle, last=last, sex=sex) )
    else if (race=="Dwarf")
      name <- lapply(ngen, function(.) gmhelper:::dwarf_name(first=first, middle=middle, last=last, sex=sex) )
    else if (race=="Gnome")
      name <- lapply(ngen, function(.) gmhelper:::gnome_name(first=first, middle=middle, last=last, sex=sex) )
    else if (race=="Halfling")
      name <- lapply(ngen, function(.) gmhelper:::halfling_name(first=first, middle=middle, last=last, sex=sex) )
    else if (race=="Orc")
      name <- lapply(ngen, function(.) gmhelper:::orc_name(first=first, middle=middle, last=last) )
    else if (race=="Troll")
      name <- lapply(ngen, function(.) gmhelper:::troll_name(first=first, middle=middle, last=last) )
    else
      name <- ""
    
    
    localstate$npc_out <- HTML(paste0(name, "<br>"))
  })
  
  invisible()
}



### Dungeon
names_dungeon_radios_label <- c("RANDOM", "Very Low", "Low", "Medium", "High", "Very High")

output$names_dungeon_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="names_dungeon_radios", 
                     label="Party Level", 
                     names_dungeon_radios_label,
                     selected="RANDOM", 
                     inline=FALSE),
        
        actionButton("names_dungeon_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$dungeon_out
        })
      )
    )
  )
})



names_dungeon <- function(input)
{
  observeEvent(input$names_dungeon_fit, {
    
    whichdn <- input$names_dungeon_radios
    if (whichdn==names_dungeon_radios_label[1])
      qual <- NA
    else if (whichdn==names_dungeon_radios_label[2])
      qual <- 1
    else if (whichdn==names_dungeon_radios_label[3])
      qual <- 2
    else if (whichdn==names_dungeon_radios_label[4])
      qual <- 3
    else if (whichdn==names_dungeon_radios_label[5])
      qual <- 4
    else if (whichdn==names_dungeon_radios_label[6])
      qual <- 5

    name <- lapply(ngen, function(.) gmhelper:::dungeon_name(qual=qual))

    
    
    localstate$dungeon_out <- HTML(paste0(name, "<br>"))
  })
  
  invisible()
}



### Old School Adventure
output$names_adventure_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        actionButton("names_adventure_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$adventure_out
        })
      )
    )
  )
})



names_adventure <- function(input)
{
  observeEvent(input$names_adventure_fit, {
    name <- lapply(ngen, function(.) gmhelper:::adventure_name())
    
    localstate$adventure_out <- HTML(paste0(name, "<br>"))
  })
  
  invisible()
}



### Tavern & Inn
output$names_tavern_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        checkboxGroupInput(inputId="names_tavern_whichtype",
                          label="Establishment (neither for random)", 
                          c("Tavern", "Inn"),
                          inline=FALSE),
        
        actionButton("names_tavern_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$tavern_out
        })
      )
    )
  )
})



names_tavern <- function(input)
{
  observeEvent(input$names_tavern_fit, {
    tmp <- input$names_tavern_whichtype
    tavern <- "Tavern" %in% tmp
    inn <- "Inn" %in% tmp
    if (!(tavern || inn))
      tavern <- inn <- "rand"
    
    name <- lapply(ngen, function(.) gmhelper:::tavern_name(tavern=tavern, inn=inn))
    
    localstate$tavern_out <- HTML(paste0(name, "<br>"))
  })
  
  invisible()
}



### Town
output$names_town_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId="names_town_race", 
                     label="Founding Race", 
                     c("Human", "Dwarf", "Elf", "Halfling"), 
                     selected="Human", 
                     inline=FALSE),
        
        actionButton("names_town_fit", "Generate!")
      ),
      
      mainPanel(
        renderUI({
          localstate$town_out
        })
      )
    )
  )
})



names_town <- function(input)
{
  observeEvent(input$names_town_fit, {
    race <- input$names_town_race
    if (race=="Human")
      race <- 'h'
    else if (race=="Dwarf")
      race <- 'd'
    else if (race=="Elf")
      race <- 'e'
    else if (race=="Halfling")
      race <- 'hl'
    
    name <- lapply(ngen, function(.) gmhelper:::town_name(race=race))
    
    localstate$town_out <- HTML(paste0(name, "<br>"))
  })
  
  invisible()
}
