library(shiny)
library(gmhelper)


shinyServer(function(input, output, session){
  files <- dir("./pages", recursive=TRUE, pattern="[.]r$")
  files <- paste0("./pages/", files)
  for (file in files)
    source(file=file, local=TRUE)
  
  
  localstate <- reactiveValues()
  init_dice()
  
  
  names_npc(input)
  names_dungeon(input)
  names_adventure(input)
  names_tavern(input)
  names_town(input)
  names_society(input)
  
  loot_pockets(input)
  loot_potions(input)
  
  misc_crits(input)
  
  dice_roller(input)
  dice_roller_advanced(input)
})
