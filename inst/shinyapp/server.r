library(shiny)
library(gmhelper)


shinyServer(function(input, output, session){
  files <- dir("./pages", recursive=TRUE, pattern="[.]r$")
  files <- paste0("./pages/", files)
  for (file in files) source(file=file, local=TRUE)
  
  
  localstate <- reactiveValues()
  
  
  names_npc(input)
  names_dungeon(input)
  names_adventure(input)
  names_tavern(input)
})
