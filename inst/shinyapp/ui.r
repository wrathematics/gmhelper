library(shiny)


about.thisapp <- HTML("
  <h1>GM Helper</h1>
  
  <img src='img.jpg' style='height:300px'>
  
  <h3>Random Rollers for Lazy Jerks</h3>
  
  <p style='font-size:120%;'>
  This is a collection of generators for use with fantasy tabletop
  gaming useful to game/dungeon masters.  When you're too lazy to
  even roll a die and look up the result in a table, there's
  the GM Helper.
  </p>
")


shinyUI(
  fluidPage(theme="superhero.min.css",
    # tags$head(
    #   tags$link(rel="stylesheet", type="text/css", href="superhero.min.css")
    # ),
    
    navbarPage(
      title="GM Helper", 
      windowTitle="GM Helper", id="nav_tag", 
      inverse=TRUE, collapsible=FALSE, 
      
      tabPanel("About", helpText(about.thisapp)),
      tabPanel("Names", uiOutput("main_names")),
      tabPanel("Loot", uiOutput("main_loot")),
      tabPanel("Misc", uiOutput("main_misc")),
      tabPanel("Dice", uiOutput("main_dice"))
      )
    )
  )
