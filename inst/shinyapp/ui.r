library(shiny)


about.thisapp <- HTML("
  <p>
  This is a collection of rollers for use with fantasy tabletop
  gaming useful to game/dungeon masters.
  
  The generators are written in R, and the web framework is 
  written in shiny.  The source code for each is available
  here 
  <a href='https://github.com/wrathematics/gmhelper'>
  https://github.com/wrathematics/gmhelper
  </a>.
  </p>
")


shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="superhero.min.css")
    ),
    
    navbarPage(
      title="GM Helper", 
      windowTitle="GM Helper", id="nav_tag", 
      inverse=TRUE, collapsible=FALSE, 
      
      tabPanel("Names", uiOutput("main_names")),
      tabPanel("Loot", uiOutput("main_loot")),
      # tabPanel("Roller", uiOutput("main_roller")),
      tabPanel("About", helpText(about.thisapp))
      )
    )
  )
