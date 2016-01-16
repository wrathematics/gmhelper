library(shiny)


about.thisapp <- HTML("
  <h1>GM Helper</h1>
  
  <img src='img.jpg' style='height:325px'>
  
  <h3>Random Rollers for Lazy Assholes</h3>
  
  <p style='font-size:120%;'>
  This is a collection of generators for use with fantasy tabletop
  gaming useful to game/dungeon masters.  When you're too lazy to
  even roll a die and look up the result in a table, there's
  the GM Helper.
  </p>
  
  <p style='font-size:120%;'>
  The generators are written in R, and the web framework is 
  written in shiny.  The source code for each is available
  here 
  <a href='https://github.com/wrathematics/gmhelper'>
  https://github.com/wrathematics/gmhelper
  </a>.
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
      
      tabPanel("Names", uiOutput("main_names")),
      tabPanel("Loot", uiOutput("main_loot")),
      tabPanel("Misc", uiOutput("main_misc")),
      # tabPanel("Roller", uiOutput("main_roller")),
      tabPanel("About", helpText(about.thisapp))
      )
    )
  )
