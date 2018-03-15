library(shiny)


about <- HTML("
  <center>
  <img src='img.jpg'>
  </center>
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
      
      tabPanel("About", helpText(about)),
      tabPanel("Names", uiOutput("main_names")),
      tabPanel("Loot", uiOutput("main_loot")),
      tabPanel("Misc", uiOutput("main_misc")),
      tabPanel("Dice", uiOutput("main_dice")),
      tabPanel("Tables", uiOutput("main_tables"))
      )
    )
  )
