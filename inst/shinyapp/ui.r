library(shiny)


about.thisapp <- HTML("
  <p>
  TODO
  </p>
")


shinyUI(
  fluidPage(
    navbarPage(
      title="GM Helper", 
      windowTitle="GM Helper", id="nav_tag", 
      inverse=TRUE, collapsible=FALSE, 
      
      tabPanel("Names", uiOutput("main_names")),
      tabPanel("Loot", uiOutput("main_loot")),
      tabPanel("Roller", uiOutput("main_roller")),
      tabPanel("About", helpText(about.thisapp))
      )
    )
  )
