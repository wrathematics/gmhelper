opts = list(
  pageLength = -1,
  searching=FALSE,
  scroller=TRUE,
  lengthChange=FALSE,
  info=FALSE,
  paginate=FALSE
)



tables_about <- HTML("
  <p>
  This is a collection of tables for use with OSR games. The prices should be
  largely compatible with OSRIC.
  </p>
")

output$main_tables <- renderUI({
  mainPanel(
    tabsetPanel(
      
      tabPanel("About", helpText(tables_about)),
      tabPanel("Henchmen", uiOutput("tables_henchmen_ui")),
      tabPanel("Services", uiOutput("tables_services_ui")),
      tabPanel("Weapons", uiOutput("tables_weapons_ui"))
    )
  )
})



services = gmhelper:::gmh_services

output$tables_services_ui = renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="tables_services_radio", label="Services",
        c("Construction", "Inn", "Livestock", "Lodgings", "Menu"),
        inline=FALSE)
      ),
      
      mainPanel(
        renderDataTable(options=opts, {
          switch(input$tables_services_radio,
            Construction=services$construction,
            Inn=services$inn,
            Livestock=services$livestock,
            Lodgings=services$lodgings,
            Menu=services$menu
          )
        })
      )
    )
  )
})



henchmen = gmhelper:::gmh_henchmen

output$tables_henchmen_ui = renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="tables_henchmen_radio", label="Services",
        c("Experts", "Hirelings", "Mercenaries", "Shipmen"),
        inline=FALSE)
      ),
      
      mainPanel(
        renderDataTable(options=opts, {
          switch(input$tables_henchmen_radio,
            Experts=henchmen$experts,
            Hirelings=henchmen$hirelings,
            Mercenaries=henchmen$mercenaries,
            Shipmen=henchmen$shipmen
          )
        })
      )
    )
  )
})



weapons = gmhelper:::gmh_weapons

output$tables_weapons_ui = renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="tables_weapons_radio", label="Weapons",
        c("Ammo", "Bladed", "Hafted", "Misc", "Missile", "Pole", "Siege"),
        inline=FALSE)
      ),
      
      mainPanel(
        renderDataTable(options=opts, {
          switch(input$tables_weapons_radio,
            Ammo=weapons$ammo,
            Bladed=weapons$bladed,
            Hafted=weapons$hafted,
            Misc=weapons$misc,
            Missile=weapons$missile,
            Pole=weapons$pole,
            Siege=weapons$siege
          )
        })
      )
    )
  )
})
