opts = list(
  pageLength = -1,
  searching=FALSE,
  scroller=TRUE,
  lengthChange=FALSE,
  info=FALSE,
  paginate=FALSE
)



output$main_tables <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Services", uiOutput("tables_services_ui")),
      tabPanel("Henchmen", uiOutput("tables_henchmen_ui"))
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
