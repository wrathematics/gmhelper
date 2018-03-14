opts = list(
  pageLength = -1,
  searching=FALSE,
  scroller=TRUE,
  lengthChange=FALSE
)



output$main_tables <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Services", uiOutput("tables_services_ui"))
    )
  )
})



services = gmhelper:::gmh_services
tables_services = c("Construction", "Inn", "Livestock", "Lodgings", "Menu")

output$tables_services_ui = renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="tables_services_radio", label="Services", tables_services, inline=FALSE)
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
