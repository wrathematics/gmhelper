dice_values =  c("d4", "d6", "d8", "d10", "d12", "d20", "d100")

roll = function(n, d) sum(sample(d, size=n, replace=TRUE))

output$main_dice <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Basic", uiOutput("dice_roller_basic_ui")),
      # tabPanel("Advanced", uiOutput("dice_roller_advanced_ui")),
      tabPanel("Scatter", uiOutput("dice_roller_scatter_ui"))
    )
  )
})



output$dice_roller_basic_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="dice_d", label="Die", dice_values, selected="d20", inline=FALSE),
        checkboxInput(inputId="dice_accum", label="Accumulate?", value = FALSE, width = NULL),
        
        actionButton("dice_button_basic_roll", "Roll!")
      ),
      
      mainPanel(
        renderUI({
          localstate$dice_roller_basic
        })
      )
    )
  )
})



dice_roller_basic = function(input)
{
  observeEvent(input$dice_button_basic_roll, {
    accum = input$dice_accum
    
    d = as.integer(sub(input$dice_d, pattern="^d", replacement=""))
    res = roll(1, d)
    
    if (accum)
    {
      old = localstate$dice_roller_basic_accum
      localstate$dice_roller_basic_accum = c(old, res)
      res = sum(old) + res
      
      resf = paste(
        paste(cumsum(old), collapse="<br/>"), 
        "<br/>",
        paste("<font size=55>", res, "</font>"))
    }
    else
    {
      if (!accum && d == 20 && (res == 1 || res == 20))
        resf = paste("<font size=55 color='red'>", res, "</font>")
      else
        resf = paste("<font size=55>", res, "</font>")
    }
    
    localstate$dice_roller_basic = HTML(resf)
  })
  
  invisible()
}



output$dice_roller_advanced_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="dice_advanced_d", label="Die", dice_values, selected="d20", inline=FALSE),
        
        actionButton("dice_button_clear", "Clear"),
        actionButton("dice_button_add", "Add"),
        actionButton("dice_button_roll", "Roll!")
      ),
      
      mainPanel(
        renderUI({
          localstate$dice_roller_advanced
        })
      )
    )
  )
})



init_dice = function()
{
  bins = sapply(dice_values, function(.) 0L)
  names(bins) = dice_values
  
  localstate$dice_bins = bins
}

reset_dice = function()
{
  for (i in 1:length(localstate$dice_bins))
    localstate$dice_bins[i] = 0L
  
  invisible()
}



dice_roller_advanced = function(input)
{
  observeEvent(input$dice_button_add, {
    die = input$dice_advanced_d
    localstate$dice_bins[die] = localstate$dice_bins[die] + 1L
    
    localstate$dice_roller_advanced = HTML(paste(localstate$dice_bins, collapse=" "))
  })
  
  observeEvent(input$dice_button_clear, {
    init_dice()
    localstate$dice_roller_advanced = HTML("")
  })
  
  observeEvent(input$dice_button_roll, {
    res = ""
    tot = 0L
    bins = localstate$dice_bins
    first = TRUE
    
    for (i in 1:length(bins))
    {
      if (bins[i] > 0)
      {
        n = bins[i]
        d = as.integer(gsub(names(bins[i]), pattern="^d", replacement=""))
        
        val = roll(n, d)
        
        if (first)
        {
          res = paste(res, val)
          first = FALSE
        }
        else
          res = paste(res, "+", val)
      }
    }
    
    reset_dice()
    localstate$dice_roller_advanced = HTML(res)
  })
  
  invisible()
}



output$dice_roller_scatter_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        actionButton("dice_button_scatter_roll", "Roll!")
      ),
      
      mainPanel(
        renderPlot({
          localstate$dice_roller_scatter
        })
      )
    )
  )
})



dice_roller_scatter = function(input)
{
  observeEvent(input$dice_button_scatter_roll, {
    localstate$dice_roller_scatter = gmhelper:::roll_scatter()
  })
  
  invisible()
}
