dice_values =  c("d4", "d6", "d8", "d10", "d12", "d20", "d100")

roll = function(n, d) sum(sample(d, size=n, replace=TRUE))

output$main_dice <- renderUI({
  mainPanel(
    tabsetPanel(
      tabPanel("Basic", uiOutput("dice_roller_ui")),
      tabPanel("Advanced", uiOutput("dice_roller_advanced_ui"))
    )
  )
})



output$dice_roller_ui <- renderUI({
  list(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId="dice_d", label="Die", dice_values, selected="d20", inline=FALSE),
        checkboxInput(inputId="dice_accum", label="Accumulate?", value = FALSE, width = NULL),
        
        actionButton("dice_button_basic_roll", "Roll!")
      ),
      
      mainPanel(
        renderUI({
          localstate$dice_roller
        })
      )
    )
  )
})



dice_roller = function(input)
{
  observeEvent(input$dice_button_basic_roll, {
    d = as.integer(sub(input$dice_d, pattern="^d", replacement=""))
    res = roll(1, d)
    
    if (input$dice_accum)
    {
      
      res = as.integer(localstate$dice_roller) + res
    }
    
    localstate$dice_roller = HTML(paste(res))
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
