ngen <- 1:15 # number of items to generate



list_to_bullets <- function(x)
{
  paste0(
    "<ul>\n", 
    paste0("<li>", x, "</li>", "\n", collapse=""),
    "</ul>"
  )
}
