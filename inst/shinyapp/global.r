ngen <- 1:15



list_to_bullets <- function(x)
{
  paste0(
    "<ul>\n", 
    paste0("<li>", x, "</li>", "\n", collapse=""),
    "</ul>"
  )
}
