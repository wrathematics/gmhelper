ngen <- 1:15 # number of items to generate


tarot_cards = dir("www/tarot", full.names=FALSE)
playing_cards = dir("www/playing", full.names=FALSE)
playing_cards_nojoker = playing_cards[-grep(playing_cards, pattern="Joker")]

list_to_bullets <- function(x)
{
  paste0(
    "<ul>\n", 
    paste0("<li>", x, "</li>", "\n", collapse=""),
    "</ul>"
  )
}
