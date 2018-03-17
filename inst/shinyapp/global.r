ngen <- 1:15 # number of items to generate


tarot_cards = dir("www/tarot", full.names=FALSE, pattern="*.jpg")
tarot_mapping = read.table("www/tarot/mapping.csv", header=TRUE, sep="\t", stringsAsFactors=FALSE)
domt = tarot_mapping[which(tarot_mapping$Plague != ""), ]

playing_cards = dir("www/playing", full.names=FALSE, pattern="*.png")
playing_cards_nojoker = playing_cards[-grep(playing_cards, pattern="Joker")]

list_to_bullets <- function(x)
{
  paste0(
    "<ul>\n", 
    paste0("<li>", x, "</li>", "\n", collapse=""),
    "</ul>"
  )
}
