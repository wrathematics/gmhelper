root = "~/dev/gmhelper/"
data_root = paste0(root, "/data/")
setwd(paste0(root, "/data_raw/dungeon"))

reader = function(f) read.csv(f, sep="\t", header=TRUE, stringsAsFactors=FALSE)

files = dir(pattern="*.csv")
dungeon_names = sapply(strsplit(files, split=".", fixed=TRUE), `[`, 1)

gmh_dungeon = lapply(files, reader)
names(gmh_dungeon) = dungeon_names

outfile = paste0(data_root, "gmh_dungeon.rda")
save(file=outfile, gmh_dungeon)
