root = "~/dev/gmhelper/"
data_root = paste0(root, "/data/")
setwd(paste0(root, "/data_raw/items/gear/weapons"))

reader = function(f) read.csv(f, sep="\t", header=TRUE, stringsAsFactors=FALSE)

files = dir(pattern="*.csv")
weapons_names = sapply(strsplit(files, split=".", fixed=TRUE), `[`, 1)

gmh_weapons = lapply(files, reader)
names(gmh_weapons) = weapons_names

outfile = paste0(data_root, "gmh_weapons.rda")
save(file=outfile, gmh_weapons)
