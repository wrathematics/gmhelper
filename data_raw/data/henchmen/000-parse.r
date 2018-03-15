root = "~/dev/gmhelper/"
data_root = paste0(root, "/data/")
setwd(paste0(root, "/data_raw/data/henchmen"))

reader = function(f) read.csv(f, sep="\t", header=TRUE, stringsAsFactors=FALSE)

files = dir(pattern="*.csv")
henchmen_names = sapply(strsplit(files, split=".", fixed=TRUE), `[`, 1)

gmh_henchmen = lapply(files, reader)
names(gmh_henchmen) = henchmen_names

outfile = paste0(data_root, "gmh_henchmen.rda")
save(file=outfile, gmh_henchmen)
