root = "~/dev/gmhelper/"
data_root = paste0(root, "/data/")
setwd(paste0(root, "/data_raw/data/names/races"))

reader = function(f) read.csv(f, sep="\t", header=TRUE, stringsAsFactors=TRUE)

files = dir(pattern="*.csv")
names_names = sapply(strsplit(files, split=".", fixed=TRUE), `[`, 1)

gmh_racenames = lapply(files, reader)
names(gmh_racenames) = names_names # lel

outfile = paste0(data_root, "gmh_racenames.rda")
save(file=outfile, gmh_racenames)
