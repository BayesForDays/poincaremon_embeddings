library(tidyverse)
library(ggdendro)

df <- read_tsv("./20191005_mon_vecs_type-egg-ability-only.txt")
df$clean_name <- stringr::str_replace(df$X1, "mon_", "")
df_numeric <- df %>% select(-X1)

###
w1_sample <- sample(rownames(df_numeric), size=400, replace=FALSE)
w1_clusterz <- stats::hclust(dist(df_numeric[w1_sample,], method='euclidean'))
w1_clusterz$labels <- df[w1_sample,]$clean_name
ggdendrogram(w1_clusterz)

library(Rtsne)

features <- read_tsv("./20191005_mon_types-abilities-egg_features.tsv")

tsne_2d <- Rtsne(df_numeric)
tsne_2d <- as.data.frame(tsne_2d$Y)
names(tsne_2d) <- c("tsne_x", "tsne_y")
tsne_2d$mon <- df$clean_name
ggplot(tsne_2d, aes(x=tsne_x, y=tsne_y)) + geom_point()
