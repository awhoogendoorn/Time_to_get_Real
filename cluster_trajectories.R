# Cluster analysis
rm(list = ls(all.names = TRUE)) 


# libraries -----------------------------------------------------------------------------
library(GiG)
library(data.table)
library(tidyverse)
library(gtools)

library(FactoMineR)
library(factoextra)
library(fastcluster)
library(NbClust)
library(parallelDist)
library(seriation)
library(dbscan)


# load patient data ---------------------------------------------------------------------
load("./data/patients.Rda")


# Select matrix --------------------------------------------------------------------------

m <- patients %>%
  select(starts_with("mins_")) %>%
  # mins to hours
  mutate_at(.vars = vars(starts_with("mins_")), .funs = function(x) { x / 60 })
names(m) <- names(m) %>% str_sub(6)

#m$mins <- patients$mins
#m$mins.prev <- patients$mins.prev
#m$mins.prev[is.na(m$mins.prev)] <- 0


# rescale ------------------------------------------------------------------------
max_m <- max(m)
m <- m %>% mutate_all(.funs = function(x) { 
  #x <- scales::rescale(log10(x + 1))
  #x <- cut(x, breaks = c(-1, seq(from = 0, to = 1, by = .1)))
  # as.numeric(x / max_m)
  # as.numeric(as.numeric(x) > 1)
  #as.numeric(x) - 1
  x
})


# Sample -------------------------------------------------------------------------------

n_sample = nrow(m)
sample = sample(1:nrow(m), n_sample, replace = FALSE)
ms <- m[sample, ]

# no zero variance
ms <- ms[names(ms)[unlist(lapply(ms, function(x) sd(x) > 0) ) == TRUE]]


# dist -----------------------------------------------------------------------------------
memory.limit(10240)
di <-  parDist(as.matrix(ms), method = "fJaccard")
qplot(as.numeric(di), geom = "density")

# simplify ------------------------------------------------------------------------------- (20m)
pc <- cmdscale(di, k = 8, eig = TRUE)
plot(pc$eig[1:15])
rm(di)

# cluster --------------------------------------------------------------------------------
x <- parDist(as.matrix(pc$points), method = "euclidean")

# optimal n clusters --------------------------------------------------------------------- (5m)
res <- NbClust(pc$points, diss = x, distance = NULL, 
               min.nc = 2, max.nc = 20, 
               method = "ward.D", index = "silhouette")
res$Best.nc
# ---------------------------------------------------------------------------------------- (7m)
fviz_nbclust(pc$points, diss = x, hcut, method = "silhouette", k.max = 50)

hc <- hclust(x, "ward.D")

k <- res$Best.nc[1]
k <- 9
clusters <- cutree(hc, k = c(3, k))
clusters <- bind_cols(index = 1:nrow(clusters), as.data.frame(clusters))
names(clusters) <- c("ind", "c3", "ck")
clusters <- as.data.frame(clusters)

d <- patients[sample, c("client_id", "mins")] %>%
  bind_cols(clusters) %>%
  mutate(ck = fct_reorder(factor(ck), mins)) %>% 
  mutate(c3 = fct_reorder(factor(c3), mins)) %>% 
  mutate(c3 = as.numeric(factor(c3))) %>%
  group_by(c3) %>%
  mutate(ck = as.numeric(factor(ck)))
d$clusters <- factor(apply(d[c("c3", "ck")], 1, paste, collapse = "."))
clusters <- as.character(d$clusters[d$ind])
  
sil <- cluster::silhouette(as.numeric(factor(clusters)), x)
fviz_silhouette(sil)

# Figures ----------------------------------------------------------------------

table(clusters)

# Hclust
png(file = paste0("figures/hclust n =", nrow(ms), ".png"),
    width = 32.44, height = 13.18, units = "cm", res = 144,
    pointsize = 22)

op = par(mar = c(0, 0, 0, 0))
plot(hc, labels = FALSE, hang = -1, main = NA, 
     cex = 1.2, xlab = "cluster", sub = "", col = "#3e525b", 
     lwd = 2, axes = FALSE)
source("my.rect.hclust.R")
my.rect.hclust(hc, k = k, border = "#bf7e6f")
# my.rect.hclust(hc, k = k, border = c(1:9))# )c("#bf7e6f", "red"))
rect.hclust.labels(hc, k = k, border = "#bf7e6f", 
                   labels = c("2.4", "2.3", "2.2", "2.1", "3.1", "1.1", "1.2", "1.3", "1.4"))
par(op)

dev.off()

# profile % -----------------------------------------------------------
d <- patients[sample, ] %>%
  mutate(cluster = clusters) %>%
  select(mins, cluster, starts_with("mins_"))

mins <- d$mins
d <- d %>%
  mutate_at(.vars = vars(starts_with("mins_")), .funs = function(x) ((x / mins) * 100)) %>%
  group_by(cluster) %>%
  summarise_all(.funs = mean, trim = .3)

d <- gather(d, dim, score, -cluster, -mins)
d$dim <- d$dim %>% str_sub(6)
d$hours <- round(d$mins / 60, 0)

d <- subset(d,  score > 1)
d$cluster <- fct_reorder(factor(d$cluster), d$mins)

ggplot(d, 
       aes(y = score, x = dim)) + 
  geom_bar(stat = "identity", aes(fill = hours), color = "black") +
  scale_fill_gradient2(low = "#3db6ae", mid = "white", high = "#3e525b") +
  facet_wrap(~ cluster, nrow = 1) + 
  xlab("") + scale_y_log10("%") +
  #scale_y_log10("", breaks = c(50), labels = scales::percent_format(scale = 1)) +
  coord_flip() + theme_minimal(24)  + 
  theme(axis.text.x = element_blank()) +
  theme(strip.background = element_rect(colour="grey", fill=NA))

ggsave(paste0("figures/cluster profile n =", nrow(ms), ".png"),
       width = 32.44, height = 13.18, units = "cm")


# correlation with time ----------------------------
d <- patients[sample, ] %>%
  mutate(cluster = clusters) 

ggplot(d, aes(x = fct_reorder(factor(cluster), mins), y = (mins/60))) + 
  geom_violin(scale = "count", fill = "#2e8184", 
              draw_quantiles = c(.5), lwd = 1.1) + 
  scale_y_log10(breaks = c(10, 50, 250, 1200, 6000)) +
  xlab("cluster") +
  ylab("hours") + 
  theme_minimal(24)

ggsave(paste0("figures/time n =", nrow(ms), ".png"),
       width = 32.44, height = 13.18, units = "cm")


# correlation with costs ------------------------
d <- patients[sample, ] %>%
  mutate(cluster = clusters) 

ggplot(d, aes(x = fct_reorder(factor(cluster), mins), y = (costs / 1000))) + 
  geom_violin(scale = "count", fill = "#2e8184", 
              draw_quantiles = c(.5), lwd = 1.1) + 
  scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
  xlab("cluster") +
  ylab("claimed costs (K)") + 
  theme_minimal(24)

ggsave(paste0("figures/costs n =", nrow(ms), ".png"),
       width = 32.44, height = 13.18, units = "cm")



p <- patients[sample, ] %>%
  mutate(cluster = clusters)

patients_cluster <- p
save(file = "./data/patients_cluster.Rda", patients_cluster)
