library(XML)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(grid)

###### download tables
table1 <- readHTMLTable("http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3843406/table/T1/", 
                        stringsAsFactors = F)
table1 <- table1[[2]]
table2 <- readHTMLTable("http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3843406/table/T2/",
                        stringsAsFactors = F)
table2 <- table2[[2]]
table3 <- readHTMLTable("http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3843406/table/T3/",
                        stringsAsFactors = F)
table3 <- table3[[2]]

###### make basic data frame with groups and N from table 1
df <- data.frame(group = rep(c("0-10", "11-20", "21-40", "41-60"), 8),
                 sex = factor(rep(rep(c("M", "F"), each = 4), 4), levels = c("M", "F")),
                 refr.status = factor(rep(c("Emmetropia", "Myopia", "Hypermetropia", "Astigmatism"), each = 8),
                                      levels = c("Astigmatism", "Hypermetropia", "Myopia", "Emmetropia"))
)
n1 <- sapply(table1[, -1], function(x) substr(x, 1, 2), USE.NAMES = F) %>% as.vector
df1 <- mutate(df, N = n1)
df1$N <- as.numeric(df1$N)

###### make df2 for table 2 and df3 for table 3 with conf. int. for the mean
acd <- c(table2[c(1,3,5,7), -1], recursive = T) %>% as.numeric
acdsd <- c(table2[c(2,4,6,8), -1], recursive = T) %>% as.numeric
df2 <- mutate(df1, acd = acd, sd = acdsd, se = sd/sqrt(N),
              t = qt(.975, N - 1), lower = acd-t*se, upper = acd+t*se)

axl <- c(table3[c(1,3,5,7), -1], recursive = T) %>% as.numeric
axlsd <- c(table3[c(2,4,6,8), -1], recursive = T) %>% as.numeric
df3 <- mutate(df1, AL = axl, sd = axlsd, se = sd/sqrt(N),
              t = qt(.975, N - 1), lower = AL-t*se, upper = AL+t*se)

###### plot
my_theme <- theme(strip.text = element_text(face = "bold"),
                  strip.text.y = element_text(angle = 0),
                  strip.background = element_rect(fill = "snow3"),
                  axis.title = element_blank(),
                  #axis.text = element_text(color = "black"),
                  plot.title = element_text(face = "bold", vjust = 2, size = rel(1.5)),
                  panel.background = element_rect(fill = "snow2"),
                  panel.margin.y = unit(1, "cm"),
                  legend.position = "none"
)

ggplot(df2, aes(x = refr.status, y = acd, ymin = lower, ymax = upper, colour = refr.status)) +
        geom_hline(yintercept = 3, linetype = "dashed", color = "grey") +
        geom_pointrange(size = 0.7) +
        geom_errorbar(size = 1, width = 0.2) +
        scale_y_continuous(breaks= seq(2, 3.8, 0.2), limits = c(2, 3.8)) +
        scale_color_brewer(palette = "Set1") +
        facet_grid(group ~ sex) +
        ggtitle("Anterior chamber depth in mm by sex and age") +
        coord_flip() +
        my_theme 

ggplot(df3, aes(x = refr.status, y = AL, ymin = lower, ymax = upper, color = refr.status)) +
        geom_pointrange() +
        geom_errorbar(size = 1, width = 0.2) +
        scale_y_continuous(breaks= seq(21, 26)) +
        scale_color_brewer(palette = "Set1") +
        facet_grid(group ~ sex) +
        ggtitle("Axial length in mm by sex and age") +
        coord_flip() +
        my_theme

## Simulate data with mean and sd given for acd
## rnorm2 function comes from here
## http://stackoverflow.com/questions/18919091/r-generate-random-numbers-with-fixed-mean-and-sd
rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) } 
acdsim <- list()
set.seed(5555)
for (i in 1:nrow(df2)){
        acdsim[[i]] <- rnorm2(n = df2$N[i], mean = df2$acd[i], sd = df2$sd[i]) %>% as.vector
}

dfsim2 <- data.frame()
for (i in 1:nrow(df2)) {
        for (j in 1:length(acdsim[[i]])) {
                dfsim2 <- rbind(dfsim2, data.frame(group = df2$group[i],
                                                   sex = df2$sex[i],
                                                   refr.status = df2$refr.status[i],
                                                   acd = acdsim[[i]][j]))
        }
}

## Plot simulated data
ggplot(dfsim2, aes(x = refr.status, y = acd, fill = refr.status)) + 
        geom_hline(yintercept = 3, linetype = "dashed", color = "grey") +
        geom_boxplot(varwidth = T) +
        scale_y_continuous(breaks= seq(1.6, 4, 0.4), limits = c(1.6, 4)) +
        scale_fill_brewer(palette = "Set1") +
        facet_grid(group ~ sex) +
        ggtitle("Anterior chamber depth distributions values in mm") +
        coord_flip() +
        my_theme
