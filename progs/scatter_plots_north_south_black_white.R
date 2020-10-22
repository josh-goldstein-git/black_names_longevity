## Create scatter plots of bni vs longevity for north vs south and black vs white
## Include family fixed effects and such so that they come close to regression models


## (1) Create the bunmd_sib_data.csv (locally)
## only needs to be run once

library(data.table)
library(lfe)
library(tidyverse)
library(stargazer)
if(0) {
    source("data_extract.R")
    source("data_prep.R")
     rm(list = ls())
}

dt <- fread("~/Downloads/bunmd_v1/bunmd_sib_data.csv")

south_socstate.vec = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 05, 22, 40, 48)*100
table(dt$socstate %in% south_socstate.vec)
dt[, south_socstate := socstate %in% south_socstate.vec]

## MPC Nickname Indicator

## read in mpc nickname file (cleaned)
nicknames_mpc <- fread("../data/mpc_nicknames.csv")
## join onto dt
dt <- merge(dt, nicknames_mpc, all.x = T, by = c("sex", "fname"))
dt[is.na(nickname_mpc), nickname_mpc := 0]
dt[is.na(fname_std), fname_std := fname ]


## note: key == "" is in here 35886
dt = dt[byear %in% 1895:1920]

dt[sex == 1, nkey_male := .N, by = key]
dt[, table(nkey_male)]

# add root bni -----------------------------------------------------------------

bni_root <- read_csv("../data/bni_root.csv")

dt <- left_join(dt, bni_root, by = "fname_std")

##### let's select only the pertinent cases for Blacks
min_freq = 500
my.dt = dt[race == 2 & ## black
           nkey_male %in% 2:5 & ## with sibs
           byear %in% 1885:1920 & ## birth year
           sex == 1 & ## male
           n_fname_root >= min_freq]
## make sure we have no single keys
my.dt[, nkey := .N, by = key]
my.dt <- my.dt[nkey > 1]

m =  felm(death_age ~ bni_root | key + as.factor(byear),
          data = my.dt)
stargazer(m, type = 'text')


## now a scatter plot with FEs removed
## from https://stackoverflow.com/questions/30491545/predict-method-for-felm-from-lfe-package

library(fixest)
model_feols <- feols(data = my.dt,
                     death_age ~ bni_root | key + as.factor(byear))
hat = predict(model_feols, newdata = data.frame(bni_root = 0, byear = my.dt$byear, key = my.dt$key))

my.dt[, adj_death_age := death_age - hat]

out = my.dt[, .(mean_adj_death_age = mean(adj_death_age),
                bni_root = mean(bni_root), .N),
            by = fname_std]
## plot
out[, plot(bni_root, mean_adj_death_age, type = "n", ylim = c(-2, 2))]
m.out = out[, lm( mean_adj_death_age ~ bni_root, weights = N)]
abline(m.out)
abline(h = -1, col = "grey")
out[, text(bni_root, mean_adj_death_age, tools::toTitleCase(tolower(fname_std)),
           cex = (N/30)^(1/2))]
## replot names all same sizesame size
out[, plot(bni_root, mean_adj_death_age, type = "n", ylim = c(-2, 2))]
m.out = out[, lm( mean_adj_death_age ~ bni_root, weights = N)]
abline(m.out)
abline(h = -1, col = "grey")
out[, text(bni_root, mean_adj_death_age, tools::toTitleCase(tolower(fname_std)),
           cex = .8)]


get_name_plot <- function(dt, points_only = FALSE, samesize = FALSE, mycex = .8, ...)
{
    ## make sure all keys have at least 2 brothers
    dt[, nkey := .N, key]
    dt <- dt[nkey > 1]

    ## fit fe models using both packages
    model_felm =  felm(death_age ~ bni_root | key + as.factor(byear),
                       data = dt)
    model_feols <- feols(data = dt,
                         death_age ~ bni_root | key + as.factor(byear))

    ## now do a grouped (by fname) regression on fe-adjusted longevity
    hat = predict(model_feols, newdata = data.frame(bni_root = 0, byear = dt$byear, key = dt$key))
    dt[, adj_death_age := death_age - hat]

    out = dt[, .(mean_adj_death_age = mean(adj_death_age),
                 bni_root = mean(bni_root), .N),
             by = fname_std]
    model_grouped = lm(mean_adj_death_age ~ bni_root, weights = N, data = out)
   ## regression table
    sg_out = stargazer(model_felm,  model_grouped, type = "text") ## note model_feols doesn't have method for sg

    ## plot
    out[, plot(bni_root, mean_adj_death_age, type = "n", ...)]
    m.out = out[, lm( mean_adj_death_age ~ bni_root, weights = N)]
    abline(m.out)
    ##    abline(h = -1, col = "grey")
    grid()

    if (samesize & !points_only)
        {
        out[, text(bni_root, mean_adj_death_age,
                   tools::toTitleCase(tolower(fname_std)),
                   cex = mycex)]
        out[fname_std %in% c("JESSE", "MOSES", "DONALD", "STANLEY"),
            text(bni_root, mean_adj_death_age,
                   tools::toTitleCase(tolower(fname_std)),
                 cex = mycex, col = "red")]
        }
    if (!samesize & !points_only)
    {
        out[, text(bni_root, mean_adj_death_age,
                   tools::toTitleCase(tolower(fname_std)),
                   cex = (N/20)^(1/2))]
        out[fname_std %in% c("JESSE", "MOSES", "DONALD", "STANLEY"),
            text(bni_root, mean_adj_death_age,
                   tools::toTitleCase(tolower(fname_std)),
                   cex = (N/20)^(1/2), col = "red")]
    }
    if (points_only)
    {
        out[, points(bni_root, mean_adj_death_age,
                     cex = (N/20)^(1/2))]
        out[fname_std %in% c("JESSE", "MOSES", "DONALD", "STANLEY"),
            points(bni_root, mean_adj_death_age,
                   cex = (N/20)^(1/2), col = "red", pch = 16)]
        }
    return(sg_out)
}

par(mfrow = c(2,2))
get_name_plot(my.dt, samesize = FALSE)
get_name_plot(my.dt, samesize = TRUE)
get_name_plot(my.dt, points_only = TRUE)



## now do north and south
my.dt.south = my.dt[south_socstate == TRUE]
my.dt.north = my.dt[south_socstate == FALSE]
pdf("north_south_scatter.pdf", width = 10, height = 12)
par(mfcol = c(3,2))
get_name_plot(my.dt.south, samesize = FALSE, ylim = c(-5, 5), xlim = c(0,1))
title("South, character size proportional to sqrt(N)")
get_name_plot(my.dt.south, samesize = TRUE, ylim = c(-5, 5), xlim = c(0,1))
title("South, constant character size")
get_name_plot(my.dt.south, points_only = TRUE, ylim = c(-5, 5), xlim = c(0,1))
title("South, circle size proportional to sqrt(N)")
get_name_plot(my.dt.north, samesize = FALSE, ylim = c(-5, 5), xlim = c(0,1))
title("North")
get_name_plot(my.dt.north, samesize = TRUE, ylim = c(-5, 5), xlim = c(0,1))
title("North")
get_name_plot(my.dt.north, points_only = TRUE, ylim = c(-5, 5), xlim = c(0,1))
title("North")
dev.off()
system("open north_south_scatter.pdf")

## ok, and now we want to compare to Whites!!!

