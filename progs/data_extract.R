## reduces big BUNMD download to fewer records

## use just cc_weight

library(data.table)
dt <- fread("~/Downloads/bunmd_v1/bunmd_v1.csv")

cc.dt = dt[!is.na(ccweight)]
## nrow(cc.dt)
## [1] 19221021
## nrow(dt)
## [1] 49337827

dt <- fwrite(cc.dt, "~/Downloads/bunmd_v1/cc_bunmd_new.csv")
