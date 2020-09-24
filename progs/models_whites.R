###################################################
# White and Black Comparison: BNI and Longevity
##################################################

# Analysis to explore the relationship between BNI and longevity. 
# Is BNI of root (standardized) name a predictor of life expectancy at birth? 

# Sibling fixed-effects, birth cohort fixed effects, birth order fixed effects. Controls for nicknames.
# Nicknames indicators from MPC Nickname file (include common mispellings)

# init --------------------------------------------------------------------

library(data.table) ## for working with big data
library(lfe) ## for fixed effects (with lots of FEs)
library(stargazer) ## for regression output tables
library(tidyverse) ## data manipulation and data viz 

# Data --------------------------------------------------------------

dt = fread("~/Downloads/bunmd_v1/bunmd_sib_data.csv")

## alternatively read in data on FC server
dt <- fread("/censoc/data/working_files/bunmd_sib_data.csv")


# add nicknames -----------------------------------------------------------

## Josh's nickname indicator (not used in this analysis)
dt[, nick := (fname != "LESLIE" & (
  grepl("IE$", fname) |
    fname %in% c("MOSE", "DAN", "JOHNNY", "ABE", "JAKE", "JIM", "JIMMY",
                 "TOM", "ED", "EDD", "CHARLEY", "JEFF", "BEN"))
)]

## MPC Nickname Indicator

## read in mpc nickname file (cleaned)
nicknames_mpc <- fread("../data/mpc_nicknames.csv")

## join onto dt 
dt <- merge(dt, nicknames_mpc, all.x = T, by = c("sex", "fname"))
dt[is.na(nickname_mpc), nickname_mpc := 0]
dt[is.na(fname_std), fname_std := fname ]


## note: key == "" is in here 35886
dt = dt[byear <= 1920]

dt[sex == 1, nkey_male := .N, by = key]
dt[, table(nkey_male)]

# add root bni -----------------------------------------------------------------

bni_root <- read_csv("../data/bni_root.csv")

dt <- left_join(dt, bni_root, by = "fname_std")

# Models ------------------------------------------------------------------

min_freq = 500

dt[sex == 1, nkey_male := .N, by = key]

## fixed effect model: standardized vs. unstandardized

white.m.fe = felm(death_age ~ bni_root | key + as.factor(byear),
            subset =
              nkey_male %in% 2:5 &
              race == 1 &
              sex == 1 &
              n_fname_root >= min_freq,
            data = dt)

white.m.fe.nick = update(white.m.fe,  death_age ~ bni_root + nickname_mpc | key + as.factor(byear))

## fixed effect model (cohorts 1905-1915)
white.m.fe.lim = update(m.fe,
                  subset =
                    nkey_male %in% 2:5 &
                    race == 1 &
                    sex == 1 &
                    byear %in% 1905:1915 &
                    n_fname_root >= min_freq,
                  data = dt)

white.m.fe.lim.nick = update(m.fe.nick,
                       subset =
                         nkey_male %in% 2:5 &
                         race == 1 &
                         sex == 1 &
                         byear %in% 1905:1915 &
                         n_fname_root >= min_freq,
                       data = dt)

## print output
out = stargazer(white.m.fe, white.m.fe.nick, white.m.fe.lim, white.m.fe.lim.nick,
                object.names = TRUE,
                type = "text")


# blacks ------------------------------------------------------------------

## fixed effect model: standardized vs. unstandardized

black.m.fe = felm(death_age ~ bni_root | as.factor(birth_order) + key + as.factor(byear),
                  subset =
                    nkey_male %in% 2:5 &
                    race == 2 &
                    sex == 1 &
                    n_fname_root >= min_freq,
                  data = dt)

black.m.fe.nick = update(black.m.fe,  death_age ~ bni_root + nickname_mpc | as.factor(birth_order) + key + as.factor(byear))

## fixed effect model (cohorts 1905-1915)
black.m.fe.lim = update(black.m.fe,
                        subset =
                          nkey_male %in% 2:5 &
                          race == 2 &
                          sex == 1 &
                          byear %in% 1905:1915 &
                          n_fname_root >= min_freq,
                        data = dt)

black.m.fe.lim.nick = update(black.m.fe.nick,
                             subset =
                               nkey_male %in% 2:5 &
                               race == 2 &
                               sex == 1 &
                               byear %in% 1905:1915 &
                               n_fname_root >= min_freq,
                             data = dt)

## print output
out = stargazer(black.m.fe, black.m.fe.nick, black.m.fe.lim, black.m.fe.lim.nick,
                object.names = TRUE,
                type = "text")


# comparison of blacks and whites -----------------------------------------

out = stargazer(black.m.fe, white.m.fe, black.m.fe.nick,
                object.names = TRUE,
                type = "latex")


# Geography ---------------------------------------------------------------

# Models Controlling for ZIP Beneficiaries --------------------------------

dt[!is.na(zip_ben), stan_zip_ben := (zip_ben - mean(zip_ben))/sd(zip_ben)]


# Models controlling for Geography (southern vs. non-southern) ----------------------------------------

## define southern states
south_socstate.vec = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 05, 22, 40, 48)*100

table(dt$socstate %in% south_socstate.vec)
dt[, south_socstate := socstate %in% south_socstate.vec]

## models for north

m.fe.north = felm(death_age ~ bni_root | key + as.factor(byear) + as.factor(birth_order),
                  subset =
                    south_socstate == FALSE &
                    nkey_male %in% 2:5 &
                    race == 1 &
                    sex == 1 &
                    n_fname_root >= min_freq,
                  data = dt)

m.fe.north.nick = update(m.fe.north,  death_age ~ bni_root + nickname_mpc | key + as.factor(byear) + as.factor(birth_order))

## models for north with ben zip

m.fe.zip.north = update(m.fe.north,  death_age ~ bni_root + stan_zip_ben | key + as.factor(byear) + as.factor(birth_order))

m.fe.zip.north.nick = update(m.fe.north,  death_age ~ bni_root + nickname_mpc + stan_zip_ben | key + as.factor(byear) + as.factor(birth_order))

## models (non-nicknames)
m.fe.north.nonick = update(m.fe.north,  death_age ~ bni_root | key + as.factor(byear) + as.factor(birth_order),
                           subset =
                             nickname_mpc == FALSE &
                             south_socstate == FALSE &
                             nkey_male %in% 2:5 &
                             race == 1 &
                             sex == 1 &
                             n_fname_root >= min_freq)


out = stargazer(m.fe.north, m.fe.north.nick, m.fe.north.nonick, m.fe.zip.north, ## m.fe.zip.north.nick  ,
                object.names = TRUE,
                type = "text",
                omit.stat=c("f", "ser"))

## ========================================================================
##                                  Dependent variable:                    
##              -----------------------------------------------------------
##                                       death_age                         
##                 (1)           (2)              (3)             (4)      
##              m.fe.north m.fe.north.nick m.fe.north.nonick m.fe.zip.north
## ------------------------------------------------------------------------
## bni_root       -0.154       -0.150           -0.152           -0.154    
##               (0.106)       (0.106)          (0.111)         (0.106)    
##                                                                         
## nickname_mpc               -0.211**                                     
##                             (0.084)                                     
##                                                                         
## stan_zip_ben                                                 0.319***   
##                                                              (0.023)    
##                                                                         
## ------------------------------------------------------------------------
## Observations  154,395       154,395          147,232         154,395    
## R2             0.626         0.626            0.641           0.627     
## Adjusted R2    0.199         0.199            0.199           0.201     
## ========================================================================
## Note:                                        *p<0.1; **p<0.05; ***p<0.01

## models for south

m.fe.south = felm(death_age ~ bni_root | key + as.factor(byear),
                  subset =
                    south_socstate == TRUE &
                    nkey_male %in% 2:5 &
                    race == 1 &
                    sex == 1 &
                    n_fname_root >= min_freq,
                  data = dt)

m.fe.south.nick = update(m.fe.south,  death_age ~ bni_root + nickname_mpc | key + as.factor(byear))

## models for south (beneficiary zip)

m.fe.zip.south = update(m.fe.south,  death_age ~ bni_root + stan_zip_ben | key + as.factor(byear))

m.fe.nickzip.south = update(m.fe.south,  death_age ~ bni_root + nickname_mpc + stan_zip_ben | key + as.factor(byear))


## models for south (no nicknames)
m.fe.nonick.south = update(m.fe.south,  death_age ~ bni_root | key + as.factor(byear),
                           subset =
                             nickname_mpc == FALSE &
                             south_socstate == TRUE &
                             nkey_male %in% 2:5 &
                             race == 1 &
                             sex == 1 &
                             n_fname_root >= min_freq)


out = stargazer(m.fe.south, m.fe.south.nick, m.fe.nonick.south, m.fe.zip.south, ## m.fe.nickzip.south,
                object.names = TRUE,
                type = "text",
                omit.stat=c("f", "ser"))
## ========================================================================
##                                  Dependent variable:                    
##              -----------------------------------------------------------
##                                       death_age                         
##                 (1)           (2)              (3)             (4)      
##              m.fe.south m.fe.south.nick m.fe.nonick.south m.fe.zip.south
## ------------------------------------------------------------------------
## bni_root      -0.406*       -0.402*          -0.330          -0.421*    
##               (0.240)       (0.240)          (0.260)         (0.239)    
##                                                                         
## nickname_mpc                -0.063                                      
##                             (0.141)                                     
##                                                                         
## stan_zip_ben                                                 0.210***   
##                                                              (0.050)    
##                                                                         
## ------------------------------------------------------------------------
## Observations   39,446       39,446           36,321           39,446    
## R2             0.674         0.674            0.699           0.675     
## Adjusted R2    0.189         0.189            0.189           0.190     
## ========================================================================
## Note:                                        *p<0.1; **p<0.05; ***p<0.01

## see if we can get more precise region effects if we pool (no sib FE)

