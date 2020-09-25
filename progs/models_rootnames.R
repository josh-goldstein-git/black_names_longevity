###################################################
# BNI and Longevity: Sibling Fixed Effects Analysis
# Root Names
##################################################

# Analysis to explore the relationship between BNI and longevity. 
# Sibling fixed-effects + controls for nicknames.
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

m.fe = felm(death_age ~ bni_root | key + as.factor(byear),
            subset =
              nkey_male %in% 2:5 &
              race == 2 &
              sex == 1 &
              n_fname_root >= min_freq,
            data = dt)

m.fe.unstandardized = felm(death_age ~ bni | key + as.factor(byear),
            subset =
              nkey_male %in% 2:5 &
              race == 2 &
              sex == 1 &
              n_fname >= min_freq,
            data = dt)


## print output
out = stargazer(m.fe, m.fe.unstandardized,
                object.names = TRUE,
                type = "text")


# =========================================================
#   Dependent variable:
#   -------------------------------------
#   death_age
# (1)                (2)
# m.fe        m.fe.unstandardized
# ---------------------------------------------------------
#   bni_root                -0.928**
#   (0.465)
# 
#   bni                                        -1.049***
#   (0.401)
# 
# ---------------------------------------------------------
# Observations              10,015             9,851
# R2                        0.672              0.678
# Adjusted R2               0.233              0.236
# Residual Std. Error 4.773 (df = 4281)  4.772 (df = 4152)
# =========================================================
#   Note:                         *p<0.1; **p<0.05; ***p<0.01



m.fe.nick = update(m.fe,  death_age ~ bni_root + nickname_mpc | key+ as.factor(byear))

m.fe.nick.old = update(m.fe,  death_age ~ bni_root + nick | key + as.factor(byear))

## fixed effect model (cohorts 1905-1915)
m.fe.lim = update(m.fe,
                  subset =
                    nkey_male %in% 2:5 &
                    race == 2 &
                    sex == 1 &
                    byear %in% 1905:1915 &
                    n_fname_root >= min_freq,
                  data = dt)

m.fe.lim.nick = update(m.fe.nick,
                       subset =
                         nkey_male %in% 2:5 &
                         race == 2 &
                         sex == 1 &
                         byear %in% 1905:1915 &
                         n_fname_root >= min_freq,
                       data = dt)

## pooled model
m.pooled = update(m.fe,  death_age ~ bni_root | as.factor(byear))

m.pooled.nick = update(m.fe,  death_age ~ bni_root + nickname_mpc | as.factor(byear))

## print output
out = stargazer(m.pooled, m.pooled.nick, m.fe, m.fe.nick, m.fe.lim, m.fe.lim.nick,
                object.names = TRUE,
                type = "text")

# =============================================================================================================================
#                                                                Dependent variable:                                           
#                     ---------------------------------------------------------------------------------------------------------
#                                                                     death_age                                                
#                            (1)               (2)               (3)               (4)              (5)              (6)       
#                         m.pooled        m.pooled.nick         m.fe            m.fe.nick         m.fe.lim      m.fe.lim.nick  
# -----------------------------------------------------------------------------------------------------------------------------
# bni_root                 -0.438            -0.419           -0.928**          -0.932**          -2.145*          -2.144*     
#                          (0.297)           (0.297)           (0.465)           (0.465)          (1.197)          (1.199)     
#                                                                                                                              
# nickname_mpc                               -0.279*                             -0.383                             0.006      
#                                            (0.156)                             (0.240)                           (0.670)     
#                                                                                                                              
# -----------------------------------------------------------------------------------------------------------------------------
# Observations             10,015            10,015            10,015            10,015            3,535            3,535      
# R2                        0.183             0.184             0.672             0.672            0.874            0.874      
# Adjusted R2               0.181             0.182             0.233             0.233            0.165            0.163      
# Residual Std. Error 4.931 (df = 9992) 4.931 (df = 9991) 4.773 (df = 4281) 4.773 (df = 4280) 4.622 (df = 535) 4.627 (df = 534)
# =============================================================================================================================
# Note:                                                                                             *p<0.1; **p<0.05; ***p<0.01

## Idea: we could look at Blacks who get SSN at age 20 and die over age 65 and see about name changes.

## guy's idea pooled with state x byear effects

m.guy <-  felm(death_age ~ bni_root | key + as.factor(byear)*as.factor(socstate),
               subset =
                 nkey_male %in% 1:5 &
                 race == 2 &
                 sex == 1 &
                 n_fname_root >= min_freq,
               data = dt)

m.guy.nick <-  felm(death_age ~ bni_root + nickname_mpc| key + as.factor(byear)*as.factor(socstate),
                    subset =
                      nkey_male %in% 1:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname_root >= min_freq,
                    data = dt)

out = stargazer(m.pooled, m.guy, m.guy.nick, m.fe, m.fe.nick,
                object.names = TRUE,
                type = "text")

## =============================================================================================================
##                                                        Dependent variable:                                   
##                     -----------------------------------------------------------------------------------------
##                                                             death_age                                        
##                            (1)               (2)               (3)               (4)               (5)       
##                         m.pooled            m.guy          m.guy.nick           m.fe            m.fe.nick    
## -------------------------------------------------------------------------------------------------------------
## bni_root                 -0.438            -0.945*           -0.947*          -0.928**          -0.932**     
##                          (0.297)           (0.572)           (0.572)           (0.465)           (0.465)     
##                                                                                                              
## nickname_mpc                                                 -0.295                              -0.383      
##                                                              (0.300)                             (0.240)     
##                                                                                                              
## -------------------------------------------------------------------------------------------------------------
## Observations             10,015            179,263           179,263           10,015            10,015      
## R2                        0.183             0.986             0.986             0.672             0.672      
## Adjusted R2               0.181             0.117             0.117             0.233             0.233      
## Residual Std. Error 4.931 (df = 9992) 5.165 (df = 2807) 5.165 (df = 2806) 4.773 (df = 4281) 4.773 (df = 4280)
## =============================================================================================================
## Note:                                                                             *p<0.1; **p<0.05; ***p<0.01

## doesn't seem to be the the story



# Models Controlling for ZIP Beneficiaries --------------------------------

dt[!is.na(zip_ben), stan_zip_ben := (zip_ben - mean(zip_ben))/sd(zip_ben)]

m.fe.zip = update(m.fe,  death_age ~ bni_root + stan_zip_ben  | key + as.factor(byear))
m.fe.zip.state = update(m.fe,  death_age ~ bni_root + stan_zip_ben  | key + as.factor(byear) + as.factor(socstate))

m.fe.zip.star = update(m.fe,  death_age ~ bni_root * stan_zip_ben | key + as.factor(byear))

out = stargazer(m.fe, m.fe.zip, m.fe.zip.star, m.fe.zip.state,
                object.names = TRUE,
                type = "text")


## =============================================================================================
##                                                 Dependent variable:                          
##                       -----------------------------------------------------------------------
##                                                      death_age                               
##                              (1)               (2)               (3)               (4)       
##                             m.fe            m.fe.zip        m.fe.zip.star    m.fe.zip.state  
## ---------------------------------------------------------------------------------------------
## bni_root                  -0.928**          -0.935**           -0.688           -1.105**     
##                            (0.465)           (0.463)           (0.545)           (0.502)     
##                                                                                              
## stan_zip_ben                                0.428***            0.211           0.473***     
##                                              (0.086)           (0.265)           (0.093)     
##                                                                                              
## nickname_mpc                                 -0.365            -0.367            -0.327      
##                                              (0.240)           (0.240)           (0.263)     
##                                                                                              
## bni_root:stan_zip_ben                                           0.406                        
##                                                                (0.471)                       
##                                                                                              
## ---------------------------------------------------------------------------------------------
## Observations               10,015            10,015            10,015             8,841      
## R2                          0.672             0.674             0.674             0.686      
## Adjusted R2                 0.233             0.238             0.238             0.232      
## Residual Std. Error   4.773 (df = 4281) 4.759 (df = 4279) 4.759 (df = 4278) 4.761 (df = 3611)
## =============================================================================================
## Note:                                                             *p<0.1; **p<0.05; ***p<0.01


# Models controlling for Geography (southern vs. non-southern) ----------------------------------------

## define southern states
south_socstate.vec = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 05, 22, 40, 48)*100

table(dt$socstate %in% south_socstate.vec)
dt[, south_socstate := socstate %in% south_socstate.vec]

## models for north

m.fe.north = felm(death_age ~ bni_root | key + as.factor(birth_order) + as.factor(byear),
                  subset =
                    south_socstate == FALSE &
                    nkey_male %in% 2:5 &
                    race == 2 &
                    sex == 1 &
                    n_fname_root >= min_freq,
                  data = dt)

m.fe.north.nick = update(m.fe.north,  death_age ~ bni_root + nickname_mpc | key + as.factor(byear))

## models for north with ben zip

m.fe.zip.north = update(m.fe.north,  death_age ~ bni_root + stan_zip_ben | key + as.factor(birth_order) + as.factor(byear))

m.fe.zip.north.nick = update(m.fe.north,  death_age ~ bni_root + nickname_mpc + stan_zip_ben | key + as.factor(birth_order) + as.factor(byear))

## models (non-nicknames)
m.fe.north.nonick = update(m.fe.north,  death_age ~ bni_root | key + as.factor(birth_order) + as.factor(byear),
                           subset =
                             nickname_mpc == FALSE &
                             south_socstate == FALSE &
                             nkey_male %in% 2:5 &
                             race == 2 &
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
## bni_root     -2.125***     -2.140***        -2.161**         -2.099**   
##               (0.820)       (0.818)          (0.857)         (0.816)    
##                                                                         
## nickname_mpc               -1.338**                                     
##                             (0.535)                                     
##                                                                         
## stan_zip_ben                                                 0.541***   
##                                                              (0.143)    
##                                                                         
## ------------------------------------------------------------------------
## Observations   3,880         3,880            3,601           3,880     
## R2             0.719         0.720            0.736           0.722     
## Adjusted R2    0.235         0.238            0.244           0.242     
## ========================================================================
## Note:                                        *p<0.1; **p<0.05; ***p<0.01



## models for south

m.fe.south = felm(death_age ~ bni_root | key + as.factor(birth_order) + as.factor(byear),
                  subset =
                    south_socstate == TRUE &
                    nkey_male %in% 2:5 &
                    race == 2 &
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
                             race == 2 &
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
## bni_root       -0.446       -0.446           -0.530           -0.435    
##               (0.615)       (0.616)          (0.688)         (0.615)    
##                                                                         
## nickname_mpc                 0.027                                      
##                             (0.290)                                     
##                                                                         
## stan_zip_ben                                                 0.339***   
##                                                              (0.118)    
##                                                                         
## ------------------------------------------------------------------------
## Observations   6,135         6,135            5,286           6,135     
## R2             0.708         0.708            0.752           0.709     
## Adjusted R2    0.238         0.238            0.251           0.241     
## ========================================================================
## Note:                                        *p<0.1; **p<0.05; ***p<0.01

## see if we can get more precise region effects if we pool (no sib FE)



# Comparison North and South ----------------------------------------------


stargazer(m.fe.south, m.fe.north,
          object.names = TRUE,
          type = "latex")

N_b_south <- dt %>% 
  filter(
    south_socstate == TRUE &
      nkey_male %in% 2:5 &
      race == 2 &
      sex == 1 &
      n_fname_root >= min_freq)  %>%
  group_by(key) %>% 
  tally() %>% 
  tally()

## 3764

N_b_north <- dt %>% 
  filter(
    south_socstate == F &
      nkey_male %in% 2:5 &
      race == 2 &
      sex == 1 &
      n_fname_root >= min_freq)  %>%
  group_by(key) %>% 
  tally() %>% 
  tally()

## 2437


## ===================================================================
##                                       Dependent variable:          
##                             ---------------------------------------
##                                            death_age               
##                                     (1)                 (2)        
##                                  m.region          m.region.nick   
## -------------------------------------------------------------------
## south_socstate                   -0.211***           -0.205***     
##                                   (0.078)             (0.078)      
##                                                                    
## bni_root                         -0.526***           -0.523***     
##                                   (0.111)             (0.111)      
##                                                                    
## nickname_mpc                                          -0.060*      
##                                                       (0.032)      
##                                                                    
## south_socstateTRUE:bni_root       0.336**             0.334**      
##                                   (0.138)             (0.138)      
##                                                                    
## -------------------------------------------------------------------
## Observations                      204,683             204,683      
## R2                                 0.199               0.199       
## Adjusted R2                        0.199               0.199       
## Residual Std. Error         4.925 (df = 204654) 4.925 (df = 204653)
## ===================================================================
## Note:                                   *p<0.1; **p<0.05; ***p<0.01


## no effect : is this because sibs live in same zip_ben
z = felm(zip_ben ~ bni_root | key,
         subset =
           nkey_male %in% 2:5 &
           race == 2 &
           sex == 1 &
           n_fname_root >= min_freq,
         data = dt)

z.nick = felm(zip_ben ~ bni_root + nickname_mpc| key,
              subset =
                ##                   south_socstate == FALSE &
                nkey_male %in% 2:5 &
                race == 2 &
                sex == 1 &
                n_fname_root >= min_freq,
              data = dt)

stargazer(z, z.nick,
          object.names = TRUE,
          type = "text")

##=======================================================
##                            Dependent variable:        
##                    -----------------------------------
##                                  zip_ben              
##                           (1)               (2)       
##                            z              z.nick      
##-------------------------------------------------------
##bni_root                  0.001             0.001      
##                         (0.008)           (0.008)     
##                                                       
##nickname_mpc                               -0.005      
##                                           (0.004)     
##                                                       
##-------------------------------------------------------
##Observations             10,015            10,015      
##R2                        0.681             0.681      
##Adjusted R2               0.257             0.257      
##Residual Std. Error 0.087 (df = 4301) 0.087 (df = 4300)
##=======================================================
##Note:                       *p<0.1; **p<0.05; ***p<0.01


# Comparison with whites --------------------------------------------------

## but whites have no BNI

## redefine NA bni --> 0 (for whites)
dt[, my.bni := bni_root]
dt[is.na(my.bni), my.bni := 0]

## pick a random sample of 100k whites

bar = dt[, .(white_lines =.I[race == 1])]
set.seed(10)
s = 1:nrow(dt) %in% sample(bar$white_lines, 10^5, replace = F)

dt[s, table(race)]

dt[, wni := 1 - my.bni]


## pooled race models (no sib fixed effects + controlling for nicknames)
m.pl.race = felm(death_age ~ my.bni + as.factor(race) + nickname_mpc |  as.factor(byear),
                 subset =
                   nkey_male %in% 2:5 &
                   race %in% 1:2 &
                   sex == 1 &
                   n_fname_root >= min_freq,
                 data = dt)


## ok, so we have only whites with sibs
## note: sib FE makes not sense with race, because brothers typically same race

m.pl.race.inr = felm(death_age ~ south_socstate*(my.bni + as.factor(race)) + nickname_mpc  |  as.factor(byear),
                     subset =
                       nkey_male %in% 2:5 &
                       race %in% 1:2 &
                       sex == 1 &
                       n_fname_root >= min_freq,
                     data = dt)

m.pl.race.nrth = felm(death_age ~ my.bni + as.factor(race) + nickname_mpc |  as.factor(byear),
                      subset =
                        south_socstate == FALSE &
                        nkey_male %in% 2:5 &
                        race %in% 1:2 &
                        sex == 1 &
                        n_fname_root >= min_freq,
                      data = dt)

m.pl.race.sth = felm(death_age ~ my.bni + as.factor(race) + nickname_mpc |  as.factor(byear),
                     subset =
                       south_socstate == TRUE &
                       nkey_male %in% 2:5 &
                       race %in% 1:2 &
                       sex == 1 &
                       n_fname_root >= min_freq,
                     data = dt)

stargazer(m.pl.race,
          m.pl.race.inr,
          m.pl.race.nrth,
          m.pl.race.sth,
          object.names = TRUE,
          type = "text")

## =================================================================================================================
##                                                                  Dependent variable:                             
##                                     -----------------------------------------------------------------------------
##                                                                       death_age                                  
##                                             (1)                 (2)                 (3)                (4)       
##                                          m.pl.race         m.pl.race.inr      m.pl.race.nrth      m.pl.race.sth  
## -----------------------------------------------------------------------------------------------------------------
## south_socstate                                               -0.191**                                            
##                                                               (0.075)                                            
##                                                                                                                  
## my.bni                                   -0.467***           -0.341***           -0.340***          -0.522***    
##                                           (0.062)             (0.071)             (0.071)            (0.138)     
##                                                                                                                  
## as.factor(race)2                         -0.849***           -0.844***           -0.843***          -0.687***    
##                                           (0.050)             (0.078)             (0.078)            (0.068)     
##                                                                                                                  
## nickname_mpc                             -0.150***           -0.123***           -0.160***            -0.049     
##                                           (0.046)             (0.046)             (0.056)            (0.081)     
##                                                                                                                  
## south_socstateTRUE:my.bni                                     -0.178                                             
##                                                               (0.153)                                            
##                                                                                                                  
## south_socstateTRUE:as.factor(race)2                            0.163                                             
##                                                               (0.102)                                            
##                                                                                                                  
## -----------------------------------------------------------------------------------------------------------------
## Observations                              203,856             203,856             158,275             45,581     
## R2                                         0.164               0.165               0.163              0.165      
## Adjusted R2                                0.164               0.164               0.163              0.164      
## Residual Std. Error                 4.765 (df = 203827) 4.764 (df = 203824) 4.743 (df = 158248) 4.837 (df = 45553
## =================================================================================================================
## Note:                                                                                  *p<0.1; **p<0.05; ***p<0.0


m.white = lm(death_age ~ as.factor(byear) + nickname_mpc,
             subset =
               nkey_male %in% 2:5 &
               race %in% 1 &
               sex == 1 &
               n_fname_root >= min_freq,
             data = dt)
## so 1910 byear white lives 93.000 -7.4913 = [1] 85.5087

m.black.pooled.bni = felm(death_age ~ bni_root + nickname_mpc +  as.factor(byear),
                          subset =
                            nkey_male %in% 2:5 &
                            race %in% 2 &
                            sex == 1 &
                            n_fname_root >= min_freq,
                          data = dt)
## so a 1910 byear black (with 0 bni) lives
## 91.2886  -6.2576 + 0 = [1] 85.031, about .5 years less
## and with bni of 1 -- > 1.2 years less

## just racial disparities and geography

m.race.disparity = felm(death_age ~ as.factor(race)*south_socstate + nickname_mpc | as.factor(byear),
                        subset =
                          nkey_male %in% 2:5 &
                          race %in% 1:2 &
                          sex == 1 &
                          n_fname_root >= min_freq,
                        data = dt)


stargazer(m.race.disparity,
          object.names = TRUE,
          type = "text")


##  ===========================================================
##                                      Dependent variable:    
##                                  ---------------------------
##                                           death_age         
##                                       m.race.disparity      
##  -----------------------------------------------------------
##  as.factor(race)2                         -0.883***         
##                                            (0.077)          
##                                                             
##  south_socstate                           -0.294***         
##                                            (0.027)          
##                                                             
##  nickname_mpc                             -0.129***         
##                                            (0.046)          
##                                                             
##  as.factor(race)2:south_socstate            0.156           
##                                            (0.101)          
##                                                             
##  -----------------------------------------------------------
##  Observations                              203,856          
##  R2                                         0.164           
##  Adjusted R2                                0.164           
##  Residual Std. Error                 4.765 (df = 203826)    
##  ===========================================================
##  Note:                           *p<0.1; **p<0.05; ***p<0.01



## Conclusion: MPC nicknames con't change the analysis much compared to the crude nicknames. 

## Qn: why when nickname dummy is added, it creates a big change in the bni coefficient, but the nickname dummy itself is not significant?






