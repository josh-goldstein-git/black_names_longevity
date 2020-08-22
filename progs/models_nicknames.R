###################################################
# BNI and Longevity: Sibling Fixed Effects Analysis
##################################################

# Analysis to explore the relationship between BNI and longevity. 
# Sibling fixed-effects + controls for nicknames.
# Nicknames indicators from MPC Nickname file (include common mispellings)


# init --------------------------------------------------------------------

library(data.table) ## for working with big data
library(lfe) ## for fixed effects (with lots of FEs)
library(stargazer) ## for regression output tables

# Data --------------------------------------------------------------

## dt = fread("./bunmd_sib_data.dt")
## dt = fread("~/Downloads/bunmd_v1/bunmd_sib_data.csv")

## alternatively read in data on FC server
## dt <- fread("/censoc/data/working_files/bunmd_sib_data.csv")


# add nicknames -----------------------------------------------------------

## Josh's nickname indicator (not used for this analysis)
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
 
## note: key == "" is in here 35886
dt = dt[byear <= 1920]

dt[sex == 1, nkey_male := .N, by = key]
dt[, table(nkey_male)]


# Models ------------------------------------------------------------------

min_freq = 500

dt[sex == 1, nkey_male := .N, by = key]

## fixed effect model 
m.fe = felm(death_age ~ bni | key + as.factor(byear),
         subset =
             nkey_male %in% 2:5 &
             race == 2 &
             sex == 1 &
             n_fname >= min_freq,
         data = dt)

m.fe.nick = update(m.fe,  death_age ~ bni + nickname_mpc | as.factor(byear))

## fixed effect model (cohorts 1905-1915)
m.fe.lim = update(m.fe,
         subset =
             nkey_male %in% 2:5 &
             race == 2 &
             sex == 1 &
             byear %in% 1905:1915 &
         n_fname >= min_freq,
         data = dt)

m.fe.lim.nick = update(m.fe.nick,
                       subset =
                           nkey_male %in% 2:5 &
                           race == 2 &
                           sex == 1 &
                           byear %in% 1905:1915 &
                           n_fname >= min_freq,
                       data = dt)

## pooled model
m.pooled = update(m.fe,  death_age ~ bni | as.factor(byear))

m.pooled.nick = update(m.fe,  death_age ~ bni + nickname_mpc | as.factor(byear))

## print output
out = stargazer(m.pooled, m.pooled.nick, m.fe, m.fe.nick, m.fe.lim, m.fe.lim.nick,
                object.names = TRUE,
                type = "text")


## Idea: we could look at Blacks who get SSN at age 20 and die over age 65 and see about name changes.

## guy's idea pooled with state x byear effects

m.guy <-  felm(death_age ~ bni | key + as.factor(byear)*as.factor(socstate),
         subset =
             nkey_male %in% 1:5 &
             race == 2 &
             sex == 1 &
             n_fname >= min_freq,
         data = dt)

m.guy.nick <-  felm(death_age ~ bni + nickname_mpc| key + as.factor(byear)*as.factor(socstate),
               subset =
                   nkey_male %in% 1:5 &
                   race == 2 &
                   sex == 1 &
                   n_fname >= min_freq,
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
## bni                     -0.652**          -1.089**           -1.009*          -1.049***          -0.522*     
##                          (0.254)           (0.498)           (0.536)           (0.401)           (0.279)     
##                                                                                                              
## nickname_mpc                                                 -0.135                              -0.201      
##                                                              (0.340)                             (0.179)     
##                                                                                                              
## -------------------------------------------------------------------------------------------------------------
## Observations              9,851            176,652           176,652            9,851             9,851      
## R2                        0.185             0.986             0.986             0.678             0.185      
## Adjusted R2               0.183             0.115             0.114             0.236             0.183      
## Residual Std. Error 4.936 (df = 9828) 5.171 (df = 2702) 5.172 (df = 2701) 4.772 (df = 4152) 4.936 (df = 9827)
## =============================================================================================================
## Note:                                                                             *p<0.1; **p<0.05; ***p<0.01

## doesn't seem to be the the story



# Models Controlling for ZIP Beneficiaries --------------------------------

dt[!is.na(zip_ben), stan_zip_ben := (zip_ben - mean(zip_ben))/sd(zip_ben)]

m.fe.zip = update(m.fe,  death_age ~ bni + stan_zip_ben + nickname_mpc | key + as.factor(byear))
m.fe.zip.state = update(m.fe,  death_age ~ bni + stan_zip_ben + nickname_mpc | key + as.factor(byear) + as.factor(socstate))

m.fe.zip.star = update(m.fe,  death_age ~ bni * stan_zip_ben | key + as.factor(byear))

out = stargazer(m.fe, m.fe.zip, m.fe.zip.star, m.fe.zip.state,
          object.names = TRUE,
          type = "text")
##  ===========================================================================================
##                                                Dependent variable:                          
##                      -----------------------------------------------------------------------
##                                                     death_age                               
##                             (1)               (2)               (3)               (4)       
##                            m.fe            m.fe.zip        m.fe.zip.star    m.fe.zip.state  
##  -------------------------------------------------------------------------------------------
##  bni                     -1.049***         -0.925**          -1.022**          -1.045**     
##                           (0.401)           (0.433)           (0.480)           (0.469)     
##                                                                                             
##  stan_zip_ben                              0.399***           0.394*           0.433***     
##                                             (0.087)           (0.238)           (0.095)     
##                                                                                             
##  nickname_mpc                               -0.172                              -0.122      
##                                             (0.273)                             (0.299)     
##                                                                                             
##  bni:stan_zip_ben                                              0.011                        
##                                                               (0.413)                       
##                                                                                             
##  -------------------------------------------------------------------------------------------
##  Observations              9,851             9,851             9,851             8,702      
##  R2                        0.678             0.680             0.680             0.692      
##  Adjusted R2               0.236             0.240             0.240             0.234      
##  Residual Std. Error 4.772 (df = 4152) 4.761 (df = 4150) 4.761 (df = 4150) 4.766 (df = 3501)
##  ===========================================================================================
##  Note:                                                           *p<0.1; **p<0.05; ***p<0.01


# Models controlling for Geography (southern vs. non-southern) ----------------------------------------

## define southern states
south_socstate.vec = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 05, 22, 40, 48)*100

table(dt$socstate %in% south_socstate.vec)
dt[, south_socstate := socstate %in% south_socstate.vec]

## models for north

m.fe.north = felm(death_age ~ bni | key + as.factor(byear),
                  subset =
                      south_socstate == FALSE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)

m.fe.north.nick = update(m.fe.north,  death_age ~ bni + nickname_mpc | as.factor(byear))

## models for north with ben zip

m.fe.zip.north = update(m.fe.north,  death_age ~ bni + stan_zip_ben | key + as.factor(byear))

m.fe.zip.north.nick = update(m.fe.north,  death_age ~ bni + nickname_mpc + stan_zip_ben | key + as.factor(byear))

## models (non-nicknames)
m.fe.north.nonick = update(m.fe.north,  death_age ~ bni | key + as.factor(byear),
                           subset =
                               nickname_mpc == FALSE &
                      south_socstate == FALSE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq)


out = stargazer(m.fe.north, m.fe.north.nick, m.fe.north.nonick, m.fe.zip.north, ## m.fe.zip.north.nick  ,
          object.names = TRUE,
          type = "text")


## =========================================================================================== 
##                                               Dependent variable:                          
##                     -----------------------------------------------------------------------
##                                                    death_age                               
##                            (1)               (2)               (3)               (4)       
##                        m.fe.north      m.fe.north.nick  m.fe.nonick.north  m.fe.zip.north  
## -------------------------------------------------------------------------------------------
## bni                     -1.631**           -0.201           -1.792**          -1.579**     
##                          (0.754)           (0.457)           (0.841)           (0.750)     
##                                                                                            
## nickname_mpc                              -1.058***                                        
##                                            (0.346)                                         
##                                                                                            
## stan_zip_ben                                                                  0.546***     
##                                                                                (0.145)     
##                                                                                            
## -------------------------------------------------------------------------------------------
## Observations              3,821             3,821             3,575             3,821      
## R2                        0.722             0.199             0.736             0.725      
## Adjusted R2               0.235             0.194             0.242             0.242      
## Residual Std. Error 4.831 (df = 1389) 4.958 (df = 3797) 4.822 (df = 1244) 4.808 (df = 1388)
## ===========================================================================================
## Note:                                                           *p<0.1; **p<0.05; ***p<0.01


## models for south

m.fe.south = felm(death_age ~ bni | key + as.factor(byear),
                  subset =
                      south_socstate == TRUE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)

m.fe.south.nick = update(m.fe.south,  death_age ~ bni + nickname_mpc | as.factor(byear))

## models for south (beneficiary zip)

m.fe.zip.south = update(m.fe.south,  death_age ~ bni + stan_zip_ben | key + as.factor(byear))

m.fe.nickzip.south = update(m.fe.south,  death_age ~ bni + nickname_mpc + stan_zip_ben | key + as.factor(byear))


## models for south (no nicknames)
m.fe.nonick.south = update(m.fe.south,  death_age ~ bni | key + as.factor(byear),
                           subset =
                               nickname_mpc == FALSE &
                      south_socstate == TRUE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq)


out = stargazer(m.fe.south, m.fe.south.nick, m.fe.nonick.south, m.fe.zip.south, ## m.fe.nickzip.south,
          object.names = TRUE,
          type = "text")

##  ===========================================================================================
##                                                Dependent variable:                          
##                      -----------------------------------------------------------------------
##                                                     death_age                               
##                             (1)               (2)               (3)               (4)       
##                         m.fe.south      m.fe.south.nick  m.fe.nonick.south  m.fe.zip.south  
##  -------------------------------------------------------------------------------------------
##  bni                      -0.868*          -0.712**           -0.674            -0.853*     
##                           (0.515)           (0.355)           (0.661)           (0.515)     
##                                                                                             
##  nickname_mpc                                0.142                                          
##                                             (0.212)                                         
##                                                                                             
##  stan_zip_ben                                                                   0.296**     
##                                                                                 (0.121)     
##                                                                                             
##  -------------------------------------------------------------------------------------------
##  Observations              6,030             6,030             5,254             6,030      
##  R2                        0.714             0.179             0.752             0.715      
##  Adjusted R2               0.242             0.176             0.252             0.243      
##  Residual Std. Error 4.718 (df = 2273) 4.918 (df = 6009) 4.682 (df = 1738) 4.713 (df = 2272)
##  ===========================================================================================
##  Note:                                                           *p<0.1; **p<0.05; ***p<0.01


## see if we can get more precise region effects if we pool (no sib FE)

m.region = felm(death_age ~ south_socstate*bni | as.factor(byear),
                  subset =
##                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                data = dt)

m.region.nick = felm(death_age ~ south_socstate*bni + nickname_mpc | as.factor(byear),
                  subset =
##                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                data = dt)

stargazer(m.region, m.region.nick,
          object.names = TRUE,
          type = "text")


## ==============================================================
##                                  Dependent variable:          
##                        ---------------------------------------
##                                       death_age               
##                                (1)                 (2)        
##                             m.region          m.region.nick   
## --------------------------------------------------------------
## south_socstate              -0.193***           -0.194***     
##                              (0.069)             (0.070)      
##                                                               
## bni                         -0.471***           -0.467***     
##                              (0.098)             (0.099)      
##                                                               
## nickname_mpc                                     -0.008       
##                                                  (0.037)      
##                                                               
## south_socstateTRUE:bni      0.325***            0.327***      
##                              (0.119)             (0.120)      
##                                                               
## --------------------------------------------------------------
## Observations                 201,672             201,672      
## R2                            0.199               0.199       
## Adjusted R2                   0.199               0.199       
## Residual Std. Error    4.926 (df = 201643) 4.926 (df = 201642)
## ==============================================================
## Note:                              *p<0.1; **p<0.05; ***p<0.01


## no effect : is this because sibs live in same zip_ben
z = felm(zip_ben ~ bni | key,
         subset =
             nkey_male %in% 2:5 &
             race == 2 &
             sex == 1 &
             n_fname >= min_freq,
         data = dt)

z.nick = felm(zip_ben ~ bni + nickname_mpc| key,
               subset =
##                   south_socstate == FALSE &
                   nkey_male %in% 2:5 &
                   race == 2 &
                   sex == 1 &
                   n_fname >= min_freq,
               data = dt)

stargazer(z, z.nick,
          object.names = TRUE,
          type = "text")

## =======================================================
##                             Dependent variable:        
##                     -----------------------------------
##                                   zip_ben              
##                            (1)               (2)       
##                             z              z.nick      
## -------------------------------------------------------
## bni                      -0.004            -0.0003     
##                          (0.007)           (0.008)     
##                                                        
## nickname_mpc                               -0.006      
##                                            (0.005)     
##                                                        
## -------------------------------------------------------
## Observations              9,851             9,851      
## R2                        0.685             0.685      
## Adjusted R2               0.257             0.257      
## Residual Std. Error 0.086 (df = 4172) 0.086 (df = 4171)
## =======================================================
## Note:                       *p<0.1; **p<0.05; ***p<0.01


# Comparison with whites --------------------------------------------------

## but whites have no BNI

## redefine NA bni --> 0 (for whites)
dt[, my.bni := bni]
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
                      n_fname >= min_freq,
                  data = dt)


## ok, so we have only whites with sibs
## note: sib FE makes not sense with race, because brothers typically same race

m.pl.race.inr = felm(death_age ~ south_socstate*(my.bni + as.factor(race)) + nickname_mpc  |  as.factor(byear),
                  subset =
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)

m.pl.race.nrth = felm(death_age ~ my.bni + as.factor(race) + nickname_mpc |  as.factor(byear),
                           subset =
                               south_socstate == FALSE &
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)

m.pl.race.sth = felm(death_age ~ my.bni + as.factor(race) + nickname_mpc |  as.factor(byear),
                           subset =
                               south_socstate == TRUE &
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)

stargazer(m.pl.race,
          m.pl.race.inr,
          m.pl.race.nrth,
          m.pl.race.sth,
          object.names = TRUE,
          type = "text")

## ================================================================================================================== 
##                                                                  Dependent variable:                               
##                                     ------------------------------------------------------------------------------ 
##                                                                       death_age                                    
##                                             (1)                 (2)                 (3)                (4)         
##                                          m.pl.race         m.pl.race.inr      m.pl.race.nrth      m.pl.race.sth    
## ------------------------------------------------------------------------------------------------------------------ 
## south_socstate                                               -0.296***                                             
##                                                               (0.027)                                              
##                                                                                                                    
## my.bni                                   -0.530**             -0.535              -0.509             -0.575*       
##                                           (0.247)             (0.415)             (0.414)            (0.319)       
##                                                                                                                    
## as.factor(race)2                         -0.619***           -0.613***           -0.624***           -0.409**      
##                                           (0.145)             (0.229)             (0.228)            (0.194)       
##                                                                                                                    
## nickname_mpc                             -0.154***           -0.123**            -0.175***            -0.021       
##                                           (0.049)             (0.049)             (0.060)            (0.086)       
##                                                                                                                    
## south_socstateTRUE:my.bni                                      0.037                                               
##                                                               (0.517)                                              
##                                                                                                                   
## south_socstateTRUE:as.factor(race)2                            0.168                                               
##                                                               (0.296)                                              
##                                                                                                                    
## ------------------------------------------------------------------------------------------------------------------ 
## Observations                              202,166             202,166             157,047             45,119       
## R2                                         0.164               0.164               0.163              0.165        
## Adjusted R2                                0.163               0.164               0.162              0.164        
## Residual Std. Error                 4.766 (df = 202137) 4.765 (df = 202134) 4.743 (df = 157020) 4.839 (df = 45091) 
## ================================================================================================================== 
## Note:                                                                                  *p<0.1; **p<0.05; ***p<0.01 



m.white = lm(death_age ~ as.factor(byear) + nickname_mpc,
                  subset =
                      nkey_male %in% 2:5 &
                      race %in% 1 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)
## so 1910 byear white lives 93.000 -7.4913 = [1] 85.5087

m.black.pooled.bni = felm(death_age ~ bni + nickname_mpc +  as.factor(byear),
                           subset =
                               nkey_male %in% 2:5 &
                               race %in% 2 &
                               sex == 1 &
                               n_fname >= min_freq,
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
                               n_fname >= min_freq,
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
##  as.factor(race)2                         -0.889***         
##                                            (0.078)          
##                                                             
##  south_socstate                           -0.295***         
##                                            (0.027)          
##                                                             
##  nickname_mpc                             -0.136***         
##                                            (0.049)          
##                                                             
##  as.factor(race)2:south_socstate            0.159           
##                                            (0.102)          
##                                                             
##  -----------------------------------------------------------
##  Observations                              202,166          
##  R2                                         0.164           
##  Adjusted R2                                0.164           
##  Residual Std. Error                 4.765 (df = 202136)    
##  ===========================================================
##  Note:                           *p<0.1; **p<0.05; ***p<0.01





















