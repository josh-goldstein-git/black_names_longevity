## let's do the models for sib analysis
library(data.table) ## for working with big data
library(lfe) ## for fixed effects (with lots of FEs)
library(stargazer) ## for regression output tables


## dt = fread("./bunmd_sib_data.dt")
dt = fread("~/Downloads/bunmd_v1/bunmd_sib_data.csv")

## note: key == "" is in here 35886

dt = dt[byear <= 1920]

dt[sex == 1, nkey_male := .N, by = key]
dt[, table(nkey_male)]

## normalized age at death (for plotting)
dt[, norm_death_age := death_age - mean(death_age), by = byear]

## let's restrict to names that are mostly male
my.dt = dt
my.dt[, sex_score := mean(sex), by = fname]
tmp = my.dt[sex_score < 1.2 &
            n_fname >= 200 &
         nkey_male %in% 2:5 &
         race == 2 &
         sex == 1,
         .(bni = mean(bni), norm_death_age = mean(norm_death_age), .N),
         by = fname]
tmp[, plot(bni, norm_death_age, cex = sqrt(N)/10, type = 'n',
           ylim = c(-3, 2),
           xlim = c(0, 1.1))]
tmp[, abline(lm(norm_death_age ~ bni, weight = sqrt(N)))]
norm_factor = 4
tmp[, text(bni, norm_death_age, fname, cex = sqrt(N)/norm_factor,
           col = "orange")]
tmp[fname %in% c("MOSES", "AARON", "ELIJAH", "SAMUEL",
                 "ISAAC", "JOSEPH",
                 "ISAIAH", "ISIAH",
                 "ELI",
                 "ABRAHAM", "DAVID",
                 "NOAH", "SOLOMON",
                 "RUBEN", "RUBIN",
                 "JEREMIAH",
                 "EZEKIEL", "JOSHUA",
                 "AMOS", "SILAS"),
    text(bni, norm_death_age, fname, cex = sqrt(N)/norm_factor,
           col = "blue")]
tmp[grepl("Y$", fname) |
    grepl("IE$", fname) |
    fname %in% c("MOSE", "DAN", "JOHNNY", "ABE", "JAKE", "JIM", "JIMMIE",
                 "TOM", "ED", "EDD"),
    text(bni, norm_death_age, fname, cex = sqrt(N)/norm_factor,
           col = "red")]
tmp[grepl("EL$", fname),
    text(bni, norm_death_age, fname, cex = sqrt(N)/norm_factor,
           col = "blue")]
tmp[grepl("US$", fname),
    text(bni, norm_death_age, fname, cex = sqrt(N)/norm_factor,
           col = "green")]
tmp[fname %in% "FREEMAN",
    text(bni, norm_death_age, fname, cex = sqrt(N)/norm_factor,
           col = "black")]
## could be systemized a bit more


####### name pyramid
par(mfrow = c(1,1))
tmp[, jitter.N := jitter(N, factor = 3)]
tmp[N > 4, plot(bni, jitter.N, log = 'y', type = 'n')]
tmp[N > 4, text(bni, jitter.N, fname, cex = .6)]


########## models


min_freq = 500
dt[sex == 1, nkey_male := .N, by = key]
m.fe = felm(death_age ~ bni | key + as.factor(byear),
         subset =
             nkey_male %in% 2:5 &
             race == 2 &
             sex == 1 &
             n_fname >= min_freq,
         data = dt)

m.pooled = update(m.fe,  death_age ~ bni | as.factor(byear))

out = stargazer(m.pooled, m.fe,
          object.names = TRUE,
          type = "text")

tt = dt[nkey_male %in% 2:5 &
        race == 2 &
        sex == 1 &
        n_fname >= min_freq,
        table(fname)]
length(tt)


## limit year to 93+ because of zipcode stuff
## dt <- dt[dyear >= 1993]
## dt[sex == 1, name_freq := .N, by = fname]


## let's control for nicknames

dt[, nick := (fname != "LESLIE" & (
    grepl("IE$", fname) |
    fname %in% c("MOSE", "DAN", "JOHNNY", "ABE", "JAKE", "JIM", "JIMMY",
                 "TOM", "ED", "EDD", "CHARLEY", "JEFF", "BEN")
                                   )
)]

tt = dt[race == 2 & n_fname >= min_freq & nick == TRUE & sex == 1, table(fname)]
sort(tt)

dt[race == 2 & n_fname >= min_freq & (nick == TRUE | fname == "JESSE" | grepl("^JOHN", fname)) & sex == 1,
   .(.N, mean(norm_death_age)), by = fname][order(N)]

m.fe.nick = update(m.fe,  death_age ~ bni + nick | as.factor(byear))

## Idea: we could look at Blacks who get SSN at age 20 and die over age 65 and see about name changes.



out = stargazer(m.pooled, m.fe, m.fe.nick,
          object.names = TRUE,
          type = "text")
##                            (1)               (2)               (3)
##                         m.pooled            m.fe            m.fe.nick
## -------------------------------------------------------------------------
## bni                     -0.740***         -0.997**           -0.547*
##                          (0.265)           (0.425)           (0.325)

## nick                                                         -0.223
##                                                              (0.218)

## -------------------------------------------------------------------------
## Observations              9,306             9,306             9,306

## doesn't seem to be the the story


## now let's control for zip_ben
dt[!is.na(zip_ben), stan_zip_ben := (zip_ben - mean(zip_ben))/sd(zip_ben)]

m.fe.zip = update(m.fe,  death_age ~ bni + stan_zip_ben | key + as.factor(byear))
m.fe.zip.state = update(m.fe,  death_age ~ bni + stan_zip_ben | key + as.factor(byear) + as.factor(socstate))

m.fe.zip.star = update(m.fe,  death_age ~ bni * stan_zip_ben | key + as.factor(byear))

out = stargazer(m.fe, m.fe.zip, m.fe.zip.star, m.fe.zip.state,
          object.names = TRUE,
          type = "text")
##                            (1)               (2)               (3)               (4)
##                           m.fe            m.fe.zip        m.fe.zip.star    m.fe.zip.state
## -------------------------------------------------------------------------------------------
## bni                     -0.997**          -0.969**          -1.041**          -1.003**
##                          (0.425)           (0.424)           (0.510)           (0.463)

## stan_zip_ben                              0.366***           0.425*           0.408***
##                                            (0.089)           (0.250)           (0.098)

## bni:stan_zip_ben                                             -0.111
##                                                              (0.439)

## -------------------------------------------------------------------------------------------
## Observations              9,306             9,306             9,306             8,237



## region
south_socstate.vec = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 05, 22, 40, 48)*100
table(dt$socstate %in% south_socstate.vec)
dt[, south_socstate := socstate %in% south_socstate.vec]

m.fe.north = felm(death_age ~ bni | key + as.factor(byear),
                  subset =
                      south_socstate == FALSE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)
m.fe.nick.north = update(m.fe.north,  death_age ~ bni + nick | as.factor(byear))
m.fe.zip.north = update(m.fe.north,  death_age ~ bni + stan_zip_ben | key + as.factor(byear))

m.fe.nonick.north = update(m.fe.north,  death_age ~ bni | key + as.factor(byear),
                           subset =
                               nick == FALSE &
                      south_socstate == FALSE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq)

m.fe.nickzip.north = update(m.fe.north,  death_age ~ bni + nick + stan_zip_ben | key + as.factor(byear))

out = stargazer(m.fe.north, m.fe.nick.north, m.fe.nonick.north, m.fe.zip.north, ## m.fe.nickzip.north,
          object.names = TRUE,
          type = "text")
##                            (1)               (2)               (3)               (4)
##                        m.fe.north      m.fe.nick.north  m.fe.nonick.north  m.fe.zip.north
## -------------------------------------------------------------------------------------------
## bni                     -1.668**           -0.292            -0.985           -1.574**
##                          (0.795)           (0.518)           (0.914)           (0.792)

## nick                                      -0.951**
##                                            (0.424)

## stan_zip_ben                                                                  0.546***
##                                                                                (0.147)

## -------------------------------------------------------------------------------------------
## Observations              3,616             3,616             3,426             3,616

## just the south

m.fe.south = felm(death_age ~ bni | key + as.factor(byear),
                  subset =
                      south_socstate == TRUE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)
m.fe.nick.south = update(m.fe.south,  death_age ~ bni + nick | as.factor(byear))
m.fe.zip.south = update(m.fe.south,  death_age ~ bni + stan_zip_ben | key + as.factor(byear))

m.fe.nonick.south = update(m.fe.south,  death_age ~ bni | key + as.factor(byear),
                           subset =
                               nick == FALSE &
                      south_socstate == TRUE &
                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq)

m.fe.nickzip.south = update(m.fe.south,  death_age ~ bni + nick + stan_zip_ben | key + as.factor(byear))

out = stargazer(m.fe.south, m.fe.nick.south, m.fe.nonick.south, m.fe.zip.south, ## m.fe.nickzip.south,
          object.names = TRUE,
          type = "text")
##                            (1)               (2)               (3)               (4)
##                        m.fe.south      m.fe.nick.south  m.fe.nonick.south  m.fe.zip.south
## -------------------------------------------------------------------------------------------
## bni                      -0.719            -0.667            -0.820            -0.701
##                          (0.546)           (0.422)           (0.738)           (0.545)

## nick                                        0.036
##                                            (0.259)

## stan_zip_ben                                                                   0.267**
##                                                                                (0.124)

## -------------------------------------------------------------------------------------------
## Observations              5,690             5,690             5,033             5,690

## see if we can get more precise region effects if we pool (no sib FE)

m.region = felm(death_age ~ south_socstate*bni | as.factor(byear),
                  subset =
##                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                data = dt)
m.region.nick = felm(death_age ~ south_socstate*bni + nick | as.factor(byear),
                  subset =
##                      nkey_male %in% 2:5 &
                      race == 2 &
                      sex == 1 &
                      n_fname >= min_freq,
                data = dt)

stargazer(m.region,m.region.nick,
          object.names = TRUE,
          type = "text")



## no effect : is this because sibs live in same zip_ben
z = felm(zip_ben ~ bni | key,
         subset =
             nkey_male %in% 2:5 &
             race == 2 &
             sex == 1 &
             n_fname >= min_freq,
         data = dt)
z.nick = felm(zip_ben ~ bni + nick| key,
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
##                                   zip_ben
##                            (1)               (2)
##                             z              z.nick
## -------------------------------------------------------
## bni                      -0.007            -0.002
##                          (0.008)           (0.009)

## nick                                       -0.006
##                                            (0.006)

## -------------------------------------------------------
## Observations              9,306             9,306


################## compare with whites
## but whites have no BNI

x1 = c(0,0,0,1,1,1)
x2 = c(0,.5,1,0,0,0)
y = x1 + x2 * 1 + rnorm(length(x1), sd = .2)
lm(y ~ x1 + x2)

## redefine NA bni --> 0 (for whites)
dt[, my.bni := bni]
dt[is.na(my.bni), my.bni := 0]

## pick a random sample of 100k whites

bar = dt[, .(white_lines =.I[race == 1])]
set.seed(10)
s = 1:nrow(dt) %in% sample(bar$white_lines, 10^5, replace = F)

dt[s, table(race)]

dt[, wni := 1 - my.bni]
m.pooled.race = felm(death_age ~ my.bni + as.factor(race) |  as.factor(byear),
                  subset =
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)

dt[                   nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq]
## ok, so we have only whites with sibs
## note: sib FE makes not sense with race, because brothers typically same race

m.pooled.race.inter = felm(death_age ~ south_socstate*(my.bni + as.factor(race)) |  as.factor(byear),
                  subset =
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)
m.pooled.race.north = felm(death_age ~ my.bni + as.factor(race) |  as.factor(byear),
                           subset =
                               south_socstate == FALSE &
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)
m.pooled.race.south = felm(death_age ~ my.bni + as.factor(race) |  as.factor(byear),
                           subset =
                               south_socstate == TRUE &
                      nkey_male %in% 2:5 &
                      race %in% 1:2 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)



stargazer(m.pooled.race,
          m.pooled.race.inter,
          m.pooled.race.north,
          m.pooled.race.south,
          object.names = TRUE,
          type = "text")


m.white = lm(death_age ~ as.factor(byear),
                  subset =
                      nkey_male %in% 2:5 &
                      race %in% 1 &
                      sex == 1 &
                      n_fname >= min_freq,
                  data = dt)
## so 1910 byear white lives 93.000 -7.4913 = [1] 85.5087

m.black.pooled.bni = felm(death_age ~ bni +   as.factor(byear),
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

m.race.disparity = felm(death_age ~ as.factor(race)*south_socstate | as.factor(byear),
                           subset =
                               nkey_male %in% 2:5 &
                               race %in% 1:2 &
                               sex == 1 &
                               n_fname >= min_freq,
                   data = dt)



stargazer(m.race.disparity,
          object.names = TRUE,
          type = "text")
