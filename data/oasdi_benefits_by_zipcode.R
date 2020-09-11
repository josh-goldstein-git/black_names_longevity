## Let's do a scatter plot of mean age of death for 1905 cohort in BUNMD
## by social security benefit level

## read in bunmd
library(data.table)
dt <- fread("~/Downloads/bunmd_v1/bunmd_v1.csv", nrows = 10)

million = 10^6
dt <- fread("~/Downloads/bunmd_v1/bunmd_v1.csv",
            select = c("byear",
                       "dyear",
                       "race_first",
                       "zip_residence",
                       "death_age", "ccweight",
                       "sex"),
            keepLeadingZeros = TRUE)
nrow(dt)/million
## [1] 49.45929
my.dt <- dt[dyear %in% 1988:2005 & death_age >= 65 & !is.na(ccweight)]
nrow(my.dt)/million
## [1] 10.06269



## read in social security benefit level
## oasdi <- fread("~/Documents/data/oasdi_by_zip/oasdi_zip05_combined_new.csv")
oasdi <- fread("./oasdi_zip05_combined_new.csv")

oasdi_small <- oasdi[, .(zip, zip_ben)]
oasdi_small[, .N, by = zip]

## merge data
my.dt[, zip5 := as.numeric(substr(zip_residence, 1, 5))]

mdt <- merge(my.dt, oasdi_small[!is.na(zip)], by.x = "zip5", by.y = "zip")

## do scatter plot
out <- mdt[byear %in% 1900:1910, .(.N, ave_death_age = mean(death_age), zip_ben = mean(zip_ben)), by = zip5]
my.N = 250
out[N > my.N, plot(zip_ben, ave_death_age)]
out[N > my.N, abline(lm(ave_death_age ~ zip_ben))]

## compare blacks and whites
m_ben <- lm(death_age ~ as.factor(byear) + zip_ben,
        data = mdt,
        subset = byear %in% 1900:1910)
m_ben_sex_race = update(m_ben, .~ . + as.factor(sex) + as.factor(race_first))
m_sex_race = update(m_ben_sex_race, .~ . -zip_ben)

mtable(m_ben, m_ben_sex_race, m_sex_race)


## ok, now let's look at differences in effect by race

m_ben_w <- lm(death_age ~ as.factor(byear) + zip_ben + as.factor(sex),
        data = mdt,
        subset = race_first == 1 & byear %in% 1900:1910)
m_ben_b <- lm(death_age ~ as.factor(byear) + zip_ben + as.factor(sex),
        data = mdt,
        subset = race_first == 2 & byear %in% 1900:1910)
mtable(m_ben_w, m_ben_b)

## now do race and sex

m_ben_w_m <- lm(death_age ~ as.factor(byear) + zip_ben,
        data = mdt,
        subset = sex == 1 & race_first == 1 & byear %in% 1900:1910)
m_ben_b_m <- lm(death_age ~ as.factor(byear) + zip_ben,
        data = mdt,
        subset = sex == 1 & race_first == 2 & byear %in% 1900:1910)
m_ben_w_f <- lm(death_age ~ as.factor(byear) + zip_ben,
        data = mdt,
        subset = sex == 2 & race_first == 1 & byear %in% 1900:1910)
m_ben_b_f <- lm(death_age ~ as.factor(byear) + zip_ben,
        data = mdt,
        subset = sex == 2 & race_first == 2 & byear %in% 1900:1910)

mtable(m_ben_w_m, m_ben_b_m, m_ben_w_f, m_ben_b_f)


## a map of ohio?

