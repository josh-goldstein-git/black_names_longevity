## read in data, clean the names, create the keys
## restrict it


library(data.table) ## for working with big data
library(lfe) ## for fixed effects (with lots of FEs)
library(stargazer) ## for regression output tables
library(tidyverse) ## data manipulation

## read in, depending if local on josh's laptop
sysinfo = Sys.info()
if(sysinfo["nodename"] == "Joshuas-MacBook-Pro.local")
    dt <- fread("~/Downloads/bunmd_v1/cc_bunmd_new.csv")
if(sysinfo["nodename"] != "Joshuas-MacBook-Pro.local")
    dt <- fread("/censoc/data/censoc_files_for_website/bunmd_v1.csv")

print(nrow(dt))

## rename race_first to race
dt[, race := race_first]
## data checks
range(dt$byear)
## [1] 1895 1940
range(dt$death_age)
## [1]  65 100
range(dt$dyear)
## [1] 1988 2005


## restrict latest byear to 1925
latest_byear = 1925
dt <- dt[byear <= latest_byear]
range(dt$byear)
## [1] 1895 1925
range(dt$death_age)
## [1]  65 100  ## note still 65
range(dt$dyear)
## [1] 1988 2005

print(nrow(dt))
## [1] 14614705

## First names cleaning
## various NA strings
na.strings = c("^$", # empty string
               "^NA$",
               ## varieties of "UNKNOWN"
               "^UKN", # start with UKN ...
               "^UNK", # start with UNK ...
               "^UN$", "^U$", "^NS$",
                "^UK$",
               ## varieties of NOT and NONE
               "NONENAMED",
               "^NONE$",
               "^NOT",
               "NONAME",
               ## variaeties of X, XX, XXXXXX
               "^X*$",
               ## other
               "UNSTATED",
               "UNLISTED",
               "^UNAV", ## unavailable and misspellings thereof
               "MISSING",
               "^UNDISCLO",
               "^UNSPEC",
               "WITHHELD",
               "DECEASED",
               "^NAME$")
na.regx = paste(na.strings, collapse = "|") ## regexp for 'or'
clean_first_names <- function(fname)
{
    ## Missing --> NA
    fname[grepl(pattern = na.regx, fname)] <- NA
    ## first word only
    fname <- gsub(pattern = " .*$", replacement = "", x = fname)
    ## NA if 0 or 1 characters
    fname[nchar(fname) < 2] <- NA
    fname
}
## carry out
dt[, fname_clean := clean_first_names(fname)]
dt[, mother_fname_clean := clean_first_names(mother_fname)]
dt[, father_fname_clean := clean_first_names(father_fname)]
dt[, father_lname_clean := clean_first_names(father_lname)]
dt[, mother_lname_clean := clean_first_names(mother_lname)]


## create key
## dt[, key := NULL]
dt[!is.na(father_lname) & father_lname != "" &
   !is.na(father_fname_clean) &
   !is.na(mother_lname) & mother_lname != "" &
   !is.na(mother_fname_clean) ,
   key := paste(father_lname_clean,
                father_fname_clean,
                mother_lname_clean,
                mother_fname_clean, sep = "_")]
## if key == "" --> NA
dt[key == "", key := NA]

## create zip5 as string
dt[, zip5 := substr(zip_residence, 1,5)]
dt[is.na(as.numeric(zip5)), zip5 := NA]
summary(is.na(dt$zip5))
## create fborn
dt[, fborn := bpl >= 15000]

nrow(dt)
## [1] 14614705
dt <- dt[fborn == FALSE & !is.na(zip5)]
nrow(dt)
## nrow(dt)
## [1] 12135765

getwd()
## merge
bni.dt = fread("../data/bni.csv")
zip.dt = fread("../data/oasdi_zip05_combined_new.csv")
zip.dt[, zip5 := sprintf("%05d", zip)]

## note not all zipcodes in bunmd are in the zip_ben data
dt[, table(zip5 %in% zip.dt$zip5)]
## FALSE     TRUE
## 83907 12051858
## Not a huge problem, but we loose some individuals
dt[, prop.table(table(zip5 %in% zip.dt$zip5))]
##       FALSE        TRUE
## 0.006914026 0.993085974
## could potentially impute to neighboring zips, but probably not worth it.



## merge using zip
mdt1 = merge(dt, zip.dt, by.x = "zip5", by.y = "zip5")
print(nrow(mdt1))
## merge again using bni
mdt2 = merge(mdt1, bni.dt, by = "fname")
print(nrow(mdt2))
mdt2[race == 1, bni := NA] ## assign NA for bni to Whites
## rename our merged data set my.dt
my.dt = mdt2

print(nrow(mdt1))
## [1] 12051858
print(nrow(mdt2))
## [1] 11392068
print(nrow(mdt1[sex == 1]))
## [1] 5464900
print(nrow(mdt2[sex == 1]))
## [1] 5443422


## restrict
my.dt = my.dt[ fborn == FALSE &
               !is.na(fname_clean) &
               race %in% 1:2 &
               sex %in% 1:2 &
               byear %in% 1895:latest_byear &
               death_age >= 65 &
               age_first_application < 65]
print(nrow(my.dt))
## [1] 9904893


my.dt[, n_fname := .N, by = fname]

## ## let's check on zip5
## my.dt[, mean(zip5 > 0), by = dyear][order(dyear)]
## my.dt[race == 1, mean(zip5 > 0), by = dyear][order(dyear)]
## my.dt[race == 2, mean(zip5 > 0), by = dyear][order(dyear)][, plot(dyear, V1)]
## my.dt[race == 1, mean(zip5 > 0), by = dyear][order(dyear)][, lines(dyear, V1)]
## ## actually slightlyh better for blacks than whites, but only basically complete as of 93. Bummer!

## calculate birth order
my.dt <- my.dt %>%
    group_by(key) %>%
    arrange(byear, bmonth, bday, .by_group = TRUE) %>%
    mutate(birth_order = row_number())

## save
fwrite(my.dt[, .(fname, bni, lname, zip5, zip_ben, byear, bmonth, bday, dyear, socstate, race,
       sex, bpl, age_first_application, death_age, ccweight, key, birth_order, n_fname)],
       file = "~/Downloads/bunmd_v1/bunmd_sib_data.csv")

## ## only Blacks
## fwrite(my.dt[race == 2, .(fname, bni, lname, zip5, zip_ben, byear, dyear, socstate, race,
##        sex, bpl, age_first_application, death_age, ccweight, key, n_fname)],
##        file = "../data/bunmd_black_sib_data.csv")

## foo = fread("./bunmd_sib_data.dt")
## rm(foo)
