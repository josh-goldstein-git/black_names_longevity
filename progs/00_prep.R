#############################################################
# Prepare BUNMD data
#############################################################

## Summary: This script cleans the BUNMD data and selects siblings. 
## this does nicknames and bni and outputs data set for making figures and modeling
## subsetting: only males, only blacks and whites
## note: no restrictions on byear

library(data.table)

## 1. read bunmd

if (0) {
  library(data.table)
  dt <- fread("/censoc/data/censoc_v2/bunmd_v2.csv")
  cc.dt = dt[!is.na(ccweight)]
  ## nrow(cc.dt)
  ## [1] 19221021
  ## nrow(dt)
  ## [1] 49337827
  dt <- fwrite(cc.dt, "~/Downloads/bunmd_v1/cc_bunmd_new.csv")
}

dt <- fread("~/Downloads/bunmd_v1/cc_bunmd_new.csv")

## 1a. some checks

## data checks
range(dt$byear)
## [1] 1895 1940
range(dt$death_age)
## [1]  65 100
range(dt$dyear)
## [1] 1988 2005

## 1b. keep only blacks and whites

## rename race_first to race
dt[, race := race_first]

## drop everyone whose not black or white
dt = dt[race %in% 1:2]

## 1c. keep only males
dt = dt[sex == 1]

initial_size = nrow(dt)
print(initial_size)

## 2. clean names

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

## 3. get family (keys)

## dt[, family := NULL]
dt[!is.na(father_lname) & father_lname != "" &
     !is.na(father_fname_clean) &
     !is.na(mother_lname) & mother_lname != "" &
     !is.na(mother_fname_clean) ,
   family := paste(father_lname_clean,
                   father_fname_clean,
                   mother_lname_clean,
                   mother_fname_clean, sep = "_")]
## if family == "" --> NA
dt[family == "", family := NA]

## 4. standardize names using IPUMS dictionary

## read in mpc + abe nickname file (cleaned)
nickname_file <- fread("../black_names_longevity/data/nickname_crosswalk_master.csv")

## josh's named vector trick
## we have a vector with observed names as labels and standardized names as values
male_nickname.vec = nickname_file[sex == 1]$fname_std
names(male_nickname.vec) = nickname_file[sex == 1]$fname
## tail(male_nickname.vec)
##    ROYCE   SUMMERS   WALDRON        ED     EDDIE      MOSE
##  "ROYCE" "SUMMERS" "WALDRON"  "EDWARD"  "EDWARD"   "MOSES"
male_nickname.vec[male_nickname.vec == "WILLIAM"]
##   WILLIAM        WM    WILLIE      WILL      BILL     WILLY   WILLIAN     BILLY
## "WILLIAM" "WILLIAM" "WILLIAM" "WILLIAM" "WILLIAM" "WILLIAM" "WILLIAM" "WILLIAM"
##    WILIAM    BILLIE      WILY    WILLAM     WILLI    WILLIA     WILLE     WILLM

dt[, fname_std := fname_clean] ## we default to original clean name
dt[fname_clean %in% names(male_nickname.vec), ## if appears in nickname file, standardize
   fname_std := male_nickname.vec[fname_clean] ]
## dt[, fname_std := male_nickname.vec[fname_clean] ]

## 5. generate bni (of std names)

## no restriction on byear
tt = dt[race %in% 1:2,
        table(fname_std, race)]
ptt = prop.table(tt, 2)
p_black = ptt[,2]
p_white = ptt[,1]
bni.dt = data.table(fname_std = rownames(tt),
                    bni = p_black/ (p_black + p_white),
                    N = rowSums(tt))
## use same named-vector trick for assigning bni to dt
bni.vec = bni.dt$bni
names(bni.vec) = bni.dt$fname_std
dt[, bni_std := bni.vec[fname_std]]

## 6. export data set

## drop a few columns that we really don't need
columns_to_drop = c("race_first_cyear",
                    "race_last_cmonth",
                    "race_first_cmonth",
                    "number_claims") 
## https://stackoverflow.com/questions/9202413/how-do-you-delete-a-column-by-name-in-data-table
dt[, (columns_to_drop) := NULL]

## add a column with fname_std_freq
dt[, fname_std_freq := .N, by = fname_std]

## add a column with sex score
dt[,sex_score := mean(sex), by = fname_std]

final_size = nrow(dt)

print("sizes")
print("initial_size")
print(initial_size)
print("final_size")
print(final_size)

fwrite(x = dt, file = "/censoc/data/working_files/00_cleaned_sibs.csv")
