###################################################
# Create Standardized Name BNI Index
###################################################



# init --------------------------------------------------------------------

library(data.table)
library(tidyverse)

bunmd <- fread("/censoc/data/censoc_files_for_website/bunmd_v1.csv")

bunmd <- bunmd %>% 
  filter(race_first %in% c(1:2))


## Josh original code
# tt = table(bunmd$fname, bunmd$race_first)
# ptt = prop.table(tt, 2) ## not sure if it’s “2”, we want proportions to add up to 1 along columns.
# bni_new = ptt[,2] / (ptt[,2] + ptt[,1])
# bni_new <- as.data.frame(bni_new)


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

setDT(bunmd)
## carry out
bunmd[, fname := clean_first_names(fname)]


## read in mpc nickname file (cleaned)
nicknames_mpc <- fread("../data/mpc_nicknames.csv")

## join onto dt 
bunmd <- merge(bunmd, nicknames_mpc, all.x = T, by = c("sex", "fname"))
bunmd[is.na(nickname_mpc), nickname_mpc := 0]
bunmd[is.na(fname_std), fname_std := fname ]

## note: fname_std is the standardized mpc name (e.g., "bill" -> "william", "william" -> "william")
## only available for names in the MPC nickname file (appeared 100+ time in 1940 census)

## calculate BNI 

bunmd_bni <- bunmd %>% 
  filter(byear %in% c(1900:1920)) %>% 
  mutate(count = 1) %>% 
  group_by(fname_std) %>% 
  summarize(n = n(),
            black = sum(count[race_first == 2]),
            white = sum(count[race_first == 1])) %>% 
  mutate(p_black = black / sum(black), 
         p_white = white / sum(white)) %>% 
  mutate(bni_root = p_black / (p_black + p_white)) %>% 
  select(fname_std, bni_root, n_fname_root = n)

fwrite(bunmd_bni, "../data/bni_root.csv")








