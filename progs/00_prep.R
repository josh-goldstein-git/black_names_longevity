#############################################################
# Identify Additional Siblings 
#############################################################

## Summary: This script identifies additional siblings based off of parent's names. 
## Specifcially, identify additional siblings who match on (1) BPL (2) mother and father's last name 
## (3) either father or mother last name but not both. (We already matched siblings with an exact match.)
## We then compare levenstein stringdist and establish siblingship if above threshold of 0.7. 
## Finally, we compare additional siblings based on (1) BPL and (2) mother and father's last name who don't 
## match on either father fname or mother fname. For these, we require the levenstein stringdist to be greater
## than 0.8. 

# init --------------------------------------------------------------------

library(fastLink)
library(lfe)
library(tidyverse)

## read in data 
dt <- fread("/censoc/data/working_files/00_cleaned_sibs.csv")

## Filter to Black Males with non-missing parents names
potential_sibs <- dt %>%
  filter(sex == 1 & race == 2) %>% 
  filter(!(is.na(father_fname_clean) | father_fname_clean == "")) %>% 
  filter(!(is.na(father_lname_clean) | father_lname_clean == "")) %>% 
  filter(!(is.na(mother_fname_clean) | mother_fname_clean == "")) %>% 
  filter(!(is.na(mother_lname_clean) | mother_lname_clean == "")) %>% 
  filter(!(is.na(fname_std) | fname_std == "")) 

## Restrict to men without sibling
potential_sibs <- potential_sibs %>% 
  group_by(family) %>% 
  filter(n() == 1)

# Establish siblingship exact match + similiar father's first name  ----------------

## Create keys for sibs who match on bpl, father lname, mother lname, father fname, but differ on mother fname
additional_sibs_mother_mismatch <- potential_sibs %>% 
  mutate(family = paste(father_lname_clean, mother_lname_clean, father_fname_clean, "FNAME-STD", bpl, sep = "_")) 

## Compare mother fname for potential sibs; establish as sibs if lev_distance > 0.7
new_sibs_mother <- additional_sibs_mother_mismatch %>%
  group_by(family) %>% 
  filter(n() == 2) %>% 
  mutate(lev_distance = levenshteinSim(mother_fname_clean[1], mother_fname_clean[2]))  %>% 
  filter(lev_distance > .7) %>% 
  arrange(family)

# Establish siblingship exact match + similiar mother's first name  ----------------

## find potential sibs who match on bpl, father lname, mother lname, father fname, but differ on father fname
additional_sibs_father_mismatch <- potential_sibs %>% 
  mutate(family = paste(father_lname_clean, mother_lname_clean, mother_fname_clean, "FATHER-FNAME-STD", bpl, sep = "_")) 

## Compare father fname for potential sibs; establish as sibs if lev_distance > 0.7
new_sibs_father <- additional_sibs_father_mismatch %>%
  group_by(family) %>% 
  filter(n() == 2) %>% 
  mutate(lev_distance_father = levenshteinSim(father_fname_clean[1], father_fname_clean[2])) %>% 
  filter(lev_distance_father > .7) %>% 
  arrange(family)

# establish siblingship for brothers with similar (but not identical) father's first name --------

## find potential sibs who match on bpl, father lname, mother lname, father fname, but differ on mother fname
cadditional_sibs_mother_mismatch <- potential_sibs %>% 
  mutate(family = paste(father_lname_clean, mother_lname_clean, father_fname_clean, "FNAME-STD", bpl, sep = "_")) 

## Compare mother fname for potential sibs; establish as sibs if lev_distance > 0.7
new_sibs_mother <- additional_sibs_mother_mismatch %>%
  group_by(family) %>% 
  filter(n() == 2) %>% 
  mutate(lev_distance_mother = levenshteinSim(mother_fname_clean[1], mother_fname_clean[2]))  %>% 
  filter(lev_distance_mother > .7) 

# establish siblingship for pairs with similar (but not identical) mother and father first name --------

## find potential sibs who match on bpl, father lname, mother lname, but differ on mother fname and father fname 
additional_sibs_fnames_mismatch <- potential_sibs %>% 
  mutate(family = paste(father_lname_clean, mother_lname_clean, "FNAMES-STD", bpl, sep = "_")) 

## Compare father fname for potential sibs; establish as sibs if lev_distance > 0.8
new_sibs_mother_father_mismatch <- additional_sibs_fnames_mismatch %>%
  group_by(family) %>% 
  filter(n() == 2) %>% 
  mutate(lev_distance_mother = levenshteinSim(mother_fname_clean[1], mother_fname_clean[2]),
         lev_distance_father = levenshteinSim(father_fname_clean[1], father_fname_clean[2]))  %>% 
  filter(lev_distance_mother > .8 & lev_distance_father > .8) 

## select sibs not already matched 
new_sibs_mother_father <- new_sibs_mother_father_mismatch %>% 
  filter(!(lev_distance_mother == 1 | lev_distance_father == 1)) 

## combine all sibs
new_sibs <- bind_rows(new_sibs_mother, new_sibs_father, new_sibs_mother_father) %>% 
  arrange(family)

## write out
fwrite(x = new_sibs, file = "/censoc/data/working_files/01_additional_sibs.csv")



