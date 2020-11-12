## Some starter-code to find more brothers

## We'll focus on black population

## The idea will be to find cases where there's exact matching on
## mother and father's last names but a small mismatch on first names

## There's a more complicated issue of when we have more than 1 pairwise comparison
## for example if we have three different "family" keys

## Here we'll just work on the case where we have exactly two men with
## last-name matches for mother and father but first-name mismatches
## for mother and father. So, we'll find missing sets of two brothers,
## where we thought there was only 1.

## I use levenstein stringdist because it seemed to work better,
## giving roughly equal weight to the mother and father's first
## names. jarowinkler emphasizes the beginning of the string and thus
## privileges the fathers namem.


library(data.table)
library(RecordLinkage)
dt <- fread("cuny_data.csv")
b <- dt[race == 2] ## black only

## now we concatenate mother and father's last names (and separately their first names)

## fm_lname : father and mothers last names
b[, fm_lname := NULL]
## drop blanks and missings. Note: needs validation and checking
b[!is.na(father_lname_clean) & father_lname_clean != "" &
    !is.na(mother_lname_clean) & mother_lname_clean != "",
  fm_lname := paste(father_lname_clean, mother_lname_clean, sep = "_")]
b[!is.na(father_fname_clean) & father_fname_clean != "" &
    !is.na(mother_fname_clean) & mother_fname_clean != "",
  fm_fname := paste(father_fname_clean, mother_fname_clean, sep = "_")]
## fm_fname : father and mothers first names
b[, fm_fname := NULL]
## drop blanks and missings. Note: needs validation and checking
b[!is.na(father_fname_clean) & father_fname_clean != "" &
    !is.na(mother_fname_clean) & mother_fname_clean != "",
  fm_fname := paste(father_fname_clean, mother_fname_clean, sep = "_")]
b[!is.na(father_fname_clean) & father_fname_clean != "" &
    !is.na(mother_fname_clean) & mother_fname_clean != "",
  fm_fname := paste(father_fname_clean, mother_fname_clean, sep = "_")]

## In order to select the samp
## Now get sibship "counts" according to fm_lname only and per family only
b[, n_fm_lname := .N, by = fm_lname]
b[, n_family := .N, by = family]
setkey(b, fm_lname) ## order by fm_lname for viewing

## Now get number of "families" per fm_lname (we want to focus on exactly 2)
## Here we focus only on cases where n_family == 2
## and total number of potential sibs we are considering == 2
b[, n_fm_fname_per_fm_lname := length(unique(fm_fname)), by = fm_lname]
## restrict to 2 and 2 as above
my.b = b[n_fm_fname_per_fm_lname == 2 & n_fm_lname == 2]
## nrow(my.b)
## [1] 88584
my.b[, length(unique(fm_lname))]
##  [1] 44292
## so we have the _potential_ of adding perhaps another nearly 44k brothers!? (only if they're all true brothers)


## get Jaro-Winkler string disparity
## by comparing mother and father's first names of the two people with same mother and father lastnames
my.b[, jw := jarowinkler(fm_fname[1], fm_fname[2]), by = fm_lname]
my.b[, lev := levenshteinSim(fm_fname[1], fm_fname[2]), by = fm_lname]
my.b[, n_bpl := length(unique(bpl)),  by = fm_lname]

## now generate new brothers (conditioning on same BPL)

out = my.b[ lev > .7 & n_bpl == 1,
            .(lev, fm_lname, fm_fname, fname_clean, bpl, byear, mother_mname, father_mname, bni_std, family, death_age)]

## now apply the 1st family key (of the pair) to both
out[, family_more := family[1], by = fm_lname]
nrow(out)
## [1] 17934
nrow(out[byear %in% 1915:1925])
## [1] 10353

out_my_byear = out[byear %in% 1915:1925]
out_my_byear[, nsib := .N, by = family_more]
nrow(out_my_byear[nsib > 1])
## [1] 6660

## so we can add 6000+ brothers to our analysis

## -------------- check on lev thresshold

lev.vec <- seq(0, 1, .05)
same_bpl.vec = NULL
for(i in 1:(length(lev.vec)-1))
{
  print(lev.vec[i])
  tmp = my.b[ lev.vec[i] < lev & lev <= lev.vec[i+1] ]
  print(prop.table(table(tmp$n_bpl)))
  same_bpl.vec[i] = prop.table(table(tmp$n_bpl))["1"]
}

library(txtplot)
options(width = 100)
txtplot(lev.vec[1:(length(lev.vec)-1)], same_bpl.vec)
##     +--------------+---------------+---------------+---------------+-----------+
##     |                                                      *   *   *   *  *    |
##     |                                                  *                       |
##     |                                              *                           |
## 0.8 +                                          *                               +
##     |                                      *                                   |
##     |                                                                          |
##     |                                                                          |
## 0.6 +                                                                          +
##     |                                  *                                       |
##     |                                                                          |
##     |                                                                          |
##     |                              *                                           |
## 0.4 +                                                                          +
##     |                                                                          |
##     |                          *                                               |
##     |                      *                                                   |
## 0.2 +  *   *   *   *   *                                                       +
##     +--------------+---------------+---------------+---------------+-----------+
##                   0.2             0.4             0.6             0.8
## clear drop off at  about 0.7


par(mfrow = c(1,1))
plot(lev.vec[1:(length(lev.vec)-1)], same_bpl.vec)
abline(v = .7)
grid()
## clear drop off at  about 0.7


