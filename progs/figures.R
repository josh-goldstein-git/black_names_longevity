###########################
## some plots 
###########################


## let's do the models for sib analysis
library(data.table) ## for working with big data
library(lfe) ## for fixed effects (with lots of FEs)
library(stargazer) ## for regression output tables
library(ggpubr) ## combining ggplots


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


######### name scatter plot of longevity vs BNI

## normalized age at death (for plotting)
dt[, norm_death_age := death_age - mean(death_age), by = byear]
## let's restrict to names that are mostly male
my.dt = dt
my.dt[, sex_score := mean(sex), by = fname]

tmp = my.dt[sex_score < 1.2 &
              n_fname >= min_freq &
              nkey_male %in% 2:5 &
              race == 2 &
              sex == 1,
            .(bni = mean(bni), norm_death_age = mean(norm_death_age), .N),
            by = fname]

tmp.root = my.dt[sex_score < 1.2 &
                   n_fname >= min_freq &
                   nkey_male %in% 2:5 &
                   race == 2 &
                   sex == 1,
                 .(bni_root = mean(bni_root), norm_death_age = mean(norm_death_age), .N),
                 by = fname_std]


bni_pyramid <- tmp %>% 
  mutate(jitter.N = jitter(N, factor = 3)) %>% 
  filter(N > 4) %>% 
  ggplot(aes(x = bni, y = jitter.N, label = fname)) + 
  geom_text() + 
  scale_y_continuous(trans='log10') + 
  theme_light(base_size = 20) +
  labs(x = "BNI",
       y = "N")


bni_pyramid_root <- tmp.root %>% 
  mutate(jitter.N = jitter(N, factor = 3)) %>% 
  filter(N > 4) %>% 
  ggplot(aes(x = bni_root, y = jitter.N, label = fname_std)) + 
  geom_text() + 
  scale_y_continuous(trans='log10') + 
  theme_light(base_size = 20) +
  labs(x = "Root BNI",
       y = "N")


bni_pyramid_combined <- ggarrange(bni_pyramid, bni_pyramid_root)

bni_pyramid_combined <- annotate_figure(bni_pyramid_combined, top = text_grob("Black Name Index (BNI)", size = 30))

ggsave(plot = bni_pyramid_combined, filename = "../figures/bni_pyramid_combine.pdf", height = 10, width = 15)




# scatter Plot -----------------------------------------------------------


######### name scatter plot of longevity vs BNI

## normalized age at death (for plotting)
dt[, norm_death_age := death_age - mean(death_age), by = byear]
## let's restrict to names that are mostly male
my.dt = dt
my.dt[, sex_score := mean(sex), by = fname]


pdf("../figures/black_name_male_scatter.pdf",
    height = 12, width = 12)
tmp.root[, plot(bni_root, norm_death_age, cex = sqrt(N)/10, type = 'n',
           ylim = c(-3, 2),
           xlim = c(0, 1.1))]
m = tmp.root[, lm(norm_death_age ~ bni_root, weight = sqrt(N))]
tmp.root[, abline(m)]
norm_factor = 4
tmp.root[, text(bni_root, norm_death_age, fname_std, cex = sqrt(N)/norm_factor,
           col = "orange")]
tmp.root[fname_std %in% c("MOSES", "AARON", "ELIJAH", "SAMUEL",
                 "ISAAC", "JOSEPH",
                 "ISAIAH", "ISIAH",
                 "ELI",
                 "ABRAHAM", "DAVID",
                 "NOAH", "SOLOMON",
                 "RUBEN", "RUBIN",
                 "JEREMIAH",
                 "EZEKIEL", "JOSHUA",
                 "AMOS", "SILAS"),
    text(bni_root, norm_death_age, fname_std, cex = sqrt(N)/norm_factor,
         col = "blue")]
tmp.root[grepl("Y$", fname_std) |
      grepl("IE$", fname_std) |
        fname_std %in% c("MOSE", "DAN", "JOHNNY", "ABE", "JAKE", "JIM", "JIMMIE",
                   "TOM", "ED", "EDD"),
    text(bni_root, norm_death_age, fname_std, cex = sqrt(N)/norm_factor,
         col = "red")]
tmp.root[grepl("EL$", fname_std),
    text(bni_root, norm_death_age, fname_std, cex = sqrt(N)/norm_factor,
         col = "blue")]
tmp.root[grepl("US$", fname_std),
    text(bni_root, norm_death_age, fname_std, cex = sqrt(N)/norm_factor,
         col = "green")]
tmp.root[fname_std %in% "FREEMAN",
    text(bni_root, norm_death_age, fname_std, cex = sqrt(N)/norm_factor,
         col = "black")]

## could be systemized a bit more
system("open ../figures/black_name_male_scatter.pdf")
dev.off()
  


my.formula <- norm_death_age ~ bni_root  

bni_scatter <- tmp.root %>% 
  ungroup() %>% 
  ggplot() + 
  geom_text(aes(x = bni_root, y = norm_death_age, label = fname_std), size = 1.2*sqrt(tmp.root$N)) + 
  geom_smooth(aes(x = bni_root, y = norm_death_age), se = F, method = "lm")+ 
  theme_light(base_size = 20) +
  xlim(0, 1.1) + 
  ylim(-3, 2) + 
  labs(x = "Standardized BNI",
       y = "Normalized Death Age") 

ggsave(plot = bni_scatter, filename = "../figures/bni_scatter_standardized.pdf", height = 10, width = 10)



