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
                   n_fname_root >= min_freq &
                   #nkey_male %in% 2:5 &                                                                                           
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
  mutate(fname_std = tools::toTitleCase(tolower(fname_std))) %>% 
  mutate(jitter.N = jitter(N, factor = 3)) %>% 
  filter(jitter.N > 100) %>% 
  ggplot(aes(x = bni_root, y = jitter.N, label = fname_std)) + 
  geom_text(size = 5) + 
  scale_y_continuous(trans='log10') + 
  theme_classic(base_size = 20) +
  labs(x = "Standardized BNI",
       y = "N")


# bni_pyramid_combined <- ggarrange(bni_pyramid, bni_pyramid_root)
# bni_pyramid_standardized <- annotate_figure(bni_pyramid_root, top = text_grob("Black Name Index (BNI)", size = 30))
# ggsave(plot = bni_pyramid_combined, filename = "../figures/bni_pyramid_combine.pdf", height = 10, width = 15)


ggsave(plot = bni_pyramid_root, filename = "../figures/bni_pyramid_root.pdf", height = 12, width = 17)
# scatter Plot -----------------------------------------------------------

######### name scatter plot of longevity vs BNI


m = tmp.root[, lm(norm_death_age ~ bni_root)]

bni_scatter <- tmp.root %>% 
  mutate(fname_std = tools::toTitleCase(tolower(fname_std))) %>% 
  filter(N> 100) %>% 
  ggplot() + 
  geom_text(aes(x = bni_root, y = norm_death_age, label = fname_std), size = 5) + 
  geom_smooth(aes(x = bni_root, y = norm_death_age), method = "lm", se = F, color = "red")+ 
  theme_classic(base_size = 20) +
  ylim(-2.5, 0.5) + 
  labs(x = "Standardized BNI",
       y = "Normalized Death Age") 

ggsave(plot = bni_scatter, filename = "../figures/bni_scatter_standardized_blacks.pdf", height = 12, width = 17)


tmp.root.white = my.dt[sex_score < 1.2 &
                   n_fname_root >= min_freq &
                   #nkey_male %in% 2:5 &                                                                                           
                   race == 1 &
                   sex == 1,
                 .(bni_root = mean(bni_root), norm_death_age = mean(norm_death_age), .N),
                 by = fname_std]

bni_scatter_whites <- tmp.root.white %>% 
  mutate(fname_std = tools::toTitleCase(tolower(fname_std))) %>% 
  filter(N> 100) %>% 
  ggplot() + 
  geom_text(aes(x = bni_root, y = norm_death_age, label = fname_std), size = 5) + 
  geom_smooth(aes(x = bni_root, y = norm_death_age), method = "lm", se = F, color = "red")+ 
  theme_classic(base_size = 20) +
  ylim(-2.5, 0.5) + 
  labs(x = "Standardized BNI",
       y = "Normalized Death Age",
       lab = "Normalized Death Age and Standardized BNI for Whites") 


ggsave(plot = bni_scatter_whites, filename = "../figures/bni_scatter_standardized_whites.pdf", height = 12, width = 17)


tmp.root.white = my.dt[sex_score < 1.2 &
                         n_fname_root >= min_freq &
                         #nkey_male %in% 2:5 &                                                                                           
                         race == 1 &
                         sex == 1,
                       .(bni_root = mean(bni_root), norm_death_age = mean(norm_death_age), .N),
                       by = fname_std]

bni_scatter_whites <- tmp.root.white %>% 
  mutate(fname_std = tools::toTitleCase(tolower(fname_std))) %>% 
  filter(N> 100) %>% 
  ggplot() + 
  geom_text(aes(x = bni_root, y = norm_death_age, label = fname_std), size = 5) + 
  geom_smooth(aes(x = bni_root, y = norm_death_age), method = "lm", se = F, color = "red")+ 
  theme_classic(base_size = 20) +
  ylim(-2.5, 0.5) + 
  labs(x = "Standardized BNI",
       y = "Normalized Death Age",
       lab = "Normalized Death Age and Standardized BNI for Whites") 


ggsave(plot = bni_scatter_whites, filename = "../figures/bni_scatter_standardized_whites.pdf", height = 12, width = 17)

