###############################################
# "Comparison of Mullato and Black Name Indices"
###############################################
  
  
## Summary: Comparison of Black and Mullato Name indices. 


# init --------------------------------------------------------------------
library(tidyverse)

## Read in Josh's analysis 
mullato <- tibble::tribble(
  ~name,  ~value,
  "TOM", 0.38,
  "JOE",  0.4,
  "WILL",  0.4,
  "JIM",  0.4,
  "SAM",  0.4,
  "JERRY", 0.41,
  "PETER", 0.41,
  "JACK", 0.41,
  "BEN", 0.41,
  "ABRAHAM", 0.42,
  "MAJOR", 0.43,
  "GEO", 0.43,
  "MOSES", 0.44,
  "JACOB", 0.44,
  "WILSON", 0.44,
  "ELIJAH", 0.45,
  "ISAAC", 0.45,
  "LEE", 0.45,
  "STEPHEN", 0.46,
  "HENRY", 0.46,
  "ALEX", 0.46,
  "WM", 0.46,
  "NELSON", 0.46,
  "MATHEW", 0.47,
  "EDDIE", 0.47,
  "ANDREW", 0.47,
  "CHARLIE", 0.47,
  "LEWIS", 0.47,
  "CHARLEY", 0.47,
  "DANIEL", 0.48,
  "WILLIS", 0.48,
  "DAVID", 0.48,
  "FRANK", 0.48,
  "HARRISON", 0.48,
  "CHAS", 0.48,
  "JOHN", 0.48,
  "SILAS", 0.48,
  "WILLIE", 0.49,
  "BENNIE", 0.49,
  "OSCAR", 0.49,
  "LOUIS", 0.49,
  "HERBERT", 0.49,
  "JUNIUS", 0.49,
  "RICHARD", 0.49,
  "EARNEST", 0.49,
  "PHILIP", 0.49,
  "ALBERT", 0.49,
  "GEORGE", 0.49,
  "JESSIE",  0.5,
  "NATHAN",  0.5,
  "JOHNNIE",  0.5,
  "ALLEN",  0.5,
  "THOMAS",  0.5,
  "ALEXANDER",  0.5,
  "RUFUS", 0.51,
  "JAMES", 0.51,
  "FRED", 0.51,
  "WESLEY", 0.51,
  "JESSE", 0.51,
  "ROBT", 0.51,
  "WILLIAM", 0.51,
  "ARCHIE", 0.51,
  "PAUL", 0.51,
  "LUTHER", 0.51,
  "HARVEY", 0.52,
  "LEROY", 0.52,
  "SIDNEY", 0.52,
  "EDWARD", 0.52,
  "JOSEPH", 0.52,
  "ROBERT", 0.52,
  "ARTHUR", 0.52,
  "CLARENCE", 0.52,
  "FLOYD", 0.52,
  "PERCY", 0.52,
  "PRESTON", 0.52,
  "WALTER", 0.52,
  "EDGAR", 0.53,
  "SAMUEL", 0.53,
  "OTIS", 0.53,
  "EUGENE", 0.54,
  "HORACE", 0.54,
  "CHARLES", 0.54,
  "ERNEST", 0.54,
  "LINWOOD", 0.54,
  "NORMAN", 0.54,
  "HARRY", 0.54,
  "OLIVER", 0.54,
  "CORNELIUS", 0.54,
  "RAYMOND", 0.55,
  "BENJAMIN", 0.55,
  "RUSSELL", 0.55,
  "NATHANIEL", 0.55,
  "ALFRED", 0.55,
  "CLIFTON", 0.55,
  "MELVIN", 0.55,
  "HOWARD", 0.56,
  "ROY", 0.56,
  "RALPH", 0.57,
  "EARL", 0.57,
  "LEONARD", 0.57,
  "HERMAN", 0.57,
  "LAWRENCE", 0.58,
  "THEODORE", 0.59,
  "MILTON", 0.59,
  "BERNARD", 0.61
)

## read in root bni
bni_root <- fread("../data/bni_root.csv")

## cobine 
mni_bni_comparison <- bni_root %>% 
  left_join(mullato, by = c("fname_std" = "name")) %>% 
  filter(!is.na(value)) %>% 
  filter(n_fname_root > 500) 

mullato_plot <- mni_bni_comparison %>% 
  ggplot(aes(x = bni_root, y =value, label = fname_std)) + 
  geom_text() + 
  theme_light(base_size = 20) + 
  geom_smooth(method=lm, se=FALSE) + 
  labs(x = "Root BNI",
       y = "MNI",
       title = "Comparison of Black and Mullato Name Indices")

ggsave(plot = mullato_plot, filename = "../figures/bni_mni_comparison.png", height = 10, width = 10)
