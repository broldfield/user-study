# Long-linear analysis using the prefmod R package - Comparing Map Types x GBR Familiarity

library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(prefmod)
library(ggplot2)
library("RColorBrewer")

source("LLA_functions.r")



# setting up for base plotting
par(cex.main = 2,   # title text size
    cex.lab  = 2,   # axis title size ("Estimate")
    cex.axis = 2)   

mypalette <- brewer.pal(5, "Dark2")

# Read in Data
#  df_ans6 <- read.csv("<preference_data>")   

# hard wired naming convention for maps (1992, 1999, 2001, 2002, and 2005 (in that order).)
Questions1 <- c("Q1", "Q2", "Q3", "Q4", "Q5")
Questions2 <- c("Q1B", "Q2B", "Q3B", "Q4B", "Q5B")


#----------------- Run code for Map 1992 ----------------------------#

df_1992 <- extract_pref_data(i = 1, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2, GBR = TRUE)

mod_1992 <- pattR.fit(df_1992, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)
w_1992 <- patt.worth(mod_1992)
colnames(w_1992) <- c("Unfamiliar:Univariate", "Familiar:Univariate", "Unfamiliar:Bivariate", 
                 "Familiar:Bivariate")


# printing out summary table
print(mod_1992)

# Testing for differences
mod0 <- pattR.fit(df_1992, nitems = 5, formel = ~1, elim = ~Uncert*GBF)   # no covariate effects at all

mod1 <- pattR.fit(df_1992, nitems = 5, formel = ~Uncert, elim = ~Uncert*GBF)   # MapType only

mod2 <- pattR.fit(df_1992, nitems = 5, formel = ~GBF, elim = ~Uncert*GBF)   # Familiarity only

mod3 <- pattR.fit(df_1992, nitems = 5, formel = ~Uncert+GBF, elim = ~Uncert*GBF)   # both, additive

mod4 <- pattR.fit(df_1992, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)   # full interaction

#The likelihood ratio test comparing them 


# 1. Is the interaction needed?
lr_1992_interaction <- LRtest_prefmod(mod3, mod4)   # additive vs full interaction

# 2. Given no interaction, does MapType matter?
lr_1992_maptype <- LRtest_prefmod(mod2, mod3)   # GBR-only vs GBR+Uncert

# 3. Given no interaction, does familiarity matter?
lr_1992_familiarity <- LRtest_prefmod(mod1, mod3)   # Uncert-only vs Uncert+GBR

{if(lr_1992_interaction$pval <= 0.05){
  lrt_all <-  lr_1992_interaction
}
  else{
   lrt_all <- bind_rows(
      Interaction = lr_1992_interaction,
      MapType     = lr_1992_maptype,
     Familiarity = lr_1992_familiarity,
     .id = "Term"
)
  }
}


lrt_all

#----------------- Run code for Map 1999 ----------------------------#

df_1999 <- extract_pref_data(i = 2, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_1999 <- pattR.fit(df_1999, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)
w_1999 <- patt.worth(mod_1999)
colnames(w_1999) <- c("Unfamiliar:Univariate", "Familiar:Univariate", "Unfamiliar:Bivariate", 
                 "Familiar:Bivariate")

# printing out summary table
print(mod_1999)

# Testing for differences
mod0 <- pattR.fit(df_1999, nitems = 5, formel = ~1, elim = ~Uncert*GBF)   # no covariate effects at all

mod1 <- pattR.fit(df_1999, nitems = 5, formel = ~Uncert, elim = ~Uncert*GBF)   # MapType only

mod2 <- pattR.fit(df_1999, nitems = 5, formel = ~GBF, elim = ~Uncert*GBF)   # Familiarity only

mod3 <- pattR.fit(df_1999, nitems = 5, formel = ~Uncert+GBF, elim = ~Uncert*GBF)   # both, additive

mod4 <- pattR.fit(df_1999, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)   # full interaction

#The likelihood ratio test comparing them 


# 1. Is the interaction needed?
lr_1999_interaction <- LRtest_prefmod(mod3, mod4)   # additive vs full interaction

# 2. Given no interaction, does MapType matter?
lr_1999_maptype <- LRtest_prefmod(mod2, mod3)   # GBR-only vs GBR+Uncert

# 3. Given no interaction, does familiarity matter?
lr_1999_familiarity <- LRtest_prefmod(mod1, mod3)   # Uncert-only vs Uncert+GBR

{if(lr_1999_interaction$pval <= 0.05){
  lrt_all <-  lr_1999_interaction
}
  else{
   lrt_all <- bind_rows(
      Interaction = lr_1999_interaction,
      MapType     = lr_1999_maptype,
     Familiarity = lr_1999_familiarity,
     .id = "Term"
)
  }
}


lrt_all


#----------------- Run code for Map 2001 ----------------------------#

df_2001<- extract_pref_data(i = 3, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_2001 <- pattR.fit(df_2001, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)
w_2001 <- patt.worth(mod_2001)
colnames(w_2001) <- c("Unfamiliar:Univariate", "Familiar:Univariate", "Unfamiliar:Bivariate", 
                 "Familiar:Bivariate")



# printing out summary table
print(mod_2001)

# Testing for differences
mod0 <- pattR.fit(df_2001, nitems = 5, formel = ~1, elim = ~Uncert*GBF)   # no covariate effects at all

mod1 <- pattR.fit(df_2001, nitems = 5, formel = ~Uncert, elim = ~Uncert*GBF)   # MapType only

mod2 <- pattR.fit(df_2001, nitems = 5, formel = ~GBF, elim = ~Uncert*GBF)   # Familiarity only

mod3 <- pattR.fit(df_2001, nitems = 5, formel = ~Uncert+GBF, elim = ~Uncert*GBF)   # both, additive

mod4 <- pattR.fit(df_2001, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)   # full interaction

#The likelihood ratio test comparing them 


# 1. Is the interaction needed?
lr_2001_interaction <- LRtest_prefmod(mod3, mod4)   # additive vs full interaction

# 2. Given no interaction, does MapType matter?
lr_2001_maptype <- LRtest_prefmod(mod2, mod3)   # GBR-only vs GBR+Uncert

# 3. Given no interaction, does familiarity matter?
lr_2001_familiarity <- LRtest_prefmod(mod1, mod3)   # Uncert-only vs Uncert+GBR

{if(lr_2001_interaction$pval <= 0.05){
  lrt_all <-  lr_2001_interaction
}
  else{
   lrt_all <- bind_rows(
      Interaction = lr_2001_interaction,
      MapType     = lr_2001_maptype,
     Familiarity = lr_2001_familiarity,
     .id = "Term"
)
  }
}


lrt_all


#----------------- Run code for Map 2002 ----------------------------#

df_2002<- extract_pref_data(i = 4, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_2002 <- pattR.fit(df_2002, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)
w_2002 <- patt.worth(mod_2002)
colnames(w_2002) <- c("Unfamiliar:Univariate", "Familiar:Univariate", "Unfamiliar:Bivariate", 
                 "Familiar:Bivariate")


# printing out summary table
print(mod_2002)

# Testing for differences
mod0 <- pattR.fit(df_2002, nitems = 5, formel = ~1, elim = ~Uncert*GBF)   # no covariate effects at all

mod1 <- pattR.fit(df_2002, nitems = 5, formel = ~Uncert, elim = ~Uncert*GBF)   # MapType only

mod2 <- pattR.fit(df_2002, nitems = 5, formel = ~GBF, elim = ~Uncert*GBF)   # Familiarity only

mod3 <- pattR.fit(df_2002, nitems = 5, formel = ~Uncert+GBF, elim = ~Uncert*GBF)   # both, additive

mod4 <- pattR.fit(df_2002, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)   # full interaction

#The likelihood ratio test comparing them 


# 1. Is the interaction needed?
lr_2002_interaction <- LRtest_prefmod(mod3, mod4)   # additive vs full interaction

# 2. Given no interaction, does MapType matter?
lr_2002_maptype <- LRtest_prefmod(mod2, mod3)   # GBR-only vs GBR+Uncert

# 3. Given no interaction, does familiarity matter?
lr_2002_familiarity <- LRtest_prefmod(mod1, mod3)   # Uncert-only vs Uncert+GBR

{if(lr_2002_interaction$pval <= 0.05){
  lrt_all <-  lr_2002_interaction
}
  else{
   lrt_all <- bind_rows(
      Interaction = lr_2002_interaction,
      MapType     = lr_2002_maptype,
     Familiarity = lr_2002_familiarity,
     .id = "Term"
)
  }
}


lrt_all



#----------------- Run code for Map 2005 ----------------------------#


df_2005<- extract_pref_data(i = 5, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_2005 <- pattR.fit(df_2005, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)
w_2005 <- patt.worth(mod_2005)
colnames(w_2005) <- c("Unfamiliar:Univariate", "Familiar:Univariate", "Unfamiliar:Bivariate", 
                 "Familiar:Bivariate")



# printing out summary table
print(mod_2005)

# Testing for differences
mod0 <- pattR.fit(df_2005, nitems = 5, formel = ~1, elim = ~Uncert*GBF)   # no covariate effects at all

mod1 <- pattR.fit(df_2005, nitems = 5, formel = ~Uncert, elim = ~Uncert*GBF)   # MapType only

mod2 <- pattR.fit(df_2005, nitems = 5, formel = ~GBF, elim = ~Uncert*GBF)   # Familiarity only

mod3 <- pattR.fit(df_2005, nitems = 5, formel = ~Uncert+GBF, elim = ~Uncert*GBF)   # both, additive

mod4 <- pattR.fit(df_2005, nitems = 5, formel = ~Uncert*GBF, elim = ~Uncert*GBF)   # full interaction

#The likelihood ratio test comparing them 


# 1. Is the interaction needed?
lr_2005_interaction <- LRtest_prefmod(mod3, mod4)   # additive vs full interaction

# 2. Given no interaction, does MapType matter?
lr_2005_maptype <- LRtest_prefmod(mod2, mod3)   # GBR-only vs GBR+Uncert

# 3. Given no interaction, does familiarity matter?
lr_2005_familiarity <- LRtest_prefmod(mod1, mod3)   # Uncert-only vs Uncert+GBR

{if(lr_2005_interaction$pval <= 0.05){
  lrt_all <-  lr_2005_interaction
}
  else{
   lrt_all <- bind_rows(
      Interaction = lr_2005_interaction,
      MapType     = lr_2005_maptype,
     Familiarity = lr_2005_familiarity,
     .id = "Term"
)
  }
}


lrt_all
