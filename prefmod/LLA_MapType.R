# Long-linear analysis using the prefmod R package - Comparing Map Types

library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(prefmod)
library(ggplot2)
library("RColorBrewer")
library(ggrepel)

source("LLA_functions.r") # reading in relevant functions


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

df_1992 <- extract_pref_data(i = 1, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_1992 <- pattR.fit(df_1992, nitems = 5, formel = ~Uncert, elim = ~Uncert)
w_1992 <- patt.worth(mod_1992)
colnames(w_1992) <- c("Univariate", "Bivariate")

# plotting
plot_wmat(w_1992, log = "y", main = "", cex = 2, pcex = 3, tcex = 2, pcol = mypalette, lwd = 2)

plot_wmat_gg(x = w_1992, main = "1992/1993", ylab = "Worth Estimates",
                          pcol = NULL, pcex = 7, tcex = 8, lwd = 1.8,
                          log_y = TRUE, label_pos = "both", y_range = c(0.03, 0.6),
                        axis_title_size = 25, axis_text_size = 25, x_pad = 0.1,
                      main_size = 35)

# printing out summary table
print(mod_1992)

# Testing for differences
mod0 <- pattR.fit(df_1992, nitems = 5, formel = ~1,          
           elim = ~Uncert)   # no MapType effect
mod1 <- pattR.fit(df_1992, nitems = 5, formel = ~Uncert, 
     elim = ~Uncert)   # with MapType effect

#The likelihood ratio test comparing them has:

# H0: the simpler model (mod0) holds, i.e. a single common worth vector describes the 
#     preference pattern equally well for every MapType group; the item worths do not depend on MapType.
# H1: the fuller model (mod1) is needed, i.e. at least one item's estimated worth differs 
#     across MapType groups; the preference pattern for the items depends on MapType.
lr_1992 <- LRtest_prefmod(mod0, mod1)
lr_1992


#----------------- Run code for Map 1999 ----------------------------#
df_1999 <- extract_pref_data(i = 2, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_1999 <- pattR.fit(df_1999, nitems = 5, formel = ~Uncert, elim = ~Uncert)
w_1999 <- patt.worth(mod_1999)
colnames(w_1999) <- c("Univariate", "Bivariate")

# plotting
plot_wmat(w_1999, log = "y", main = "", cex = 2, pcex = 3, tcex = 2, pcol = mypalette, lwd = 2)

plot_wmat_gg(x = w_1999, main = "1999/2000", ylab = "Worth Estimates",
                          pcol = NULL, pcex = 7, tcex = 8, lwd = 1.8,
                          log_y = TRUE, label_pos = "both", y_range = c(0.03, 0.6),
                        axis_title_size = 25, axis_text_size = 25, x_pad = 0.1,
                      main_size = 35)

# printing out summary table
print(mod_1999)

# Testing for differences
mod0 <- pattR.fit(df_1999, nitems = 5, formel = ~1,          
           elim = ~Uncert)   # no MapType effect
mod1 <- pattR.fit(df_1999, nitems = 5, formel = ~Uncert, 
     elim = ~Uncert)   # with MapType effect

#The likelihood ratio test comparing them has:

# H0: the simpler model (mod0) holds, i.e. a single common worth vector describes the 
#     preference pattern equally well for every MapType group; the item worths do not depend on MapType.
# H1: the fuller model (mod1) is needed, i.e. at least one item's estimated worth differs 
#     across MapType groups; the preference pattern for the items depends on MapType.
lr_1999 <- LRtest_prefmod(mod0, mod1)
lr_1999

#----------------- Run code for Map 2001 ----------------------------#
df_2001 <- extract_pref_data(i = 3, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_2001 <- pattR.fit(df_2001, nitems = 5, formel = ~Uncert, elim = ~Uncert)
w_2001 <- patt.worth(mod_2001)
colnames(w_2001) <- c("Univariate", "Bivariate")

# plotting
plot_wmat(w_2001, log = "y", main = "", cex = 2, pcex = 3, tcex = 2, pcol = mypalette, lwd = 2)


plot_wmat_gg(x = w_2001, main = "2001/2002", ylab = "Worth Estimates",
                          pcol = NULL, pcex = 7, tcex = 8, lwd = 1.8,
                          log_y = TRUE, label_pos = "both", y_range = c(0.03, 0.6),
                        axis_title_size = 25, axis_text_size = 25, x_pad = 0.1,
                      main_size = 35)
# printing out summary table
print(mod_2001)

# Testing for differences
mod0 <- pattR.fit(df_2001, nitems = 5, formel = ~1,          
           elim = ~Uncert)   # no MapType effect
mod1 <- pattR.fit(df_2001, nitems = 5, formel = ~Uncert, 
     elim = ~Uncert)   # with MapType effect

#The likelihood ratio test comparing them has:

# H0: the simpler model (mod0) holds, i.e. a single common worth vector describes the 
#     preference pattern equally well for every MapType group; the item worths do not depend on MapType.
# H1: the fuller model (mod1) is needed, i.e. at least one item's estimated worth differs 
#     across MapType groups; the preference pattern for the items depends on MapType.
lr_2001 <- LRtest_prefmod(mod0, mod1)
lr_2001

#----------------- Run code for Map 2002 ----------------------------#
df_2002 <- extract_pref_data(i = 4, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_2002 <- pattR.fit(df_2002, nitems = 5, formel = ~Uncert, elim = ~Uncert)
w_2002 <- patt.worth(mod_2002)
colnames(w_2002) <- c("Univariate", "Bivariate")

# plotting
plot_wmat(w_2002, log = "y", main = "", cex = 2, pcex = 3, tcex = 2, pcol = mypalette, lwd = 2)

plot_wmat_gg(x = w_2002, main = "2002/2003", ylab = "Worth Estimates",
                          pcol = NULL, pcex = 7, tcex = 8, lwd = 1.8,
                          log_y = TRUE, label_pos = "both", y_range = c(0.03, 0.6),
                        axis_title_size = 25, axis_text_size = 25, x_pad = 0.1,
                      main_size = 35)

# printing out summary table
print(mod_2002)

# Testing for differences
mod0 <- pattR.fit(df_2002, nitems = 5, formel = ~1,          
           elim = ~Uncert)   # no MapType effect
mod1 <- pattR.fit(df_2002, nitems = 5, formel = ~Uncert, 
     elim = ~Uncert)   # with MapType effect

#The likelihood ratio test comparing them has:

# H0: the simpler model (mod0) holds, i.e. a single common worth vector describes the 
#     preference pattern equally well for every MapType group; the item worths do not depend on MapType.
# H1: the fuller model (mod1) is needed, i.e. at least one item's estimated worth differs 
#     across MapType groups; the preference pattern for the items depends on MapType.
lr_2002 <- LRtest_prefmod(mod0, mod1)
lr_2002


#----------------- Run code for Map 2002 ----------------------------#
df_2005 <- extract_pref_data(i = 5, df_ans6 = df_ans6, Questions1 = Questions1, Questions2 = Questions2)

mod_2005 <- pattR.fit(df_2005, nitems = 5, formel = ~Uncert, elim = ~Uncert)
w_2005 <- patt.worth(mod_2005)
colnames(w_2005) <- c("Univariate", "Bivariate")

# plotting
plot_wmat(w_2005, log = "y", main = "", cex = 2, pcex = 3, tcex = 2, pcol = mypalette, lwd = 2)

plot_wmat_gg(x = w_2005, main = "2005/2006", ylab = "Worth Estimates",
                          pcol = NULL, pcex = 7, tcex = 8, lwd = 1.8,
                          log_y = TRUE, label_pos = "both", y_range = c(0.03, 0.6),
                        axis_title_size = 25, axis_text_size = 25, x_pad = 0.1,
                      main_size = 35)

# printing out summary table
print(mod_2005)


# Testing for differences
mod0 <- pattR.fit(df_2005, nitems = 5, formel = ~1,          
           elim = ~Uncert)   # no MapType effect
mod1 <- pattR.fit(df_2005, nitems = 5, formel = ~Uncert, 
     elim = ~Uncert)   # with MapType effect

#The likelihood ratio test comparing them has:

# H0: the simpler model (mod0) holds, i.e. a single common worth vector describes the 
#     preference pattern equally well for every MapType group; the item worths do not depend on MapType.
# H1: the fuller model (mod1) is needed, i.e. at least one item's estimated worth differs 
#     across MapType groups; the preference pattern for the items depends on MapType.
lr_2005 <- LRtest_prefmod(mod0, mod1)
lr_2005
