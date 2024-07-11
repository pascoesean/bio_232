##########################################################################################
##########################################################################################
# PLOTTING PLATE READER DATA: EFFEROCYTOSIS EDITION -----
##########################################################################################
##########################################################################################

# install packages if they are not already available
if(!require(readxl)){
  install.packages("readxl")
}
if(!require(tidyverse)){
  install.packages("tidyverse") # this installs dyplr, tidyr, and ggplot2     
}


# load packages that we will need
library(readxl) # used for reading xlsx files (https://readxl.tidyverse.org/)
library(dplyr) # this is a key package for data manipulation (https://dplyr.tidyverse.org/)
library(tidyr) # this is used for making our data "tidy" (https://tidyr.tidyverse.org/)
library(ggplot2) # this is used for making nice plots (https://ggplot2.tidyverse.org/)


#############################################
# LOADING DATA ----
#############################################


# this is a vector of our column names, which we will use when we read our data in
# in R, the backwards arrow will save our column vector (that we create with the function call on the right)
# in the variable that we declare on the left (`names_of_our_columns`)

names_of_our_columns <- c("PBS_10min",
                          "Dex_2um_10min",
                          "Dex_0.5um_10min",
                          "Dex_0.1um_10min",
                          "PBS_40min",
                          "Dex_2um_40min",
                          "Dex_0.5um_40min",
                          "Dex_0.1um_40min",
                          "PBS_80min",
                          "Dex_2um_80min",
                          "Dex_0.5um_80min",
                          "Dex_0.1um_80min")

# here we read from the excel file-- the first argument to this function 
# should be the filepath relative to the project you are working in. 
# for this line to work as is, you will need to have your excel file in your project directory (folder)
eff_data <- read_xlsx("eff_dex_test.xlsx", # THIS NEEDS TO BE WHATEVER YOU'VE CALLED YOUR DATA FILE
                      range = "C24:N34", # this is the range we want to read (in excel syntax) -- not the whole sheet
                      col_names = names_of_our_columns # the column names we applied above
)

eff_data <- read_xlsx("eff_dex_test.xlsx", # THIS NEEDS TO BE WHATEVER YOU'VE CALLED YOUR DATA FILE
                      range = "C24:N34", # this is the range we want to read (in excel syntax) -- not the whole sheet
                      col_names = FALSE # I don't want to give it column names yet
) |> t() |>
  as_tibble() |>
  dplyr::select(!c(V3,V4,V5)) |>
  rename("treatment" = "V1",
         "timepoint" = "V2")


# now we have read in the data! In RStudio, you can click on the 
# variable in the "environment" panel to view it

#############################################
# TIDYING DATA ----
#############################################

# unfortunately, the tidyverse tools in the R language prefer data to be in a "tidy"
# format, meaning each row corresponds to one observation 
# (right now each row corresponds to 8 observations, one from each condition)

# to do this, we use tidyr to "pivot" our data to longer format:
# (I save this to a new variable, called "lps_data_pivoted")

eff_data_pivoted <- pivot_longer(data = eff_data, 
                                 cols = V6:V11, # which columns do we want to pivot?
                                 names_to = NULL, # what do we want to name the new column with the previous columns' names?
                                 values_to = "fluorescence_intensity" # what do we want to name the new column with the previous columns' values?
) |>
  mutate(fluorescence_intensity = as.double(fluorescence_intensity))

# before we plot our data, we are going to compute the mean and standard deviation,
# so we can plot these summary statistics instead of individual datapoints

# here I use `|>` (aka %>%) which is known as the pipe operator: 
# it "pipes" data from one function through to the next
# the final output from the piped sequence (here our summarize call) is then stored in `lps_summary`
# see also: https://r4ds.hadley.nz/data-transform.html#sec-the-pipe
# https://www.reddit.com/r/rstats/comments/vbd6jq/piping_in_r_is_like_baking/

eff_summary <- eff_data_pivoted |>
  # first, group data by condition, so both summary statistics are computed 
  # groupby treatment and timepoint
  group_by(treatment, timepoint) |> 
  # now specify which summary functions you want to use (here mean and standard deviation), and their names:
  summarize(mean_fluorescence_intensity = mean(fluorescence_intensity), # here the new column name we want is `mean_of_data`, and the function we want summarize to call is `mean()`
            standard_dev = sd(fluorescence_intensity)) # we want to compute both mean and standard dev (sd) of the `fluorescence_intensity` column


# now our data is ready to be used with ggplot2:

#############################################
# BASE PLOT ----
#############################################

# ggplot works by building layers on top of each other-- we start with data,
# then add x/y axes, colors, sizes, etc (aesthetics); then points/bars/lines/etc (geometries)

# there are a lot of internet resources to help with using this, e.g.
# https://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html (great overview/beginner friendly)
# https://www.youtube.com/watch?v=h29g21z0a68 (very long, but good overview of how/why of ggplot from a key contributor)
# https://ggplot2-book.org/ 

# first, initialize the ggplot object:
ggplot(data = eff_summary, # layer 1: data
       mapping = aes(x = timepoint, y = mean_fluorescence_intensity, color = treatment, group = treatment)) + # layer 2 is aesthetics, here x/y axis
  geom_line()

# this worked, but it was kinda ugly. let's use some ggplot tools to clean it up:

#############################################
# BETTER PLOT ----
#############################################
ggplot(data = eff_summary, # layer 1: data
       mapping = aes(x = timepoint, y = mean_fluorescence_intensity)) + # layer 2 is aesthetics, here x/y axis
  geom_line(aes(color = treatment, group = treatment)) +
  # add x/y axis + title labels with `labs()`
  labs(x = "Time", y = "Mean fluorescence Intensity", 
       title = "Efferocytosis of Dead Cells") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))

# this is pretty good, and probably good to shoot for for the notebook; relatively easy while also clean/nice to read

# you can export the plot with `ggsave()` or by just clicking Export on the plots panel in RStudio
# might want to adjust aspect ratio before saving/copying to clipboard

# we can also make boxplots to better show the data:

ggplot(data = eff_data_pivoted, # layer 1: data
       mapping = aes(x = timepoint, y = fluorescence_intensity, color = treatment)) + # layer 2 is aesthetics, here x/y axis
  geom_boxplot() + # or use geom_violin
  # add x/y axis + title labels with `labs()`
  labs(x = "Time", y = "Mean fluorescence Intensity", 
       title = "Efferocytosis of Dead Cells") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))


# you can take this a step further though if you want to play around with it:

#############################################
# BONUS PLOT ----
#############################################

# here, I'm setting my own colors and column orders. 

vals <- c("Dex 0.1 uM" = "#a7c957", 
          "Dex 0.5 uM" = "#6a994e",
          "Dex 2 uM" = "#386641", 
          "PBS" = "#B48018")

lims <- c("Dex 0.1 uM", 
          "Dex 0.5 uM",
          "Dex 2 uM", 
          "PBS")

labes <- c("Dex 0.1 µM", 
           "Dex 0.5 µM",
           "Dex 2 µM", 
           "PBS")

# this version is a bit neater, but you can play around with using 
# points vs boxplots; chose whatever you think represents the data best



ggplot() +
  geom_point(data = eff_data_pivoted, # here I need to give `geom_point` different data than I gave `geom_line`
              mapping = aes(x = timepoint, y = fluorescence_intensity, color = treatment),
             position = position_jitterdodge(jitter.width = 0.1), size = 1) +
  geom_line(data = eff_summary,
            mapping = aes(x = timepoint, y = mean_fluorescence_intensity,
                          color = treatment, group = treatment), linewidth = 0.8) + 
  scale_color_manual(values = vals,
                    labels = labes,
                    limits = lims) +
  labs(y = "Fluorescence Intensity", color = "Treatment",
       x = "Timepoint",
       title = "Efferocytosis of Dead Cells") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.background = element_rect(color = "black"))


ggplot() +
  geom_boxplot(data = eff_data_pivoted, # here I need to give `geom_point` different data than I gave `geom_line`
             mapping = aes(x = timepoint, y = fluorescence_intensity, color = treatment)) +
  geom_line(data = eff_summary,
            mapping = aes(x = timepoint, y = mean_fluorescence_intensity,
                          color = treatment, group = treatment), linewidth = 1) + 
  scale_color_manual(values = vals,
                     labels = labes,
                     limits = lims) +
  labs(y = "Fluorescence Intensity", color = "Treatment",
       x = "Timepoint",
       title = "Efferocytosis of Dead Cells") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.background = element_rect(color = "black"))



