##########################################################################################
##########################################################################################
# PLOTTING PLATE READER DATA -----
##########################################################################################
##########################################################################################

# any text following a hashtag is "commented out"-- it won't be read as code by the interpreter,
# so it's good to use to add notes for others (or yourself later on)

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

names_of_our_columns <- c("no_beads", # here I am using the function c() to create a vector with these entries
                          "beads_dex_2um",
                          "beads_dex_0.5um",
                          "beads_dex_0.1um",
                          "beads_res_0.5nm",
                          "beads_res_0.25nm",
                          "beads_res_0.05nm",
                          "beads_PBS")

# here we read from the excel file-- the first argument to this function 
# should be the filepath relative to the project you are working in. 
# for this line to work as is, you will need to have your excel file in your project directory (folder)
lps_data <- read_xlsx("lps_testdata.xlsx", # THIS NEEDS TO BE WHATEVER YOU'VE CALLED YOUR DATA FILE
                      range = "E29:L32", # this is the range we want to read (in excel syntax) -- not the whole sheet
                      col_names = names_of_our_columns # the column names we applied above
                      )


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

lps_data_pivoted <- pivot_longer(data = lps_data, 
                                 cols = everything(), # which columns do we want to pivot?
                                 names_to = "condition", # what do we want to name the new column with the previous columns' names?
                                 values_to = "fluorescence_intensity" # what do we want to name the new column with the previous columns' values?
                                 )

# before we plot our data, we are going to compute the mean and standard deviation,
# so we can plot these summary statistics instead of individual datapoints

# here I use `|>` (aka %>%) which is known as the pipe operator: 
# it "pipes" data from one function through to the next
# the final output from the piped sequence (here our summarize call) is then stored in `lps_summary`
# see also: https://r4ds.hadley.nz/data-transform.html#sec-the-pipe
# https://www.reddit.com/r/rstats/comments/vbd6jq/piping_in_r_is_like_baking/

lps_summary <- lps_data_pivoted |>
  # first, group data by condition, so both summary statistics are computed 
  # for each group in the condition column
  group_by(condition) |> 
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
ggplot(data = lps_summary, # layer 1: data
       mapping = aes(x = condition, y = mean_fluorescence_intensity)) + # layer 2 is aesthetics, here x/y axis
  geom_col() + # now add a layer that is our bar graph (created by geom_col)
  geom_errorbar(aes(ymin = mean_fluorescence_intensity - standard_dev, 
                    ymax = mean_fluorescence_intensity + standard_dev)) # add another layer created by geom col-- this requires extra aesthetics


# this worked, but it was kinda ugly. let's use some ggplot tools to clean it up:

#############################################
# BETTER PLOT ----
#############################################

ggplot(data = lps_summary, # layer 1: data
       mapping = aes(x = condition, y = mean_fluorescence_intensity)) + 
  geom_col(width = 0.8, fill = "royalblue") + 
  geom_errorbar(aes(ymin = mean_fluorescence_intensity - standard_dev, 
                    ymax = mean_fluorescence_intensity + standard_dev),
                width = 0.3) +
  # add x/y axis + title labels with `labs()`
  labs(x = "Treatment", y = "Mean fluorescence Intensity", 
       title = "Phagocytosis of Beads (LPS Treatment)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))

# this is pretty good, and probably good to shoot for for the notebook; relatively easy while also clean/nice to read

# you can export the plot with `ggsave()` or by just clicking Export on the plots panel in RStudio
# might want to adjust aspect ratio before saving/copying to clipboard


# you can take this a step further though if you want to play around with it:

#############################################
# BONUS PLOT ----
#############################################

# here, I'm setting my own colors and column orders. 

vals <- c("no_beads" = "#FDF0D5", 
          "beads_PBS" = "#F4DDB0",
          "beads_dex_0.1um" = "#FF8585", 
          "beads_dex_0.5um" = "#FF3333", 
          "beads_dex_2um" = "#CC0000",
          "beads_res_0.05nm" = "#9CBED3", 
          "beads_res_0.25nm" = "#669BBC", 
          "beads_res_0.5nm" = "#447A9C")

lims <- c("no_beads", 
          "beads_PBS",
          "beads_dex_0.1um",
          "beads_dex_0.5um",
          "beads_dex_2um",
          "beads_res_0.05nm",
          "beads_res_0.25nm",
          "beads_res_0.5nm")

labes <- c("No beads", 
           "PBS + beads",
           "Dex 0.1µm",
           "Dex 0.5µm",
           "Dex 2µm",
           "Resolvin 0.05nm",
           "Resolvin 0.25nm",
           "Resolvin 0.5nm")

# this version is pretty and neat; but not super colorblind friendly

ggplot(data = lps_summary, 
       mapping = aes(x = condition, y = mean_fluorescence_intensity, fill = condition)) + 
  geom_col(width = 0.8) + 
  geom_errorbar(aes(ymin = mean_fluorescence_intensity - standard_dev, 
                    ymax = mean_fluorescence_intensity + standard_dev),
                width = 0.3) +
  scale_fill_manual(values = vals,
                    labels = labes,
                    limits = lims) +
  scale_x_discrete(limits = lims,
                   labels = labes) + 
  labs(y = "Mean fluorescence Intensity", fill = "Treatment",
       title = "Phagocytosis of Beads (LPS Treatment)",
       caption = "error bars represent one standard deviation") + 
  theme_classic() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.background = element_rect(color = "black"))

# redundant axis labeling can also be good:

ggplot(data = lps_summary, 
       mapping = aes(x = condition, y = mean_fluorescence_intensity, fill = condition)) + 
  geom_col(width = 0.8) + 
  geom_errorbar(aes(ymin = mean_fluorescence_intensity - standard_dev, 
                    ymax = mean_fluorescence_intensity + standard_dev),
                width = 0.3) +
  scale_fill_manual(values = vals,
                    labels = labes,
                    limits = lims) +
  scale_x_discrete(limits = lims,
                   labels = labes) + 
  labs(x = "Treatment", y = "Mean fluorescence Intensity", 
       title = "Phagocytosis of Beads (LPS Treatment)",
       caption = "error bars represent one standard deviation") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.title = element_blank())
