# AESTHETIC AND INFORMATIVE DATA VISUALIZATION:
# USING GGPLOT2 TO COMMUNICATE YOUR RESULTS
# AUTHOR: ELIN SWANK
# 10/06/2023
# elinswank@gmail.com

################################################################################

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

################################################################################

# MAKING DIFFERENT PLOTS WITH GGPLOT2

# Import data from the Living Planet Index - population trends of vertebrate species from 1970 to 2014
LPI <- read.csv("06_Data_vis_1/LPIdata_CC.csv")

# Reshape data into long form
# By adding 9:53, we select columns 9 to 53, the ones for the different years of monitoring
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)
LPI2$year <- parse_number(LPI2$year)
# When manipulating data it's always good check if the variables have stayed how we want them
# Use the str() function
str(LPI2)

# Abundance is also a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)
unique(LPI2$Common.Name)

vulture <- filter(LPI2, Common.Name == "Griffon vulture / Eurasian griffon")
head(vulture)
# There are a lot of NAs in this dataframe, so we will get rid of the empty rows using na.omit()
vulture <- na.omit(vulture)

################################################################################

# HISTOGRAMS TO VISUALIZE DATA DISTRIBUTION

# With base R graphics
base_hist <- hist(vulture$abundance)

# With ggplot2: creating graph with no brackets (must call object to display)
vulture_hist <- ggplot(vulture, aes(x = abundance))  +
  geom_histogram() 
# Calling the object to display it in the plot viewer
vulture_hist

# With brackets: you create and display the graph at the same time
(vulture_hist <- ggplot(vulture, aes(x = abundance))  +
    geom_histogram())


# For another way to check whether your data is normally distributed, you can 
# either create density plots using package ggpubr and command ggdensity(), 
# OR use functions qqnorm() and qqline()

# BEGIN BEAUTIFICATION
(vulture_hist <- ggplot(vulture, aes(x = abundance)) +                
    geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept = mean(abundance)),                       # Adding a line for mean abundance
               colour = "red", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_bw() +                                                      # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nGriffon vulture abundance")  +                              # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                          # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))              # Putting a 1 cm margin around the plot

# We can see from the histogram that the data are very skewed 
# - a typical distribution of count abundance data