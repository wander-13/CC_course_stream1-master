# Some Practice with Linear Models
# By: Elin Swank
# 10/06/2023

################################################################################

install.packages("agridat")
library(agridat)

# Loading the dataset from agridat
apples <- agridat::archbold.apple
head(apples)
summary(apples)