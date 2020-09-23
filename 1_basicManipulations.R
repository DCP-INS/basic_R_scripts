
##############################################################
##################### Install packages #######################
##############################################################

# Install a package
install.packages("lme4")

# Load a package
library(lme4)

##############################################################
##################### Organize dataFrame #####################
##############################################################

# You data frame should always be in a "long" format, i.e. eac
# h line is one observation. 

# You should have 
# subject sex condition measurement
# 1   M   control         7.9
# 1   M     cond1        12.3
# 1   M     cond2        10.7
# 2   F   control         6.3
# 2   F     cond1        10.6
# 2   F     cond2        11.1
# 3   F   control         9.5
# 3   F     cond1        13.1
# 3   F     cond2        13.8
# 4   M   control        11.5
# 4   M     cond1        13.4
# 4   M     cond2        12.9

# and NOT : 
# subject sex control cond1 cond2
# 1   M     7.9  12.3  10.7
# 2   F     6.3  10.6  11.1
# 3   F     9.5  13.1  13.8
# 4   M    11.5  13.4  12.9

# To go from wide to long, you can :
library(tidyr)  # You need the "tidyr" library
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
df_long = gather(df_bad, key=condition, value=measurement, 
                 c('control, cond1, cond2'), factor_key=TRUE)


##############################################################
##################### Load a dataFrame #######################
##############################################################

# Set working directory
dname = "/media/jacques/DATA/2019_statsLearning" # Include your path
setwd(dname)

# Load using read.csv for CSV files
fname = "dataFrame.csv" # Include your file name
sep = "," # By default the separator is ","
df = read.csv(fname, sep = sep)

# Or load using readxlsx for XLS files
fname = "dataFrame.xls" # Include your file name
sheet = 1
df = read.xls(fname, sheet = sheet)

# Or load using read.xlsx for XLSX files
library(xlsx) # You need the "xlsx" library
fname = "dataFrame.xlsx" # Include your file name
df = read.xlsx(fname, sheetName = sheetName)


##############################################################
######### For this tutorial, a preloaded dataset #############
##############################################################

library(MASS)
data("birthwt")
df = birthwt


##############################################################
##################### Explore dataFrame ######################
##############################################################

# Access lines
df[4,] # Line
df[1:10,] # Multiple lines
df[df$age < 16,] # Specific lines

# Access one column
df[,2]
df$age

# Access one data point
df[4, 2]
df[4,]$age

# Summary of the model columns
summary(df) # Print summary of all variables
head(df, 4) # Plot the first 4 lines

# Get data class
class(df$race)

# From class numeric to class factor
df$race_factor = factor(df$race, labels = c("white", "black", "other"))
df$smoke_factor = factor(df$smoke, labels = c('non-smoker', 'smoker'))
# levels(df$condition_factor) = c('Control', 'Experimental') # Change factor order

# From class factor to class numeric
df$session = as.numeric(as.character(df$session)) # Example


##############################################################
#################### Summarize dataFrame #####################
##############################################################

# Aggregate data (average)
FUN = mean # Aggregate using averaging
df_average = aggregate(bwt ~ low, data=df, FUN=FUN)
df_average

# Using summarySE
library(Rmisc) # You need the "Rmisc" library
df_average = summarySE(measurevar="bwt", groupvars=c("smoke_factor"), data=df)
df_average


##############################################################
################# Basic plotting functions ###################
##############################################################

# Basing plotting
plot(df_average$smoke_factor, df_average$bwt)

# Better plotting
library(ggplot2)  # You need the "ggplot2" library
colors = c("red", "#2371AE") # Define colors by group (see http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
p <- ggplot(data=df, aes(x=smoke_factor, y=bwt)) +              # Basic data
  geom_boxplot(fill=colors) +                                   # Boxplot
  geom_jitter(width=0.2, size=1) +                              # Add points and jitter
  scale_x_discrete(name = "Smoking status") +                            # x-axis name
  scale_y_continuous(name = "Birth weight (g)") +               # y-axis name
  ggtitle("Birth weight \n by smoking status") +              # Title
  theme_bw() +                                                  # Remove background
  theme(panel.grid.major = element_blank(),                     # Esthetics ++
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.line = element_line(size=0.5, colour="black"))
p

# Save plot
bmp('bwt_smoke.bmp')
p
dev.off()


