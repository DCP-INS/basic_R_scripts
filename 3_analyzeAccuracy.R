
##############################################################
########################## Packages ##########################
##############################################################

library(lme4)
library(lmerTest)
library(ggplot2)
library(Rmisc)


##############################################################
###################### Load dataFrame ########################
##############################################################

# Set working directory
dname  =  "/media/jacques/DATA/2019_statsLearning/scipts_labo" # Include your path
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

# Just for this script : load dataFrame from rtdists package
library('rtdists')
data('speed_acc')
df = speed_acc


##############################################################
##################### Explore dataFrame ######################
##############################################################

# Summary of dataFrame
summary(df)

# Remove errors
idx = df$response != c('error')
df = df[idx, ]
df$response = droplevels(df$response)

# Create an "accuracy" variable
df$acc = df$response == df$stim_cat

# Aggregate data
df_aggregate  =  aggregate(acc ~ id + condition, data = df, FUN = mean) # First average every subject
df_average  =  summarySE(measurevar = "acc", groupvars = c("condition"), data = df_aggregate) # Then average across subjects
df_average

# Plot data aggregated by subject
colors  =  c("red", "#2371AE") # Define colors by group (see http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
p1 <- ggplot(data = df_aggregate, aes(x = condition, y = acc)) +              # Basic data
  geom_boxplot(fill = colors) +                                   # Boxplot
  geom_jitter(width = 0.2, size = 1) +                              # Add points and jitter
  scale_x_discrete(name  =  "Condition") +                            # x-axis name
  scale_y_continuous(name  =  "Accuracy") +               # y-axis name
  ggtitle("Accuracy \n by condition") +              # Title
  theme_bw() +                                                  # Remove background
  theme(panel.grid.major  =  element_blank(),                     # Esthetics ++
        panel.grid.minor  =  element_blank(),
        panel.border  =  element_blank(),
        panel.background  =  element_blank(),
        plot.title  =  element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line  =  element_line(size = 0.5, colour = "black"))
p1

# Save plot
bmp('acc_condition.bmp')
p1
dev.off()


##############################################################
############# Statistics on aggregated data ##################
##############################################################

# Aggregate data
df_aggregate  =  aggregate(acc ~ id + condition, data = df, FUN = mean) # Average inside subjects
df_aggregate

# Run linear mixed model
m0 = lmer(acc ~ 1 + (1|id), data = df_aggregate) # Null model : just an intercept
m1 = lmer(acc ~ condition + (1|id), data = df_aggregate) # Model with effect
anova(m0, m1) # Compare models : take the smallest AIC
summary(m1) # Explore the best model

# Check model assumptions
# --> Are the residuals normally distributed ?
x = residuals(m1)
plot(density(x))
plot(m1)
qqnorm(x, pch = 1, frame = FALSE)
qqline(x, col = "steelblue", lwd = 2)
ks.test(x, mean(x), sd(x))


##############################################################
################# Statistics on all data #####################
##############################################################

# Run linear mixed model
m0 = glmer(acc ~ 1 + (1|id), data = df, family = "binomial") # Null model : just an intercept
m1 = glmer(acc ~ condition + (1|id), data = df, family = "binomial") # Model with effect
anova(m0, m1) # Compare models : take the smallest AIC
summary(m1) # Explore the best model

# Check model
plot(m1)



