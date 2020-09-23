
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

# Plot distribution of reaction times
plot(density(df$rt))

# Something is wrong ... remove outliers, i.e. 0.15 > rt > 1.5 seconds
idx  =  df$rt < 1.500 & df$rt > 0.150 
sum(idx)/length(df[,1]) # 3 % of data removed
plot(density(df[idx, ]$rt))

# Remove outliers
df = df[idx,]

# Aggregate data
df_aggregate  =  aggregate(rt ~ id + condition, data = df[idx, ], FUN = mean) # First average every subject
df_average  =  summarySE(measurevar = "rt", groupvars = c("condition"), data = df_aggregate) # Then average across subjects
df_average

# Plot data aggregated by subject
colors  =  c("red", "#2371AE") # Define colors by group (see http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
p1 <- ggplot(data = df_aggregate, aes(x = condition, y = rt)) +              # Basic data
  geom_boxplot(fill = colors) +                                   # Boxplot
  geom_jitter(width = 0.2, size = 1) +                              # Add points and jitter
  scale_x_discrete(name  =  "Condition") +                            # x-axis name
  scale_y_continuous(name  =  "Reaction time (s)") +               # y-axis name
  ggtitle("Reaction time \n by condition") +              # Title
  theme_bw() +                                                  # Remove background
  theme(panel.grid.major  =  element_blank(),                     # Esthetics ++
        panel.grid.minor  =  element_blank(),
        panel.border  =  element_blank(),
        panel.background  =  element_blank(),
        plot.title  =  element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line  =  element_line(size = 0.5, colour = "black"))
p1

# Save plot
bmp('rt_condition.bmp')
p1
dev.off()

# Plot data by subject
p2 <- ggplot(data = df, aes(x = condition, y = rt)) +              # Basic data
  geom_boxplot(aes(fill = condition), outlier.size = 0.1) +        # Boxplot
  geom_jitter(width = 0.2, size = 0.1) +                              # Add points and jitter
  scale_x_discrete(name  =  "Condition") +                            # x-axis name
  scale_y_continuous(name  =  "Reaction time (s)") +               # y-axis name
  ggtitle("Reaction time \n by condition") +              # Title
  theme_bw() +                                                  # Remove background
  theme(panel.grid.major  =  element_blank(),                     # Esthetics ++
        panel.grid.minor  =  element_blank(),
        panel.border  =  element_blank(),
        panel.background  =  element_blank(),
        plot.title  =  element_text(size = 14, face = "bold", hjust = 0.5),
        axis.line  =  element_line(size = 0.5, colour = "black")) + 
  facet_grid(. ~ id)
p2


##############################################################
############# Statistics on aggregated data ##################
##############################################################

# Aggregate data
df_aggregate  =  aggregate(rt ~ id + condition, data = df[idx, ], FUN = mean) # Average inside subjects
df_aggregate

# Run linear mixed model
m0 = lmer(rt ~ 1 + (1|id), data = df_aggregate) # Null model : just an intercept
m1 = lmer(rt ~ condition + (1|id), data = df_aggregate) # Model with effect
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
m0 = lmer(rt ~ 1 + (1|id), data = df) # Null model : just an intercept
m1 = lmer(rt ~ condition + (1|id), data = df) # Model with effect
anova(m0, m1) # Compare models : take the smallest AIC
summary(m1) # Explore the best model

# Check model assumptions
# --> Are the residuals normally distributed ?
x = residuals(m1)
plot(density(x))
plot(m1)
qqnorm(x, pch = 1, frame = FALSE)
qqline(x, col = "steelblue", lwd = 2) # Problem here with skewness ...

# So we need to transform the data 
df$logRT = log(df$rt) # log is an intersting transformation in this case

# Run linear mixed model
m0 = lmer(logRT ~ 1 + (1|id), data = df) # Null model : just an intercept
m1 = lmer(logRT ~ condition + (1|id), data = df) # Model with effect
anova(m0, m1) # Compare models : take the smallest AIC
summary(m1) # Explore the best model

# Check model assumptions
# --> Are the residuals normally distributed ?
x = residuals(m1)
plot(density(x))
plot(m1)
qqnorm(x, pch = 1, frame = FALSE)
qqline(x, col = "steelblue", lwd = 2) # Much better
ks.test(x, mean(x), sd(x)) # OK










