library(tidyverse)
library (dplyr)
library(ggplot2)
library(ggthemes)
library('corrplot')
library(lubridate)
library(gridExtra)


# load the data set from a CSV file named "melanoma.csv"
melanoma <-read.csv("melanoma.csv")

# transforming the numberical variables to categorical variables
melanoma <- melanoma %>%
  mutate(
    sex = recode_factor(sex, `0` = "female", `1` = "male"),
    ulcer = recode_factor(ulcer, `0` = "absent", `1` = "present"),
    status = recode_factor(status, `1` = "died", `2` = "Alive", `3` = "unrelated"),
    
  )
#Summarize the tranformed dataset
summary(melanoma)

par(mfrow=c(2,2))

plt1 <- ggplot(melanoma, aes(x = thickness)) +
  geom_histogram(binwidth = 2, fill = "purple", color = "black") +
  labs(title = "Distribution of Tumour Thickness", x = "Thickness (mm)", y = "Frequency") +
  theme_minimal()

plt2 <- ggplot(melanoma, aes(x = time)) +
  geom_histogram(bins = 10, fill = "purple", color = "black") +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
  labs(title = "Distribution of Survival time (by day)", x = "Time (days)", y = "Frequency") +
  theme_minimal()

plt3 <- ggplot(melanoma, aes(x = age)) +
  geom_histogram(binwidth = 7, fill = "purple", color = "black") +
  labs(title = "Distribution of Age", x = "Age ", y = "Frequency") +
  theme_minimal()

plt4 <- ggplot(melanoma, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Distribution of Surgery Year", x = "Year", y = "Frequency") +
  theme_minimal()

grid.arrange(plt1, plt2, plt3, plt4, ncol = 2)




# TIME - AGE

cor(age, time, method="pearson")

length(age)
length(time)
age
melanoma$age

# ORDINARY LEAST SQUARES REGRESSION
# here thickness is our x in graph and hb our y dependent
my_model2 = lm(formula = melanoma$time ~ melanoma$age) 
my_model2
summary(my_model2)

#predictions 

# my_model24 <- lm(formula = time ~ age, data = melanoma2)
# summary( my_model24)
# predict( my_model24, newdata = data.frame(age = 25))

#  new_data <- data.frame(age = 25)
# predict(ols_model2, newdata = list(age =25))


ggplot(data = melanoma, mapping = aes(x = age, y = time))+
  geom_point(shape = 16, color = "red", size = 3)+
  labs(title = "Time AND AGE",x = "AGE",y = "TIME")

# model regline
ggplot(data = melanoma, mapping = aes(x = age, y = time)) +
  geom_point(shape = 16, color = "black", size = 2) +
  labs(title = "Time AND AGE",x = "Age", y = "Time (days)") +
  geom_abline(intercept = coef(my_model2)[1], slope = coef( my_model2)[2], col = "red", lwd = 1)

 
  
  

# TIME - THICHKNESS
ggplot(data = melanoma, mapping = aes(x = thickness, y = time))+
  geom_point(shape = "x", color = "black", size = 3)+
  labs(title = "Disease Thickness with Survival Time",x = "Thickness (mm)",y = "Time (days)")


# lets attach the dataset that helps us compute 
attach(melanoma)
cor(thickness, time, method="pearson")

# ORDINARY LEAST SQUARES REGRESSION
# here thickness is our x in graph and .hb our y dependent
my_model = lm(formula =melanoma2$time ~ melanoma2$thickness) 
my_model <- lm(formula = melanoma2$time ~ melanoma2$thickness)
my_model
summary( my_model)

# abline( my_model, col = "green", lwd = 2)

ggplot(data = melanoma2, mapping = aes(x = thickness, y = time)) +
  geom_point(shape = 16, color = "black", size = 2) +
  labs(title = "TIME BY THICKNESS", x = "Thickness (mm)", y = "Time (days)") +
  geom_abline(intercept = coef(my_model)[1], slope = coef( my_model)[2], col = "purple", lwd = 1)

coef( my_model)[1]
coef( my_model)[2]

#making predictions 

# my_model1 <- lm(formula = time ~ thickness, data = melanoma2)
# summary( my_model1)
# predict( my_model1, newdata = data.frame(thickness = 15))

# # Create a scatter plot
# plot(mlm2$thickness, mlm2$time)
# # Add a regression line using abline
# abline(ols_model, col = "green", lwd = 2) 



# THICKNESS - AGE

cor(age, thickness, method="pearson")

# ORDINARY LEAST SQUARES REGRESSION
# here thickness is our x in graph and hb our y dependent
my_model3 = lm(formula = # THICKNESS - AGE

cor(age, thickness, method="pearson")

# ORDINARY LEAST SQUARES REGRESSION
# here thickness is our x in graph and hb our y dependent
my_model3 = lm(formula = # THICKNESS - AGE

cor(age, thickness, method="pearson")

# ORDINARY LEAST SQUARES REGRESSION
# here thickness is our x in graph and hb our y dependent
my_model3 = lm(formula = # THICKNESS - AGE

cor(age, thickness, method="pearson")

# ORDINARY LEAST SQUARES REGRESSION
# here thickness is our x in graph and hb our y dependent
my_model3 = lm(formula = melanoma$thickness~ melanoma2$age) 
my_model3
summary( my_model3)


ggplot(data = melanoma, mapping = aes(x = age, y = thickness))+
  geom_point(shape = "x", color = "black", size = 3)+
  labs(title = "Disease Thickness by AGE",x = "Age",y = "THICKNESS (mm)")

#model regline
ggplot(data = melanoma, mapping = aes(x = age, y = thickness)) +
  geom_point(shape = 16, color = "black", size = 2) +
  labs(title = "Disease Thickness by AGE", x = "Age", y = "Thickness (mm)") +
  geom_abline(intercept = coef( my_model3)[1], slope = coef( my_model3)[2], col = "purple", lwd = 1)


#------------ T-TESTING -------------------------------------------------------

#Appropriate two sample significance tests for the variables in part (iii) grouped by gender 
# THICKNESS, TIME, AGE 
# THICKNES grouped by gender 
qplot(x = sex , y = thickness,
      geom = "boxplot", data = melanoma,aes(group = sex),
      xlab = "Gender",
      ylab = "Thickness (mm)",
      fill = I("purple"))

#this is a basic explanation - male have thicker tumours to justify this based on mathe
#theory
male <- filter(melanoma, sex == "Male")
female <- filter(melanoma, sex == "Female")

mean(male$thickness)
mean(female$thickness)

# further Eda

melanoma |> 
  group_by(sex) |> 
  summarize(num.obs = n(),
            mean_thickness = round( mean(thickness), 1),
            sd_thickness = round( sd(thickness), 1),
            se_thickness = round( sd(thickness) / sqrt(num.obs), 1))

# Now to discover how statistically significant this difference is 

# -- Null hypothesis Ho: the mean thickness are the same i.e u1 = u2 .
# -- ALternative  hypothesis H1: the mean thickness are different i.e u1 =/ u2 

t_thickness_test <- t.test(thickness ~ sex, data = melanoma)
t_thickness_test

# Hence the p-value is less than the typical significance level of 0.05, so you would 
# reject the null hypothesis.
# Based on the results of the t-test, you can conclude that there is a statistically
# significant difference in the mean thickness between Male and Female. The negative t-value 
# and the negative lower bound of the confidence interval suggest that the mean thickness in 
# the Female group is significantly lower than that in the Male group.


# TIME grouped by gender 
qplot(x = sex , y = time,
      geom = "boxplot", data = melanoma, aes(group = sex),
      xlab = "Gender",
      ylab = "Time (days)",
      fill = I("purple"))
melan |> 
  group_by(sex) |> 
  summarize(num.obs = n(),
            mean_time = round( mean(time), 0),
            sd_time = round( sd(time), 0),
            se_time = round( sd(time) / sqrt(num.obs), 0))
# Now to discover how statistically significant this difference is 

# -- Null hypothesis Ho: the mean time are the same i.e u1 = u2 .
# -- ALternative  hypothesis H1: the mean time are different i.e u1 =/ u2 

t_time_test <- t.test(time ~ sex, data = melanoma)
t_time_test
# The p-value is less than the typical significance level of 0.05, so you would 
# reject the null hypothesis.
# Based on the results of the t-test, you can conclude that there is a statistically 
# significant difference in the mean time between Male and Female. The positive t-value 
# and the positive lower bound of the confidence interval suggest that the mean time in the 
# Female group is significantly higher than that in the Male group


# AGE grouped by gender 
qplot(x = sex , y = age,
      geom = "boxplot", data = melanoma, aes(group = sex),
      xlab = "Gender",
      ylab = "Age",
      fill = I("purple"))
melan |> 
  group_by(sex) |> 
  summarize(num.obs = n(),
            mean_age = round( mean(age), 0),
            sd_age = round( sd(age), 0),
            se_age = round( sd(age) / sqrt(num.obs), 0))

# Now to discover how statistically significant this difference is 

# -- Null hypothesis Ho: the mean age are the same i.e u1 = u2 .
# -- ALternative  hypothesis H1: the mean age are different i.e u1 =/ u2 

t_age_test <- t.test(age ~ sex, data = melanoma)
t_age_test  

# The p-value is greater than the typical significance level of 0.05, 
# so you would fail to reject the null hypothesis
# The confidence interval for the difference in means includes 0, 
# indicating that the difference is not statistically significant.
# Therefore, there is no strong evidence to suggest a significant difference 
# in mean age between Male and Female.
# 
# Based on the results of the t-test, you cannot conclude that there is a statistically 
# significant difference in mean age between Male and Female. The p-value is relatively
# high, and the confidence interval includes 0, suggesting that any observed difference 
# in mean age could be due to random chance.




# QQ PLOT ----------------Normality test


# THICKNES grouped by gender 
qq_thickness <- ggplot(data = melanoma, aes(sample = thickness))
qq_thickness + stat_qq() + stat_qq_line()
qq_thickness + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)+
  xlab("Theoretical Quantiles") +  
  ylab("Thickness Sample")

# TIME grouped by gender 
qq_time <- ggplot(data = melanoma, aes(sample = time))
qq_time + stat_qq() + stat_qq_line()
qq_time + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)+
  xlab("Theoretical Quantiles") +  
  ylab("Time Sample") 

# AGE grouped by gender
qq_age <- ggplot(data = melanoma, aes(sample = age))
qq_age + stat_qq() + stat_qq_line()
qq_age + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)+
  xlab("Theoritical Quantitles") + 
  ylab("Age Sample") 
