#Assignment_3
#Exercise_2

#a. Load the dataset in R

load("/Users/priya/Downloads/gss2018.rda")
GSS

#b.Clean the happiness, sex, degree, and tech use variables so they have clear variable
#names, category labels, and missing value coding

library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

#removing NA values

GSS %>%
  drop_na(USETECH,HAPPY,SEX,DEGREE,AGE)

nrow(GSS)

#creating GSS copy
GSS_1 <- GSS


#Happy variable label

GSS$HAPPY <- factor(GSS$HAPPY,
                               levels = c(1, 2, 3),
                               labels = c("Very happy", "Pretty happy", "Not too happy"),
                               exclude = NULL)

# sex variable label
GSS$SEX <- factor(GSS$SEX,
                      levels = c(1, 2),
                      labels = c("Male", "Female"),
                      exclude = NULL)

# degree variable label
GSS$DEGREE <- factor(GSS$DEGREE ,
                    levels = c(0,1, 2, 3, 4),
                    labels = c("Less than high school", "High school", "Associate/junior college", "Bachelors", "Graduate"),
                    exclude = NULL)

head(GSS)

#c.Use graphics and descriptive statistics to examine tech use by degree, by sex, and by
#happiness. Explain your findings.
#by_degree 
degree_usetech <- GSS %>%
  group_by(DEGREE) %>%
  summarise(mean_usetech = mean(USETECH), sd_usetech = sd(USETECH))
degree_usetech
# Bar chart of tech use by degree
ggplot(degree_usetech, aes(x = DEGREE, y = mean_usetech)) +
  geom_bar(stat = "identity", fill = "blue" )+
  labs(x = "Degree", y = "Tech Use (% of work_hours)") +
  ggtitle("Tech Use by Degree")
summary(GSS$USETECH)
GSS %>%
  drop_na(GSS$HAPPY)

# Tech Use by Sex
sex_usetech <- GSS %>%
  group_by(SEX) %>%
  summarise(mean_tech = mean(USETECH), sd_tech = sd(USETECH))
sex_usetech

# Bar chart of tech use by sex
ggplot(sex_usetech, aes(x = SEX, y = mean_tech)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "Sex", y = "Tech Use (% of work_hours)") +
  ggtitle("Tech Use by Sex")

# Tech Use by Happiness
happy_usetech <- GSS %>%
  group_by(HAPPY) %>%
  summarise(mean_tech = mean(USETECH), sd_tech = sd(USETECH))
GSS_clean <- happy_usetech %>% drop_na()

#plot
ggplot(GSS_clean, aes(x =HAPPY, y = mean_tech)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(x = "Happy", y = "Tech Use (% of work_hours)") +
  ggtitle("Tech Use by Sex")

#anova test for assumptions
kruskal.test(USETECH ~ HAPPY, data = GSS)

#view(GSS$HAPPY)
