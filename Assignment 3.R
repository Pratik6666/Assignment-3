library(tidyverse)
# reading the mall data 
md <- read.csv("mall_customers.csv")
# checking the structure 
str(md)
#setting gender as categorical
md$Gender <- as.factor(md$Gender)
#first six rows of mall_customers dataset 
head(md)
#using summary fucntion for the output
summary(md)
#gender distribution across mall_customers datasets
# bar plot
ggplot(data = md, aes(x = Gender)) +
  geom_bar()

#####################################################################
# pie chart 
pie_data <- data.frame(summary(md$Gender))

pie_data <- pie_data %>% 
  mutate(percetage = summary.md.Gender./sum(summary.md.Gender.)*100) %>% 
  mutate(gender = c("Female", "Male"))

pie(x = pie_data$summary.md.Gender., labels = (c(pie_data$gender)), main = "Gender distribution")

#using ggplot
ggplot(data = pie_data, aes(x = " ", y = summary.md.Gender., fill = gender)) +
  geom_col() +
  coord_polar("y", start = 0) 

###############################################################
#analysing the ages 
summary(md$Age)

hist(md$Age, xlab = "Age", main = "Histogram of ages")
## tallet tower is for 30-35 with 35 customers

#using ggplot
ggplot(data = md, aes(x = Age, fill = Gender)) + 
  geom_histogram(binwidth =5)

## minimum and maximum age of the customers
summary(md$Age)
min_cust_age <- min(md$Age)
max_cust_age <- max(md$Age)

###############################################################
## annual income analysis
names(md)[5] <- "Spending Score (1-100)"

## summary and histogram 
hist(md$`Annual Income (K$)`, xlab = "Annual Income", main = "Annual income distribution")
summary(md$`Annual Income (K$)`)

## density plot for income
## highest frequency for income between 70k - 80k
ggplot(data = md, aes(x = `Annual Income (K$)`, fill = Gender)) +
  geom_density(alpha = 0.5)



