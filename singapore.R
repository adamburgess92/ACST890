# a
dataset <- read.csv("singapore.economy.csv", header = TRUE, sep =",")
                   

# b
dataset <- na.omit(dataset)

# c
library(ggplot2)
ggplot(dataset, aes(x = time, y = gdp)) + 
  geom_line() + 
  ggtitle("Singapore GDP Growth") + 
  xlab("Time") + 
  ylab("GDP (%)")


# d
Period_1_mean <- mean(dataset[dataset$period == 1,]$gdp)
Period_2_mean <- mean(dataset[dataset$period == 2,]$gdp)
Period_3_mean <- mean(dataset[dataset$period == 3,]$gdp)

Period_1_sd <- sd(dataset[dataset$period == 1,]$gdp)
Period_2_sd <- sd(dataset[dataset$period == 2,]$gdp)
Period_3_sd <- sd(dataset[dataset$period == 3,]$gdp)

stat.table <- data.frame("Mean" = c(Period_1_mean, Period_2_mean, Period_3_mean), "SD" = c(Period_1_sd, Period_2_sd, Period_3_sd))


# e
pairs(dataset[-c(dataset$time, dataset$period)])


# f
model <- lm(gdp ~ exp, data = dataset)
summary(model)


# g
model2 <- lm(gdp ~ exp + epg + hpr + oil + gdpus + crd, data = dataset)
summary(model2)


# h
# Find rows where gdp growth is in the lowest 5 percentile:
cutoff <- quantile(dataset$gdp, probs = 0.05)
state <- as.numeric(dataset$gdp < cutoff) # 1 if in crisis, 0 otherwise

# Add new column to dataframe:
dataset <- data.frame(dataset, state)

# Split dataset into training and testing data (pre and post 2007)
train=(dataset$time <2007)
pre2007 = dataset[train,]
post2007= dataset[!train ,]

# Fit model
glm1 <- glm(state ~ bci, data = pre2007, family = binomial(link = "logit"), subset = train)
summary(glm1)

# Use model to predict post 2007 crisis states:
fitted_vals <- predict.glm(glm1, newdata = post2007, type = "response")

# using a p = 05 cutoff
table(post2007$state, fitted_vals>0.5)


