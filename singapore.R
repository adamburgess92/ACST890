# a
dataset <- read.csv("singapore.economy.csv", header = TRUE, sep =",")
                   

# b
dataset <- na.omit(dataset)

# c
library(ggplot2)
ggplot(dataset, aes(x = time, y = gdp)) + geom_line() + ggtitle("Singapore GDP Growth") + xlab("Time") + ylab("GDP (%)")


# d
Period_1_mu <- mean(dataset[dataset$period == 1,]$gdp)
Period_2_mu <- mean(dataset[dataset$period == 2,]$gdp)
Period_3_mu <- mean(dataset[dataset$period == 3,]$gdp)
data.frame(Period_1_mu, Period_2_mu, Period_3_mu)

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


