 install.packages(c("tidyverse", "readxl", "janitor", "car", "broom","randomForest"))

library(tidyverse) 
library(readxl)     
library(janitor)   
library(car)       
library(broom)      
 library(randomForest) 
 datad <- diamonds
 view(datad)
 names(datad)
 summary(datad)
 #Need to take the log 
 library(dplyr)
 
 diamonds <- diamonds %>%
   mutate(
     log_price = log(price),
     log_carat = log(carat)
   )
 
 
 str(diamonds)
 sum(is.na(diamonds))
 sum(duplicated(diamonds))
 set.seed(123)
 
 train_index <- sample(1:nrow(diamonds), 0.8 * nrow(diamonds))

 train_data <- diamonds[train_index, ]
 test_data  <- diamonds[-train_index, ]

 nrow(train_data)
 nrow(test_data)
 
 ggplot(diamonds, aes(x=price))+
   geom_histogram(binwidth = 500, fill= "lightpink",color="lavender")+
   labs(title= "Prices of Diamonds", x="Price", y="Count")
 # Price by Cut
 ggplot(diamonds, aes(x = cut, y = price)) +
   geom_boxplot(fill = "lightblue") +
   labs(title = "Diamond Price by Cut", x = "Cut", y = "Price") +
   theme_minimal()
 
 # Price by Color
 ggplot(diamonds, aes(x = color, y = price)) +
   geom_boxplot(fill = "lightgreen") +
   labs(title = "Diamond Price by Color", x = "Color", y = "Price") +
   theme_minimal()
 
 # Price by Clarity
 ggplot(diamonds, aes(x = clarity, y = price)) +
   geom_boxplot(fill = "lightpink") +
   labs(title = "Diamond Price by Clarity", x = "Clarity", y = "Price") +
   theme_minimal()
 
 #Price by Carat
 ggplot(diamonds, aes(x = carat, y = price)) +
   geom_boxplot(fill = "lavender") +
   labs(title = "Diamond Price by Carat", x = "Carat", y = "Price") +
   theme_minimal()
 
 
 model1<- lm(log_price ~ log_carat + depth + table + cut + color + clarity, data = diamonds)
summary(model1)
anova(model1)
scatterplot(model1)
ggplot(diamonds, aes(x = log_carat, y = log_price)) +
  geom_point() +                          # Add data points
  geom_smooth(method = "lm", se = TRUE) + # Add the linear regression line with confidence interval
  labs(title = "Linear Regression Model Visualization",
       x = "log_carat",
       y = "log_price")

#Multicolinearity 
m<-vif(model1)
boxplot(m)
summary(m)
car::vif(model1)

#For correlation
num_data <- diamonds %>% dplyr::select_if(is.numeric)
cor_matrix <- cor(num_data, use = "complete.obs")
cm<- round(cor_matrix, 2)
plot(cm)

datad <- datad %>% select(-x, -y, -z)
model_clean <- lm(price ~ carat + depth + table + cut + color + clarity, data = datad)
summary(model_clean)
car::vif(model_clean)

#random forrest 
rf_model <- randomForest(
  log_price ~ log_carat + depth + table + cut + color + clarity,
  data = train_data,
  ntree = 500,      # number of trees
  mtry = 3,         # number of variables tried at each split
  importance = TRUE # to calculate variable importance
)
# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# R-squared and RMSE
rf_r2 <- cor(test_data$log_price, rf_predictions)^2
rf_rmse <- sqrt(mean((rf_predictions - test_data$log_price)^2))

print(paste("Random Forest R-squared:", round(rf_r2, 3)))
print(paste("Random Forest RMSE:", round(rf_rmse, 3)))

# Variable importance
importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance in Random Forest")

ggplot(diamonds, aes(x = cut , fill = cut)) +
  theme_bw() +
  geom_bar()+
  labs(x = "Quality of Diamonds",
       y = "Diamonds Count",
       title = "Quality of the Diamonds")

