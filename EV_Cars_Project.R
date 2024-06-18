library(ggplot2)
library(dplyr) 
options(scipen = 999) #avoid scientific notation in 

#EV cars dataset
cars <- read.csv("EV_cars.csv")

#new df containing only numeric vars
cars_numeric <- cars %>% select_if(is.numeric)

#view correlations, summary
cor(cars_numeric)
summary(cars_numeric)

#scatterplot efficiency v price
ggplot(cars_numeric, aes(x = Efficiency, y = Price.DE.)) +
  geom_point(color = "blue") +
  labs(title = "Efficiency vs. Price", x = "Efficiency", y = "Price") +
  theme_minimal()

#scatterplot topspeed v price
ggplot(cars_numeric, aes(x = Top_speed, y = Price.DE.)) +
  geom_point(color = "blue") +
  labs(title = "Top Speed vs. Price", x = "Top Speed", y = "Price") +
  theme_minimal()


#boxplot
boxplot(cars$Battery, col = "skyblue", main = "Battery Distribution")
boxplot(cars_numeric)


#Price model vs all numeric factors
model <- lm(cars_numeric$Price.DE. ~., data = cars_numeric)
summary(model)

#price model vs all numeric factors minus range (potential collinearity)
carsnorange <- cars_numeric %>% select(-Range)
model2 <- lm(carsnorange$Price.DE. ~., data = carsnorange)
summary(model2)

#create brand name column
cars$Brand <- sapply(strsplit(cars$Car_name, " "), function(x) x[1])

#create average price per by brand column
cars <- cars %>%
  group_by(Brand) %>%
  mutate(Average_Price = mean(Price.DE., na.rm = TRUE))

#find top 10 brands
carssort <- cars %>%
  arrange(desc(Average_Price))

top_10_brands <- carssort %>%
  distinct(Brand, .keep_all = TRUE) %>%
  head(10)

#top 10 brands bar chart
barplot(top_10_brands$Average_Price, names.arg = top_10_brands$Brand,
        main = "Top 10 Highest Average Priced EV Car Brands", xlab = "Car Brand",
        ylab = "Average Price", las = 2)

#pretty ggplot version
ggplot(top_10_brands, aes(x = Brand, y = Average_Price, fill = Brand)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Top 10 Highest Average Priced EV Car Brands",
    x = "Car Brand",
    y = "Average Price"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # Remove legend 
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

#see how many of each brand
table(cars$Brand)

#model 2 correlating average brand price to brand
carbrandmodel <- lm(cars$Price.DE. ~ cars$Brand, data = cars)
summary(carbrandmodel)

