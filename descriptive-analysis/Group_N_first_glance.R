# Read the data
coffee_data <- read.csv("arabica_data_cleaned.csv")

# Remove unnecessary columns
coffee_data$Altitude <- NULL # redundant information with altitude_high_meters and altitude_low_meters columns
coffee_data$Species <- NULL # All coffees belong to the Arabica genus
coffee_data$Total.Cup.Points <- NULL # Linearly dependent from other columns

# Summary of data
summary(coffee_data)
head(coffee_data)

# Target variable: Cupper.points -> How much did the taster like this coffee?
hist(coffee_data$Cupper.Points, xlim = c(5,10), breaks = seq(0,10,0.25))

