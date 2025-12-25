
# Libraries
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Read the data
coffee_data <- read.csv("~/ML2 arabica-coffee-analayze/arabica_data_cleaned.csv", na.strings = c("", "NA", " ", "n/a"))


## to see the features (columns), their types and some examples of the values that the columns contain.
str(coffee_data)
## Number of observations
nrow(coffee_data)
## Number of features
ncol(coffee_data)
## Summary of the dataset
summary(coffee_data)
## Some of NAN values in each column
colSums(is.na(coffee_data))


#Visualization
ggplot(coffee_data, aes(x = Total.Cup.Points)) +
  geom_density(fill = '#6F4E37', alpha = 0.7) +
  theme_minimal() +
  labs(title = 'Distribution of Total Coffee Quality Scores', x = 'Total Cup Points', y = 'Density')

#Visualization
ggplot(coffee_data, aes(x = Cupper.Points)) +
  geom_density(fill = '#6F4E37', alpha = 0.7) +
  theme_minimal() +
  labs(title = 'Distribution of Cupper Points', x = 'Cupper Points', y = 'Density')


# Correlation between Cupper Points and Total Cup Points
output_cols <- coffee_data %>% select(Cupper.Points, Total.Cup.Points)

# Calculate correlation matrix
M <- cor(output_cols, use = "complete.obs")

# Visualize
corrplot(M, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", 
         title = "Correlation Between Cupper Points and Total Cup Points")


# Select only the numerical sensory columns (adjust indices as needed)
# Usually columns like Aroma:Cupper.Points
sensory_cols <- coffee_data %>% select(Aroma, Flavor, Aftertaste, Acidity, Body, Balance, Sweetness, Uniformity, Clean.Cup, Moisture)

# Calculate correlation matrix
M <- cor(sensory_cols, use = "complete.obs")

# Visualize
corrplot(M, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", 
         title = "Correlation Between Sensory Attributes")


# Top 10 countries by FREQUENCY in the dataset
top_countries <- coffee_data %>%
  count(Country.of.Origin, sort = TRUE) %>%
  top_n(10) %>%
  pull(Country.of.Origin)

# Filter and plot
coffee_data %>%
  filter(Country.of.Origin %in% top_countries) %>%
  ggplot(aes(x = reorder(Country.of.Origin, Cupper.Points), y = Cupper.Points)) +
  geom_boxplot(fill = "steelblue") +
  coord_flip() + # Makes country names easier to read
  theme_minimal() +
  labs(title = "Quality Scores by Country of Origin",
       subtitle = "Ranked by frequency in the dataset from highest to lower",
       x = "Country",
       y = "Total Points")



# Find The Top 10 countries by AVERAGE Cupper Points
top_quality_list <- coffee_data %>%
  group_by(Country.of.Origin) %>%
  summarise(avg_score = mean(Cupper.Points, na.rm = TRUE)) %>%
  arrange(desc(avg_score)) %>% # Sort from highest to lowest
  slice_head(n = 10) %>%        # Take the top 10
  pull(Country.of.Origin)

# Filter the original data to include only these top countries
coffee_data %>%
  filter(Country.of.Origin %in% top_quality_list) %>%
  ggplot(aes(x = reorder(Country.of.Origin, Cupper.Points), y = Cupper.Points)) +
  geom_boxplot(fill = "#228B22") + # Forest Green for 'Top' quality
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Countries by Average Cupper Points",
       subtitle = "Ranked by highest quality",
       x = "Country",
       y = "Cupper Points")


# Find the bottom 10 countries by AVERAGE Cupper Points
least_quality_list <- coffee_data %>%
  group_by(Country.of.Origin) %>%
  summarise(avg_score = mean(Cupper.Points, na.rm = TRUE)) %>%
  arrange(avg_score) %>% # Sort from lowest to highest
  slice_head(n = 10) %>%        # Take the first 10
  pull(Country.of.Origin)

# Filter the original data to include only these lowest rated countries
coffee_data %>%
  filter(Country.of.Origin %in% least_quality_list) %>%
  ggplot(aes(x = reorder(Country.of.Origin, Cupper.Points), y = Cupper.Points)) +
  geom_boxplot(fill = "orange") + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Lowest Rated 10 Countries by Average Cupper Points",
       subtitle = "Ranked by lowest quality",
       x = "Country",
       y = "Cupper Points")



# Calculate NAs and Percentages
na_summary <- data.frame(
  Column_Name = names(coffee_data),
  Missing_Count = colSums(is.na(coffee_data)),
  Missing_Percentage = round((colSums(is.na(coffee_data)) / nrow(coffee_data)) * 100, 2)
)

# Sort by most missing values
na_summary <- na_summary[order(-na_summary$Missing_Count), ]

# Show in a table
na_summary %>%
  filter(Missing_Count > 0) %>% # Only show columns that actually have NAs
  kable(col.names = c("Feature", "Total NAs", "% Missing"), 
        caption = "Summary of Missing Data in Coffee Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# How many unique values are there in categorical variables
categorical_summary <- coffee_data %>%
  summarise(across(where(is.character), ~n_distinct(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "Unique_Count")

print(categorical_summary, n=25)
