################################################################################
# Load Libraries and Data
################################################################################

# Load Libraries
library("tidyverse")

# Load Data
our_dataset <- read.csv("Electric_Vehicle_population_Data.csv", 
                        header = TRUE, na.strings = "NA")

################################################################################
# Data Cleaning
################################################################################

# We are going to rename our dataset to preserve our original dataset.
our_dataset_clean <- our_dataset

# Summary Stats
summary(our_dataset_clean)

# We will start with taking care of our missing data. Since we have a very large
# number of observations, let's say we have decided to delete all observations 
# with missing values.
our_dataset_clean <- na.omit(our_dataset_clean)

# Let's say we want to imputing all missing data with 0.We would run this code.
#our_dataset_clean[is.na(our_dataset_clean)] <- 0

# Now, let's clean up some variable names and values.
# Let's rename Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility!
our_dataset_clean <- our_dataset_clean %>%
  rename(CAFV.Eligibility = Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)

# Check the number of unique elements from CAFV.Eligibility for renaming.
unique(our_dataset_clean$CAFV.Eligibility)

# Rename values within the CAFV.Eligibility variable
our_dataset_clean <- our_dataset_clean %>%
  mutate(CAFV.Eligibility = str_replace_all(CAFV.Eligibility, 
                                            fixed("Clean Alternative Fuel Vehicle Eligible"),
                                            "Eligible")) %>%
  mutate(CAFV.Eligibility = str_replace_all(CAFV.Eligibility, 
                                            fixed("Eligibility unknown as battery range has not been researched"),
                                            "Unknown Eligibility")) %>%
  mutate(CAFV.Eligibility = str_replace_all(CAFV.Eligibility, 
                                            fixed("Not eligible due to low battery range"),
                                            "Not Eligible"))


# Let's also rename Electric.Vehicle.Type
our_dataset_clean <- our_dataset_clean %>%
  rename(EVT = Electric.Vehicle.Type)

# Check the number of unique elements from EVT vector for renaming.
unique(our_dataset_clean$EVT)

# Let's rename the EVT values.
our_dataset_clean <- our_dataset_clean %>%
  mutate(EVT = ifelse(EVT == "Battery Electric Vehicle (BEV)", "BEV", "PHEV"))


# Let's fix some of the data types of our variables.
# Factors are a type of categorical variable that have levels, which are the 
# unique values that the factor can take. What we have with our variable are 
# unordered factors.
factor_variables <- c("County", "City", "State", "Postal.Code", "Model.Year", 
                      "Make", "Model", "EVT", "CAFV.Eligibility", 
                      "Legislative.District", "Electric.Utility")
our_dataset_clean[factor_variables] <- lapply(our_dataset_clean[factor_variables], factor)

# Help
?lapply

# Summary Stats
summary(our_dataset_clean)



################################################################################
# Exploratory Data Analysis
################################################################################

# Exploratory Data Visualizations

# Let's create a bar plot of Model.Year
ggplot() +
  geom_bar(data = our_dataset_clean, mapping = aes(x = Model.Year))

# Add fill by CAFV.Eligibility
ggplot() +
  geom_bar(data = our_dataset_clean, mapping = aes(x = Model.Year, fill = CAFV.Eligibility))

# Let's create a bar plot of EVT
ggplot() +
  geom_bar(data = our_dataset_clean, mapping = aes(x = EVT))

# Add fill by CAFV.Eligibility
ggplot() +
  geom_bar(data = our_dataset_clean, mapping = aes(x = EVT, fill = CAFV.Eligibility))

# Histogram of Electric.Range with CAFV.Eligibility fill
ggplot() +
  geom_histogram(data = our_dataset_clean, mapping = aes(x = Electric.Range, fill = CAFV.Eligibility))

# Histogram of Electric.Range with Make fill
ggplot() +
  geom_histogram(data = our_dataset_clean, mapping = aes(x = Electric.Range, fill = Make))

  
# Now, we are going to focus on some exploratory data visualizations with Base.MSRP.

# Let's take another look at our summary statistics.
summary(our_dataset_clean)

# First, we are going to remove observations where Base.MSRP is 0.
our_dataset_clean <- filter(our_dataset_clean, Base.MSRP > 0)

# Summary stats again
summary(our_dataset_clean)

# Now, we'll look at a histogram of the variable Base.MSRP.
ggplot() +
  geom_histogram(data = our_dataset_clean, mapping = aes(x = Base.MSRP))

# So, let's make some plots to see if we can visually see any correlations
# between Base.MSRP and the other Variables.

# Boxplot of Base.MSRP 
ggplot() +
  geom_boxplot(our_dataset_clean, mapping = aes(x = Base.MSRP))

# Boxplot of Base.MSRP and County
ggplot() +
  geom_boxplot(our_dataset_clean, mapping = aes(x = Base.MSRP, y = County))

# Boxplot of Base.MSRP and Legislative.District
ggplot() +
  geom_boxplot(our_dataset_clean, mapping = aes(x = Base.MSRP, y = Legislative.District))

# Boxplot of Base.MSRP and Model
ggplot() +
  geom_boxplot(our_dataset_clean, mapping = aes(x = Base.MSRP, y = Model))

# Scatter Plot of Base.MSRP and Electric.Range
ggplot() +
  geom_point(our_dataset_clean, mapping = aes(x = Base.MSRP, y = Electric.Range))

################################################################################
# Analysis
################################################################################

# Correlation between Base.MSRP and Electric.Range
cor(our_dataset_clean$Base.MSRP, our_dataset_clean$Electric.Range)

# Linear Model
?lm
our_model <- lm(Base.MSRP ~ Electric.Range, data = our_dataset_clean)
summary(our_model)



