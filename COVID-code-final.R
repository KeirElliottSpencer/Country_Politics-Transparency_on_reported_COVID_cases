#Sets the working directory to the correct folder
setwd("/Users/elysia/Sheffield Uni Work/Intro to Data Science")

#Packages:
library(tidyverse) #loads tidyverse package involved in data exploration, manipulation, and plotting
library(lubridate) #loads lubridate package involved in dealing with date data
#Loads 2 different packages that are used in plotting missing data visualisations, both for different purposes and analyses
library(VIM) #^
library(naniar) #^
library(zoo) #loads zoo package, used linear interpolation of linear time series data (used in NA handling)
library(ggpubr) #loads ggpubr package used for calculation of correlation coefficient and statistical significance for ggplot graphs
library(corrplot) #loads corrplot package used in plotting correlation plots
library(forecast) #loads forecast package used in time series forecasting
library(tsibble) #loads tsibble package used to analyse time series analysis of groups (allows for data frames using temporal structure)
library(fable) #loads fable package used in time series forecasting
library(feasts) #loads feasts package used in analysing tidy time series data
library(Metrics) #loads the Metrics package used in calculating RMSE values for predicted vs actual data 


#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 1: Setting up the data
##

#Imports the covid csv file from the data folder into a variable called covid_dataset
covid_dataset <- read.csv("data/owid-covid-data.csv")
#Imports 5 other csv files from the data file that will be analysed alongside the covid dataset
political_regime_dataset <- read.csv("data/V-Dem-v12.csv") #v2x_polyarchy
transparency_dataset <- read.csv("data/government-transparency.csv")
democracy_dataset <- read.csv("data/democracy.csv")
freedom_dataset <- read.csv("data/democracy-freedom.csv")
humanrights_dataset <- read.csv("data/human-rights-vdem.csv")

#Views each of the initial datasets
View(covid_dataset)
View(political_regime_dataset)
View(transparency_dataset)
View(democracy_dataset)
View(freedom_dataset)
View(humanrights_dataset)

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 2: Cleaning the initial datasets and combining them
##

#Cleans up the political regime dataset - , 
political_regime_cleaned <- political_regime_dataset %>%
  select(country_name, year, v2x_polyarchy) %>% #selecting only relevant variables
  filter(year == "2021") %>% #filters year by 2021 to remove duplicate instances, as level_of_democracy is not date dependent
  mutate(year = NULL) %>%
  rename("level_of_democracy" = "v2x_polyarchy") #renames the unclear variable v2x_polyarchy

#Cleans up the goverment transparency dataset
transparency_dataset_cleaned <- transparency_dataset %>%
  select(Entity, Year, gov_transparency) %>%
  filter(Year == "2010") %>%
  mutate(Year = NULL)

#Cleans up the democracy dataset
democracy_dataset_cleaned <- democracy_dataset %>%
  select(Entity, Year, electdem_vdem_owid) %>%
  filter(Year == "2021") %>%
  mutate(Year = NULL) %>%
  rename("electoral_democracy" = "electdem_vdem_owid")

#Cleans up the freedom dataset
freedom_dataset_cleaned <- freedom_dataset %>%
  select(Entity, Year, regime_fh) %>%
  filter(Year == "2021") %>%
  mutate(Year = NULL) %>%
  rename("political_freedom" = "regime_fh")

#Views the cleaned datasets
View(political_regime_cleaned)
View(transparency_dataset_cleaned)
View(democracy_dataset_cleaned)
View(freedom_dataset_cleaned)

#Creates a variable that contains all of the non-country location strings to be removed
non_country_locations <- c("Africa", "Asia", "Europe", "North America", "South America", "European Union", "High income",
                           "International", "Low income", "Lower middle income", "Oceania", "Upper middle income", "World")

#Joins the cleaned datasets to the covid dataset, named covid_dataset_complete
covid_dataset_complete <- covid_dataset %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% #converts the date column into a date format
  mutate(year = year(date), month = month(date), day = day(date)) %>% #splits each section of the date (year, month, day) into their own columns
  #Joins the cleaned datasets to the covid dataset by the country and date/year (depending on the dataset)
  left_join(political_regime_cleaned, by = c("location" = "country_name")) %>%
  left_join(transparency_dataset_cleaned, by = c("location" = "Entity")) %>%
  left_join(democracy_dataset_cleaned, by = c("location" = "Entity")) %>%
  left_join(freedom_dataset_cleaned, by = c("location" = "Entity")) %>%
  filter(!location %in% non_country_locations) #filters out locations that are not countries

View(covid_dataset_complete) #views the complete dataset

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 3: Initial analysis of dataset - looking at different statistics and levels of missing values
##

ncol(covid_dataset_complete) #counts the amount of columns in the complete dataset #output: 74
covid_nrow <- nrow(covid_dataset_complete) #counts the amount of rows in the complete dataset #output in variable: 187101

#Calculates min, median, max, mean, sd, and sum of blanks for each variable and stores it in covid_dataset_complete.sum
covid_dataset_complete.sum <- covid_dataset_complete %>%
  select(-day, -year, -month) %>% #removes day, year and month columns from the dataset
  summarise(across(where(is.numeric), 
                   list(
                     ZZmin = min, 
                     ZZmedian = median, 
                     ZZmax = max,
                     ZZmean = mean, 
                     ZZsd = sd,
                     ZZblank = ~ sum(is.na(.))
                   ), na.rm=TRUE))

#Tidies the 1 row .sum table into a cleaner table split by variable and statistics (as listed above)
covid_dataset_complete.stats.tidy <- covid_dataset_complete.sum %>% 
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "ZZ") %>% ##uses ZZ character as a separator as underscores cannot be used due to variable names
  spread(stat, val) %>%
  select(var, min, median, max, mean, sd, blank) %>%
  mutate(percentblank = blank / covid_nrow * 100) %>% #calculates the % of missing data
  ##Creates variable called coef_skew that measures the skewness of each variable. Values >0.5/<-0.5 are significantly skewed
  mutate(coef_skewness = (3* mean - median) / sd)

View(covid_dataset_complete.stats.tidy) #views the stats table that shows the stats for each variable

#Writes covid_dataset_complete.stats.tidy to a text document for easy input to word doc table
write.table(covid_dataset_complete.stats.tidy, file = "olstab.txt", sep = ",", quote = FALSE, row.names = F)

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 4: Sorting out missing data for variables of interest - mean for skew <0.5, regression for skew >0.5
##

#Creates a subset dataset from the complete dataset that only contains the variables of interest
covid_dataset_interest <- covid_dataset_complete %>%
  select(
    location, date, year, population, total_cases, total_cases_per_million, level_of_democracy, 
    gov_transparency, political_freedom, electoral_democracy, gdp_per_capita, life_expectancy, 
    population_density, stringency_index, total_deaths_per_million, aged_70_older) %>%
  mutate(political_freedom = as.factor(political_freedom)) %>% #converts political_freedom variable to a factor (0,1,2)
  filter(!is.na(population)) #removes rows that have no population recorded - filters out 0.17% of the dataset

View(covid_dataset_interest) #views the dataset of containing only variables of interest


#Visualisations of missing data for analysis of missing data:

#Creates subset dataset including only relevant variables for missing data visualisations
covid_dataset_interest_NA_vis <- covid_dataset_interest %>%
  select(location, date, year, population, total_cases, total_cases_per_million, level_of_democracy, gov_transparency)

#Creates a histogram and pattern plot looking into missing data
aggr_plot <- aggr(covid_dataset_interest_NA_vis, col=c('lightblue','red'), 
                  sortVars=TRUE, labels=names(covid_dataset_interest), 
                  cex.axis=.4, gap=3, ylab=c("Histogram of proportion of missing data","Combination"))

#Creates a matrix plot looking at the positions of missing data in the dataset against each variable
matrixplot(covid_dataset_interest_NA_vis, sortby = 2, ylab = "Observation", cex.axis = 0.4)

#Create missing values plot detailing levels of missing data that overlap with different variables
gg_miss_upset(covid_dataset_interest_NA_vis, nsets = n_var_miss(covid_dataset_interest))

#Creates a margin plot plotting distribution of data and missing data of government transparency and level ofdemocracy
marginplot(covid_dataset_interest_NA_vis[ ,c("gov_transparency","level_of_democracy")])


###Imputations:

###1. NA: total_cases
#Groups dataset by country, and replaces missing values of total_cases with a linear interpolation (in order of date)
covid_dataset_interest <- covid_dataset_interest %>% 
  group_by(location) %>% #groups the data by country
  arrange(date) %>% #orders data by date
  #Creates a temporary variable and uses the zoo package na.approx to linearly approximate total case values per country
  #with missing data removed, and always fill in the blanks (rule 2)
  mutate(total_cases2 = na.approx(total_cases, na.rm=F, rule=2)) %>% 
  ungroup() %>% #ungroups the data for further manipulation
  mutate(total_cases = ifelse(is.na(total_cases), total_cases2, total_cases)) %>% #replaces NA values with the approximate value
  mutate(total_cases2 = NULL) %>% #removes the temporary variable from the dataset
  filter(!is.na(total_cases)) #removes any countries wherein there are not enough data for total_cases to make an approximation

sum(is.na(covid_dataset_interest$total_cases)) #checks for the amount of missing values in the total_cases column

#As total_cases is cumulative, can't take an overall mean as this could mean previous values in time could be larger than later values


###2. NA: total_cases_per_million 

#Calculates total_cases_per_million from the imputed total_cases values and replaces NAs
covid_dataset_interest <- covid_dataset_interest %>% 
  mutate(total_cases_per_million = ifelse(is.na(total_cases_per_million), (population / 1000000 * total_cases), total_cases_per_million))

sum(is.na(covid_dataset_interest$total_cases_per_million)) #checks for the amount of missing values in the total_cases_per_million column


###3. NA: level_of_democracy

#Creates a new subset dataset looking at electoral democracy and level of democracy correlation
covid_dataset_interest_elect <- covid_dataset_interest %>%
  filter(!is.na(electoral_democracy)) %>% #filters out NA values
  filter(!is.na(level_of_democracy)) %>% #filters out NA values
  #Filters by specific date so that 1 point for each country is recorded (both variables are date independent so the date is not relevant)
  filter(date == "2021-12-31")

#Plots a graph of electoral democracy vs level of freedom
ggplot(covid_dataset_interest_elect, aes(x = electoral_democracy, y = level_of_democracy)) +
  geom_point(size = 2, alpha = 0.4, colour = 'blue') +
  labs(
    x="Electoral Democracy", 
    y="Level of Democracy",
    title='Electoral Democracy vs Level of Democracy',
    caption='Source: Our World in Data and V-Dem Project',
    tag='A') 
#shows 1 to 1 correlation between variables
#no values of level_of_democracy for some (e.g. United States) but there is data for electoral_democracy

#Creates a new subset dataset looking at political freedom and level of democracy correlation
covid_dataset_interest_freedom <- covid_dataset_interest %>%
  filter(!is.na(political_freedom)) %>%
  filter(!is.na(level_of_democracy)) %>%
  filter(date == "2021-12-31") %>%
  mutate(political_freedom = as.factor(political_freedom)) #converts political_freedom variable to a factor (0,1,2)

#Plot of political freedom against level of democracy
ggplot(covid_dataset_interest_freedom, aes(x = political_freedom, y = level_of_democracy)) +
  geom_jitter(size = 2, alpha = 0.4, colour = 'blue', width = 0.1) + #adds jitter to points as plotting categorical data
  scale_x_discrete(labels = c('Not Free','Partly Free','Free')) +
  labs(
    x="Political Freedom", 
    y="Level of Democracy",
    title='Political Freedom vs Level of Democracy',
    caption='Source: Our World in Data and V-Dem Project',
    tag='B'
    )

#Imputes level_of_democracy variable by (in order): 
#1. electoral democracy values, 
#2. mean values by level of freedom groups, 
#3. the mean of the level of democracy

covid_dataset_interest <- covid_dataset_interest %>%
  #Replaces any missing values in level_of_democracy with the electoral_democracy value
  mutate(level_of_democracy = ifelse(is.na(level_of_democracy), electoral_democracy, level_of_democracy)) %>%
  #Calculates the mean for level_of_democracy and creates a new variable to store it
  mutate(level_of_democracy_mean = mean(level_of_democracy, na.rm=TRUE)) %>%
  #Calculates the mean for level_of_democracy for each category of political freedom and stores it in a new variable
  group_by(political_freedom) %>%
  mutate(level_of_democracy_mean_freedom = mean(level_of_democracy, na.rm=TRUE)) %>%
  #If the political freedom row had no value, then replaces the mean_freedom value with the overall mean
  mutate(level_of_democracy_mean_freedom = ifelse(is.na(political_freedom), level_of_democracy_mean, level_of_democracy_mean_freedom)) %>%
  #Replaces any missing values with the mean values calculated (either of groups of political freedom, or overall if political freedom NA)
  mutate(level_of_democracy = ifelse(is.na(level_of_democracy), level_of_democracy_mean_freedom, level_of_democracy)) %>%
  ungroup() %>% #ungroups the dataset
  mutate(
    level_of_democracy_mean = NULL,
    level_of_democracy_mean_freedom = NULL,
    electoral_democracy = NULL,
    political_freedom = NULL) #removes the temporary variables used to store means

sum(is.na(covid_dataset_interest$level_of_democracy))  #checks for the amount of missing values in the level_of_democracy column


###4. NA: government transparency
sum(is.na(covid_dataset_interest$gov_transparency)) #checks for the amount of missing values in the gov_transparency column

covid_dataset_interest <- covid_dataset_interest %>%
  filter(!is.na(gov_transparency)) #removes countries with no transparency data

###5.
summary(covid_dataset_interest) #shows statistics for each of the variables in the dataset
View(covid_dataset_interest)

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 5: Analysis of dataset: correlations
##

#Creates a new dataset that controls for date
covid_dataset_analysis_dateconst <- covid_dataset_interest %>%
  filter(date == "2022-07-03")

View(covid_dataset_analysis_dateconst)

#Plots a scatterplot of level of democracy against total cases per million
ggplot(covid_dataset_analysis_dateconst, aes(x = level_of_democracy, y = total_cases_per_million)) +
  geom_point(size = 2, alpha = 0.4, colour = 'blue') +
  labs(
    x="Level of Democracy (MPI)", 
    y="Total Cases (per million)",
    title='MPI vs Total Cases (per million)', 
    subtitle = 'between: 2020-01-01 and 2022-07-03',
    caption='Source: Our World in Data and V-Dem Project',
    tag='A') + 
  geom_smooth(method = 'lm', se = FALSE, colour = 'darkorange') + #plots a linear line of best fit
  stat_cor(method = "pearson", label.x = 0.05, label.y = 5.1e+05, size = 3.5) #calculates R and p values and plots them

#Plots a scatterplot of government transparency against total cases per million
ggplot(covid_dataset_analysis_dateconst, aes(x = gov_transparency, y = total_cases_per_million)) +
  geom_point(size = 2, alpha = 0.4, colour = 'blue') +
  ylim(-2.1e+05, 6e+05) +
  labs(
    x="Government Transparency", 
    y="Total Cases (per million)",
    title='Government Transparency vs Total Cases (per million)', 
    subtitle = '2022-07-03',
    caption='Source: Our World in Data and HRV Transparency Project',
    tag='B') + 
  geom_smooth(method = 'lm', se = FALSE, colour = 'darkorange') + #plots a linear line of best fit
  stat_cor(method = "pearson", label.x = -6.4, label.y = 5.1e+05, size = 3.5) #calculates R and p values and plots them

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 6: Analysis of dataset: government transparency bar chart
##

#Creates a subset dataset including the 8 highest and 8 lowest transparency index countries
covid_dataset_country_transparency <- covid_dataset_analysis_dateconst %>%
  filter(gov_transparency >= 4.66 | gov_transparency <= -0.95) %>%
  arrange(desc(gov_transparency)) #orders dataset by largest to smallest government transparency

View(covid_dataset_country_transparency)

#Plots a bar graph of the top 8 and bottom 8 countries for government transparency
ggplot(covid_dataset_country_transparency, aes(x = gov_transparency, y = reorder(location, gov_transparency))) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  geom_text(
    aes(label = sprintf("%.2f", round(gov_transparency, 2))), size = 3,
    hjust = ifelse(covid_dataset_country_transparency$gov_transparency >= 0, -0.2, 1.1)) + #includes values for each bar 
  theme_minimal() + #changes the theme of the bar chart to minimal
  xlim(-8, 6.8) + #forces x limit values onto the chart
  labs(
    x="Government Transparency", 
    y="Country",
    title='Bar Chart of Government Transparency', 
    subtitle = 'top 8 & bottom 8 countries',
    caption='Source: HRV Transparency Project',
    tag='')

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 7: Analysis of dataset: correlations with other variables
##

#Creates a subset dataset called covid_dataset_corr
covid_dataset_corr <- covid_dataset_interest %>%
  select(where(is.numeric) | date) %>% #selects numeric variables + date
  select(-year) %>% #removes the year variable
  filter(date == "2022-07-03") %>% #filters the data by date
  mutate(date = NULL) #removes the date variable

View(covid_dataset_corr)

#Creates a variable correlations that stores the correlations values of covid_dataset_corr variables
correlations <- cor(covid_dataset_corr, use = "pairwise.complete.obs") #removes NAs by pairwise deletion
p.mat <- cor.mtest(correlations) #creates p values + confidence intervals for the correlations
#Creates a variable col that includes a list of hexidecimal colour list
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) 
#Plots a correlation plot only including significant correlations between variables
corrplot(correlations, method="color", col=col(200), 
         type="upper", order="hclust", 
         addCoef.col = "black", #add correlation coefficient 
         tl.col="black", tl.srt=45,  tl.cex = 0.5, #alters the text label colour, rotation, and position
         p.mat = p.mat, sig.level = 0.01, insig = "blank", number.cex=0.75, #filters out squares with insignificant p values
         diag=FALSE) #hides diagonal squares

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 8: Prediction of Time Series Data - total_cases_per_million
##

#Example subset dataset of Bulgaria
covid_dataset_predict_Bulgaria <- covid_dataset_interest %>%
  filter(location == "Bulgaria")

#Example plot of Bulgaria showing the linear nature of date and total cases per million
ggplot(covid_dataset_predict_Bulgaria, aes(x = date, y = total_cases_per_million)) +
  geom_line(size = 0.8, alpha = 1, colour = 'blue') +
  labs(
    x="Date", 
    y="Total Cases (per million)",
    title='Cumulative Total Cases (per million) of Bulgaria', 
    subtitle = 'between: 2020-03-08 and 2022-07-03',
    caption='Source: Our World in Data',
    tag='') + 
  geom_smooth(method = 'lm', se = FALSE, colour = 'darkorange')  + 
  stat_cor(method = "pearson", label.y = 70000, size = 3.5)


#Creates a training dataset
train_data <- covid_dataset_predict %>%
  select(location, date, total_cases_per_million) %>%
  #Filters out 20% of the dates at the end of the data set #80% of 861 = 689
  filter(date <= "2021-12-23") %>%
  add_column(forecast = "train") #adds a new variable to denote data as training data for plot differentiation

train_data.cat <- train_data %>%
  as_tsibble(key = location, index = date) %>% #converts training dataset to tsibble format by location
  fill_gaps() %>% #fills out missing dates
  model(arima = ARIMA(total_cases_per_million)) %>% #Creates a model of each country using a linear ARIMA model 
  forecast(h = 192) #forecasts/predicts 192 dates in the future (same length as test data)

train_arima_forecast <- as.data.frame(train_data.cat) %>% #converts tsibble dataset back to a dataframe for analysis
  select(location, date, .mean) %>% #selects the country, date and forecast columns
  rename("total_cases_per_million" = ".mean") %>% #renames total_cases_per_million prediction data column
  add_column(forecast = "forecast") #denotes data as forecasting data

#Creates a test dataset
test_data <- covid_dataset_predict %>%
  select(location, date, total_cases_per_million) %>%
  #Contains 20% of the dates at the end of the data set, to be used to test against the forecast
  filter(date > "2021-12-23") %>%
  add_column(forecast = "test")  #denotes data as test data

#Views the training, forecasting, and test cleaned datasets
View(train_data)
View(train_arima_forecast)
View(test_data)

#Combines the 3 datasets above together by the 4 column variables
prediction_dataset_final <- train_data %>%
  full_join(train_arima_forecast) %>%
  full_join(test_data)

#Views the prediction dataset
View(prediction_dataset_final)

#Creates a subset of data from the prediction dataset for Bulgaria
example_plot <- prediction_dataset_final %>%
  filter(location == "Bulgaria")

#Plots a line graph of date vs total_cases_per_million separating forecast, test, and training data by colour
ggplot(example_plot, aes(x = date, y = total_cases_per_million, group=forecast, color=forecast)) +
  geom_line() +
  labs(
    x="Date", 
    y="Total Cases (per million)",
    title='Bulgaria Total Cases (per million) Forecasts',
    tag='')

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 9: Root Mean Squared Error (RMSE) and Normalised Root Mean Squared Error (nRMSE) of each country
##

test_data2 <- test_data %>%
  mutate(forecast = NULL) %>% #removes the forecast column
  rename("actual" = "total_cases_per_million") #renames test data to 'actual'

train_arima_forecast2 <- train_arima_forecast %>%
  mutate(forecast = NULL) %>%  #removes the forecast column
  rename("predicted" = "total_cases_per_million") #renames forecast data to 'predicted'

#Creates a subset dataset that joins the test and forecast data together
dataset_rmse <- test_data2 %>%
  left_join(train_arima_forecast2, by = c("location", "date")) #joins data by both location and date

View(test_data2)
View(train_arima_forecast2)
View(dataset_rmse)

dataset_rmse_corr <- dataset_rmse %>%
  group_by(location) %>%
  mutate(rmse = rmse(actual, predicted)) %>% #Creates a new variable that calculates the RMSE of actual vs predicted
  mutate(minvalue = min(actual)) %>% #finds the minimum value of actual and temporarily stores it
  mutate(maxvalue = max(actual)) %>% #finds the minimum value of actual and temporarily stores it
  mutate(nrmse = rmse / (maxvalue - minvalue)) %>% #calculates a normalised RMSE based off RMSE/(max - min)
  mutate(minvalue = NULL, maxvalue = NULL) #removes temporary min and max values

View(dataset_rmse_corr) #views the dataset containing both RMSE and nRMSE values

#---------------------------------------------------------------------------------------------------------------------------------------
##
###SECTION 10: nRMSE correlation analysis between accuracy and government transparency 
##

#Creates a temporary datset that filters for a single date (to remove duplicates)
covid_dataset_interest_temp <- covid_dataset_interest %>%
  select(location, date, gov_transparency) %>%
  filter(date == "2022-07-03") %>%
  mutate(date = NULL)

#Creates a correlation dataset that takes nRMSE values and joins the gov_transparency variable back to the dataset
dataset_rmse_correlation <- dataset_rmse_corr %>%
  left_join(covid_dataset_interest_temp, by = c("location")) %>%
  filter(date == "2022-07-03") %>%
  mutate(date = NULL, actual = NULL, predicted = NULL) #cleans up the dataset by removing non-used columns

#Plots government transparency vs nRMSE to test for correlation
ggplot(dataset_rmse_correlation, aes(x = gov_transparency, y = nrmse)) +
  geom_point(size = 2, alpha = 0.4, colour = 'blue') +
  labs(
    x="Government Transparency", 
    y="Normalised RMSE (lower better)",
    title='Government Transparency correlation against nRMSE', 
    tag='') + 
  geom_smooth(method = 'lm', se = FALSE, colour = 'darkorange') + #plots a linear line of best fit
  stat_cor(method = "pearson", label.x = -6.4, label.y = 3, size = 3.5) #uses Pearson's correlation coefficient - plots R and p value


dataset_rmse_corr22 <- dataset_rmse_corr %>%
  select(location, date, rmse, nrmse) %>%
  filter(date == "2022-07-03") %>%
  mutate(date = NULL)

write.table(dataset_rmse_corr22 , file = "olstab222.txt", sep = ",", quote = FALSE, row.names = F)
