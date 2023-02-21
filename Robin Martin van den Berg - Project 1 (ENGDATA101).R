# ENGDATA101 Project 1 #
# Robin Martin van den Berg #
# 18/03/2022 #

# Install necessary packages if still needed:

#library(tidyverse)
#library(ggplot2)
#library(dplyr)
#library(readr)
#library(readxl)

#install.packages("viridis")
#library(viridis)

# 1) Importing Data ####

cdc <- read_delim("/Users/robinvandenberg/Library/Mobile Documents/com~apple~CloudDocs/UCR/3rd Semester/Data/Project/Datasets/CDC.txt")

# I imported the cdc data set and assigned it to an object called cdc.

cdc

sum(cdc$`1999`)

# The cdc data set was described as the reported causes of American deaths by the US Center for Disease Control. 

# Visually inspecting the data, we can see that the columns (variables) are years, the rows (observations) are the causes of death, and the values are the proportion of total deaths in a year as a result of a particular cause. We can sum along a column to confirm this and see that the sum of a column is equal to 1.  The data spans from 1999 to 2016.

google <- read_csv("/Users/robinvandenberg/Library/Mobile Documents/com~apple~CloudDocs/UCR/3rd Semester/Data/Project/Datasets/Google-trends.csv")

# I imported the Google data set and assigned it to an object named google.

google

# The google data set was described as the causes of death that Google users search for using the Google search engine. 

# Visually inspecting the data, we can see that the layout of the data set is very similar to the CDC data set. The primary difference is that the rows are the years, and the columns are the causes of death. Given the description of the data set, the values seem to be the proportion of total Google cause-of-death-searches in the USA attributable to a particular cause in a given year. The data spans from 2004 to 2016. 

paths <- list.files(path = "/Users/robinvandenberg/Library/Mobile Documents/com~apple~CloudDocs/UCR/3rd Semester/Data/Project/Datasets/NYT", full.names = T)

nyt <- map_df(paths, read_csv)

# I imported the NYT data sets. However, the importing was slightly trickier this time, as the data for all the years were saved as separate data sets. First the file paths for all the separate data sets were saved as an object called paths. Then the map_df function was used to read in all the data sets using the paths object and combine them into a single data set. This is then assigned to an object called nyt.

nyt

# The nyt data set was described as the causes of death reported in the New York Times in a given year. 

#The nyt data set already tidy - each column is a variable and every row is an observation. The columns are Year, Cause and Count. The rows are individual observations, with each observation having their own particular Year, Cause and Count value. Given the description of the data set, the count column seems to indicate the number of reports in the NYT reporting a particular cause of death in a given year. This could be measured by, for example, the number of articles with a cause of death in the headline, the number of mentions of a cause of death across articles etc. The data spans from 1999 to 2016. 

# 2) Data Wrangling ####

## 2.1) NYT ####

# The nyt dataset is tidy. But it is difficult to manipulate in this form. I will use pivot_wider to make it easier to see all the variables and values, and therefore what data wrangling might be necessary.

nyt <- pivot_wider(nyt, id_cols = year, names_from = Words, values_from = count)

nyt

# Because there are a near infinite number of ways to clean/wrangle the data and combine similar causes together, I have decided to use the "Project_1_prepared_data" file as a guide. The causes of death in that data set are: accident, alzheimers, cancer, diabetes, heart disease, homicide, kidney disease, overdose, respiratory disease, stroke, suicide, terrorism. I will therefore ensure that all causes of death are rearranged into these categories in all data sets. 

# I added new columns for the newly defined causes, and assigned this extended data set to a new object called nyt_1.

nyt_1 <- nyt %>% mutate(
  Accident = `car accident` + pileup + `car crash` + 
    `unintentional injuries`, #I defined accident to include car accidents and unintentional injuries. 
  Heart_disease = `heart disease` + `heart failure` + 
    `cardiovascular disease` , # I defined heart disease to include all causes of death directly related to the heart. 
  Respiratory_disease = `respiratory disease` + bronchitis + emphysema +
    asthma + Influenza + pneumonia + flu , # I defined respiratory disease to include all deaths resulting from a disease that primarily affect the respiratory system. 
  Homicide = homicide + murder + manslaughter + assassination + 
    shootings + `gun violence` + `knife attack` + knifing + lynching , # Homicide is defined to include all deaths caused by another person. However, I exclude deaths as a result of accidents (as defined above) or terrorism (as defined below), as these categories are large enough to warrant their own classification. 
  Terrorism = terrorism + terrorist + `terror attack`, #Terrorism is defined to include all terror related deaths in this data set. It seems trivial to have terrorist and terror attack as additional categories
  Suicide = suicide + `self-harm` , # Deaths resulting from self-harm are included in suicide, as the death is still self-inflicted. 
  Cancer = cancer + `malignant neoplasms` , # Malignant neoplasms are added to cancer as they are a form of cancer.
  Stroke =  stroke + `cerebrovascular diseases` , # Cerebrovascular diseases are included in stroke as they also concern conditions where blood supply to the brain is disrupted. 
  Kidney_disease = `kidney disease` + nephrosis + nephritis + 
    `nephrotic syndrome` # Nephrosis, nephritis and nephrotic syndrome are all included in kidney disease as they are all conditions that affect the kidneys. 
  )

# I then removed all of the now redundant columns. 

nyt_1 <- nyt_1 %>% select(
  -`car accident`, -pileup, -`car crash`, -`unintentional injuries`,
  -`heart disease`, -`heart failure`, -`cardiovascular disease`,
  -`respiratory disease`, - bronchitis, - emphysema, - asthma, - Influenza, -pneumonia, -flu ,
  -homicide, -murder, -manslaughter, -assassination, -shootings, -`gun violence`, 
  -`knife attack`, -knifing, -lynching ,
  -terrorism, -terrorist, -`terror attack`,
  -suicide, -`self-harm` ,
  -cancer, -`malignant neoplasms` ,
  -stroke, -`cerebrovascular diseases` ,
  -`kidney disease`, -nephrosis, -nephritis, -`nephrotic syndrome` 
  )

# The remaining original causes are renamed to comply with the conventions used above.

nyt_1 <- nyt_1 %>% rename(
  Year = year,
  Alzheimers = `alzheimer's disease`, 
  Overdose = `drug overdose`, 
  Diabetes = `diabetes`)

# The data set is now converted back to tidy form using pivot_longer, such that it will have one variable per column and one observation per row.

nyt_1 <- nyt_1 %>% pivot_longer(cols = `Alzheimers`:Kidney_disease, names_to = "Cause", values_to = "n")

# Because the other data sets report causes as a proportion of yearly deaths/searches, the NYT data set is manipulated so that the figures are reported as proportions (or relative frequencies), rather than counts.

nyt_1 <- nyt_1 %>% group_by(Year) %>% mutate(Total_deaths = sum(n)) %>% ungroup()

nyt_1 <- nyt_1 %>% group_by(Year) %>% mutate(n = n/Total_deaths) %>% ungroup()

nyt_1 <- nyt_1 %>% rename( rfreq = n )

# A Source column is added, which will be useful when the data sets are merged. 

nyt_1 <- nyt_1 %>% mutate(Source = "NYT")

# Redundant columns are removed and the final data set is assigned to a new object called nyt_final.

nyt_final <- nyt_1 %>% select(-Total_deaths)

## 2.2) Google ####

google

# Inspecting the data set, we can see that there is an average row at the bottom. So we remove this and assign the adjusted data set to a new object called gogle_1:

google_1 <- google %>% slice(-14)

# The years column is named words, so I rename this:

google_1 <- rename(google_1, Year = Words)

# We can check the class of the Year column below to see the data type of the objects stored within it.

class(google_1$Year)

# The years are stored as character strings. We can parse the strings to convert them to integers:

google_1 <- google_1 %>% mutate(Year = parse_integer(google_1$Year) )

class(google_1$Year)

# The Year column now contains integer objects.

# We can confirm that the values represent proportions of total cause-of-death Google searches in the US in a given year attributable to a particular cause of death. This is because the rows sum to 1. 

i = 1
while(i < 14) {
  cat("Row ", i," sums to: ", sum(google_1[i,2:14]) , "\n")
  i = i + 1 
}

# We can now re-categorise the causes of death, as we did with the NYT data set. We create a new column for these new causes.

google_1 <- google_1 %>% mutate(
  Respiratory_disease = `respiratory disease` + pneumonia # Respiratory diseases is redefined to include pneumonia, as it primarily affects the lungs. 
  )

# The now redundant causes are removed:

google_1 <- google_1 %>% select(-`respiratory disease`, - pneumonia)

google_1 <- google_1 %>% rename(
  Accident = `car accidents`,
  Alzheimers = `alzheimer's`,
  Cancer = cancer,
  Diabetes = diabetes,
  Heart_disease = `heart disease`,
  Homicide = homicide,
  Kidney_disease = `kidney disease`,
  Overdose = overdose,
  Stroke = stroke,
  Suicide = suicide,
  Terrorism = terrorism
)

# The causes are renamed to fit the convention used in the wrangling of the NYT data set.

google_1 <- google_1 %>% pivot_longer(cols = Alzheimers:Respiratory_disease, names_to = "Cause", values_to = "rfreq" )

# The data set is converted to tidy form.

google_final <- google_1 %>% mutate(Source = "Google")

# A new column called Source is added, which is filled with the name of the source of the data in this data set. The finalized data set is assigned to a new object called google_final.

## 2.3) CDC ####

cdc

# We can see that in the cdc data set, the rows are the causes of death and the columns are the years. We can swap the rows and the columns so that is in the same form as the Google data set by transposing the data set and assigning this to a new object called cdc_1.

cdc_1 <- t(cdc)

cdc_1

# Inspecting the data set, we can see that the column names (causes) are missing and are instead included as a row. Similarly, the column containing the years are loaded in as row names. We can save the desired column names (causes) and the current row names (years) as new objects, and then add them to the data set in the desired positions.

col_names_cdc <- as.character(cdc_1[1,])
row_names_cdc <- as.character(row.names(cdc_1))

col_names_cdc
row_names_cdc

class(col_names_cdc)
class(row_names_cdc)

# Before re-adding them to the data set, it is useful to first note the class of the data set itself. As can be seen below, it is not a tibble, but it can be converted to one:

class(cdc_1)
cdc_1 <- as_tibble(cdc_1)
class(cdc_1)

# We can now rename the columns and add a new Year column to the tibble as follows:

cdc_1 <- cdc_1 %>% rename_with(~col_names_cdc)
cdc_1 <- cdc_1 %>% mutate(Year = row_names_cdc)

# We can also move the Year column to the beginning and remove the now redundant causes row:

cdc_1 <- cdc_1 %>% relocate(Year, .after = )
cdc_1 <- cdc_1 %>% slice(-1)

# We should also ensure that the Year column stores the years as numeric data rather than character strings:

cdc_1 <- cdc_1 %>% mutate(Year = as.integer(Year))

# But as we can see, all the other columns store the data as character strings rather than as numeric data:

class(cdc_1$heart)

# Therefore, to make the process of converting the values in the columns to double easier, we can tidy the data first:

cdc_1 <- cdc_1 %>% pivot_longer(cols = heart:terrorism, names_to = "Causes", values_to = "rfreq") 

# We can confirm that the column of relative frequencies still contains character objects:

class(cdc_1$rfreq)

# Thus we can convert it to numeric:

cdc_1 <- cdc_1 %>% mutate(rfreq = as.double(rfreq))

class(cdc_1$rfreq)

# We can now un-tidy the data set so that we can manipulate it:

cdc_1 <- cdc_1 %>% pivot_wider(id_cols = Year, names_from = Causes, values_from = rfreq)

# We can now create new columns for new causes of death:

cdc_1 <- cdc_1 %>% mutate(
  Cancer = cancer_all + cancer_lung + cancer_anal + cancer_breast, # We combine all the forms of caners into a single cause. 
  Homicide = homicide_all + homicide_firearm + homicide_legmil, # As above, we include all forms of killing causes by another, except accidents and terrorism.
  Respiratory_disease = loresp + influpneu # Assuming loresp means lower respiratory tract infection, we group it with respiratory diseases. 
)

# We get rid of all the now redundant columns:

cdc_1 <- cdc_1 %>% select(
  -cancer_all, -cancer_lung, -cancer_anal, -cancer_breast,
  -homicide_all, -homicide_firearm, -homicide_legmil,
  -loresp, -influpneu
)

# We rename the columns using the convention used above:

cdc_1 <- cdc_1 %>% rename(
  Heart_disease = heart,
  Accident = accident,
  Stroke = stroke,
  Alzheimers = alzheimer,
  Diabetes = diabetes,
  Kidney_disease = kidney,
  Suicide = suicide,
  Overdose = overdose,
  Terrorism = terrorism
)

# We can now re-tidy the data:

cdc_1 <- cdc_1 %>% pivot_longer(cols = Heart_disease:Respiratory_disease, names_to = "Cause", values_to = "rfreq" )

# We add a Source column and allocate the finalised data set to a new object called cdc_final. 

cdc_final <- cdc_1 %>% mutate(Source = "CDC")

# 3) Joining ####

# Since we can only join 2 data sets at a time, we start the joining process by creating an intermediate data set which joins cdc_final and nyt_final.

intermediate <- full_join(cdc_final, nyt_final, by = c("Year", "Cause", "Source", "rfreq") )

# We join the intermediate data set with the google_final to create the final data set:

final <- full_join(intermediate, google_final, by = c("Year", "Cause", "Source", "rfreq"))

# Because we know that the google data set does not have data between 1999 and 2003, we can omit these years from the data so that the comparisons in the graphs can cover the same time periods. We assign this filtered data set to a new object called final_1:

final_1 <- final %>% filter(Year != 1999, Year != 2000, Year != 2001, Year != 2002, Year != 2003)

# 4) Visualization ####

# It is very challenging to produce only a single graphic to answer the question, as there is a lot of information to condense into a single plot. Therefore, some compromises with respect to clarity and size had to be made in order to comply with the instructions of mainly relying on a single graphic. 

# THIS GRAPH IS MEANT TO BE DISPLAYED IN FULL SCREEN MODE:

ggplot(final_1) +
  geom_col(aes(x = Year, y = rfreq, fill = Cause)) +
  facet_wrap(~Source, nrow = 3) +
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(breaks = seq(2004, 2016, 1)) +
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 90) ) +
  labs(title = "Proportion of Total Deaths Reported/Searched In The US By Cause (2004-2016)")

# In the graphic we can clearly see that the leading causes of death recorded by the CDC over the time frame are cancer and heart disease. However, the causes of death most often reported by the NYT are terrorism and homicide. On the other hand, the most searched for cause of death on Google seems to be respiratory diseases. However, this would likely be due to the fact that respiratory diseases (like the flu) are very common, and thus often searched for on Google (such as when an infected person is looking for medical help, advice, symptom description etc.). 

# We can treat the CDC data as a proxy for the actual prevalence of different causes of death, the Google data as a proxy for public perception of the prevalence of different causes of death, and the NYT data as the depiction of the prevalence of different causes of death in news media. Therefore, the graph suggests that the news media certainly over report some of the least prevalent causes of death in the US, and under report the most prevalent causes of death. The CDC data suggest that terrorism and homicide contribute a very small amount to yearly deaths, but they are by far the most reported on in the news. Furthermore, cancer and heart disease are the most prevalent in the CDC data, but are among the least reported on in the NYT data. Lastly, public perception of the prevalence of different causes of death in the US seems to fall between the actual prevalence and their depicted prevalence.   

# We can investigate this with more precision and clarity with one additional graphic. We can subset the data to only look at cancer, heart disease, homicide and terrorism reported by the CDC and in the NYT:

final_2 <- final_1 %>% filter(Cause == "Homicide" | Cause == "Cancer" | Cause == "Terrorism" | Cause == "Heart_disease")

final_2 <- final_2 %>% filter(Source == "NYT" | Source == "CDC")

# We can now plot this in a line graph below. THIS GRAPH IS MEANT TO BE DISPLAYED IN FULL SCREEN MODE:

ggplot(final_2) +
  geom_line(aes(x = Year, y = rfreq, color = Cause, linetype = Source)) +
  scale_x_continuous(breaks = seq(2004, 2016, 1)) + 
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 90) ) +
  labs(title = "Proportion of Total Deaths Reported In The US By The CDC and NYT (2004-2016)")

# As we can see, cancer and heart disease make up a large proportion of total deaths in the US over the time period. But, the proportion of deaths caused by cancer or heart disease reported by the NYT is much lower (the dotted red and green lines are far below the solid red and green lines). 

# Furthermore, the proportion of total deaths in the US as a result of terrorism or homicide is very low over the time period. However, the proportion of deaths caused by terrorism or homicide reported in the NYT is much higher (the dotted purple and blue lines are much higher than the solid purple and blue lines). 
