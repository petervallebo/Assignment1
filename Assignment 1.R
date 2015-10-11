library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(countrycode)
library(stringr)
library(ggmap)
library(rvest)
library(gsubfn)

# Read in the data

df = read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")

# Question 1: Create a new dataframe of the stock of paintings at MOMA for each month in the year.

df$Month_acquired=as.yearmon(df$DateAcquired)

stock_month = summarise( group_by(df,Month_acquired),n=n())
stock_month$stock=cumsum(stock_month$n)

# Question 2: Use ggplot2 and your new data frame to plot the the stock of paintings on the y-axis and the date on the x-axis.

#             what kind of geom do you think is appropriate? why?
#             - geom_line, since a line graph is good at visualising the movement in stocks
#             color the geom you have chosen red
#             add a title and custom axis labels

p = ggplot(stock_month, aes(x=as.numeric(Month_acquired), y=stock)) + geom_line(aes(colour='red'))
p + labs(x='Time', y='Number of paintings in stock',title='Number of paintings in stock over time') + theme_minimal() +theme(legend.position="none")

# Question 3: Create the same plot but this time the color should reflect the stock of paintings
#             for curator approved and non-curator approved paintings, respectively

stock_month2 = summarise( group_by(df, CuratorApproved ,Month_acquired),n=n())
stock_month2 = mutate(group_by(stock_month2,CuratorApproved), stock=cumsum(n))

p = ggplot(stock_month2, aes(x=as.numeric(Month_acquired), y=stock, group=CuratorApproved)) + geom_line(aes(color=CuratorApproved))
p + labs(x='Time', y='Number of paintings in stock by curator approved or not',title='Number of paintings in stock over time') + theme_minimal()

# Question 4: Create a new dataframe of the stock of paintings grouped by what department the painting belongs to.

stock_month3 = summarise( group_by(df, Department ,Month_acquired),n=n())
stock_month3 = mutate(group_by(stock_month3,Department), stock=cumsum(n))

# Question 5: Plot this data frame using ggplot2. Which department has had the highest increase in their stock of paintings?

p = ggplot(stock_month3, aes(x=as.numeric(Month_acquired), y=stock, group=Department)) + geom_line(aes(color=Department))
p + labs(x='Time', y='Number of paintings in stock by department',title='Number of paintings in stock over time') + theme_minimal()

#             Prins and illustrated books have had the largest increase in their collection.

# Question 6: Write a piece of code that counts the number of paintings by each artist in the dataset.
#             List the 10 painters with the highest number of paintings in MOMA's collection.

artist =    df %>%
            filter(!is.na(Artist),Artist!=' ') %>%  # Can't get the filter to take he observations with no artist away
            group_by(Artist) %>%
            summarise(n=n()) %>%
            arrange(desc(n))

head(artist,n=11) # List the 10 artists with most paintings - 11 since no. 6 is unknown artists

# Question 7: The variable ArtistBio lists the birth place of each painter. Use this information to create a world map
#             where each country is colored according to the stock of paintings in MOMA's collection.

# Step 1: Find birthplace from the ArtistBio variable

# Scenario 1: First word (nationality)

df$birth1 = as.character(str_trim(str_replace_all(word(df$ArtistBio,1),"[, ( )]", " ")))
head(df$birth1)

# Scenario 2: If born is followed by a country (prioritized)

df$birth2 = as.character(str_trim(str_replace_all(strapply(df$ArtistBio,"born (.*)",simplify = TRUE),"[0-9 ( ) .]","")))
head(df$birth2)

# Step 2: Scraping a conversion table between country and nationality

nat1 <- html("https://www.englishclub.com/vocabulary/world-countries-nationality.htm") %>%
  html_nodes("td:nth-child(2)") %>%
  html_text() %>% 
  as.character()
nat2 <- html("https://www.englishclub.com/vocabulary/world-countries-nationality.htm") %>%
  html_nodes("td:nth-child(1)") %>%
  html_text() %>% 
  as.character()

nat<-data.frame(nat2,nat1) # Conversion table
names(nat)=c("birth3","birth1")
head(nat)

nat2=nat[!duplicated(nat[,2]),] # Removing duplicates in nationality, eg. Dutch meaning both Netherlands and Holland

# Step 3: Use it to convert the nationality variable to countries

df2 = left_join(df,nat2,by="birth1") 

# Step 4: Combining into one varible and summarising

df2$Birth_place = ifelse(is.na(df2$birth2) | df2$birth2=="NULL" | df2$birth2=="" ,
                         as.character(df2$birth3),
                         as.character(df2$birth2))

Birth_place = df2 %>%
              group_by(Birth_place) %>%
              summarise(n=n())              # Can't figure our how to improve the names in R, e.g. all the differnt american.....

# Step 6: Preparing for plot

world = map_data("world")

Birth_place$region = Birth_place$Birth_place

world2=inner_join(world,Birth_place)

# Step 7: PLotting

p = ggplot(world2, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = world2$n)) #+ scale_fill_gradient(trans = "log") + # Can be used to see the difference, and check that each vountry is actually represented
  expand_limits() + 
  theme_minimal()
plot(p)


# Question 8: The Dimensions variable lists the dimensions of each painting. Use your data manipulation skills to calculate
#             the area of each painting (in cm's). Create a data frame of the five largest and five smallest paintings in 
#             MOMA's collection.

# Make two varibles from the dimesions -> muliply to area -> sort by area -> use head and tail functions to find largest and smallest 5

df$test=substring(df$Dimensions,first=str_locate(pattern = '"',df$Dimensions)[,1]+1)
df$test2=str_trim(str_replace_all(df$test,pattern = '[( cm  ")]', " "))
head(df$test2)

df$dim1=as.numeric(substring(df$test2,first=1,last=str_locate(df$test2,pattern="x")[,1]-1))
head(df$dim1) # First dimension

df$dim2=as.numeric(substring(df$test2,first=str_locate(df$test2,pattern="x")[,1]+1))
head(df$dim2) # Second dimenstion

df$area = df$dim1 * df$dim2 # Calculates area

head(df$area)

# Dataframe with the five largest

Largest = head(arrange(df,desc(area)),n=5)

# Dataframe with the five smallest

Smallest = head(arrange(df,area),n=5)
