library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(countrycode)
library(stringr)
library(ggmap)

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

# Make birthplace variable !!!!!!!!!!

df$nat=word(df$ArtistBio,1)
df$nat2=str_trim(str_replace_all(df$nat,"[, ( )]", " "))

world=      df %>%
            group_by(word(nat2,1)) %>%
            summarise(n=n())

map_data=map_data("world")

# Make map


# Question 8: The Dimensions variable lists the dimensions of each painting. Use your data manipulation skills to calculate
#             the area of each painting (in cm's). Create a data frame of the five largest and five smallest paintings in 
#             MOMA's collection.

# Make two varibles from the dimesions -> muliply to area -> sort by area -> use head and tail functions to find largest and smallest 5

df$test=substring(df$Dimensions,first=str_locate(pattern = '"',df$Dimensions)[,1]+1)
df$test2=str_trim(str_replace_all(test,pattern = '[( cm  ")]', " "))
head(df$test2)

df$dim1=as.numeric(substring(df$test2,first=1,last=str_locate(df$test2,pattern="x")[,1]-1))
head(df$dim1)

df$dim2=as.numeric(substring(df$test2,first=str_locate(df$test2,pattern="x")[,1]+1))
head(dim2)

df$area = df$dim1 * df$dim2

head(area)

# Five largest

Largest = head(arrange(df,desc(area)),n=5)

# Five smallest

Smallest = head(arrange(df,area),n=5)
