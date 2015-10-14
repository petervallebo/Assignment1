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
  filter(!is.na(Artist),Artist!='Unknown photographer',Artist!='') %>%
  group_by(Artist) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

head(artist,n=10)

# Question 7: The variable ArtistBio lists the birth place of each painter. Use this information to create a world map
#             where each country is colored according to the stock of paintings in MOMA's collection.



#Step 1: First we scrape adjectival names of countries from wikipedia

url <- "https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations"
scrape <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table(fill=TRUE)
scrape <- scrape[[1]]

cn <- select(scrape, `Country name`, Adjectivals)

#Corrections to the scraped data are added

cn$`Country name`[94] = "Great Britain"
cn$`Country name`[115] = "Ireland"
cn$`Country name`[119] = "Ivory Coast"
cn$`Country name`[195] = "USSR"
cn$`Country name`[251] = "United Kingdom"
cn$`Country name`[252] = "United States"

cn$`Country name` <- (str_split_fixed(cn$`Country name`, ",", n=3)[,1])


#Creating references
cn$`Country name`[1] = "United States"
cn$Adjectivals[1] = "U"

cn$`Country name`[2] = "South Africa"
cn$Adjectivals[2] = "South"


cn$`Country name`[4] = "New Zealand"
cn$Adjectivals[4] = "New"

cn$`Country name`[11] = "Yugoslavia"
cn$Adjectivals[4] = "Yugoslav"

cn$Adjectivals[85:87] ="nonamefrench"
cn$Adjectivals[94] = "British"
cn$Adjectivals[129] = "Korean"
cn$Adjectivals[168] = "Dutch"
cn$Adjectivals[169] = "newcaldonian"
cn$Adjectivals[209] = "Scottish"


cn$Adjectivals[222:224] ="nonamesouthies"
cn$Adjectivals[252] = "American"

cn$Adjectivals <- (str_split_fixed(cn$Adjectivals, ",", n=3)[,1])
cn$Adjectivals <- (str_split_fixed(cn$Adjectivals, " or ", n=3)[,1])

#Step 2:
#We extract adjectivals from the artist bio. regex could be better - but I'm not completely familiar with it yet
#Furthermore a charmatch() would be usefull, it is though not supported in my version of RStudio.

df$Adjectivals <- str_extract(df$ArtistBio, "[A-Z][a-z]*")



#-------------This could be used for having three options of matches, I just do not know how to combine them with left join------
#cn$part1 <- (str_split_fixed(cn$Adjectivals, ",", n=3)[,1])
#cn$part2 <- (str_split_fixed(cn$Adjectivals, ",", n=3)[,2])
#cn$part3 <- (str_split_fixed(cn$Adjectivals, ",", n=3)[,3])


#Step 3: Preparing for maps
#Combining the two, we get something that can be read in maps.

dfmaps <- left_join(df, cn, by = "Adjectivals")

mapsum= dfmaps %>%
  group_by(`Country name`) %>%
  summarise(n=n()) 

world = map_data("world")

mapsum$region = mapsum$`Country name`


world2=inner_join(world,mapsum)

#Step 4: Plot

p = ggplot(world2, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = world2$n)) + scale_fill_gradient(trans = "log") + # Can be used to see the difference, and check that each vountry is actually represented
  expand_limits() + 
  theme_minimal()
plot(p)

#I think a better solution would be to have all countries plotted, as one country having 1 contribution isn't a lot different from 0.
#I do not know how to do this in maps.



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
