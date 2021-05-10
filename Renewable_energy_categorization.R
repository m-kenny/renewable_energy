library(tidyverse)
library(readr)
library(dplyr)
library(skimr)
library(lubridate)
library(stringr)

if (!require("countrycode")) install.packages("countrycode")
library(countrycode)

savehistory(file = "Q1.Rhistory")

loadhistory(file = "./Q1.Rhistory")

history(max.show = 25, reverse = FALSE, pattern, ...)

timestamp(stamp = date(),
          prefix = "##------ ", suffix = " ------##",
          quiet = FALSE)

#get kaggle data
#url <- https://www.kaggle.com/unitednations/international-energy-statistics/download
remove(energystats)
energystats <- read_csv('all_energy_statistics.csv')

#review the Data to get a good understanding of it
#View(energystats)
#names(energystats)

names(energystats)<- c('country', 'commodity', 'year', 'unit', 'quantity', 'quantFootnote', 'category')


unique(energystats$unit)


length(unique(energystats$country))
length(unique(energystats$unit))
length(unique(energystats$commodity))
length(unique(energystats$category))

# remove(energystats)
# extract text after the - so that we can focus on specific data
energystats$subcat<- str_trim(gsub("-.*","",energystats$commodity))
energystats$subcat2 <- str_trim(gsub(".*-","",energystats$commodity))


length(unique(energystats$subcat))
length(unique(energystats$subcat2))

View(energystats)
#View the categories to subcats

categories <- energystats %>%
              group_by(category, subcat, subcat2)%>%
              summarise(quant = sum(quantity))
View(categories)

#Focus on: electricity data set with just total produced!!
electricity <- energystats %>%
              select(country, year, unit, quantity, subcat, subcat2)%>%
              filter(subcat == 'Electricity' & str_detect(subcat2, "total"))
 
View(unique(electricity$subcat2))

electricity <- electricity[!grepl("total net",electricity$subcat2),]
electricity <- electricity[!grepl("total production",electricity$subcat2),]



electricity <- data.frame(electricity)
is.data.frame(electricity)


#add continents on so that it can be more easily viewed
electricity$continent <- countrycode(sourcevar = electricity[, "country"],
                            origin = "country.name",
                            destination = "continent")

#Leaves with the areas of interest for me.
View(unique(electricity$continent))  

#find the NA countries - recehck when renaming completed.
NAcontinent <- electricity %>%
                filter(is.na(continent))
View(unique(NAcontinent$country))

# replace unwanted punction
electricity$country <- str_trim(str_replace_all(electricity$country, "[[:punct:]]", " "))

# tried as if loop couldnt get it working

electricity$continent[electricity$country == 'Ethiopia  incl  Eritrea' ]<-'Africa'
electricity$continent[electricity$country == 'Central African Rep' ] <-'Africa'

electricity$continent[electricity$country == 'Czechoslovakia  former' ] <-'Europe'
electricity$continent[electricity$country == 'German Dem  R   former' ] <-'Europe'
electricity$continent[electricity$country == 'Serbia and Montenegro' ] <-'Europe'
electricity$continent[electricity$country == 'Yugoslavia  SFR  former' ] <-'Europe'

electricity$continent[electricity$country == 'Korea  Dem Ppl s Rep' ] <-'Asia'
electricity$continent[electricity$country == 'Other Asia' ] <-'Asia'
electricity$continent[electricity$country == 'Yemen Arab Rep   former' ] <-'Asia'
electricity$continent[electricity$country == 'Yemen  Dem   former' ] <-'Asia'

electricity$continent[electricity$country == 'Neth  Antilles  former' ] <-'Americas'
electricity$continent[electricity$country == 'United States Virgin Is' ] <-'Americas'

electricity$continent[electricity$country == 'Pacific Islands  former' ] <-'Oceania'



#check units & remove columns not of interest
unique(electricity$category)
unique(electricity$subcat2)


electricitygraph <- electricity %>%
              select(country, year, quantity,subcat2, continent)
electricity
#graphings
ggplot(electricity, aes(year, quantity, fill = continent, colour = subcat2))+
  geom_line()

ggplot(electricity, aes(year, quantity, fill = subcat2))+
  geom_bar(stat = 'identity',position = position_dodge())+
  labs(title = 'Electricity production Category per year', x = 'Year', y= 'Quantity', fill = 'Category')

ggplot(electricitygraph, aes(year, quantity, colour = subcat2, shape = continent))+
  geom_point()+
  labs(title = 'Electricity production Category per year')


#Total Electricity per category produced globally
electot <- electricitygraph %>%
            group_by(year, subcat2)%>%
            summarise(qnt = sum(quantity))

ggplot(electot, aes(year, qnt, colour = subcat2))+
  geom_point()+
  labs(title = 'Global Electricity production Category')

#Electricity production per continent per year.
elecCont <- electricitygraph %>%
  group_by(year,subcat2, continent, country)%>%
  summarise(qnt = sum(quantity))


ggplot(elecCont, aes(year, qnt, colour = subcat2, shape = continent))+
  geom_point()+
  labs(title = 'Electricity production Category per year')

ggplot(elecCont, aes(continent, qnt, fill = subcat2))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title = 'Electricity production',x= 'Continent', y= 'Quantity', fill = 'Category')

electricitygraph %>%
  group_by(continent)%>%
  summarise(cnt = length(unique(country)))
  

#Electricity production per continent.


View(elecCont)
View(elecCont2)
#Animations
library(plotly)
theme_set(theme_bw())



ani <- ggplot(elecCont,  aes(subcat2, qnt, color = continent))+
  geom_point(stat = 'identity', aes(size = qnt, frame = year))+
  labs(y = "Total production per year", x = "")+
  coord_flip()+
  labs(title = "Total Production of Electricity", x = '', y = "Kilowatt-hours, million")
ggplotly(ani)

#change the scall of the data to see it more clearly
ani2 <- ggplot(elecCont,  aes(subcat2, qnt, color = continent))+
  geom_point(stat = 'identity', aes(size = qnt, frame = year))+
  scale_y_log10()+
  labs(y = "Total production per year", x = "")+
  coord_flip()+
  labs(title = "Total Production of Electricity", x = '', y = "Kilowatt-hours, million")
ggplotly(ani2)

#different layout
plotly1 <- ggplot(elecCont,  aes(continent, qnt, label = subcat2, color = subcat2))+
  geom_point( aes(size = qnt, frame=year))
ggplotly(plotly1)

#remove thermal
unique(electricitygraph$subcat2)

elecNothermal <- electricitygraph %>%
  group_by(year,subcat2)%>%
  filter(subcat2 !="total thermal production")%>%
  summarise(qnt = sum(quantity))

ggplot(elecNothermal, aes(year, qnt, colour = subcat2))+
  geom_point()+
  labs(title = 'Electricity production Category per year')


#bar
elecNothermal <- electricitygraph %>%
  group_by(year,subcat2, continent)%>%
  filter(subcat2 !="total thermal production")%>%
  summarise(qnt = sum(quantity))

ggplot(elecNothermal, aes(continent, qnt, fill = subcat2))+
  geom_bar(stat='identity', position = position_dodge())+
  labs(title = 'Electricity production Category per year')
