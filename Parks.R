library(ggplot2)
library(tidyverse)
library(ggmap)
library(leaflet)
library(zipcodeR)
library(tidycensus)
library(foreach)
library(stringr)
library(dplyr)
library(data.table)

#Google API key
ggmap::register_google(key = "AIzaSyDjpa_BSzUs11JA1rBa2YFZs3YMrfPiu-A")

#imports data for Austin parks and median income per zipcode
austin_parks = read.csv('/Users/franklinstudent/Desktop/GitHub/City_of_Austin_Parks_data.csv')
travis_parks = read.csv('/Users/franklinstudent/Desktop/GitHub/Travis_Parks.csv')
austin_medianincome = read.csv('/Users/franklinstudent/Desktop/GitHub/Parks/Median_incomes.csv')


#removes unnecessary columns
austin_parks = select(austin_parks, -(1:2), -(4:29), -(31:45), -(47:49), -(51:55))
travis_parks = select(travis_parks, -1, -3, -(6:20))


#removes first column
austin_medianincome = austin_medianincome[,-1]
#removes unnecessary columns
austin_medianincome = select(austin_medianincome,-1, -(2:3), -6)


#removes zipcodes not managed by the city of Austin or Travis county
austin_medianincome = subset(austin_medianincome, austin_medianincome$Zip.Code != '78728')
austin_medianincome = subset(austin_medianincome, austin_medianincome$Zip.Code != '78738')


#renames column
austin_medianincome = rename(austin_medianincome, zipcode = Zip.Code)
austin_medianincome = rename(austin_medianincome, avg.income = Avg..Income.H.hold)


#converts variables to numeric
austin_medianincome$avg.income <- as.numeric(gsub('[$,]', '', austin_medianincome$avg.income))
austin_medianincome$Population <- as.numeric(gsub(',', '', austin_medianincome$Population))


#finds longitude and latitude
travis_parks = travis_parks %>%
  mutate(geocode(FIRST_Address)) 


#finds associated addresses of longitude and latitude
address <- do.call(rbind,
                   lapply(1:nrow(travis_parks),
                          function(i)revgeocode(as.numeric(travis_parks[i,4:5]))))


#combines address vector with austin_parks
travis_parks <- cbind(travis_parks,address)


#extracts zipcode from address vector
travis_parks$zipcode <- substr(str_extract(travis_parks$address," [0-9]{5}, .+"),2,6)


#removes newly created address column 
travis_parks = travis_parks[,-6]


#renames columns in travis parks
travis_parks = rename(travis_parks, address = FIRST_Address)
travis_parks = rename(travis_parks, acres = SUM_Park_Acres)


#finds longitude and latitude associated with each park
austin_parks = austin_parks %>%
  mutate(geocode(ADDRESS)) 


#renames columns in austin parks
austin_parks = rename(austin_parks, address = ADDRESS)
austin_parks = rename(austin_parks, zipcode = ZIPCODE)
austin_parks = rename(austin_parks, acres = ASSET_SIZE)
austin_parks = rename(austin_parks, Name = LOCATION_NAME)

#removes non-Austin zipcodes
austin_parks = austin_parks %>%
  filter(zipcode >= 78700)

#removes non-Austin zipcodes
travis_parks = travis_parks %>%
  filter(zipcode >= 78700)

#combines both data sets of parks in austin
all_austin_parks = rbind(austin_parks, travis_parks)

#finds park percentages
zip_totals = summarize(group_by(all_austin_parks, zipcode), total = n()) %>%
  mutate(percentage = total/sum(total)) 

sum(zip_totals$total)


#adds row for zipcode 78742, zero parks
zip_totals = zip_totals %>% 
  add_row(zipcode = '78742', total = 0, percentage = 0)

#finds ave acre percentages
avg_acre = summarize(group_by(all_austin_parks, zipcode), total = sum(acres)) %>%
  mutate(percentage = total/sum(all_austin_parks$acres)) 

#adds row for zipcode 78742, zero parks
avg_acre = avg_acre %>% 
  add_row(zipcode = '78742', total = 0, percentage = 0)

#barplot of avg acre
ggplot(data = avg_acre, mapping = aes(x = zipcode, y = total))+ 
  geom_col( mapping = aes(x = reorder(zipcode, total), y = total), fill = 'lightblue') + 
  coord_flip() + scale_y_continuous(name = "Percentage of Parks") +
  scale_x_discrete(name = "Zipcode") + ggtitle("Park Distribution in Austin") +
  theme(plot.title = element_text(hjust = 0.5))

#barplot of avg income
ggplot(data = austin_medianincome, mapping = aes(x = zipcode, y = avg.income))+ 
  geom_col( mapping = aes(x = reorder(zipcode, avg.income), y = avg.income), fill = 'red') + 
  coord_flip() + scale_y_continuous(name = "Percentage of Parks") +
  scale_x_discrete(name = "Zipcode") + ggtitle("Park Distribution in Austin") +
  theme(plot.title = element_text(hjust = 0.5))



data = avg_acre%>%
  mutate(ifelse(match(austin_medianincome$zipcode, zip_totals$zipcode), zip_totals$total), 0)

cbind(austin_medianincome, data)

for (i in 1:nrow(austin_medianincome)) 

mean(austin_medianincome$avg.income)
sum(austin_medianincome$Population)


#barplot totals
ggplot(data = zip_totals, mapping = aes(x = zipcode, y = total))+ 
  geom_col(mapping = aes(x = reorder(zipcode, total), y = total), fill = 'darkgreen') + 
  coord_flip() + scale_y_continuous(name = "Total Parks") +
  scale_x_discrete(name = "Zipcode") + ggtitle("Park Distribution in Austin") +
  theme(plot.title = element_text(hjust = 0.5))
            

#barplot percentages
ggplot(data = zip_totals, mapping = aes(x = zipcode, y = percentage)) + 
  geom_bar(stat = "identity")

ggplot(data = zip_totals, mapping = aes(x = zipcode, y = percentage, fill = zipcode))+ 
  geom_col( mapping = aes(x = reorder(zipcode, percentage), y = percentage)) + 
  coord_flip() + scale_y_continuous(name = "Percentage of Parks") +
  scale_x_discrete(name = "Zipcode") + ggtitle("Park Distribution in Austin") +
  theme(plot.title = element_text(hjust = 0.5))






austin_medianincome = austin_medianincome[,-(2:4)]
austin_medianincome = austin_medianincome[,-3]

new_austin_medianincome = cbind(austin_medianincome, zip_totals)


match(austin_medianincome, zip = austin_medianincome$Zip.Code==zip_totals$zipcode)




#creates an interactive google map
austin_parks %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

###############MAY NOT NEED#########################
#creates map of park points
p <- ggmap(get_googlemap(center = c(lon =  -97.733330, lat = 30.266666),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
figure1 = p + geom_point(aes(x = lon, y = lat), 
                         data = all_austin_parks, size = 0.5)+ xlab("Longitude") + ylab("Latitude") + ggtitle("Figure 1") +
  theme(plot.title = element_text(hjust = 0.5))
figure1


austin_parks = austin_parks %>%
  mutate(zipcode = search_radius(lat = lat, lng = lon, radius = 1))

#####################################################




#sums acres in Austin parks
sum(all_austin_parks$acres)
mean(all_austin_parks$acres)


ggplot(all_austin_parks, aes(x = acres)) + 
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 5) +
  geom_vline(aes(xintercept=mean(acres)),
             color="red", linetype="dashed", size=1) + 
  ggtitle("Austin Parks Mean") + theme(plot.title = element_text(hjust = 0.5))


aaa = for (i in austin_medianincome$Population){
  for(i in zip_totals$zipcode){
    vector(population)
  }
}

Aaustin_medianincome = data.frame(zipcode = as.character(austin_medianincome$zipcode))

aaa = bind_rows(Aaustin_medianincome, zip_totals)



aaa = ddply(austin_medianincome, summarise, .variable = c("population"), decreasing = TRUE)


  
lm(Population ~ avg.income, data = austin_medianincome)


#imports data for NYC parks
nyc_parks = read.csv('/Users/franklinstudent/Desktop/GitHub/Parks/OpenData_ParksProperties.csv')


nyc_parks = nyc_parks %>%
  drop_na()


#sums acres in NYC parks
sum(nyc_parks$ACRES)
sum(austin_medianincome$Population)

x <- nyc_parks$NAME311
duplicated(x)
x1 = x[!duplicated(x)]





#creates histogram of Austin Parks
ggplot(austin_parks, aes(x = Acres.of.Land)) + 
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 50) +
  geom_vline(aes(xintercept=mean(Acres.of.Land)),
             color="red", linetype="dashed", size=1) + 
  ggtitle("Austin Parks") + theme(plot.title = element_text(hjust = 0.5))


#creates a histogram of NYC Parks
ggplot(nyc_parks, aes(x = ACRES)) + 
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 50) +
  geom_vline(aes(xintercept=mean(ACRES)),
             color="red", linetype="dashed", size=1) + 
  ggtitle("NYC Parks") + theme(plot.title = element_text(hjust = 0.5))


