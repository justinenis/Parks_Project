library(ggplot2)
library(tidyverse)
library(ggmap)
library(leaflet)
library(zipcodeR)
library(tidycensus)
library(foreach)
library(stringr)
library(dplyr)
library(NbClust)
library(cluster)
library(kableExtra)

#Google API key
ggmap::register_google(key = "AIzaSyDjpa_BSzUs11JA1rBa2YFZs3YMrfPiu-A")

#imports data for Austin parks and median income per zipcode
austin_parks = read.csv('/Users/franklinstudent/Desktop/GitHub/Parks_Project/City_of_Austin_Parks_data.csv')
travis_parks = read.csv('/Users/franklinstudent/Desktop/GitHub/Parks_Project/Travis_Parks.csv')
more_parks = read.csv('/Users/franklinstudent/Desktop/GitHub/Parks_Project/more_parks.csv')
austin_medianincome = read.csv('/Users/franklinstudent/Desktop/GitHub/Parks_Project/Austin_median_income.csv')



#removes unnecessary columns
austin_parks = select(austin_parks, -(1:2), -(4:29), -(31:45), -(47:49), -(51:55))
travis_parks = select(travis_parks, -1, -3, -(6:20))
more_parks = select(more_parks, -(4:5))
austin_medianincome = select(austin_medianincome,-(4:5))


##########################CLEANS INCOME DATA########################
#renames column
austin_medianincome = rename(austin_medianincome, zipcode = ZIP.Code)
austin_medianincome = rename(austin_medianincome, Population = Population..2018.Est..1)


#converts variables to numeric
austin_medianincome$Median.Household.Income <- as.numeric(gsub('[$,]', '', austin_medianincome$Median.Household.Income))
austin_medianincome$Population <- as.numeric(gsub(',', '', austin_medianincome$Population))
austin_medianincome$No..Of.Households..2018.Est.<- as.numeric(gsub(',', '', austin_medianincome$No..Of.Households..2018.Est.))
austin_medianincome$Per.Capita.Income..2018.Est. <- as.numeric(gsub('[$,]', '', austin_medianincome$Per.Capita.Income..2018.Est.))
austin_medianincome$Median.Value.Of.Owner.Occupied.Homes..2018.Est. <- as.numeric(gsub('[$,]', '', austin_medianincome$Median.Value.Of.Owner.Occupied.Homes..2018.Est.))
austin_medianincome$No..Of.Households.With.Income..200K...2108.Est. <- as.numeric(gsub(',', '', austin_medianincome$No..Of.Households.With.Income..200K...2108.Est.))
austin_medianincome$No..With.Master.s.Degrees <- as.numeric(gsub(',', '', austin_medianincome$No..With.Master.s.Degrees))
austin_medianincome$No..With.Bachelor.s.Degree <- as.numeric(gsub(',', '', austin_medianincome$No..With.Bachelor.s.Degree))


#filters zip codes only in austin
austin_medianincome = austin_medianincome %>%
  filter(Zip.Code.City. == 'Austin')

#removes column no longer necessary
austin_medianincome = select(austin_medianincome,-3)

sum(austin_medianincome$Population)




########CLEANS TRAVIS COUNTY PARK DATA
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



##########################CLEANS AUSTIN PARKS DATA############################

#finds longitude and latitude associated with each park
austin_parks = austin_parks %>%
  mutate(geocode(ADDRESS)) 

#renames columns in Austin parks
austin_parks = rename(austin_parks, address = ADDRESS)
austin_parks = rename(austin_parks, zipcode = ZIPCODE)
austin_parks = rename(austin_parks, acres = ASSET_SIZE)
austin_parks = rename(austin_parks, Name = LOCATION_NAME)

#removes non-Austin zip codes
austin_parks = austin_parks %>%
  filter(zipcode >= 78701)

#removes non-Austin zip codes
travis_parks = travis_parks %>%
  filter(zipcode >= 78701)

#combines both data sets of austin and travis county parks in austin
all_austin_parks = rbind(austin_parks, travis_parks)


##########################CLEANS MORE_PARKS DATA################################

more_parks = more_parks %>%
  mutate(geocode(Address))

#finds associated addresses of longitude and latitude
address <- do.call(rbind,
                   lapply(1:nrow(more_parks),
                          function(i)revgeocode(as.numeric(more_parks[i,4:5]))))

#combines address vector with austin_parks
more_parks <- cbind(more_parks,address)

#extracts zipcode from address vector
more_parks$zipcode <- substr(str_extract(more_parks$address," [0-9]{5}, .+"),2,6)

#removes newly created address column 
more_parks = more_parks[,-2]

#renames columns in parks
more_parks = rename(more_parks, acres = Acres.of.Land)
more_parks = rename(more_parks, Name = Park.Name)

#combines all park data
all_austin_parks = rbind(all_austin_parks, more_parks)

#reorders columns
column_order <- c("Name", "address", "zipcode", "acres", "lon", "lat")
all_austin_parks = all_austin_parks[,column_order]




#finds park percentages
zip_totals = summarize(group_by(all_austin_parks, zipcode), total = n(), acres = sum(acres)) %>%
  mutate(share_of_parks = total/sum(total)) %>%
  mutate(avg_acre_in_zip = acres/total)

#adds row for zipcode 78742, which had zero parks
zip_totals = zip_totals %>% 
  add_row(zipcode = '78742', total = 0, acres = 0,share_of_parks = 0, avg_acre_in_zip = 0)

#merges data sets 
merged_data = merge(zip_totals, austin_medianincome)

#parks per capita per 100000
merged_data = merged_data %>%
  mutate(parks_per100000 = total/Population *100000)


#barplot park totals
ggplot(data = zip_totals, mapping = aes(x = zipcode, y = total))+ 
  geom_col(mapping = aes(x = reorder(zipcode, total), y = total), fill = 'darkgreen') + 
  coord_flip() + scale_y_continuous(name = "Total Parks") +
  scale_x_discrete(name = "Zipcode") + ggtitle("Park Distribution in Austin") +
  theme(plot.title = element_text(hjust = 0.5))

#acerage per 100000
ggplot(data = merged_data, mapping = aes(x = zipcode, y = parks_per100000))+ 
  geom_col(mapping = aes(x = reorder(zipcode, parks_per100000), y = parks_per100000), fill = 'darkgreen') + 
  coord_flip() + scale_y_continuous(name = "Acreage Per 100000") +
  scale_x_discrete(name = "Zipcode") + ggtitle("Park Distribution in Austin") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=parks_per100000))






#merges data sets for parks and median income
parks_and_income = merge(all_austin_parks, austin_medianincome)

#removes address column
parks_and_income = select(parks_and_income, -3)

parks_and_income$education <-rowSums(cbind(parks_and_income$No..With.Master.s.Degrees, parks_and_income$No..With.Bachelor.s.Degree))

parks_and_income= parks_and_income %>%
  mutate(percent_of_education = education/Population)

parks_and_income= parks_and_income %>%
  mutate(educ_income = percent_of_education*Median.Household.Income)


#map of interaction variable
p <- ggmap(get_googlemap(center = c(lon =  -97.733330, lat = 30.266666),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
map1 = p + geom_point(aes(x = lon, y = lat, color = educ_income),
                      data = parks_and_income, size= 3)+ xlab("Longitude") + ylab("Latitude") + ggtitle("Figure 1") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Median Household Income") 

map1



#sums acres in Austin parks
sum(all_austin_parks$acres)
mean(all_austin_parks$acres)

tinytex::reinstall_tinytex()







##################HIERARCHICAL CLUSTERING OF MERGED DATA########################
X_parks = parks_and_income[,3:16]
X_parks = scale(X_parks, center=TRUE, scale=TRUE)
distances = dist(X_parks, method = "euclidean")

#hierarchical clustering SINGLE = NO, COMPLETE = MAYBE, AVERAGE = MAYBE, WARD.D2 = YES
cluster_parks = hclust(distances, method = 'ward.D2')
plot(cluster_parks, labels = FALSE, hang = -1)



#creates k grid
k_grid = seq(2, 30, by=1)
SSE_grid = foreach(k = k_grid, .combine = 'c') %do% {
  cluster_k = kmeans(X_parks, k, nstart = 50)
  cluster_k$tot.withinss
}

#SSE plot
plot(SSE_grid)

#cut tree for clusters
clusters <- cutree(cluster_parks, k =2)
table(clusters)

#takes cluster vector and applies to parks_and_income
parks_and_income = cbind(parks_and_income, clusters)


#map of park clusters 
map2 = p + geom_point(aes(x = lon, y = lat, color = factor(clusters)), 
                         data = parks_and_income, size = 3)+ xlab("Longitude") + ylab("Latitude") + ggtitle("Figure 2") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Cluster")

map2



#finds cluster in each park
clust_pop = summarize(group_by(parks_and_income, zipcode), total_parks = n(),
                      Population = sum(Population), clusters = sum(clusters)) %>%
  mutate(Population = Population/total_parks) %>%
  mutate(clusters = clusters/total_parks, clusters = round(clusters, digits = 0))

#groups clusters and sums population
clust_pop = summarize(group_by(clust_pop, clusters), Population = sum(Population))

#removes cluster column
clust_pop = select(clust_pop, -1)



#groups clusters and sums parks and acres
clust_totals = summarize(group_by(parks_and_income, clusters), total_parks = n(), Total.Acres = sum(acres)) %>%
  mutate(Share.of.Parks = total_parks/sum(total_parks)) %>%
  mutate(Average.Acres = Total.Acres/total_parks) 

#combines clust_totals and population totals found in clust_pop
clust_totals = cbind(clust_totals, clust_pop)

#calculates acres per 100000
clust_totals = clust_totals%>%
  mutate(Acres.Per.100000 = Total.Acres/Population*100000)

#renames columns
clust_totals = rename(clust_totals, Cluster = clusters)
clust_totals = rename(clust_totals, No.of.Parks = total_parks)

#reorders columns
column_order <- c("Cluster", "Population", "No.of.Parks", "Share.of.Parks", "Total.Acres", "Average.Acres", "Acres.Per.100000")
clust_totals = clust_totals[,column_order]

#creates table
kable(clust_totals, caption = "Results", digits = 2)

################################################################################









install.packages("formattable")


library(formattable)


formattable(clust_totals)




#############DONTNEED################
#creates an interactive google map
austin_parks %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()
###################################
