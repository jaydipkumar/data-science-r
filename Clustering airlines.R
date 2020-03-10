
#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 

library(data.table)

library(readxl)

EastWestAirlines <- read_xlsx("~/Downloads/Data Science/data set/EastWestAirlines.xlsx",sheet = "data")

colnames(EastWestAirlines)

ncol(EastWestAirlines)

sub_EastWestAirlines <- EastWestAirlines[,2:12]

norm_airline <- scale(sub_EastWestAirlines)

# Hirerachical CLustering

distanct_airline <- dist(norm_airline,method="euclidean")

str(distanct_airline)

airline_clust <- hclust(distanct_airline, method = "complete")

plot(airline_clust, hang = -1)

group_airline <- cutree(airline_clust,k=5)

EastWestAirlines_New <- cbind(EastWestAirlines, group_airline)

setnames(EastWestAirlines_New, 'group_airline', 'group_hclust')

aggregate(EastWestAirlines_New[,2:12],by= list(EastWestAirlines_New$group_hclust), FUN = mean)


airline_kmeans <- kmeans(norm_airline,5)

str(airline_kmeans)

airline_kmeans$centers

EastWestAirlines_New <- cbind(EastWestAirlines_New, airline_kmeans$cluster)

colnames(EastWestAirlines_New)

aggregate(EastWestAirlines_New[,2:12],by= list(EastWestAirlines_New$`airline_kmeans$cluster`), FUN = mean)

library(cluster)

# Using Clara function(Clustering for Large Applications) to find cluster

xcl <- clara(norm_airline,5) #Using Centroid

clusplot(xcl)

xpm <- pam(norm_airline,5) # Using Medoids

clusplot(xpm)

####################################################################

Maha_distanct_airline <- dist(norm_airline,method="manhattan")

str(Maha_distanct_airline)

Maha_airline_clust <- hclust(Maha_distanct_airline, method = "centroid")

plot(Maha_airline_clust, hang = -1)

Maha_group_airline <- cutree(Maha_airline_clust,k=5)

Maha_EastWestAirlines_New <- cbind(EastWestAirlines, group_airline)

setnames(Maha_EastWestAirlines_New, 'group_airline', 'group_hclust')

aggregate(Maha_EastWestAirlines_New[,2:12],by= list(Maha_EastWestAirlines_New$group_hclust), FUN = mean)


Maha_EastWestAirlines_New <- kmeans(norm_airline,5)

str(Maha_EastWestAirlines_New)

Maha_EastWestAirlines_New$centers

Maha_EastWestAirlines_New <- cbind(EastWestAirlines_New, Maha_EastWestAirlines_New$cluster)

colnames(Maha_EastWestAirlines_New)

aggregate(Maha_EastWestAirlines_New[,2:12],by= list(Maha_EastWestAirlines_New$`Maha_EastWestAirlines_New$cluster`), FUN = mean)
#######################################################################


distanct_maxim_airline <- dist(norm_airline,method="maximum")

str(distanct_maxim_airline)

airline_maxim__clust <- hclust(distanct_maxim_airline, method = "average")

plot(airline_maxim__clust, hang = -1)

maxim_group_airline <- cutree(airline_maxim__clust,k=6)

Max_EastWestAirlines_New <- cbind(EastWestAirlines, maxim_group_airline)

setnames(Max_EastWestAirlines_New, 'maxim_group_airline', 'groups_hclust')

aggregate(Max_EastWestAirlines_New[,2:12],by= list(Max_EastWestAirlines_New$groups_hclust), FUN = mean)

max_EastWestAirlines_New <- kmeans(norm_airline,5)

str(max_EastWestAirlines_New)

max_EastWestAirlines_New$centers

max_EastWestAirlines_New <- cbind(EastWestAirlines_New, max_EastWestAirlines_New$cluster)

colnames(Maha_EastWestAirlines_New)

aggregate(Maha_EastWestAirlines_New[,2:12],by= list(Maha_EastWestAirlines_New$`Maha_EastWestAirlines_New$cluster`), FUN = mean)

