
#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

library(data.table)

crime_data <- fread("~/Downloads/Data Science/data set/crime_data.csv")

ncol(crime_data)

crime_data_sub <- crime_data[,2:5]

# Normalized the data

norm_crime_data_sub <- scale(crime_data_sub)

# calculating distance euclidean complete

d <- dist(norm_crime_data_sub, method = "euclidean")

str(d)

crime_cluse <- hclust(d, method = "complete")

plot(crime_cluse, hang=-1)

rect.hclust(crime_cluse,plot(crime_cluse,hang=-1),k=4,border="red")

groups <- cutree(crime_cluse,k=4)

crime_data_final <- cbind(crime_data, groups)

aggregate(crime_data_final[,2:6], by=list(crime_data_final$groups), FUN = mean)

#summary: group 2 have the higher rate of crime.


# calculating distance maximum average

maxdist <- dist(norm_crime_data_sub, method = "maximum")

str(maxdist)

crime_avg_hcl <- hclust(maxdist, method = "average")

plot(crime_avg_hcl, hang=-1)

rect.hclust(crime_avg_hcl,plot(crime_avg_hcl,hang=-1),k=7,border="blue")

groups <- cutree(crime_avg_hcl,k=7)

crime_avg_hcl_final <- cbind(crime_data, groups)

aggregate(crime_avg_hcl_final[,2:6], by=list(crime_avg_hcl_final$groups), FUN = mean)

#summary: group 2 have the higher rate of crime.

airline_k_mean <- kmeans(norm_crime_data_sub,4) #provide how many cluster we want.
str(airline_k_mean)



# calculating distance manhattan centroid

manhattan_dist <- dist(norm_crime_data_sub, method = "manhattan")

str(manhattan_dist)

crime_centroid_hcl <- hclust(manhattan_dist, method = "centroid")

plot(crime_centroid_hcl, hang=-1)

rect.hclust(crime_centroid_hcl,plot(crime_centroid_hcl,hang=-1),k=7,border="blue")

groups <- cutree(crime_centroid_hcl,k=7)

crime_centroid_hcl_final <- cbind(crime_data, groups)

aggregate(crime_centroid_hcl_final[,2:6], by=list(crime_centroid_hcl_final$groups), FUN = mean)

#summary: group 6 have the higher rate of crime.


# calculating distance minkowski median

minkowski_dist <- dist(norm_crime_data_sub, method = "minkowski")

str(minkowski_dist)

crime_median_hcl <- hclust(minkowski_dist, method = "median")

plot(crime_median_hcl, hang=-1)

rect.hclust(crime_median_hcl,plot(crime_median_hcl,hang=-1),k=9,border="blue")

groups <- cutree(crime_median_hcl,k=9)

crime_median_hcl_final <- cbind(crime_data, groups)

aggregate(crime_median_hcl_final[,2:6], by=list(crime_median_hcl_final$groups), FUN = mean)

#summary: group 2 have the higher rate of crime.
