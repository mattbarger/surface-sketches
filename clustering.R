# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
ws <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(ws)

# Exercise 2:
#   * How many clusters does this method suggest?
## This method suggests three clusters.
#   * Why does this method work? What's the intuition behind it?
## This method is similar to a dendrogram in that it optimizes a minimum euclidian distance for a given 'k' number of
## means clusters. The analyst can now choose an optimal level of clusters based on the derivative between WSS clusters
## (i.e., he can eye-ball the cut-off point, much like a hierarchical dendrogram)
## (Side note, none of this was in the actual k-means or hierarchical literature.  I had to google some of this.)
#   * Look at the code for wssplot() and figure out how it works
## WSSplot is a function that returns the optimal means 

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(ws, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
## This method suggests three clusters given the "knee" (peak) in the 
## data fits 15 criteria of all the indices used.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(ws, 3, nstart=20)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster,wine$Type)
## I would consider this good a clustering, as there seem to be only 6 false positives in the predictive ratings output (at [2,1] and [2,3])

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(wine, dist = fit.km, clus = fit.km$cluster)
## I would consider this a good clustering because it explains more than 50% of the point variability and 
## there seems to be a notable difference between the three clusters. One concern I would have is the
## slight overlap between the upper cluster (cluster 2) and the left-hand-side cluster (cluster 1), but 
## those clusters are also densely packed, so the variability differences may be negligible at this level
## of specification anyway.
