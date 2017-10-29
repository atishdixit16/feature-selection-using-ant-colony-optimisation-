data <- read.csv('imports-85.data')
data <- data[,-2]

library(mlr)

pathDistance <- function(data,fraction=0.8) {
	fraction <- distance
	sample_rows <- sample ( nrow(data),  round ( fraction*nrow(data) ) )
	data.train <- data[sample_rows,]
	data.test <- data[-sample_rows,]

	trainTask <- makeRegrTask(data = data.train, target = "outcome")
	testTask <- makeRegrTask(data = data.test, target = "outcome")
	lrn <- makeLearner("regr.xgboost")
	model <- train(lrn, trainTask)
	pred <- predict(model,testTask)
	r_square <- sum ( (pred$data$truth - pred$data$response)**2 ) / length(pred$data$truth)
	r_square
}

ACO <- function(data,n_cities=10, n_ants=50, max_itr=1000, probExploit=0.7, deltaReduce=0.9, fraction=0.8) {

	totalFeatures <- ncol(data) - 1
	phermones <- importance <- rep(1,totalFeatures)

	for (itr in 1:max_itr) {
		pherImpProd <- phermones*importance
		dist_array <- NULL
		minDist <- 10e10
		for (ant in 1:n_ants) {
			path <- NULL
			citiesNotCovered <- 1:n_cities
			preCity <- sample(totalFeatures,1)
			path <- c(path,preCity)
			citiesNotCovered <- citiesNotCovered[-preCity]
			for (i in 1:(n_cities-1)) {
				if (runif(1) < probExploit) {
					nextCity <- citiesNotCovered[ which(pherImpProd[citiesNotCovered] == max(pherImpProd[,citiesNotCovered])) [1] ]
				}
				else {
					probCut <- pherImpProd / sum(pherImpProd)
					nextCity <- sample(citiesNotCovered,1,prob=probCut)
				}
				citiesNotCovered <- citiesNotCovered[-which(citiesNotCovered==nextCity)]
				path <- c(path,nextCity)
			}
			path <- c(path,citiesNotCovered)
			if (pathDistance(path,distance) < minDist) {
				minDist <- pathDistance(data[path],distance)
				minPath <- path
			}
		}
		phermones <- (1-deltaReduce)*phermones
		phermones[minPath] <- phermones[minPath] + deltaReduce*(1/minDist)
		importance[minPath] <- importance[minPath] + 1 
	}
	return(list(path=minPath, distance=minDist))
}

#a <- read.csv('f14.csv')
#distance <- as.matrix(dist(a))
#n_cities <- nrow(distance)
#phermones <- matrix( 1 , n_cities,n_cities )
output <-  ACO(data,n_cities=10, n_ants=50, max_itr=1000, probExploit=0.7, deltaReduce=0.9, fraction=0.8) 
print(output)
