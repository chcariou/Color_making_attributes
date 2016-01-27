# R function
# Extract color palette from images with kmeans algorithm
# By Christophe Cariou, January 27, 2016
# http://www.chcariou.fr


require(raster)

colors.kmeans <- function(
			url.liste, # insert an url or a list of url for images
			nk=6, # choose the number of colors for your palette
			write="" # url to write result in a file if you want  
			) {
				
	ni <- length(url.liste)
	for (i in 1:ni) {
	
		image <- brick(url.liste[i])
		colors <- getValues(image)
		colnames(colors) <- c("R","G","B")
		colors01 <- kmeans(colors,nk,nstart=25,iter.max=10000,algorithm="Lloyd")
		colors02 <- data.frame(rep(i,nk),colors01$centers/255, colors01$size/sum(colors01$size))

		if (i==1) { 
			image.info <- c(image@ncols, image@nrows)
			image.colors <- colors02
		} else { 
			image.info <- rbind(image.info, c(image@ncols, image@nrows)) 
			image.colors <- rbind(image.colors, colors02)
		}
		
	}


	# Information

		image.info <- data.frame(image.info)
		image.info$n <- 1
		image.info <- aggregate(image.info$n,by=list(Width=image.info[,1],Height=image.info[,2]),FUN=sum)
		colnames(image.info)[3] <- "Number"
		image.info <- image.info[order(image.info$Number,decreasing=TRUE),]
		print(paste("Information about your", sum(image.info$Number), "images", sep=" "))
		print(image.info)

	# Color palette

		colnames(image.colors) <- c("Alias","R","G","B","Size")
		return(image.colors)
		if (url.write!="") { write(image.colors,file=paste(url.write,"/image.colors.csv",sep=""),sep=";", row.names=FALSE) }

}
