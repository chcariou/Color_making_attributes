

# R function
# Color attribute from R,G,B
# Work in progress
# By Christophe Cariou, January 27, 2016
# http://www.chcariou.fr

# from Wikipedia: https://en.wikipedia.org/wiki/HSL_and_HSV


colors.attributes <- function(
			colors, # Alias, R, G, B and others...
			Chroma=FALSE,
			Hue=FALSE
  			) {


	n.colors <- dim(colors)[1]
	a.colors <- dim(colors)[2]
	colors$order <- seq(1,n.colors,1)

	# Chroma

		colors$M <- apply(colors[,2:4],1,max)
		colors$m <- apply(colors[,2:4],1,min)
		colors$C  <- colors$M-colors$m

	# Hue

		Red <- subset(colors,M==R & C!=0)
		Red$H <- 60*((Red$G-Red$B)/Red$C+6)
		Red_1 <- subset(Red,H < 360)
		Red_2 <- subset(Red,H >= 360)
		Red_2$H <- Red_2$H-360
		Red <- rbind(Red_1,Red_2)

		Green <- subset(colors,M==G & C!=0 & M!=R)
		Green$H <- 60*((Green$B-Green$R)/Green$C+2)

		Blue <- subset(colors,M==B & C!=0 & M!=R & M!=G)
		Blue$H <- 60*((Blue$R-Blue$G)/Blue$C+4)

		colors <- rbind(Red, Green, Blue)

	# Lightness (HSL)
	# Saturation (HSL)

	# Value (HSV)
	# Saturation (HSV)


	# Return and write


		colors <- colors[order(colors$order,decreasing=FALSE),]
		attributes.liste <- c(1:a.colors)
		if (Chroma==TRUE)  { 
			attributes.liste <- c(attributes.liste, a.colors+4)
			colnames(colors)[a.colors+4] <- "Chroma"
		}
		if (Hue==TRUE)  { 
			attributes.liste <- c(attributes.liste, a.colors+5)
			colnames(colors)[a.colors+5] <- "Hue"
		}

		attributes <- colors[, attributes.liste]
		return(attributes)




}

