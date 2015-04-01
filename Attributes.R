# Color-making attributes
# By Christophe Cariou, March 2015

# Part One: Get color of each pixel
# http://cran.r-project.org/web/packages/raster/index.html

	library(raster)
	image_loc <- "http://lab.chcariou.fr/data/Watchmen_cover1.jpg" # You can insert your image
	image <- brick(image_loc) # Transform your jpg in a raster object
	image_width <- dim(image)[2] 
	image_height <- dim(image)[1] 
	image_colors <- data.frame(getValues(image)) # Get colors
	colnames(image_colors) <- c("R","G","B") # Rename columns in Red, Green and Blue
	image_colors$id <- seq(1, image_width*image_height,1) # Create an id for each pixel of the image, important if you want to rebuild the image

	head(image_colors) # See the result
	tail(image_colors) # See the result


# Part Two: Get color-making attributes of an image
# http://en.wikipedia.org/wiki/HSL_and_HSV

	# Prepare data
	image_colors$n <- 1 # For aggregate
	image_colors$R <- image_colors$R/255 # RGB in (0,1)
	image_colors$G <- image_colors$G/255 # RGB in (0,1)
	image_colors$B <- image_colors$B/255 # RGB in (0,1)

	# Prepare data storage

	attributes <- data.frame(matrix(,nrow=1,ncol=2))
	colnames(attributes) <- c("Width","Height")
	attributes$Width <- image_width
	attributes$Height <- image_height

	# Number of differents colors
	attributes$N <- dim(aggregate(image_colors$n,by=list(image_colors$R,image_colors$G,image_colors$B),FUN=sum))[1]

	# Red, Green and Blue
	attributes$R <- mean(image_colors$R)
	attributes$G <- mean(image_colors$G)
	attributes$B <- mean(image_colors$B)

	# Intermediaries values
	image_colors$Max <- apply(image_colors[,1:3],1,max)
	attributes$Max <- mean(image_colors$Max)
	image_colors$Min <- apply(image_colors[,1:3],1,min)
	attributes$Min <- mean(image_colors$Min)


	# Lightness measures

		# Intensity
		image_colors$Intensity <-  (image_colors$R+image_colors$G+image_colors$B)/3
		attributes$Intensity <- mean(image_colors$Intensity)

		# Value
		image_colors$Value <- image_colors$Max
		attributes$Value <- mean(image_colors$Value)

		# Lightness
		image_colors$Lightness <- (image_colors$Max+image_colors$Min)/2
		attributes$Lightness <- mean(image_colors$Lightness)

	# Colorfulness measures

		# Chroma
		image_colors$Chroma <- image_colors$Max-image_colors$Min
		attributes$Chroma <- mean(image_colors$Chroma)

		# SaturationV
		image_colors$SaturationV <- 0
		id_change <- subset(image_colors, Value!=0)$id
		image_colors$SaturationV[id_change] <- image_colors$Chroma[id_change]/image_colors$Value[id_change]
		attributes$SaturationV <- mean(image_colors$SaturationV)

		# SaturationL
		image_colors$SaturationL <- 0
		id_change <- subset(image_colors, Lightness!=0 & Lightness!=1)$id
		image_colors$SaturationL[id_change] <- image_colors$Chroma[id_change]/(1-abs(2*image_colors$Lightness[id_change]-1))
		attributes$SaturationL <- mean(image_colors$SaturationL)

	# Hue measure

	image_colors$Hue <- 0
	
	id_change <- subset(image_colors, Chroma!=0 & Max==R)$id
	image_colors$Hue[id_change] <- 60*((image_colors$G[id_change]-image_colors$B[id_change])/image_colors$Chroma[id_change]+6)
	
	id_change <- subset(image_colors, Chroma!=0 & Max!=R & Max==G)$id
	image_colors$Hue[id_change] <- 60*((image_colors$B[id_change]-image_colors$R[id_change])/image_colors$Chroma[id_change]+2)

	id_change <- subset(image_colors, Chroma!=0 & Max!=R & Max!=G & Max==B)$id
	image_colors$Hue[id_change] <- 60*((image_colors$R[id_change]-image_colors$G[id_change])/image_colors$Chroma[id_change]+4)

	id_change <- subset(image_colors, Hue>=360)$id
	image_colors$Hue[id_change] <- image_colors$Hue[id_change]-360

	attributes$Hue <- mean(image_colors$Hue)


# Get XYZ for graphic reprensentations

	# HC Color Wheel
	
	image_colors$HC_X <- image_colors$Chroma*cos(image_colors$Hue/180*pi)
	image_colors$HC_Y <- image_colors$Chroma*sin(image_colors$Hue/180*pi)
	
	attributes$HC_X <- attributes$Chroma*cos(attributes$Hue/180*pi)
	attributes$HC_Y <- attributes$Chroma*sin(attributes$Hue/180*pi)


	# HSL Cylinder
	
	image_colors$HSL_X <- image_colors$SaturationL*cos(image_colors$Hue/180*pi)
	image_colors$HSL_Y <- image_colors$SaturationL*sin(image_colors$Hue/180*pi)
	image_colors$HSL_Z <- image_colors$Lightness

	attributes$HSL_X <- attributes$SaturationL*cos(attributes$Hue/180*pi)
	attributes$HSL_Y <- attributes$SaturationL*sin(attributes$Hue/180*pi)
	attributes$HSL_Z <- attributes$Lightness


	# HSV Cylinder

	image_colors$HSV_X <- image_colors$SaturationV*cos(image_colors$Hue/180*pi)
	image_colors$HSV_Y <- image_colors$SaturationV*sin(image_colors$Hue/180*pi)
	image_colors$HSV_Z <- image_colors$Value

	attributes$HSV_X <- attributes$SaturationV*cos(attributes$Hue/180*pi)
	attributes$HSV_Y <- attributes$SaturationV*sin(attributes$Hue/180*pi)
	attributes$HSV_Z <- attributes$Value


# Results
# image_colors : results by pixels
# attributes : results by image
