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
