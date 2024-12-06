# Getting Started ---------------------------------------------------------
library(terra)
library(raster)
library(viridisLite)

europe<-vect('data/shapefiles/23686383/Europe/Europe_merged.shp')
denmark<-europe[europe$COUNTRY == 'Denmark',]

# Get the full extent of the Denmark map
denmark_extent <- ext(denmark)

precip <- list.files('data/precip_01/',pattern = '\\.tif$',full.names = T)
rast <- rast(precip)
denmark_precip <- crop(rast,denmark_extent)

mask <- mask(denmark_precip,denmark)

mask[mask < 0] <- NA
names <- names(mask)
years <- sapply(strsplit(names, '_'), `[`, 4)

min_p <- min(global(mask,fun='min',na.rm=T))
max_p <- max(global(mask,fun='max',na.rm=T))

for(i in 1:24){
  pdf(file = file.path('figs/precip',paste0(years[i],'.pdf')))
  
  plot(mask[[i]],main = years[i],range=c(min_p,max_p))
  
  dev.off()
}

test <- rast('data/CHELSA_pr_01_1979_V.2.1.tif')
test
# make a gif --------------------------------------------------------------


### Make Hilda gif
#Set the paths to your PDF files (replace with your actual PDF file paths)
library('magick')
library('pdftools')
path <- 'figs/precip/'
pdfs <- list.files(path,'*.pdf')

# Initialize an empty list to store the images
images <- list()

# Loop through each PDF and convert each page to an image
for (pdf in pdfs) {
  img <- image_read_pdf(file.path(path,pdf), density = 150)  # Read the PDF with desired resolution (150 dpi)
  images <- c(images, img)  # Append the images to the list
}

# Combine the images into a GIF
gif <- image_animate(image_join(images), fps = 1)  # Set frames per second (adjust fps as needed)

# Save the GIF
image_write(gif, "precip_januari.gif")

# View the GIF (optional)
plot(gif)

plot(denmark)
