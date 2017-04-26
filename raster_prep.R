# Guide to work with Multi/Hyper-Spectral data
# file:///home/andreas/Documents/WORKSHOPS/201703_RS_jena_spring_school/09_applied_hyperspectral_data_processing-Schratz/00_hsdar.html
# https://cran.r-project.org/web/packages/hsdar/hsdar.pdf

pacman::p_load(hsdar, raster, here, mapview, rasterVis)

# list all files
datadir = '/home/andreas/Documents/WORKSHOPS/201703_RS_jena_spring_school/09_applied_hyperspectral_data_processing-Schratz/data'
files = list.files(datadir, pattern = ".tif$", full.names = TRUE)

rast = raster::brick(files[1])

#### RapidEye data processing ####
rapiddir = '/media/andreas/disk_andreas/data/RapidEye/3362407_2012-05-01_RE4_3A_326528'
rapid_files = list.files(rapiddir, pattern = '[0-9]+.tif$', full.names = TRUE)

rapid = raster::brick(rapid_files[1])

# plot respective layers:
rasterVis::levelplot(rapid[[1]], margin = FALSE, pretty = TRUE)

# mapview the position:
mapview(rapid[[5]], na.color = 'transparent', map.types = 'Esri.WorldImagery')

# Creating a hsdar useable object:
# 1st option: for most hsdar functions an object of class speclib is required
# 2nd option: for large raster files and u want high performance porcessing, use a HyperSpecRaster object

# Before: extract wavelength information of meta data
# We store the wavelength information in a vector. This vector contains the borders of our spectral bands

## hsdar::get.sensor.characteristics() - retrieve wavelength information on sensors -nice!
get.sensor.characteristics(0)
wavelength = get.sensor.characteristics(sensor = "RapidEye", response_function = FALSE)

wavelength = c(440, 510, 520, 590, 630, 685, 690, 730, 760, 850)
wavelength = c(440, 520, 630, 690, 760) # just lower borders
#! the hsdar:: vignette claims (p. 85) that a df can be supplied - doesn't seem to work

rapid_spec = hsdar::speclib(rapid, wavelength)
rapid_hspec = hsdar::HyperSpecRaster(rapid, wavelength) 
# calculations on every spectrum (every pixel) can be done with hsdar and raster package

time = Sys.time()
ndvi_rapid_spec = vegindex(rapid_spec, index = 'NDVI2')
Sys.time() - time

rasterVis::levelplot(ndvi_rapid_spec@spectra@spectra_ra, margin = FALSE,
                     pretty = TRUE, col.regions = rev(colorRamps::green2red(400)))



## Part 3, 4, 5 are more suited for hyperspectral data not Multispectral!
plot(rapid_spec) # default plots the mean values + SD across all spectra in a speclib object
# Plot a specific spectrum
plot(speclib, FUN = 10, main = '10th spectrum of rapid_spec')

1+1
#### Extra ####
## plot wavelength areas from hsdar vignette
data_wv = get.sensor.characteristics("RapidEye", TRUE)
# plot response function
plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]",
     ylab = "Spectral response")
xwl_response <- seq.int(attr(data_wv$response, "minwl"),
                        attr(data_wv$response, "maxwl"),
                        attr(data_wv$response, "stepsize"))
for (i in 1:nrow(data_wv$characteristics))
  lines(xwl_response, data_wv$response[,i], col = i)

