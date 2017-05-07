#### prepare raster tiles for immage classification
# by Andreas Scharmüller, Ken Mauser

#### related Tutorials ####
# file:///home/andreas/Documents/WORKSHOPS/201703_RS_jena_spring_school/09_applied_hyperspectral_data_processing-Schratz/00_hsdar.html
# hsdar:: vignette: https://cran.r-project.org/web/packages/hsdar/hsdar.pdf
# # rspatial.org-Tutorial: http://rspatial.org/analysis/rst/9-remotesensing.html

#### packages
require(hsdar)
require(raster)
require(rasterVis)
require(mapview)

#### RapidEye data ####
rapiddir = '/media/andreas/disk_andreas/data/RapidEye/3362507_2012-07-24_RE2_3A_326529'
rapid_files = list.files(rapiddir, pattern = '[0-9]+.tif$', full.names = TRUE)

rapid = raster::brick(rapid_files[1])
# Look at brick properties
nlayers(rapid)
crs(rapid)
ncell(rapid)
dim(rapid)
res(rapid) # spatial resolution

## plot respective layers (1=blue, 2=green, 3=red, 4=RedEdge, 5=NIR):
rasterVis::levelplot(rapid[[1]], margin = FALSE, pretty = TRUE)
plotRGB(rapid, r = 3, g = 2, b = 1, axes = TRUE, stretch = 'lin',
        main = 'RapidEye True Colour Composite')
plotRGB(rapid, r = 4, g = 3, b = 2, axes = TRUE, stretch = 'lin',
        main = 'RapidEye False Colour Composite') # 4th band = Red Edge, 5th band = NIR
mapview(rapid[[5]], na.color = 'transparent', map.types = 'Esri.WorldImagery')

## rename bands
names(rapid) = c('blue', 'green', 'red', 'rededge', 'NIR') # seet names

#### data preparation ####
# Creating a hsdar useable object:
# 1st option: for most hsdar functions an object of class speclib is required
# 2nd option: for large raster files and u want high performance porcessing, use a HyperSpecRaster object

## Gather wavelength information
get.sensor.characteristics(0)
wavelength = get.sensor.characteristics(sensor = "RapidEye", response_function = FALSE)
# get.gaussian.response(get.sensor.characteristics(sensor = "RapidEye"))
# wavelength = c(440, 510, 520, 590, 630, 685, 690, 730, 760, 850)
wavelength = c(440, 520, 630, 690, 760) # just lower borders
#! the hsdar:: vignette claims (p. 85) that a df can be supplied - doesn't seem to work
#! user upper or lower boundaries?
rapid_spec = hsdar::speclib(rapid, wavelength)
rapid_hspec = hsdar::HyperSpecRaster(rapid, wavelength) 

#### Vegetation Index ####

## 1) hsdar::vegindex()
time = Sys.time()
ndvi_rapid_spec = vegindex(rapid_spec, index = 'NDVI2')
Sys.time() - time

rasterVis::levelplot(ndvi_rapid_spec@spectra@spectra_ra, margin = FALSE,
                     pretty = TRUE, col.regions = rev(colorRamps::green2red(400)))

## 2) selfmade function
NDVI = function(img, i, k) { # i & k are indexes for the bands to be used
  bi = img[[i]]
  bk = img[[k]]
  vi = (bk-bi) / (bk+bi)
  return(vi)
}

time = Sys.time()
ndvi = NDVI(rapid, 5, 3) # NIR, red
plot(ndvi, col = rev(terrain.colors(30)), main = 'NDVI from RapidEye')
Sys.time() - time

1+1

#### Unsupervised classification ####
km = kmeans(values(ndvi), centers = 5, iter.max = 500,
            nstart = 3, algorithm = 'Lloyd')
kmr = setValues(ndvi, km$cluster)
plot(kmr, main = 'Unsupervised classifivation of RapidEyeData')


#### Supervised Classification ####


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

## Part 3, 4, 5 are more suited for hyperspectral data not Multispectral!
plot(rapid_spec) # default plots the mean values + SD across all spectra in a speclib object
# Plot a specific spectrum
plot(speclib, FUN = 10, main = '10th spectrum of rapid_spec')






