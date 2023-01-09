setwd("C:/Users/Coco/Desktop/UCL UCL/GEOG0114/Assignment/GEOG0114")

# Load the packages with library()
library("sf")
library("tmap")
library("spdep")
library("sp")
library("gstat")
library("geoR")
library(dplyr)
library(ggplot2)
library("spatialreg")


# Use read_sf() function to add shape files for LSOA and Boroughs
london_lsoa_shapefile <- read_sf("London_LSOA_areas.shp")
london_borough_shapefile <- read_sf("London Borough Areas.shp")

# Use read.csv() function to import birth data
Birth_data <- read.csv(file="births_london_lsoa.csv", header = TRUE, sep = ",")
# Use read.csv() function to import independent variables data 
Independent_variable <- read.csv(file="2015_London_LSOA.csv", header = TRUE, sep = ",")

# Join datafiles together
Full_variable <- left_join(Birth_data, Independent_variable, by = c("lsoa11" = "LSOA_code"))
# View the full dataset
View(Full_variable)
# Export ‘Full_variable’ object as .csv
write.csv(Full_variable, file = "London_Birth_and_Indicators_2015.csv", row.names = FALSE)
# Report the estimates for the birth data
summary(Full_variable$X2014_15)
sd(Full_variable$X2014_15)


# Join datafile with London LOSA shapefile uniquely by using "LSOA_CODE" and "lsoa11" column
Spatial_data <- left_join(london_lsoa_shapefile, Full_variable, by = c("LSOA_code" = "lsoa11"))
# Report the overall results
summary(Spatial_data)
# Clean the na values
Spatial_data <- na.omit(Spatial_data)
# Report the overall results
summary(Spatial_data)


# Generate an empty map to visualise outline of London’s LSOA postcodes and control transparencies for borders and areas
tm_shape(london_lsoa_shapefile) + tm_polygons() + 
  tm_compass(type = "arrow" , position = c("right", "top")) + tm_scale_bar(position = c("left", "bottom")) + 
  tm_shape(london_borough_shapefile) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") + 
  tm_layout(frame = FALSE, compass = 0.6, scale = 0.6)

# Plot the histogram of birth data
p1<-hist(Full_variable$X2014_15 , main = " " , xlab ="Birth in 2014-15")
# Plot the histograms of independent variables and combine them into one graph
attach(mtcars)
opar <- par(no.readonly=T)
par(mfrow=c(2,3))
p2<-hist(Full_variable$Employment.Score..rate. , main = " " , xlab="Employment Score Rate")
p3<-hist(Full_variable$Income.Deprivation.Affecting.Children.Index..IDACI..Score..rate. , main = " " , xlab="Income Deprivation Affecting Children Index (IDACI) Score Rate")
p4<-hist(Full_variable$health_deprivation_disability , main = " " , xlab="Health Deprivation and Disability Score")
p5<-hist(Full_variable$Living.Environment.Score , main = " " , xlab="Living Environment Score")
p6<-hist(Full_variable$Crime.Score , main = " " , xlab="Crime Score")
p7<-hist(Full_variable$Barriers.to.Housing.and.Services.Score , main = " " , xlab="Barriers to Housing and Services Score")
par(opar)
detach(mtcars)

# Plot the histogram of birth data on the log scale and combine them into one graph
p1<-hist(log(Full_variable$X2014_15) , main = " " , xlab ="log Birth in 2014-15")
# Plot the histograms of independent variables on the log scale 
attach(mtcars)
opar <- par(no.readonly=T)
par(mfrow=c(2,3))
p2<-hist(Full_variable$log_employment , main = " " , xlab="log Employment Score Rate")
p3<-hist(Full_variable$log_IDACI , main = " " , xlab="log IDACI Score Rate")
p4<-hist(Full_variable$health_deprivation_disability , main = " " , xlab="Health Deprivation and Disability Score")
p5<-hist(Full_variable$log_living_environment , main = " " , xlab="log Living Environment Score")
p6<-hist(Full_variable$Crime.Score , main = " " , xlab="Crime Score")
p7<-hist(Full_variable$Barriers.to.Housing.and.Services.Score , main = " " , xlab="Barriers to Housing and Services  Score")
par(opar)
detach(mtcars)

# Plot the map for birth data
p1<-tm_shape(Spatial_data) + tm_fill("X2014_15", style = "quantile", n = 5, palette = "Blues") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +tm_text("BOROUGHN", size = "AREA") + 
  tm_layout(frame = FALSE, legend.title.size = 0.7, legend.text.size = 0.7, compass = 0.7, scale = 0.7)
print(p1)

# Plot the map for log employment data
p2<-tm_shape(Spatial_data) + tm_fill("log_employment",style = "quantile", n = 5, palette = "Greens") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) + tm_text("BOROUGHN", size = "AREA") + 
  tm_layout(frame = FALSE, legend.title.size = 0.7, legend.text.size = 0.7, compass = 0.3, scale = 0.3)

# Plot the map for log IDACI data
p3<-tm_shape(Spatial_data) + tm_fill("log_IDACI",style = "quantile", n = 5, palette = "PuRd") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.7, legend.text.size = 0.7, compass = 0.3, scale = 0.25)

# Plot the map for health deprivation and disability data
p4<-tm_shape(Spatial_data) + tm_fill("health_deprivation_disability",style = "quantile", n = 5, palette = "Reds") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.6, legend.text.size = 0.6, compass = 0.3, scale = 0.3)

# Plot the map for log living environment data
p5<-tm_shape(Spatial_data) + tm_fill("log_living_environment",style = "quantile", n = 5, palette = "Purples") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.7, legend.text.size = 0.7, compass = 0.3, scale = 0.3)

# Plot the map for Crime data
p6<-tm_shape(Spatial_data) + tm_fill("Crime.Score",style = "quantile", n = 5, palette = "YlOrRd") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.7, legend.text.size = 0.7, compass = 0.3, scale = 0.3)

# Plot the map for Barriers to Housing and Services data
p7<-tm_shape(Spatial_data) + tm_fill("Barriers.to.Housing.and.Services.Score",style = "quantile", n = 5, palette = "BuPu") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 0.8, border.col = "black") +
  tm_compass(type = "arrow" , position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.7, legend.text.size = 0.7, compass = 0.3, scale = 0.3)

# Using tmap_arrange() function to combine the maps together
tmap_arrange(p2,p3,p4,p5,p6,p7, nrow = 3)


# Using lm() function to build a multivariable linear regression model
MLRmodel <- lm(log(X2014_15) ~ log_employment + log_IDACI + health_deprivation_disability
               + log_living_environment + Crime.Score + Barriers.to.Housing.and.Services.Score, data = Spatial_data)

# Include the 'scipen=7' argument in the summary() function to remove scientific notations
options(scipen = 7)
# Using summary() function to report the output stored in object 'MLRmodel'
summary(MLRmodel)


# Using lm() function to build a multivariable linear regression model excluding log employment variable
MLRmodelFitted <- lm(log(X2014_15) ~ log_IDACI + health_deprivation_disability + log_living_environment
                     + Crime.Score + Barriers.to.Housing.and.Services.Score, data = Spatial_data)

# Include the 'scipen=7' argument in the summary() function to remove scientific notations
options(scipen = 7)
# Using summary() function to report the output stored in object 'MLRmodelFitted'
summary(MLRmodelFitted)


# Extract residuals from "MLRmodelFitted" object and dump into "Spatial_data" and call the column "RESIDUALS"
Spatial_data$RESIDUALS <- MLRmodelFitted$residuals

# Using summary() function to report the estimates
summary(Spatial_data$RESIDUALS)
sd(Spatial_data$RESIDUALS)

# Generating a map to visualize whether the residuals show patterns of spatial autocorrelation
tm_shape(Spatial_data) + tm_fill("RESIDUALS", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5,scale=0.7)


##Moran's I Test
# Generating a number for each row
Spatial_data$ROWNUM <- 1:nrow(Spatial_data)
# Changing sf Spatial_data object into a new sp object
Spatial_data_2.0 <- as(Spatial_data, "Spatial")
# Create spatial weights matrix for areas
AreaWeights <- poly2nb(Spatial_data_2.0, row.names = Spatial_data_2.0$ROWNUM)
AreaWeightsMatrix <- nb2mat(AreaWeights, style='B')
AreaWeightMatrixResidual <- mat2listw(AreaWeightsMatrix , style='W')
# Using lm.morantest() function to run the test on the regression model output object "MLRmodelFitted" using lm.morantest()
lm.morantest(MLRmodelFitted, AreaWeightMatrixResidual, alternative="two.sided")



# Using lagsarlm() function to build a Spatial Lag Model 
SpatialLagModel <- lagsarlm(log(X2014_15) ~ log_IDACI + health_deprivation_disability + log_living_environment
                            + Crime.Score + Barriers.to.Housing.and.Services.Score, data = Spatial_data_2.0 , AreaWeightMatrixResidual)

# Using summary() function to report the results, including rho-coefficient, log-likelihood ratio test's p-value and the AIC
summary(SpatialLagModel)

# Extract the residuals for SpatialLagModel object and dump back to original sf Spatial_data object
Spatial_data$RESID_SLY <- SpatialLagModel$residuals
# Using moran.mc() function to conduct Moran's I test 
moran.mc(Spatial_data$RESID_SLY, AreaWeightMatrixResidual, 1000, zero.policy = T)

# Plot the map
tm_shape(Spatial_data) + tm_fill("RESID_SLY", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5 , scale=0.7)

# Using impacts to interpret the results
AreaWeights_2.0 <- as(AreaWeightMatrixResidual, "CsparseMatrix")
trMC <- trW(AreaWeights_2.0, type="MC")
# Using summary() function to report the results 
summary(impacts(SpatialLagModel, tr = trMC, R=100), zstats=TRUE)


# Using errorsarlm() function to build a Spatial Error Model 
SpatialErrorModel <- errorsarlm(log(X2014_15) ~ log_IDACI + health_deprivation_disability + log_living_environment
                                + Crime.Score + Barriers.to.Housing.and.Services.Score, data = Spatial_data_2.0 , AreaWeightMatrixResidual)

# Using summary() function to report the results, including rho-coefficient, log-likelihood ratio test's p-value and the AIC
summary(SpatialErrorModel)

# extract the residuals for SpatialErrorModel object and dump back to original sf Spatial_data object
Spatial_data$RESID_SER <- SpatialErrorModel$residuals
# Using moran.mc() function to conduct Moran's I test
moran.mc(Spatial_data$RESID_SER, AreaWeightMatrixResidual, 1000, zero.policy = T)


# Plot the map
tm_shape(Spatial_data) + tm_fill("RESID_SER", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_shape(london_borough_shapefile) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("BOROUGHN", size = "AREA") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5 , scale=0.75)












