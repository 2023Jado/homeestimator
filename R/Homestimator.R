# The package estimates the habitat usage by referring to kernel density estimator.

homestim <- function(b,crs_n, h, p, unit_area, mo, ye) {
  names(b) <- c("x", "y", "id")

  library(sp)
  library(sf)
  library(ade4)
  library(adehabitatMA)
  library(CircStats)
  library(adehabitatLT)
  library(adehabitatHR)

  # Use sp library to assign coordinates and projection
  coordinates(b) <- c("x", "y")

  # Assign a crs projection
  sf_file <- st_as_sf(b, coords=c("x", "y"), crs = crs_n)
  sf_points <- as(sf_file, "Spatial")
  #Calculations

  # Estimating the utilization distribution using "reference" bandwidth
  kud_points <- kernelUD(sf_points, h=h)

  # Display the utilization distribution
  image(kud_points)

  #Get the Volume
  vud_points <- getvolumeUD(kud_points)

  # Estimate the homerange from the utilization distribution
  homerange <- getverticeshr(kud_points, p, unout=unit_area)
  home1 <- kernel.area(kud_points, p, unout = unit_area)
  home2 <- as.data.frame(homerange)

  #Plotting
  image(vud_points)

  #Getting values
  homerange$Month <- mo
  homerange$Year <- ye

  # Package to display visualize the data
  library(ggplot2)
  library(gganimate)
  library(gifski)
  library(installr)
  library(magick)
  library(magrittr)

  # Display the area_km2 by group
  ggplot(home2, aes(x = id, y = area,)) +
    geom_point(size = 5) +
    labs(title = paste("Area used by gorilla groups in", homerange$Month, homerange$Year),
         x = "Groups",
         y = "Area (km2)")

  return(homerange)
}
