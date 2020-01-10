#plot  shark telemetry in lake
# https://rdrr.io/github/vinayudyawer/KUD3D/f/README.md

sapply(c("ks",
         "rgl",
         "raster",
         "tidyverse",
         "lubridate",
         "sf",
         "rayshader",
         "VTrack",
         "KUD3D"),
       require, character.only = TRUE)

## définition des CRS

ll<-CRS("+proj=longlat +datum=WGS84") ## lat/long
crs_utm<-CRS("+init=epsg:28348") ## projected WA (in meters)

# baby shark ----
data(GPSdata)

shark_ll <-
  GPSdata %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326)

shark_utm <-
  shark_ll %>%
  st_transform(crs=28348)

shark_df <-
  shark_utm %>%
  as_Spatial() %>%
  as_tibble() %>%
  transmute(lat = coords.x2,
            lon = coords.x1,
            dep = - Depth#,
            #dt = Date.Time
            )

# ningaloo profondeur  ----
data(ningaloo_bath)

ningaloo_ll<- rasterFromXYZ(ningaloo_bath, crs=ll)

ningaloo_utm <-
  projectRaster(ningaloo_ll, crs=crs_utm) %>%
  crop(.,
       shark_utm %>%
         as_Spatial() %>%
         extent() + 500)


## Set depth exaggeration
depth_exaggeration <- 0.1

## reconfigure bathymetry data for 3D plotting (** to correct mirrored plotting in rayshader)
bath_mat <-
  as.matrix(ningaloo_utm) %>%
  apply(., 2, rev)

# plot la baie
bath_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(bath_mat, zscale = 1/depth_exaggeration), 0.1) %>%
  add_shadow(ambient_shade(bath_mat, zscale = 1/depth_exaggeration), 0.1) %>%
  plot_3d(
    bath_mat,
    baseshape = "rectangle",
    water = T,                 ## render water
    zscale = 1/depth_exaggeration,
    wateralpha = 0.2,
    waterlinecolor = "white",
    waterlinealpha = 0.5,
    windowsize = c(1200, 700),  ## Size of window
    theta = 80, 
    phi = 20,
    fov = 60,
    zoom = 0.8
  )

# baby shark plot 

shark_df %>%
  add_points(
    ras = ningaloo_utm,
    det = .,
    zscale = 1 / depth_exaggeration,
    cont = c(95, 50),
    alphavec = c(0.1, 0.9),
    drawpoints = T,
    size = 1,
    col.pt = "black",
    colors = c("red","red")
  )

