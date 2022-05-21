################################################################################
#                 Make a railway map of Europe in R
#                 Milos Popovic
#                 2022/05/21
################################################################################

# libraries we need
libs <- c("tidyverse", "sf", "giscoR", "ggfx")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. Bulk download railway shapefiles
#------------------------------------

europeList <- function(urlfile, iso3DF) {
  
  urlfile <-'https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv'
  
  iso3DF <- read.csv(urlfile) %>%
        filter(region=="Europe") %>%
        select("alpha.3") %>%
        rename(iso3 = alpha.3)

  return(iso3DF)
}

iso3DF <- europeList()

# create a directory for railway SHPs
dir <- file.path(tempdir(), 'rail')
dir.create(dir)

downloadSHP <- function(urls) {
        
        # make URL for every country
        urls <- paste0("https://biogeo.ucdavis.edu/data/diva/rrd/", 
                iso3DF$iso3, "_rrd.zip")

        # iterate and download
        lapply(urls, function(url) download.file(url, file.path(dir, basename(url))))
}
        
downloadSHP()

# 2. Unzip and load railway shapefiles
#-------------------------------------

unzipSHP <- function(urls) {
        filenames <- list.files(dir, full.names=T)
        lapply(filenames, unzip)
}

unzipSHP()


loadSHP <- function(SHPs, rails) {
        #create a list of railway shapefiles for import
        SHPs <- list.files(pattern="_rails.*\\.shp$")

        #Use lapply to import all shapefiles in the list
        rails <- lapply(SHPs, function(rail_shp) {
                rail <- st_read(rail_shp) %>%  #next, read all shp files as "sf" object and assign WSG84 projection
                st_transform(crs = 4326)
                return(rail)
                }) %>% 
                bind_rows() #finally, merge rail lines into single data.frame

        return(rails)
}

rails <- loadSHP()


# 3. Return the national map of Europe
#-------------------------------------
europeMap <- function(europe) {
  
  europe <- giscoR::gisco_get_countries(
        year = "2016",
        epsg = "4326",
        resolution = "10",
        region = "Europe"
  )

  return(europe)
}

europe <- europeMap()


crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

get_bounding_box <- function(crsLONGLAT, bbox, new_prj, bb) {

  crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

  bbox <- st_sfc(
  st_polygon(list(cbind(
    c(-10.5, 48.5, 48.5, -10.5, -10.5), # x-coordinates (longitudes) of points A,B,C,D
    c(35.000, 35.000, 69.5, 69.5, 35.000)     # y-coordinates (latitudes) of points A,B,C,D
    ))),
  crs = crsLONGLAT)

  new_prj <- st_transform(bbox, crs = crsLAEA)
  bb <- st_bbox(new_prj)

  return(bb)
}

# 4. Plot the railway map of Europe
#----------------------------------

get_railway_map <- function(p, bb) {

        bb <- get_bounding_box()

        p <- ggplot() +
         with_outer_glow(
                geom_sf(data=rails, color="#FF8DE9", size=0.1, fill=NA),
                colour="#FF1493", sigma=15) +
         geom_sf(data=europe, color="grey80", size=0.05, fill=NA) +
         coord_sf(crs = crsLAEA, 
             xlim = c(bb["xmin"], bb["xmax"]), 
             ylim = c(bb["ymin"], bb["ymax"])) +
         theme_minimal() +
         theme(
         axis.line = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title.x = element_text(size=9, color="grey80", hjust=0.25, vjust=3, family = "mono"),
         axis.title.y = element_blank(),
         legend.position = "none",
         panel.grid.major = element_line(color = "black", size = 0.2),
         panel.grid.minor = element_blank(),
         plot.title = element_text(face="bold", size=24, color="grey80", hjust=.5, family = "mono"),
         plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines"), #added these narrower margins to enlarge maps
         plot.background = element_rect(fill = "black", color = NA), 
         panel.background = element_rect(fill = "black", color = NA), 
         legend.background = element_rect(fill = "black", color = NA),
         panel.border = element_blank()) +
         labs(x = "©2022 Milos Popovic (https://milospopovic.net)\n Data: DIVA-GIS, https://www.diva-gis.org/", 
                y = NULL, 
                title = "European railways", 
                subtitle = "", 
                caption = "")

        return(p)
}

p <- get_railway_map()
ggsave(filename="eur_railways.png", width= 8.5, height= 7, dpi = 600, device='png', p)
