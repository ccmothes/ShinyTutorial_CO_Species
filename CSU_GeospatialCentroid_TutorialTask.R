# CSU Geospatial Centroid R Spatial Tutorial
# Created by Caitlin Mothes, PhD April 2021


# In this tutorial you will learn how to read spatial data directly into R 
# from multiple open-source databases, plot interactive maps and create 
# interactive web applications with the Shiny package


# SET UP ------------------------------------------------------------------
#create package load function for reproducibility
packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }
#install and/or load packages
packageLoad(c("raster", "dplyr", "rgbif", "tmap", "shiny"))

# IMPORT DATA -------------------------------------------------------------

#set a temp directory to import data to

tempdir <- tempdir()


#retrieve a map of Colorado using raster package 'getData' function

colorado <- getData("GADM", country = 'USA', level = 1, path = tempdir) %>% 
  subset(NAME_1 == 'Colorado')

#make an extent object to import occurrences from
ext <- extent(colorado) 

# Import occurrence data for species in Colorado  -----------------------------

# Elk, Marmot, Tiger Salamander (state amphibian!)

#make a string of species names to use in the 'occ_data' function
species <- c("Cervus canadensis", "Marmota flaviventris", "Ambystoma mavortium")

#also make a string of common names to use for plotting later
common_name <- c("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander")

#write a for loop to extract occurrence data for each species

## #create an empty vector to store each species' downloaded occurrence data
occ <- vector("list", length = length(species)) 
for(i in 1:length(occ)){
  
  occ[[i]] <-
    occ_data(
      scientificName = species[i],
      hasCoordinate = TRUE,
      geometry = bbox(ext),
      limit = 2000
    ) %>%
    .$data #return just the data frame
  
  # add species name column as ID to use later
  occ[[i]]$ID <- common_name[i]
  
  #clean by removing duplicate occurrences
  occ[[i]] <-
    occ[[i]] %>% distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
    select(Species = ID,
           decimalLatitude,
           decimalLongitude,
           year,
           month,
           basisOfRecord) #only keep relevant variables
  
  
  
  print(i) # this prints each element once its finished so you can see the progress
  
}

# Bind all data frames together
occ <- do.call("rbind", occ) 

#need to edit year and month from numeric to character for display later
occ$year_char <- as.character(occ$year)
occ$month <- month.abb[occ$month]

#make spatial points data frame
occ_sp <- SpatialPointsDataFrame(coords = occ[,c("decimalLongitude", "decimalLatitude")],
                                 data = occ)


# extract environmental data ------------------------------------------

#elevation data
elev <- getData("alt", country = "USA", mask = TRUE, path = tempdir) 
elev <- elev[[1]] #just keep first element that is continental US

elev <- crop(elev, colorado) #crop to the colorado polygon

#create a new column with the elevation extracted at each point
occ_sp$elevation <- raster::extract(elev, occ_sp)


# interactive tmap  ----------------------------------------------------------------


#first get occurrences in same projection as raster layer
proj4string(occ_sp) <- crs(elev)


#initiate an interactive tmap opject
tmap_mode("view")

tm_basemap("OpenStreetMap")+
  #add occurrences
  tm_shape(shp = occ_sp)+
  #color the points by species and add pop-ups for each point with the metadata
  tm_dots(
    col = "Species",
    size = 0.1,
    palette = "Dark2",
    title = "Species Occurences",
    popup.vars = c(
      "Record Type" = "basisOfRecord",
      "Year" = "year_char",
      "Month" = "month",
      "Elevation (m)" = "elevation"
    ),
    id = "Species"
  ) +
  #add elevation
  tm_shape(elev) +
  #add title and slight transparency to view basemap
  tm_raster(alpha = 0.8, title = "Elevation (m)")


# Build a shiny app -------------------------------------


#Define the UI ------------------------------------------

ui = fluidPage(
  
  # App Title
  titlePanel("Species of Colorado"),
  
  #Add some header information
  h5(
    "This map shows occurrence data for multiple Colorado species downloaded from GBIF using the",
    code("rgbif"),
    "package."
  ),
  h5(
    "In this app you can filter the points on the map by species, date of observation, and elevation."
  ),
  h5("You can also click on individual occurrences on the map to view the metadata."),
  
  
  
  #Sidebar layout with input and output definitions
  
  sidebarLayout(
    
    #Sidebar panel for outputs, these are the elements users can interact with
    
    sidebarPanel(
      
      #First input: Species
      checkboxGroupInput(
        inputId = "species",
        label = "Species",
        choices = list(
          "Elk" = "Elk",
          "Yellow-bellied Marmot" = "Yellow-bellied Marmot",
          "Western Tiger Salamander" = "Western Tiger Salamander"
        ),
        selected = c("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander")
      ),
      
      # Second input: Year
      sliderInput(inputId = "year", label = "Year",
                  min = 1800, max = 2021, value = c(1800, 2021), sep=""),
      
      # Third input: Month 
      checkboxGroupInput(inputId = "month", label = "Month",
                         choices = list("Jan" = "Jan", "Feb" = "Feb", "Mar" = "Mar",
                                        "Apr" = "Apr", "May" = "May", "Jun" = "Jun",
                                        "Jul" = "Jul", "Aug" = "Aug", "Sep" = "Sep",
                                        "Oct" = "Oct", "Nov" = "Nov", "Dec" = "Dec"),
                         selected = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                      "Aug", "Sep", "Oct", "Nov", "Dec")
      ),
      
      #Fourth input: elevation 
      sliderInput(inputId = "elevation", label = "Elevation",
                  min = 1000, max = 4500, value = c(1000, 4500))
      
      
    ),
    
    mainPanel(
      tmapOutput("map")
    )
    
  )
  
  
)

# Define the Server -------------------------------------
server <- function(input, output){
  
  
  output$map <- renderTmap({
    
    #Filter species selected
    species <- subset(occ_sp, Species %in% input$species)
    
    #Filter elevation selected
    species_elev <-
      subset(species,
             elevation >= input$elevation[1] & elevation <= input$elevation[2])
    
    #render interactive tmap
    tmap::tmap_mode("view")
    tmap::tm_basemap("OpenStreetMap")+
      tmap::tm_shape(shp = species_elev)+
      tmap::tm_dots(
        col = "Species",
        size = 0.1,
        palette = "Dark2",
        title = "Species Occurences",
        popup.vars = c(
          "Record Type" = "basisOfRecord",
          "Year" = "year",
          "Month" = "month",
          "Elevation (m)" = "elevation"
        ),
        id = "Species"
      ) +
      tmap::tm_shape(elev) +
      tmap::tm_raster(alpha = 0.8, title = "Elevation (m)")
    
    
  })
  
  
  
}




# Run the app ----------------------------------------------

shinyApp(ui = ui, server = server)

