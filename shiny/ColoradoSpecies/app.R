# CSU Geospatial Centroid R Spatial Tutorial
# Created by Caitlin Mothes, PhD April 2021


# In this tutorial you will learn how to read spatial data directly into R 
# from multiple open-source data bases, plot interactive maps and create 
# interactive web applications with the Shiny package


library(raster)
library(rgdal)
library(dplyr)
library(tmap)
library(rgbif)
library(shiny)


#Read in data to plot

elev <- raster::raster("elevation.tif")

occ <- read.csv("occurrences.csv")
occ$year_char <- as.character(occ$year_char) # To remove comma in popup

occ_sp <- sp::SpatialPointsDataFrame(coords = occ[,c("decimalLongitude", "decimalLatitude")],
                                 data = occ)

proj4string(occ_sp) <- crs(elev)



#Define the UI ---------------------------------

ui = fluidPage(
    
    # App Title
    titlePanel("Species of Colorado"),
    
    h5("This map shows occurrence data for multiple Colorado species downloaded from GBIF using the", code("rgbif"), "package."),
    h5("In this app you can filter the points on the map by species, date of observation, and elevation."),
    h5("You can also click on individual occurrences on the map to view the metadata."),
    
    #Sidebar layout with input and output definitions
    
    sidebarLayout(
        
        #Sidebar panel for outputs
        
        sidebarPanel(
            
            #First input: Species (selectInput)
            checkboxGroupInput(inputId = "species", label = "Species",
                               choices = list("Elk" = "Elk", "Yellow-bellied Marmot" = "Yellow-bellied Marmot",
                                              "Western Tiger Salamander" = "Western Tiger Salamander"),
                               selected = c("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander")),
            
            # Second input: Year (sliderInput)
            sliderInput(inputId = "year", label = "Year",
                        min = 1800, max = 2021, value = c(1800, 2021), sep = ""),
            
            # Third inpit: Month (checkboxGroupInput)
            checkboxGroupInput(inputId = "month", label = "Month",
                               choices = list("Jan" = "Jan", "Feb" = "Feb", "Mar" = "Mar",
                                              "Apr" = "Apr", "May" = "May", "Jun" = "Jun",
                                              "Jul" = "Jul", "Aug" = "Aug", "Sep" = "Sep",
                                              "Oct" = "Oct", "Nov" = "Nov", "Dec" = "Dec"),
                               selected = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                            "Aug", "Sep", "Oct", "Nov", "Dec")
            ),
            
            #Forth input: elevation (sliderInput)
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
        
        #Filter data selected
        species <- subset(occ_sp, Species %in% input$species) %>% 
            subset(year >= input$year[1] & year <= input$year[2]) %>% 
            subset(month %in% input$month) %>% 
            subset(elevation >= input$elevation[1] & elevation <= input$elevation[2])
        
        #render parts of map that will not change dynamically
        tmap::tmap_mode("view")
        tmap::tm_basemap("OpenStreetMap")+
            tmap::tm_shape(shp = species)+
            tmap::tm_dots(col = "Species", size = 0.1, palette = "Dark2", title = "Species Occurences",
                          popup.vars = c("Record Type" = "basisOfRecord", "Year" = "year_char",
                                         "Month" = "month", "Elevation (m)" = "elevation"), id = "Species")+
            tmap::tm_shape(elev) +
            tmap::tm_raster(alpha = 0.8, title = "Elevation (m)")
        
        
        
    })
    
    
    
}



# Run the app ----------------------------------------------

shinyApp(ui = ui, server = server)