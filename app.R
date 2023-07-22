

library(tidyr) #used to create tidy data
library(tidyverse) #need t he tidyverse package to read files
library(lubridate) #makes it easier to do the things R does with date-times and possible to do the things R does not
library(ggplot2) #to create charts and add layer for visualization
library(plotly) #makes interactive, publication-quality graphs
library(TSstudio) #provides a set of tools descriptive and predictive analysis of time series data

#Methods and tools for displaying and analysing univariate time series forecasts including exponential smoothing
#via state space models and automatic ARIMA modelling
library(forecast)

#A collection of evaluation metrics, including loss, score and utility functions, that measure regression, 
#classification and ranking performance
library(MLmetrics)

#grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data 
#manipulation challenges: mutate, select, filter, summarize, arrange
library(dplyr)

#Color maps designed to improve graph readability for readers with common forms of color blindness and/or color 
#vision deficiency. The color maps are also perceptually-uniform, both in regular form and also when converted to 
#black-and-white for printing. This is the 'lite' version of the 'viridis' package that also contains 'ggplot2' 
#bindings for discrete and continuous color and fill scales 
library(viridisLite)

library(viridis) #to generate a palette

#Transforms datetime data into a format ready for analysis.It offers two core functionalities; aggregating data to 
#a higher level interval (thicken) and imputing records where observations were absent (pad).
library(padr)

#HTML formats and templates for 'rmarkdown' documents, with some extra features such as automatic table of contents, 
#lightboxed figures, dynamic crosstab helper
library(rmdformats)

#A recipe prepares your data for modeling. We provide an extensible framework for pipeable sequences of feature 
#engineering steps provides preprocessing tools to be applied to data. Statistical parameters for the steps can be 
#estimated from an initial data set and then applied to other data sets. The resulting processed output can then be 
#used as inputs for statistical or machine learning models.
library(recipes)
library(tsibble)#for date manipulation to convert to correct formatting
library(directlabels) # Adding direct labels to ggplot2
library(ggrepel) #using so label text will not overlap
library(xts) #Time Series manipulation
library(scales) #fix the exponent problem to appear without abbrev
library(patchwork) #This is a package that will allow me to add two visuals to same plot
library(rgdal)  # for vector work; sp package should always load with rgdal 
library(sf) # classes and functions for vector data
library(spData)        # load geographic data
library(spDataLarge) #Large datasets for spatial analysis
library(ggspatial) #need this to add the compass
library(usmap) #import the package
library(maps) #contains outlines
library(mapdata) #higher resolution outlines
library(mapproj) #map projections collection
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny) #interactive data
library(leaflet) # For making maps
library(DT) # For making fancy tables
library(vcd)
library(ca)
library(seriation)
library(FactoMineR)
library(vcdExtra)
library(mosaic)
library(hrbrthemes)
#install.packages("heatmaply")
library(heatmaply)


################################################GET_DATA################################################
# read the Samples csv data file and assign the data to Samples
#Read data for CT Accidental Drug Related Deaths 2012-2021
drug_deaths <- read.csv("Accidental_Drug_Related_Deaths_2012-2021.csv")

#Read data for CT population and total deaths 2012-2021
ct_population <- read.csv("CT_PopulationAndDeaths_2012-2021.csv")

#####################################################################################################

#Dover Plains appears to be out of CT with coordinates 41.741205, -73.576515, but those coordinates
#are for Dover Plains, NY. According to the USPS, there is not a Dover Plains, CT. Only one record
#affected

#Groton Long Point, CT with coordinates 41.314944, -72.006353 looks to be outside of CT, but it is an 
#island

#CT shape file from https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

####################################################################################################

##Changes to make to entire file once

#Add new column to count each death
drug_deaths$NumberDeaths = 1  

#replace empty strings with NA because currently most appear as '' instead
drug_deaths[drug_deaths == ''] <- NA


#change Date field from string data type to date data type; Add Year field
drug_deaths <- drug_deaths %>% 
  mutate(Date = mdy(Date),
         Year = year(Date),
         Month_Year = floor_date(ymd(Date), 'month'),
         age_group = 
           case_when(
             Age < 20 ~ '0 - 19',
             Age < 30 ~ '20 - 29',
             Age < 40 ~ '30 - 39',
             Age < 50 ~ '40 - 49',
             Age < 60 ~ '50 - 59',
             Age < 70 ~ '60 - 69',
             TRUE ~ '70 and up'
           )) %>% 
  
  #remove Dover Plains since it is not in CT
  #Leave in the NA's for now, but remove them later.
  filter(is.na(Injury_Latitude) | Injury_Latitude != "41.741205")


drug_deaths_heatmap <- drug_deaths %>% 
  #remove the data with no gender and no age; assuming these are unidentified bodies
  filter(!is.na(Age)) %>% 

  group_by(age_group) %>%
  summarize(
    Heroin = sum(!is.na(Heroin)),
    Cocaine = sum(!is.na(Cocaine)),
    Fentanyl = sum(!is.na(Fentanyl)),
    Oxycodone = sum(!is.na(Oxycodone)),
    Oxymorphone = sum(!is.na(Oxymorphone)),
    Ethanol = sum(!is.na(Ethanol)),
    Hydrocodone = sum(!is.na(Hydrocodone)),
    Benzodiazepine = sum(!is.na(Benzodiazepine)),
    Methadone = sum(!is.na(Methadone)),
    Meth_Amphetamine = sum(!is.na(Meth.Amphetamine)),
    Amphet = sum(!is.na(Amphet)),
    Tramad = sum(!is.na(Tramad)),
    Hydromorphone = sum(!is.na(Hydromorphone)),
    Morphine_Not_Heroin = sum(!is.na(Morphine..Not.Heroin.)),
    Xylazine = sum(!is.na(Xylazine)),
    Gabapentin = sum(!is.na(Gabapentin)),
    Opiate_NOS = sum(!is.na(Opiate.NOS))
  ) 
#pivot table to prepare for heat map

drug_deaths_heatmap_pivotted <- drug_deaths_heatmap %>%
  pivot_longer(-age_group,
               names_to = "Drug", 
               values_to = "Number_of_Deaths")

shinyApp(
  shinyUI( 
    fluidPage(
      
      #Create a title for this Shiny R Lab
      titlePanel("Selections"),      
      
      sidebarPanel(            
        #drop down for drugs
        selectizeInput(
          inputId = "DrugID", 
          label = "Select a drug(s)", 
          choices = sort(unique(drug_deaths_heatmap_pivotted$Drug)), 
          selected = c("Fentanyl","Heroin","Cocaine","Ethanol"),
          multiple = TRUE #select numerous genres
        ),
        
        #drop down for ages
        selectizeInput(
          inputId = "AgeID", 
          label = "Select an Age Range(s)", 
          choices = sort(unique(drug_deaths_heatmap_pivotted$age_group)), 
          selected = c("20 - 29","30 - 39","40 - 49","50 - 59","60 - 69"),
          multiple = TRUE #drop down list, but only select one value
        )
      ),
      mainPanel(plotOutput("plot1"))
    ) #fluidPage end
  ), #ShinyUI end
  
  shinyServer(
    function(input, output) 
    { 
      selectedData <- reactive(
        {drug_deaths_heatmap_pivotted%>% filter(Drug %in% input$DrugID & age_group %in% input$AgeID)}
      )
      output$plot1 <- renderPlot(
        {ggplot(selectedData(), 
                aes(selectedData()$age_group,
                    y=reorder(selectedData()$Drug,selectedData()$Number_of_Deaths), 
                    fill = selectedData()$Number_of_Deaths))+
            scale_fill_gradient2(low = "white", high = "red") +     
            geom_tile(color = "black")+
            guides(fill = guide_colourbar(title = "Number of Deaths"))+
            theme(plot.margin = margin(10, 20, 10, 10),
                  plot.title=element_text(hjust=.5,size=20, face = "bold"),
                  plot.subtitle = element_text(hjust=.5,size=12),
                  axis.title.x = element_text(size=12, face = "bold"),
                  axis.title.y = element_text(size=12, face = "bold"),
                  plot.caption = element_text(color="red", face = "italic", size=10,hjust=.5),
                  plot.background = element_rect(fill="#BFD5E3"),
                  text = element_text(family = "serif")) +
            labs( title = "Accidental Deaths from Drug Overdose in Connecticut 2012-2021",
                  subtitle = "By Drug & Age Group",
                  x = "Age Group",
                  y = "Drug",
                  caption = "Person May Have Multiple Drugs") 
          #ggplotly(heatmap_plotted, tooltip = c("fill"))
        } #ggplot end
      ) #RenderPlot end
    } #function end
  ) #ShinyServer end
) #ShinyApp end      