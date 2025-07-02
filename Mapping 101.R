packages <- c("haven", "ggplot2", "gapminder", "tidyverse", "dplyr", "stringr", "readxl", "tidyr","reshape2",
              "lubridate", "viridis", "haven", "janitor", "wesanderson", "cowplot", "forcats", "ggrepel", 
              "hrbrthemes","sf","tigris", "censusapi","tmap", "tidycensus", "mapview","ggmap",
              "readxl","openxlsx","fuzzyjoin","tidygeocoder","leaflet","reshape2",
              "tidytuesdayR","treemap","rnaturalearth","wordcloud","sfheaders")
# invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, library, character.only = TRUE))

