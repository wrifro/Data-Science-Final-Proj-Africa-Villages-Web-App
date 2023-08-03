library(shiny)
library(tidyverse)
library(leaflet)
library(sf)

## Reading in info for each landscape
congohhlevel <- read_csv('lab2_fixed.csv')
ndokigs <- read.csv('ndoki_gs.csv')
iturigs <- read.csv('ituri_gs.csv')
ituri <- st_read('ituri.geojson')
ndoki <- st_read("ndoki_4326.geojson")
makira <- st_read("makira_buffer2.geojson")
lactelegs <- read.csv('lactele_gs.csv')
lactele <- st_read('lac_tele.geojson')
makirags <- read.csv('makira_gs.csv')
makirahhlevel <- read.csv('makira_hhLevel.csv')
crossrivergs <- read.csv('crossriver_gs.csv')
crossriverhhlevel <- read.csv('crossriver_hhLevel.csv')
crossriver <- st_read('crossriver_bounds2.geojson')


### SETUP
## Looking at the number and year of survey rounds in each village
iturihhs <- congohhlevel %>% 
  filter(Landscape == 'ituri') %>% 
  mutate(Round = ifelse(SurveyYear == 2020, 2021, SurveyYear)) %>% 
  mutate(Round = as.factor(Round))

villages <- congohhlevel %>%
  group_by(Landscape,SurveyYear,Village) %>%
  summarize(num=n())%>%
  arrange(Landscape,Village) 

iturivillages <- iturihhs %>% 
  group_by(Landscape,Round,Village) %>%
  summarize(num=n())%>%
  arrange(Landscape,Village)

makiravillages <- makirahhlevel %>% 
  group_by(Landscape, SurveyYear, Village) %>% 
  summarize(num = n()) %>% 
  arrange(Landscape, Village)

crossrivervillages <- crossriverhhlevel %>% 
  group_by(Landscape, SurveyYear, Village) %>% 
  summarize(num = n()) %>% 
  arrange(Landscape, Village)

villagespivot <- villages %>%
  pivot_wider(id_cols = Village, names_from = SurveyYear, values_from = num)

iturivillagespivot <- iturivillages %>% 
  pivot_wider(id_cols = Village, names_from = Round, values_from = num)

makira_villagespivot <- makiravillages %>% 
  pivot_wider(id_cols = Village, names_from = SurveyYear, values_from = num)

crossriver_villagespivot <- crossrivervillages %>% 
  pivot_wider(id_cols = Village, names_from = SurveyYear, values_from = num)

# add a variable that will serve as a a Round ID - unique to village and year
congohhlevel <- congohhlevel %>%
  mutate(villyear = str_c(Village,SurveyYear)) #Command that joins 2 strings

iturihhs <- iturihhs %>% 
  mutate(villyear = str_c(Village,Round))

makirahhlevel <- makirahhlevel %>% 
  mutate(villyear = str_c(Village,SurveyYear)) #Command that joins 2 strings

crossriverhhlevel <- crossriverhhlevel %>% 
  mutate(villyear = str_c(Village,SurveyYear)) #Command that joins 2 strings

villroundinfo <- congohhlevel %>%
  select(Landscape, SurveyYear, Village, villyear, vlgLat, vlgLong) %>%
  group_by(Village) %>%
  arrange(Village, SurveyYear) %>%
  distinct(villyear, .keep_all = TRUE) %>% 
  mutate(SurveyRound = row_number(Village))

iturivillroundinfo <- iturihhs %>%
  select(Landscape, Round, Village, villyear, vlgLat, vlgLong) %>%
  group_by(Village) %>%
  arrange(Village, Round) %>%
  distinct(villyear, .keep_all = TRUE) %>% 
  mutate(SurveyRound = row_number(Village))

makiravillroundinfo <- makirahhlevel %>%
  select(Landscape, SurveyYear, Village, villyear, vlgLat, vlgLong) %>%
  group_by(Village) %>%
  arrange(Village, SurveyYear) %>%
  distinct(villyear, .keep_all = TRUE) %>% 
  mutate(SurveyRound = row_number(Village))

crossrivervillroundinfo <- crossriverhhlevel %>%
  select(Landscape, SurveyYear, Village, villyear, vlgLat, vlgLong) %>%
  group_by(Village) %>%
  arrange(Village, SurveyYear) %>%
  distinct(villyear, .keep_all = TRUE) %>% 
  mutate(SurveyRound = row_number(Village))

congohhlevel <- congohhlevel %>%
  left_join(x = congohhlevel, y = villroundinfo, by = 'villyear')

iturihhs <- iturihhs %>% 
  left_join(x = iturihhs, y = iturivillroundinfo, by = 'villyear')

makirahhlevel <- makirahhlevel %>% 
  left_join(x = makirahhlevel, y = makiravillroundinfo, by = 'villyear')

crossriverhhlevel <- crossriverhhlevel %>% 
  left_join(x = crossriverhhlevel, y = crossrivervillroundinfo, by = 'villyear')


## Summarising data for each landscape
# Lac Tele
lactele_gs_rounds <- lactelegs %>% 
  left_join(congohhlevel,by="id") %>% 
  group_by(gs,landscape,SurveyRound)

lactele_table <- lactele_gs_rounds %>% 
  summarise(necProp = mean(necessary), haveProp = mean(have))%>% 
  mutate(diffNecessaryHave = necProp - haveProp)

# Ndoki
ndoki_gs_rounds <- ndokigs %>% 
  left_join(congohhlevel,by="id") %>% 
  group_by(gs,landscape,SurveyRound)

ndoki_table <- ndoki_gs_rounds %>% 
  summarise(necProp = mean(necessary), haveProp = mean(have) ) %>% 
  mutate(diffNecessaryHave = necProp - haveProp)

# Ituri
ituri_gs_rounds<- iturigs %>% 
  left_join(iturihhs,by="id") %>% 
  group_by(gs,landscape,SurveyRound)

ituri_table <- ituri_gs_rounds %>% 
  summarise(necProp = mean(necessary), haveProp = mean(have) )%>% 
  mutate(diffNecessaryHave = necProp - haveProp)


# Makira
makira_gs_rounds <- makirags %>% 
  left_join(makirahhlevel,by="id") %>% 
  group_by(gs,landscape,SurveyRound)

makira_table <- makira_gs_rounds %>% 
  summarise(necProp = mean(necessary), haveProp = mean(have) )%>% 
  mutate(diffNecessaryHave = necProp - haveProp)

# Crossriver
crossriver_gs_rounds <- crossrivergs %>% 
  left_join(crossriverhhlevel,by="id") %>% 
  group_by(GS,Landscape,SurveyRound)

crossriver_table <- crossriver_gs_rounds %>% 
  summarise(necProp = mean(Necessary), haveProp = mean(Have) )%>% 
  mutate(diffNecessaryHave = necProp - haveProp)


#####################
#### SHINY APP ######
#####################
ui <- navbarPage(title = "Exploring Necessities in Protected Landscapes",
                 tabPanel("Ndoki",
                          mainPanel(
                            "The Nouabalé-Ndoki National Park covers more than 4,000 km2 in the northern Republic of Congo. The park is a part of the larger Sangha Tri-National Forest Landscape, a World Heritage Site and is considered to be one of the most intact forest ecosystems in the Congo Basin. Both Poaching and commercial hunting are some of the major conservation challenges in this region."
                          ),
                          sidebarPanel(
                            selectInput(
                              inputId = 'ndokiInput',
                              label = "Select Survey Round",
                              choices = c(1,2,3)),
                            selectInput(
                              inputId = 'ndokiSelect',
                              label = "Select Variable to Sort By",
                              choices = c('PrctNecessary', 'PrctHave', 'diffNecessaryHave')),
                            selectInput(inputId = 'ndokiMapInput',
                                        label = 'Select Variable to Map',
                                        choices = ndoki_table$gs),
                            
                          ),
                          tableOutput('ndokiTable'),
                          plotOutput('ndokiPlot'),
                          leafletOutput('ndokiMap'),
                 ),
                 tabPanel("Ituri", 
                          mainPanel(
                            "The Ituri forest is located within the eastern Democratic Republic of Congo and covers a region of over 63,000 square kilometers. It is known for its rich biodiversity; however, its proximity to the most densely populated region in DRC has enabled deforestation and resource extraction." ,
                          ),
                          sidebarPanel(
                            selectInput(
                              inputId = 'ituriInput',
                              label = "Select Survey Round",
                              choices = c(1,2,3,4)),
                            selectInput(
                              inputId = 'ituriSelect',
                              label = "Select Variable to Sort By",
                              choices = c('PrctNecessary', 'PrctHave', 'diffNecessaryHave')),
                            selectInput(inputId = 'ituriMapInput',
                                        label = 'Select Variable to Map',
                                        choices = ituri_table$gs)
                            ),
                          tableOutput('ituriTable'),
                          plotOutput('ituriPlot'),
                          leafletOutput('ituriMap'),
                 ),                 
                 tabPanel("Lac Tele", 
                          mainPanel(
                            "Lac Télé Community Reserve is the Congo’s only community reserve and covers 4,400 square kilometers between the Sangha and Oubangui rivers. This area is known for its levels of biodiversity, and is home to the highest known density of gorillas in the world. Commercial hunting and overfishing are some of the major challenges in this area that threaten access to goods and services."
                          ),
                          sidebarPanel(
                            selectInput(
                              inputId = 'lacteleInput',
                              label = "Select Survey Round",
                              choices = c(1,2,3)),
                            selectInput(
                              inputId = 'lacteleSelect',
                              label = "Select Variable to Sort By",
                              choices = c('PrctNecessary', 'PrctHave', 'diffNecessaryHave')),
                            selectInput(inputId = 'lacteleMapInput',
                                        label = 'Select Variable to Map',
                                        choices = lactele_table$gs)
                            ),
                          tableOutput('lacTeleTable'),
                          plotOutput('lactelePlot'),
                          leafletOutput('lacteleMap'),
                 ),
                 tabPanel("Cross River", 
                          mainPanel(
                            "The Cross River National Park (Okwangwo Division) covers an area of 640 km2 at the headwater of the River Cross. The park is contiguous with Takamanda National Park in Cameroon, and together there is a crucial habitat for the endangered Cross River gorilla. Illegal logging and the bushmeat trade are two of the major conservation challenges in the area."
                                    ),
                          sidebarPanel(
                            selectInput(
                              inputId = 'crossriverInput',
                              label = "Select Survey Round",
                              choices = c(1,2)),
                            selectInput(
                              inputId = 'crossriverSelect',
                              label = "Select Variable to Sort By",
                              choices = c('PrctNecessary', 'PrctHave', 'diffNecessaryHave')),
                            selectInput(inputId = 'crossriverMapInput',
                                        label = 'Select Variable to Map',
                                        choices = crossriver_table$GS)
                          ),
                          tableOutput('crossriverTable'),
                          plotOutput('crossriverPlot'),
                          leafletOutput('crossriverMap'),
                 ),                 
                 tabPanel("Makira", 
                          mainPanel("The Makira Natural Park is located in Madagascar and is the only habitat in the world with all 5 lemur species. It has rich biodiversity and is known for its levels of endemism. Deforestation is one of the major conservation challenges in the region. "),
                          sidebarPanel(
                            selectInput(
                              inputId = 'makiraInput',
                              label = "Select Survey Round",
                              choices = c(1,2,3)),
                            selectInput(
                              inputId = 'makiraSelect',
                              label = "Select Variable to Sort By",
                              choices = c('PrctNecessary', 'PrctHave', 'diffNecessaryHave')),
                            selectInput(inputId = 'makiraMapInput',
                                        label = 'Select Variable to Map',
                                        choices = makira_table$gs)
                          ),
                          tableOutput('makiraTable'),
                          plotOutput('makiraPlot'),
                          leafletOutput('makiraMap')
                 ),
                 tabPanel("Methods + Sources",
                          mainPanel("The data we are looking at are a series of surveys that were conducted between 2015 and 2021 in villages within 5 protected landscapes across the Congo, Madagascar, and Nigeria.  These data identify the goods and services that the surveyed individuals identified as being necessary, whether or not the surveyed individuals have these goods or services, and what quantity they had of these items. These data compare lists of the access to goods and services within each landscapes, and how this varies across all of the selected landscapes. 
 We are also using household level data that provides an overview of the village and each household surveyed within each landscape. To calculate the percent of residents of each village and each landscape who thought an item was necessary and the percent who had an item, we used simple R commands group_by and summarise to average the WCS-provided data."),
                          mainPanel("Project authors: Wright Frost, Sophia Holm, and Tucker Jakobe"),
                          mainPanel("Sources: "),
                          mainPanel("https://communities.wcs.org/Metric-Details/m/50. "),
                          mainPanel("https://nigeria.wcs.org/wild-places/cross-river-np-okwangwo.aspx."),
                          mainPanel("https://congo.wcs.org/Wild-Places/Lac-T%C3%A9l%C3%A9-Community-Reserve.aspx"),
                          mainPanel("https://madagascar.wcs.org/Makira-Carbon.aspx"),
                          mainPanel("https://congo.wcs.org/Wild-Places/Nouabale-Ndoki-National-Park.aspx.")
                          )
                 )

server <- function(input,output){
  ### Ndoki map background
  ndoki_gs_summarized <- ndoki_gs_rounds %>% 
    group_by(Village.x,gs,SurveyRound) %>% 
    summarize(pctHave = mean(have),pctNec = mean(necessary))
  
  ndokiPalette = colorNumeric(palette = 'magma',
                              domain = ndoki_gs_summarized$pctNec)
  #### Ituri Map Background
  ituri_gs_summarized <- ituri_gs_rounds %>% 
    group_by(Village.x,gs,SurveyRound) %>% 
    summarize(pctHave = mean(have),pctNec = mean(necessary))
  ituriPalette = colorNumeric(palette = 'magma',
                              domain = ituri_gs_summarized$pctNec)
  
  ### Lac Tele Map Background
  lactele_gs_summarized <- lactele_gs_rounds %>% 
    group_by(Village.x,gs,SurveyRound) %>% 
    summarize(pctHave = mean(have),pctNec = mean(necessary))
  lactelePalette = colorNumeric(palette = 'magma',
                              domain = lactele_gs_summarized$pctNec)
  
  ### Makira Map Background
  makira_gs_summarized <- makira_gs_rounds %>% 
    group_by(Village.x,gs,SurveyRound) %>% 
    summarize(pctHave = mean(have),pctNec = mean(necessary))
  makiraPalette = colorNumeric(palette = 'magma',
                                domain = makira_gs_summarized$pctNec)
  
  ### Crossriver Map Background
  crossriver_gs_summarized <- crossriver_gs_rounds %>% 
    rename(gs = GS,have = Have, necessary = Necessary) %>% 
    group_by(Village.x,gs,SurveyRound) %>% 
    summarize(pctHave = mean(have),pctNec = mean(necessary))
  crossriverPalette = colorNumeric(palette = 'magma',
                                domain = crossriver_gs_summarized$pctNec)
  
  output$ndokiTable <- renderTable({
    ndoki_table %>% 
      filter(SurveyRound == input$ndokiInput) %>% 
      rename(PrctNecessary = necProp, PrctHave = haveProp) %>% 
      arrange(desc(get(input$ndokiSelect))) %>% # get = get the name of column w this string
      ungroup() %>% 
      rename(DiffNecessaryHave = diffNecessaryHave, Landscape = landscape) %>% 
      slice_head(n=10)
  })
  output$ndokiPlot <- renderPlot({
    ndoki_table %>% 
      filter(gs == input$ndokiMapInput) %>% 
      ggplot()+
      geom_line(aes(x = SurveyRound, y = diffNecessaryHave)) +
      geom_point(aes(x = SurveyRound, y = diffNecessaryHave)) +
      ggtitle(paste("Difference between % have and % nec: ", input$ndokiMapInput))
  })
    output$ndokiMap <- renderLeaflet({
      ndoki_gs_summarized %>% 
        filter(gs == (input$ndokiMapInput))%>% 
        filter(SurveyRound == input$ndokiInput) %>% 
        rename(Village = Village.x) %>% 
        left_join(villroundinfo,by='Village') %>% 
        rename(lat = vlgLat, long = vlgLong) %>% 
        mutate(newLabel=(paste(Village,": ",round(pctNec*100),"% think ",gs," is necessary, ",
                                round(pctHave*100),'% have access to ',gs,sep = ""))) %>% 
        leaflet() %>% 
        addPolygons(data = ndoki,
                    fillOpacity = 0) %>% 
        addCircleMarkers(
          label = ~newLabel,
          fillColor = ~ndokiPalette(pctNec),
          fillOpacity = 1,
          radius = 6,
          stroke = FALSE
        ) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addLegend(position = 'bottomleft',
                  pal = ndokiPalette,
                  values = ~pctNec,
                  title = 'Percent that think item is necessary')
  })
  
  output$ituriTable <- renderTable({
    ituri_table %>% 
      filter(SurveyRound == input$ituriInput) %>% 
      filter(SurveyRound == input$ituriInput) %>% 
      arrange(desc(necProp)) %>% 
      rename(PrctNecessary = necProp, PrctHave = haveProp) %>% 
      arrange(desc(get(input$ituriSelect))) %>% # get = get the name of column w this string
      ungroup() %>% 
      rename(DiffNecessaryHave = diffNecessaryHave, Landscape = landscape) %>% 
      slice_head(n=10)
  })
  output$ituriPlot <- renderPlot({
    ituri_table %>% 
      filter(gs == input$ituriMapInput) %>% 
      ggplot()+
      geom_line(aes(x = SurveyRound, y = diffNecessaryHave)) +
      geom_point(aes(x = SurveyRound, y = diffNecessaryHave)) +
      ggtitle(paste("Difference between % have and % nec: ", input$ituriMapInput))
  })
  output$ituriMap <- renderLeaflet({
    ituri_gs_summarized %>% 
      filter(gs == (input$ituriMapInput))%>% 
      rename(Village = Village.x) %>% 
      left_join(villroundinfo,by='Village') %>% 
      rename(lat = vlgLat, long = vlgLong) %>% 
      mutate(newLabel=(paste(Village,": ",round(pctNec*100),"% think ",gs," is necessary, ",
                             round(pctHave*100),'% have access to ',gs,sep = ""))) %>% 
      leaflet() %>% 
      addPolygons(data = ituri,
                  fillOpacity = 0) %>% 
      addCircleMarkers(
        label = ~newLabel,
        fillColor = ~ituriPalette(pctNec),
        fillOpacity = 1,
        radius = 6,
        stroke = FALSE
      ) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addLegend(position = 'bottomleft',
                pal = ituriPalette,
                values = ~pctNec,
                title = 'Percent that think item is necessary')
  })
  output$lacTeleTable <- renderTable({
    lactele_table %>% 
      filter(SurveyRound == input$lacteleInput) %>% 
      arrange(desc(necProp)) %>% 
      rename(PrctNecessary = necProp, PrctHave = haveProp) %>% 
      arrange(desc(get(input$lacteleSelect))) %>% # get = get the name of column w this string
      ungroup() %>% 
      rename(DiffNecessaryHave = diffNecessaryHave, Landscape = landscape) %>% 
      slice_head(n=10)
  })
  output$lactelePlot <- renderPlot({
    lactele_table %>% 
      filter(gs == input$lacteleMapInput) %>% 
      ggplot()+
      geom_line(aes(x = SurveyRound, y = diffNecessaryHave)) +
      geom_point(aes(x = SurveyRound, y = diffNecessaryHave)) +
      ggtitle(paste("Difference between % have and % nec: ", input$lacteleMapInput))
  })
  output$lacteleMap <- renderLeaflet({
    lactele_gs_summarized %>% 
      filter(gs == (input$lacteleMapInput))%>% 
      rename(Village = Village.x) %>% 
      left_join(villroundinfo,by='Village') %>% 
      rename(lat = vlgLat, long = vlgLong) %>% 
      mutate(newLabel=(paste(Village,": ",round(pctNec*100),"% think ",gs," is necessary, ",
                             round(pctHave*100),'% have access to ',gs,sep = ""))) %>% 
      leaflet() %>% 
      addPolygons(data = lactele,
                  fillOpacity = 0) %>% 
      addCircleMarkers(
        label = ~newLabel,
        fillColor = ~lactelePalette(pctNec),
        fillOpacity = 1,
        radius = 6,
        stroke = FALSE
      ) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addLegend(position = 'bottomleft',
                pal = lactelePalette,
                values = ~pctNec,
                title = 'Percent that think item is necessary')
  })
  output$crossriverTable <- renderTable({
    crossriver_table %>% 
      filter(SurveyRound == input$crossriverInput) %>% 
      arrange(desc(necProp)) %>% 
      rename(PrctNecessary = necProp, PrctHave = haveProp) %>% 
      arrange(desc(get(input$crossriverSelect))) %>% # get = get the name of column w this string
      ungroup() %>% 
      rename(DiffNecessaryHave = diffNecessaryHave) %>% 
      slice_head(n=10)
  })
  output$crossriverPlot <- renderPlot({
    crossriver_table %>% 
      filter(GS == input$crossriverMapInput) %>% 
      ggplot()+
      geom_line(aes(x = SurveyRound, y = diffNecessaryHave)) +
      geom_point(aes(x = SurveyRound, y = diffNecessaryHave)) +
      ggtitle(paste("Difference between % have and % nec: ", input$crossriverMapInput))
  })
  output$crossriverMap <- renderLeaflet({
    crossriver_gs_summarized %>% 
      filter(gs == (input$crossriverMapInput))%>% 
      rename(Village = Village.x) %>% 
      left_join(crossrivervillroundinfo,by='Village') %>% 
      rename(lat = vlgLat, long = vlgLong) %>% 
      mutate(newLabel=(paste(Village,": ",round(pctNec*100),"% think ",gs," is necessary, ",
                             round(pctHave*100),'% have access to ',gs,sep = ""))) %>% 
      leaflet() %>% 
      addPolygons(data = crossriver,
                  fillOpacity = 0) %>% 
      addCircleMarkers(
        label = ~newLabel,
        fillColor = ~crossriverPalette(pctNec),
        fillOpacity = 1,
        radius = 6,
        stroke = FALSE
      ) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addLegend(position = 'bottomleft',
                pal = crossriverPalette,
                values = ~pctNec,
                title = 'Percent that think item is necessary')
  })
  output$makiraTable <- renderTable({
    makira_table %>% 
      filter(SurveyRound == input$makiraInput) %>% 
      arrange(desc(necProp)) %>% 
      rename(PrctNecessary = necProp, PrctHave = haveProp) %>% 
      arrange(desc(get(input$makiraSelect))) %>% # get = get the name of column w this string
      ungroup() %>% 
      rename(DiffNecessaryHave = diffNecessaryHave, Landscape = landscape) %>% 
      slice_head(n=10)
  })
  output$makiraPlot <- renderPlot({
    makira_table %>% 
      filter(gs == input$makiraMapInput) %>% 
      ggplot()+
      geom_line(aes(x = SurveyRound, y = diffNecessaryHave)) +
      geom_point(aes(x = SurveyRound, y = diffNecessaryHave)) +
      ggtitle(paste("Difference between % have and % nec: ", input$makiraMapInput))
  })
  output$makiraMap <- renderLeaflet({
    makira_gs_summarized %>% 
      filter(gs == (input$makiraMapInput))%>% 
      rename(Village = Village.x) %>% 
      left_join(makiravillroundinfo,by='Village') %>% 
      rename(lat = vlgLat, long = vlgLong) %>% 
      mutate(newLabel=(paste(Village,": ",round(pctNec*100),"% think ",gs," is necessary, ",
                             round(pctHave*100),'% have access to ',gs,sep = ""))) %>% 
      leaflet() %>% 
      addPolygons(data = makira,
                  fillOpacity = 0) %>% 
      addCircleMarkers(
        label = ~newLabel,
        fillColor = ~makiraPalette(pctNec),
        fillOpacity = 1,
        radius = 6,
        stroke = FALSE
      ) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addLegend(position = 'bottomleft',
                pal = makiraPalette,
                values = ~pctNec,
                title = 'Percent that think item is necessary')
  })
}

shinyApp(ui = ui, server = server)