library(shiny)
library(tidyverse)
library(leaflet)
library(ggmap)
library(readxl)
library(plotly)
library(DT)

college<-  read_csv("college.csv")
states<- read_excel("state_codes.xls") 

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

m <- map_data("state")
s <- map_data(map = "county")

# Remove Alaska and Hawaii
both <- college %>%
  filter(state!="HI" & state!="AK")%>%
   rename(scode = state )

states<- states %>%
      filter(Code !="HI" & Code!="AK" ) %>%
      rename(scode = Code )%>%
      mutate(State=tolower(State))


# create dataframe of public and private
public <- both%>%
  filter(control == "Public")

private <- both %>%
  filter(control == "Private")

#merge state and college
both <-merge(both,states, by ='scode')

both <- both%>%      # convert the first alphabet to upper case
   mutate(State = str_to_title(State))

s<- s%>%
   mutate(region = str_to_title(region))
# App starts here
list <- str_to_title(states$State)
#-----------------------------------------UI--------------------------------------
ui <- navbarPage("R Skills using Shiny",
      tabPanel("ggplot2 Map",
      plotOutput("map",width = "1000px",height = "800px")
                  ),
      
      tabPanel(" Leaflet Map",
      sidebarLayout(
      sidebarPanel(
      selectInput(inputId = "choice",
                  label = "Choose University type:",
                  choices = c("Public", "Private")
                  )
                ),
      
      
      mainPanel(
      leafletOutput("choice", width = "1000px",height = "800px")
      ) )
  ),
  
      tabPanel("Dataset sample",
               dataTableOutput("table")
               ),
  
      tabPanel("Plotly Map -State",
               sidebarLayout(
                  sidebarPanel(
                     selectInput(inputId = "select",
                                 label = "Choose State:",
                                 choices = list
                                 )
                     
                              ),
                  
           mainPanel(
              plotlyOutput("state",width = "800px",height = "500px"),
                     #  br(),
                     # verbatimTextOutput("tab")
              
                     )
           )
      )
)
  

#--------------------------------SERVER-------------------------------------------------

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$choice,
           "Public" = public,
           "Private" = private)
  })
  
   output$choice<- renderLeaflet({
          
          if(input$choice == "Public") {
            public %>%
              leaflet(data = m, options = leafletOptions(minZoom = 0, maxZoom = 10))%>%
              addTiles()%>%
              addMarkers(public$lon,public$lat, popup = ~as.character(public$name),
                         label = ~as.character(public$name))
          }  
          else {
            private %>%
              leaflet(data = m, options = leafletOptions(minZoom = 0, maxZoom = 10))%>%
              addTiles()%>%
              addMarkers(private$lon,private$lat, 
                         popup = ~as.character(private$name),
                         label = ~as.character(private$name))
          }
    
   })
  # add another tab for normal ggplot map
   output$map<- renderPlot({  
   ggplot(m) +
       geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
       coord_map() +
       theme(plot.background=element_blank(),
             panel.background = element_blank(),
             axis.title=element_blank(),
             axis.ticks=element_blank(),
             axis.text=element_blank())+
       geom_point(data=both, mapping=aes(x=lon, y=lat, color=control, size=undergrads))+
       scale_color_discrete(name = "Institution type")+
       scale_size_continuous(name = "Students UG")+
       ggtitle("Location of Universities in Continental US", subtitle = "Source: Dept. of Education")+
       theme( plot.title = element_text(color="red", size=24, face="bold.italic"))+
       guides(color = guide_legend(override.aes = list(size=8)))
   })
   
   # add table
   output$table<- renderDataTable({
     datatable(both %>%
                  select(-lat,-lon,-id))
     
   })
   
   # add state maps
   output$state<- renderPlotly({ 
      col<- both %>%
         filter(State == input$select)
      
   fig<- s%>%
         filter(region == input$select)%>%
         ggplot() +
         geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
         coord_map() +
         theme(plot.background=element_blank(),
               panel.background = element_blank(),
               axis.title=element_blank(),
               axis.ticks=element_blank(),
               axis.text=element_blank())+
         geom_point(data=col, mapping=aes(x=lon, y=lat, color=control, 
               size=undergrads,text =paste("Name:",name,"\n","Type:",control,
               "\n","City:",city,"\n",  "UG students:",undergrads), alpha = 0.4 ))+
         ggtitle(paste("Location of Universities in", input$select), 
                 subtitle = "Source: Dept. of Education")+
         theme( legend.position = "bottom")+
         scale_color_discrete(name = "Institution type")+
         scale_size_continuous(name = "Students UG")+
         theme( plot.title = element_text(color="red", size=24, face="bold.italic"))+
         guides(color = guide_legend(override.aes = list(size=8)))
   
   ggplotly(fig, tooltip = c("text"))%>% config(displayModeBar = F)  %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
   
   # statewise table
   # output$tab <- renderPrint({
   #    both%>%
   #       filter(State==input$select )%>%
   #       select(-id,-scode,-lon,-lat)%>%
   #       summarise()
   # 
   # })

   })
   
}


shinyApp(ui = ui, server = server)