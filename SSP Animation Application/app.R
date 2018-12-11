library(ggplot2)
library(gganimate)
library(readxl)
library (dplyr)
library(tweenr)
library(tidyverse)
library(readxl)
library(tibble)
library(ggstance)
library(gapminder)
library(shiny)
library(gifski)



SSP2Data <- read_excel("SSP2Data.xlsx")
SSP<- data.frame(SSP2Data)
SSP= SSP%>%
  group_by(Variable,Year)%>%
  mutate(ordering = min_rank(CountryTarget/CountryRegion) * 1.0) %>%
  ungroup()


fnRankGroup<-function(x){
  
  SSP<- subset(SSP,Variable==x)
  
}


ui<- fluidPage(
  titlePanel("Assessing SDGs under the SSP2 scenario"),
  sidebarLayout(
    sidebarPanel(
      selectInput("SDG", "Please Select SDG Name", choices=c("Education:Primary Completion", "Poverty", "Education: Lower Secondary Graduation","Hunger: Malnourished Children","Electricity Access","Hunger:Undernourishment","Access to sanitation","Access to safe water","Child Mortality"))
    ),
    mainPanel(p("This visualization helps us understand the percent of countries across regions that achieve inidivdual SDGs under SSP2. This visualization has been created using outputs from the International Futures Tool. For more information visit pardee.du.edu/ifs"),imageOutput("BarGraph"))))

server<-function(input,output){
  
  output$BarGraph<- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    
    outfile <- tempfile(fileext='.gif')
    
    SSP<-fnRankGroup(input$SDG)    
    p=(SSP) %>% 
      ggplot(aes(ordering, group=GEO.Region))+
      geom_tile(aes(y = ((CountryTarget/CountryRegion)*100)/2, 
                    height = (CountryTarget/CountryRegion)*100,
                    width = 0.9,fill=Legend), alpha = 0.9,position='identity')+
      geom_text(aes(y=(CountryTarget/CountryRegion)*100,label=SSP$GEO.Region),vjust=-0.5)+
      geom_text(aes(y=0,label=" ",vjust=2))+  
      coord_cartesian(clip = "off", expand = FALSE) +
      labs(caption='Source:International Futures:7.34',title=SSP$Variable,subtitle='{closest_state}',x="",y="Percent of countries in a region that achieve SDGs",size = 15,vjust=2) +  
      theme(plot.title = element_text(hjust = 0.5,vjust=2.12, size = 22),axis.ticks.x = element_blank(),
            axis.text.x  = element_blank())+
      transition_states(Year, 
                        transition_length = 2, state_length = 1) +
      ease_aes('cubic-in-out')
    
    anim_save("outfile.gif", animate(p,nframes=300,fps=20, width = 800, height = 400))
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)}

shinyApp(ui,server)

