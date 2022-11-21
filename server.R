library(shiny)
library(data.table)
library(gridExtra)
library(readr)
library(shinyalert)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gifski)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(tidyr)
library(ggrepel)
function(input, output, session){
  
  b<- read.csv("cp.csv", header = TRUE)
  b<- setNames(b, c("state","district","year","season","crop","area","production"))%>%na.omit(b)
  #
  output$dt<- renderDataTable(b)
  #
  output$dtst<- renderDataTable({
    b %>% 
      group_by(state)  %>%
      summarise(total_production= sum(production),total_area = sum(area))%>%
      arrange(desc(total_production))})
  #
  output$dtd<- renderDataTable({
    b %>% 
      group_by(district)  %>%
      summarise(total_production= sum(production),total_area = sum(area))%>%
      arrange(desc(total_production))
  })
  #
  output$dtc<- renderDataTable({
    b %>% 
      group_by(crop)  %>%
      summarise(total_production= sum(production),total_area = sum(area))%>%
      arrange(desc(total_production))
  })
  #
  output$dtse<- renderDataTable({
    b %>% 
      group_by(season)  %>%
      summarise(total_production= sum(production),total_area = sum(area))%>%
      arrange(desc(total_production))
  })
  #
  output$dty<- renderDataTable({
    b %>% 
      group_by(year)  %>%
      summarise(total_production= sum(production),total_area = sum(area))%>%
      arrange(desc(year))
  })
  #
  output$t0<-renderText("Vaibhav_Jaydeo_Khobragade Roll_No:2141021")
  #
  output$vp1<-renderPlotly({
    b2<- filter(b, state=="Maharashtra")
    b2<- na.omit(b2)
    b21 <- ggplot(b2, aes(production, district, color = crop)) +
      geom_point(aes(size = area, frame = year, ids = district))+
      scale_x_log10()
    ggplotly(b21)%>%
      layout(title = 'Crop production in each District',
             xaxis = list(title = 'Production')) %>%
      animation_opts(1000, easing = "elastic", redraw = TRUE)
    
  })
  #
  output$t1<- renderText({"In this plot, the district are at y-axis in Maharashtra state 
  and on x axis the total production of the crop in that year. In addition, the scatter
  plot point size is depend upon area of that district.  In this plot, cotton production 
  is very large tan the other crop. And most of the cotton is produce in Jalgaon and 
  yavatmal.  Other Rabi and Kharif pulses is the least production in most of the 
  districts. Also the one thing that I observe when the animation is run by year, 
  production of crops are sync like in some year production is low then all crop show 
  less production and in some year there is good production of all crop, not like if 
  one crop have very production and one crop have highest production. The crop production 
  of every crop is directly proportional to each other."})
  # 
  output$vp2<-renderPlotly({
    b3<-b %>% 
      group_by(state,year)  %>%
      summarise(total_production= sum(production))%>%
      arrange(desc(total_production))
    b3<- b3 %>%
      plot_ly(y= ~state,x= ~total_production,size= ~total_production,
              text = ~total_production,hoverinfo= "x+y")%>%
      layout(xaxis = list(type = "log"))%>%
      layout(title = 'State wise production with year')
    b3 %>% add_markers(color= ~state,frame= ~year,ids =~state) %>%
      animation_opts(2000,easing= "elastic",redraw = TRUE) %>% 
      animation_slider(currentvalue= list(prefix ="YEAR ", font = list(color="blue")) )
  })
  #
  output$t2<- renderText({"In this plot, data is selected by state. This plot shows 
  each state total production of crop with year.  In this plot Kerala and Andhra 
  Pradesh have large production than the other states. This two state producing 
  highest amount of crop production in almost every year. Up to 2010.Chandigarh 
  is very less crop production than the other states and after that there is no 
  production shows in the Chandīgarh. After Chandigarh Dadra and Nagar Haveli have 
  less crop production than the other state.  In year 2015, only two states Odisha 
  and Sikkim are two state have production and remaining state do not show any 
  crop production."
  })
  # 
  output$vp3<-renderPlotly({
    b6<-b %>% 
      group_by(crop, year)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      arrange(desc(total_production))
    b6%>%
      plot_ly(y= ~crop,
              x= ~total_production,
              color= ~crop,
              colors = "Paired",
              frame = ~year,
              ids= ~crop)%>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))%>%
      layout(title = 'Crop Vs Production with year')%>%
      animation_opts(2000,
                     easing="elastic",
                     redraw = TRUE,
                     mode= "afterall")%>%
      add_bars(width = 1) 
  })
  #
  output$t3<- renderText({"In this plot, data is summaries by crop with production.
  Y-axis shows crop and on x-axis shows total production. In every year coconut
  production in very high than the other crop. In 2011, Coconut production almost 
  touch to 14B. It is the highest production in all the years. After Coconut crop,
  Wheat, Rice and Sugarcane are the highest crop production than the others. 
  However, in 2015 only Maize and Moong production showing and other crop 
  production is zero. Remaining crop, I think there is production but in very 
  less amount, that is why bar is not showing. From this, we can conclude that
  India is the highest producer of coconut."})
  # 
  output$vp4<-renderPlotly({
    b7<-b %>% 
      group_by(season, year)  %>%
      summarise(total_production = sum(production),total_area = sum(area))
    b7 <- ggplot(b7, aes(total_production, total_area)) +
      geom_point(aes(size= total_production, frame = year, ids= season, color = season))
    b7<- b7 + labs(x = "production per year", y= "area per year",
                   title = "Area Vs Production with Season",colour = "Season")
    ggplotly(b7)
    
  })
  #
  output$t4<- renderText({"In this plot data is summarize by season. On y-axis total
  area of that year and on x axis total production of that year. In this plot most
  of the production is done by whole year.  In all years, whole year production 
  is very high. After Whole year, Kharif and Rabbi are producing more than the 
  other is also this two-season use large amount of area than all of the seasons. 
  In addition, in autumn season very less amount of area use for production, 
  and hence it produce least amount of production. In addition, summer season 
  is low after summer season this season is also done on less amount of area and 
  hence get less production. In every year in winter the production is almost 
  same means does not have large difference than the other, also, the area is
  almost same for every year."})
  # 
  output$vp5<-renderPlotly({
    b8<-b %>% 
      group_by(year)  %>%
      summarise(total_production = sum(production),total_area = sum(area))
    b81<- gather(b8,key="variable",value="value",total_production:total_area)
    b81%>%
      ggplot( aes(x=year, y=value,color=variable)) +
      geom_line() +
      geom_point() +
      scale_color_viridis(discrete = TRUE) +
      ggtitle("Area Vs Production with Year") +
      theme_ipsum() +
      ylab("Value of Variable") +
      transition_reveal(year)
  })
  #
  output$t5<- renderText({"In this data merged by total area with total production. 
  In this plot, we can see that the total area is slowly decreasing throughout the
  year. The production is increasing in beginning up to year 2007 after that it is
  decreasing. In year, 2010 production is drastically increase 2011 and decrease 
  drastically in year 2012 and it increase in 2013. However, after 2013 it comes 
  to almost zero in 2015. This plot shows how the production of India changes with 
  respect to year from this we have to produce more because the population of India 
  is increase and we have to produce that much of quantity of production to cater
  the demand."})
  # 
  
  ##
  output$vker <-renderPlotly({
    filter(b, state=="Kerala")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
    })
   ##
  output$vanp<-renderPlotly({
    filter(b, state=="Andhra Pradesh")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vtn<-renderPlotly({
    filter(b, state=="Tamil Nadu")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vup<-renderPlotly({
    filter(b, state=="Uttar Pradesh")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vasm<-renderPlotly({
    filter(b, state=="Assam")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  ##
  output$vmiz<-renderPlotly({
    filter(b, state=="Mizoram")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vdnh<-renderPlotly({
    filter(b, state=="Dadra and Nagar Haveli")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vcha<-renderPlotly({
    filter(b, state=="Chandigarh")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vsik<-renderPlotly({
    filter(b, state=="Sikkim")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
  ##
  output$vman<-renderPlotly({
    filter(b, state=="Manipur")%>% 
      group_by(district)  %>%
      summarise(total_production = sum(production),total_area = sum(area))%>%
      plot_ly(y= ~district,
              x= ~total_production,
              size=~total_area,
              text=~total_area,
              color= ~district,
              colors = "Paired",
              hoverinfo= "x+y+text")%>%
      layout(xaxis = list(type = "log"))%>%
      add_markers()%>%
      layout(xaxis = list(title = "Total production"), 
             yaxis = list(title = "District"))%>%
      layout(title = 'Crop production in each District (scatter size = Area)') 
    
  })
}

