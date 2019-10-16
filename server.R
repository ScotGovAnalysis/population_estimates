# dataset ----------------------------------------------------------------------


## Chart 1 and 3
pop<-read_xlsx("Mid-year population estimates.xlsx")


total <- pop %>%
  group_by(Area, Year) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

## Chart 2
comp_change<-read_csv("Components of change.csv")


long_comp_change<-comp_change %>% 
  gather(Component, Population, -Area, factor_key = T) %>%
  mutate(abs_change = cut(Population, breaks=c(-Inf, 0, Inf), 
                          labels=c("Negative", "Positive"),right = F))

## Chart 4
age_groups <- mutate(pop,
                     age_group=cut(Age, breaks = c(-1, 15, 24, 44, 64, 74, 100))) %>%
                    group_by(Area, Year, age_group) %>%
                  summarise(Population = sum(Population)) %>% 
                  ungroup()

# common features --------------------------------------------------------------

#colours

p1<- "#2ba197ff"
p2<- "#96D0CB"

#earliest and latest year available in data
minYear <- 1998
maxYear <- 2018

#labels for age groups
ageGrpLabs <- c("0 to 15","16 to 24","25 to 44",
                    "45 to 64","65 to 74","75 and over")



shinyServer(function(input, output){
  
####################
# Total population #
####################

    output$plot_Pop<- renderPlot({

      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      yearRange <- input$Years_Pop
      maxYear = yearRange[2]
      minYear = yearRange[1]
      
      inputArea <- input$Area_Pop
      
      plot_data <- filter(total, Area==inputArea, 
                          Year<=maxYear, Year>=minYear)
      
      #Years to provide an annotation for - 
      # minimum, maximum and (if there's space) the middle value
      if(maxYear-minYear > 6) {
        toAnnotate <- c(minYear,floor(mean(yearRange)),maxYear)
      } else {
        toAnnotate <- yearRange
      }
      
      plot_annot <- filter(plot_data, Year %in% toAnnotate)
      

      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------

      ggplot(plot_data, mapping = aes(x=Year, y=Population/1000))+
        
        #line and fill area for all years
        geom_area(fill=p2) +
        geom_line(colour=p1, size=1.5) + 
        
        #annotations for specific years
        geom_point(aes(x=Year, y=Population/1000), data=plot_annot,
                   size=4, colour=p1) +
        annotate("text", x=plot_annot$Year, y=plot_annot$Population*1.1/1000, 
                 label=paste0(plot_annot$Year,":\n",comma(plot_annot$Population)),
                 colour=p1, size=5) +
        
        theme_minimal() +
        scale_x_continuous(limits = c(minYear,maxYear),
                           breaks = seq(from=1980,to=2030,by=5),
                           expand = c(0, 3)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(plot_data$Population)*1.2/1000),
                           labels=comma) +
        theme(text=element_text(size=16),
              plot.margin=unit(c(5.5,20,5.5,5.5),units = "pt"),
              plot.title=element_text(hjust=0, size=20)) +
        labs(title=inputArea,
             subtitle=paste0("Population estimate (thousands), ",minYear,"-",maxYear),
             y=NULL,x=NULL)

      
    })
    
########################
# Components of change #
########################
    
    output$plot_CompCh <- renderPlot({
      
      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      inputArea <- input$Area_CompCh
      
      plot_data <- long_comp_change %>% 
        filter(Area==inputArea, 
               Component!="Population change",
               !(Area=="Scotland" & Component=="Within Scotland net migration")) %>%
        droplevels() %>%
        arrange(desc(Population)) %>% 
        mutate(x_pos = row_number(),
               #scale so largest circle has area of 0.6 
               # (so no radius exceeds 0.45 and circles stay separated)
               area = 0.6*Population/max(abs(Population)),
               radius = sqrt(abs(area)/pi))
      
      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------

      
      
      #plot text labels directly with ggplot
      p <- plot_data %>% 
        ggplot(aes(x=x_pos)) + 
        #labels naming each component
        geom_text(aes(label=str_wrap(Component, width=7)),
                  fontface="bold", y=1.4, vjust=1, size=5) +
        #the numbers for each component
        geom_text(aes(label=comma(Population),colour=abs_change),
                  fontface="bold", y=1.2, size=5) +
        scale_colour_manual(values = c("Negative"="gray50", "Positive"=p1)) +
        xlim(0.5,nrow(plot_data)+0.5) +
        ylim(0.8, 1.4) +
        theme_void()+
        theme(text=element_text(size=16),
              plot.margin=unit(c(0,0,0,0), "cm"),
              legend.position="none") +
        labs(title=inputArea,
             subtitle=paste0("Components of population change, ",
                            maxYear-1,"-",maxYear))
      
      #x-coord of centres of circles
      xcents <- plot_data$x_pos
      #radii of circles
      radii <- plot_data$radius
      #fill colour - no fill if negative
      fillcols <- ifelse(plot_data$abs_change=="Negative",0,p1)
      #line type - no line if positive
      linetypes <- ifelse(plot_data$abs_change=="Negative",1,0)
      
      #add circles to plot
      for(i in seq_along(xcents)) {
        #circle with radius sqrt(1/pi) (means diameter is across entire width of xmin to xmax)
        p <- p +annotation_custom(grob=circleGrob(r=unit(sqrt(1/pi),"npc"),
                                                  gp=gpar(lty=linetypes[i],lwd=2,
                                                          col="gray60",fill=fillcols[i])),
                                  xmin=xcents[i]-radii[i], xmax=xcents[i]+radii[i], 
                                  ymin=0, ymax=2) # centre circles at y=1
      }
      
      p
      

      
    })
    
####################
# Pop. structure   #
####################
    
    output$plot_Struct<- renderPlot({
      
      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      inputArea <- input$Area_Struct
      minYear <- input$Years_Struct[1]
      maxYear <- input$Years_Struct[2]
      
      plot_data <- filter(pop, Area==inputArea, Year==minYear)
      plot_line <- filter(pop, Area==inputArea, Year==maxYear)
      
      #maximum values for population
      maxPops <- c(-max(plot_data$Population, plot_line$Population), 
                   max(plot_data$Population, plot_line$Population))
      
      plot_annot<-data.frame(x=c(2,2), 
                             y=maxPops*1.1,
                             value=c("Males", "Females"))
      
      #ages to show on y axis
      ageAxis <- seq(from=10,to=80,by=10)
      
      #label function to show all values on x axis as positive (and add commas)
      comma.abs <- function(x,...) {
        return(comma(abs(x),...))
      }
      
      #only show range in subtitle if years are different
      yearRange <- ifelse(minYear==maxYear,maxYear,
                          paste0(minYear, " (area) to ", maxYear," (line)"))
      
      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------
      
      ggplot(plot_data) +
        
        #bars for earlier year
        geom_bar(aes(x=Age, 
                     y=ifelse(Sex=="F",Population,-Population)),
                 fill="grey80", colour="grey80",
                 stat="identity")  +
        
        #lines for later year
        geom_step(aes(x=Age-0.5,
                      y=ifelse(Sex=="F",Population,-Population),
                      group=Sex), 
                  data=plot_line,
                  size=1.2, colour=p1) +
        
        #vertical line at 0 separating male and female figures
        geom_hline(yintercept=0, colour="white") +
        
        #add "Males" and "Females" labels
        geom_text(data=plot_annot, aes(x, y, label=value), hjust="inward", fontface="bold") +
        
        scale_y_continuous(labels=comma.abs,
                           limits=maxPops*1.1) +
        
        scale_x_continuous(expand = c(0,0),breaks = seq(from=0,to=80,by=20)) +
        
        #add age labels on y axis
        annotate(geom="text",x=ageAxis,y=0,label=ageAxis,size=5) +
        
        coord_flip() +
        theme_minimal() +
        theme(text=element_text(size=16),
              axis.line.y = element_blank(),
              axis.text.y = element_blank(),
              #extra space to right margin as axis labels were getting cut off
              plot.margin=unit(c(5.5,20,5.5,20), "pt")) +
        labs(title=inputArea,
             x=NULL,y="Population",
             subtitle=paste0("Population by age and sex, ",yearRange))
      
    })

    
################
# Pop. by age  #
################
    
    output$plot_Age<- renderPlot({
      
      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      inputArea <- input$Area_Age
      minYear <- input$Years_Age[1]
      maxYear <- input$Years_Age[2]
      
      plot_data <- filter(age_groups, Area==inputArea, Year==minYear | Year==maxYear)
      plot_data$Year<-factor(plot_data$Year)
      
      if(minYear != maxYear) {
        yearBreaks <- c(minYear,maxYear)
        yearColours <- c("grey70", p1)
      } else {
        yearBreaks <- maxYear
        yearColours <- p1
      }
      

      
      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------
      
      #only show range in subtitle if years are different
      yearRange <- ifelse(minYear==maxYear,maxYear,
                          paste0(minYear, "-", maxYear))
      
      ggplot(plot_data, 
             mapping = aes(x=age_group, y=Population/1000, fill=Year)) +
        geom_bar(stat="identity", position = "dodge", width=0.6) +
        scale_fill_manual(breaks = yearBreaks,
                          values = yearColours) +
        theme_minimal() +
        theme(legend.position=c(0.9, 1), legend.justification = "top",
              text=element_text(size=16)) +
        scale_y_continuous(labels=comma) +
        scale_x_discrete(labels=str_wrap(ageGrpLabs, width=8)) +
        labs(title=inputArea,
             subtitle=paste0("Population by age group (thousands), ", yearRange),
             x="Age group",y=NULL)

      
    })    
    
####################
# % change by age  #
####################
    
    
    #outputs whether the years are different so the UI knows
    # whether to bother displaying the % change
    output$Years_Age_Different <- reactive({
      input$Years_Age[1] != input$Years_Age[2]
    })
    
    outputOptions(output, "Years_Age_Different", suspendWhenHidden = FALSE)
    
    output$plot_ChAge<- renderPlot({
      
      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      inputArea <- input$Area_Age
      minYear <- input$Years_Age[1]
      maxYear <- input$Years_Age[2]
      
      plot_data0 <- filter(age_groups, Area==inputArea, Year==minYear | Year==maxYear)
      
      #values for the start year
      fromYear <- plot_data0 %>% 
        filter(Year==minYear) %>% 
        mutate(Year = "fromYear")
      
      #values for the end year
      toYear <- plot_data0 %>% 
        filter(Year == maxYear) %>% 
        mutate(Year = "toYear")
      
      plot_data <- dplyr::union(fromYear,toYear) %>%  
        spread(key = Year, value = Population) %>% 
        mutate(perc_change = (toYear-fromYear)/toYear,
               #mark if change is negative (<0) or positive (>=0)
               abs_change = cut(perc_change, breaks=c(-Inf, 0, Inf), 
                                labels=c("Negative", "Positive"),right=F),
               #labels for chart - provide tenths of percent if no value is above 10%
               overten = any(abs(perc_change) > 0.1),
               labels= percent(round(perc_change,
                                    digits = ifelse(overten,2,3))))

      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------

      ggplot(plot_data) +
        geom_bar(aes(x=age_group, y=perc_change, fill=abs_change), 
                 stat="identity", show.legend = FALSE, colour=p1, width=0.5) +
        scale_fill_manual(values = c("Negative"=p2, "Positive"=p1)) +
        #line at 0
        geom_hline(yintercept=0, colour="grey30") +
        theme_minimal() +
        theme(text=element_text(size=16))+
        #add labels
        geom_text(aes(x=age_group, 
                      y=perc_change,
                      #below column if negative, above column if positive
                      vjust=ifelse(perc_change<0, 1.3, -0.3),
                      label=labels),
                  fontface="bold", colour=p1,size=5) +
        scale_y_continuous(labels = percent, expand=c(0.1,0)) +
        scale_x_discrete(labels=str_wrap(ageGrpLabs, width=8)) +
        labs(title=inputArea,
             subtitle=paste0("Percentage change of population by age group, ", 
                            minYear, "-", maxYear),
             x="Age group",y=NULL)
      
      
    })        

############################
# % change by council area #
############################
    
    output$plot_ChCA<- renderPlot({
      
      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      
      inputArea <- input$Area_ChCA
      minYear <- input$Years_ChCA[1]
      maxYear <- input$Years_ChCA[2]
      
      
      plot_data0 <- filter(total, Year==minYear | Year==maxYear)
      
      #values for the start year
      fromYear <- total %>% 
        filter(Year==minYear) %>% 
        mutate(Year = "oldPop")
      
      #values for the end year
      toYear <- total %>% 
        filter(Year == maxYear) %>% 
        mutate(Year = "newPop")
      
      plot_data <- dplyr::union(fromYear,toYear) %>% 
        ungroup() %>% 
        spread(key=Year,value="Population") %>% 
        mutate(perc_change = (newPop-oldPop)/oldPop,
               #plot colours - if Scotland, Highlight1, if inputArea, Highlight2.
               # otherwise "base"
               plot_colours = ifelse(Area=="Scotland","Highlight1",
                                     ifelse(Area==inputArea,"Highlight2",
                                            "base")),
               #order factor levels for CAs by percentage change
               Area = reorder(Area,perc_change)) %>% 
        arrange(perc_change)
      
      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------
      
      plot_data %>% 
        ggplot() +
        
        geom_bar(aes(x=Area,  
                     y=perc_change, 
                     fill=plot_colours), 
                 stat="identity", show.legend = FALSE, width=0.5) +
        scale_fill_manual(values=c("base"="grey70", 
                                   "Highlight1"="grey30", 
                                   "Highlight2"=p1)) +
        geom_hline(yintercept=0, colour="grey30") +
        
        theme_minimal() +
        theme(text=element_text(size=16),
              axis.text.y=
                element_text(face=ifelse(levels(plot_data$Area) %in% c("Scotland",inputArea),
                                                   "bold","plain"), 
                             colour =ifelse(levels(plot_data$Area) %in% c(inputArea), p1, "grey30")))+
        #horizontal bars
        coord_flip() +
        #labels showing percentage
        geom_text(aes(x=Area,
                      y=perc_change,
                      #left justified if negative, right just if positive
                      hjust=ifelse(perc_change<0,1.1,-0.1),
                      label=percent(round(perc_change, digits = 3))),
                  vjust=0.4) +

        scale_y_continuous(labels = scales::percent,
                           expand = c(0.25,0) ) +
        
        labs(title=inputArea,
             subtitle=paste0("Percentage population change, ", minYear, "-", maxYear),
             x=NULL,y=NULL)
      
      
    })          
    #######################################
    # % change by council area time series#
    #######################################
    
    output$plot_ChCALine<- renderPlot({
      
      # --------------------------------------------------------------------------
      # preparing dataset
      # --------------------------------------------------------------------------
      #
      
      
      inputArea <- input$Area_ChCALine
      minYear <- input$Years_ChCALine[1]
      maxYear <- input$Years_ChCALine[2]
      
      
      plot_data <- total %>% 
        filter(Year >= minYear & Year <= maxYear) %>% 
        group_by(Area) %>% 
        arrange(Area,Year) %>% 
        #note using order_by argument in first() within mutate() crashes on this version 
        # of dplyr (0.5.0), not sure when fix will come, so used arrange() first to avoid this
        mutate(initialPop=first(Population),
               pop_relative=(Population-initialPop)/initialPop,
               #plot colours - if Scotland, Highlight1, if inputArea, Highlight2.
               # otherwise "base"
               plot_colours = ifelse(Area=="Scotland","Highlight1",
                                     ifelse(Area==inputArea,"Highlight2",
                                            "base"))) %>% 
        ungroup()
      
      
      #add text labels to Scotland and selected area
      plot_text <- plot_data %>% 
        filter(Area %in% c("Scotland",inputArea),Year==maxYear) %>% 
        arrange(pop_relative) %>% 
        select(Area,Year,pop_relative,plot_colours)
  
      
      #add text labels to highest and lowest?                
      plot_text <- plot_data %>% 
        filter(Year==maxYear) %>% 
        #get largest and smallest
        arrange(pop_relative) %>% 
        filter(row_number() %in% c(1,n())) %>% 
        #centre label vertically, give it an unemphasised colour, wrap council name
        mutate(plot_colours="base") %>% 
        #combine with highlighted labels defined above
        select(Area,Year,pop_relative,plot_colours) %>%
        filter(Area!=inputArea) %>%
        dplyr::union(plot_text)
      

      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------
      
      #years from start to end
      yearRange <- maxYear-minYear
      #range for the x axis - expanded to provide space for labels
      xRange <- range(plot_data$Year)+c(0, diff(range(plot_data$Year)/3.5))
      
      #Avoiding the breaks having decimal parts
      if(all(floor(pretty(xRange)) == pretty(xRange) )) {
        xBreaks <- pretty(xRange)
      } else {
        if(yearRange >5) xBreaks <- seq(from=1980,to=2020,by=5)
        else xBreaks <- seq(from=1980,to=2020, by=2)
      }
      
      p <- plot_data %>% 
        ggplot() +
        geom_hline(yintercept=0, colour="grey80",size=1) +
        geom_line(aes(x=Year,y=pop_relative,
                      #reorder based on colours so the highlighted ones are on top
                      group=reorder(Area,plot_colours!="base"),
                      colour=plot_colours,
                      size=plot_colours)) +
        scale_colour_manual(values=c("base"="grey70",
                                     "Highlight1"="grey40",
                                     "Highlight2"=p1)) +
        #make highlighted lines thicker as well
        scale_size_manual(values=c("base"=0.5,
                                   "Highlight1"=1.3,
                                   "Highlight2"=1.3)) +

        
        #scale breaks and limits
        scale_x_continuous(limits = xRange,
                           breaks=xBreaks) +
        scale_y_continuous(labels=percent) +
        #label specific council areas
        geom_text_repel(aes(x=max(Year), 
                            y=pop_relative, 
                            label=Area,
                            colour=plot_colours),
                      
                        data=plot_text,
                        size=4.5, 
                        fontface="bold",
                        show.legend = FALSE,
                        segment.color = NA,
                        nudge_x=2.5) +
        
        theme_minimal() +
        theme(legend.position="none",text=element_text(size=16),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(title=inputArea,
             subtitle=paste0("Overall change since ",minYear," in population by council \n",
                             minYear, "-", maxYear))
      
      #Only show plot if the years are not identical.
      if(maxYear!=minYear) {
        p
      } else {
        #otherwise show a brief error message.
        ggplot() +
          theme_void() +
          theme(text=element_text(size=16)) +
          labs(title="",subtitle=str_wrap(width = 40,
                                          "To see this graph, please choose a time period covering more than one year."))
      }
      
      
      
      
    })   
    
    
})