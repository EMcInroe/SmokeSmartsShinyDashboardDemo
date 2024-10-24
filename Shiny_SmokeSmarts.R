library(ggplot2)
library(tidyr)
library(data.table) # fread()
library(dplyr)
library(knitr)
library(stringr)
library(shinydashboard)
library(shiny)
library(maps)
library(mapproj)
library(plyr)
library(ggmap)
library(zipcodeR)
library(tigris)
library(sf)
library(tidyverse)
library(viridis)

setwd("")

#############################################################################
###############                Load Data Sets             ###################
#############################################################################

#### Exploratory Data Analysis ####

table_1<-fread("table_1.csv", header = TRUE, drop=1)
colnames(table_1)[5]<-"AVOID"

table_1s<-fread("Figures/behavior_stats.csv", header=TRUE)
colnames(table_1s)[1]<-"Behavior"
colnames(table_1s)[5]<-"likely_todo"

combined<-fread("Figures/combined.csv", header=TRUE, drop=1)

version_gap<-fread("Figures/version_gap.csv", header=TRUE, drop=1)
version_gap$Behavior<-as.factor(sapply(version_gap$Behavior, function(x) gsub("AVOIDSTRENUOUS","AVOID",x)))


#### Profile Traits ####

profile_sum_stats<-fread("profile_stats.csv", header = TRUE, drop=1)

profile_counts<-fread("table_3.csv", header = TRUE, drop=1)
colnames(profile_counts)[1]<-"Behavior"
#profile_counts$Behavior<-as.factor(sapply(profile_counts$Behavior, function(x) gsub("AVOIDSTRENUOUS","AVOID",x)))

profile_percents<-fread("profile_percents.csv", header=TRUE)
#colnames(profile_percents)[4]<-"AVOID"
prof_table<-as.data.frame(t(profile_percents))
colnames(prof_table)<-prof_table[1,]
prof_table<-prof_table[-1,-1]
prof_table<-prof_table %>% mutate_all(as.numeric)
prof_table$Behavior<-row.names(prof_table)


#### Barriers ####

bar_long<-fread("Figures/barriers_long.csv", header=TRUE, drop=1)
percents_long<-fread("Figures/barrier_percents_CI.csv", header=TRUE, drop=1)


#### Model Summaries ####
CItable <-fread("Figures/CITable.csv", header=TRUE, drop=1)
CItable$Behavior<-as.factor(sapply(CItable$Behavior, function(x) gsub("AVOIDSTRENUOUS","AVOID",x)))
estDiff<-fread("Figures/estDiff.csv", header=TRUE, drop=1)

#### Maps Data Set ####
df<-fread("AllSurveys_01-04-21.csv", header = TRUE, drop=c(5,6,8,9,10))
zip<-unique(df$ZipCode)

#function gets zipcode, state, latitude and longitude
zipcode<-reverse_zipcode(zip)[,c(1,7,8,9)] 
zipcode<-subset(zipcode, lat!="NA")

#Adds zipcode information to data frame
df_zip<-left_join(df, zipcode, by = c("ZipCode" = "zipcode"))

#removes users that did not answer questions
df_zip<-subset(df_zip, QuestionID!="NA")

#Counts number of users for each zipcode and combines it with the zipcode map data
zip_count<-df_zip %>% group_by(ZipCode) %>% dplyr::summarise(n = n())
zip_count<-left_join(zip_count, zipcode, by = c("ZipCode" = "zipcode"))
zip_count<-subset(zip_count, lat!="NA")

#Gets map of US with state lines
states<-states(cb=TRUE)

#counts number of users for each state
state_count<-df_zip %>% group_by(state) %>% dplyr::summarise(n=n())
states<-left_join(states,state_count,  by=c("STUSPS"="state"))

##### Summary Statistics Table Selection Function

summary_table<-function(data, behavior = "All") {
  if(!("All" %in% behavior)) {
    data <- data %>% 
      filter(Behavior %in% behavior)
    table_title_behavior<- stringr::str_glue("{paste0(behavior, collapse=', ')} behaviors")
  } else{
    table_title_behavior<-"all behaviors"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  return(data)
}

##### Box Plot Selection Function
version_boxplot<-function(data, behavior = "All") {
  if(!("All" %in% behavior)) {
    data <- data %>% 
      filter(Behavior %in% behavior)
    plot_title_behavior<- stringr::str_glue("{paste0(behavior, collapse=', ')} behaviors")
  } else{
    plot_title_behavior<-"all behaviors"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  ggplot(data, aes(x=as.factor(Version), y=Gap, fill=Version)) + 
    geom_boxplot() + facet_wrap(~Behavior) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank()) + 
    scale_y_continuous(expand = c(0,0)) +
    labs(title="\u2206 ('Should'-'Do') by Version and Behavior",y="\u2206 (AVG %)")
}

##### Magnitude Plot Selection Function
magnitude<-function(data, behavior = "All") {
  if(!("All" %in% behavior)) {
    data <- data %>% 
      filter(Behavior %in% behavior)
    plot_title_behavior<- stringr::str_glue("{paste0(behavior, collapse=', ')} behaviors")
  } else{
    plot_title_behavior<-"all behaviors"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  ggplot(data, aes(x=reorder(Behavior,gap), y=gap)) + 
    geom_linerange(data, mapping = aes(x=reorder(Behavior,gap),ymin=Do_mean, ymax=Should_mean), linewidth = 2, color="navy") + 
    geom_point(data, mapping = aes(x=reorder(Behavior,gap), y=Should_mean), 
               pch=17, color = "navy", size = 5) +
    scale_y_continuous(sec.axis = sec_axis(~.,name = "\u2206 ('Should' - 'Do') AVG %", breaks = c(0,20,40,60,80,100)), 
                       limits=c(0,100), expand = c(0,0), n.breaks = 6) + 
    geom_point(data, mapping = aes(x=reorder(Behavior,gap), y=gap),size = 4, colour = "red3") +
    labs(x="Behavior", y="'Do' to 'Should' (AVG %)", title="Magnitude of Change Between 'Do' and 'Should'") +
    theme(axis.text.x = element_text(angle = 30, vjust=0.8), plot.title=element_text(hjust=0.5), 
          axis.text.y.right= element_text(color = "red3"), axis.text.y = element_text(color="navy"))
}


##### Likelihood Selection Function
likelihood_plot<-function(data, behavior = "All") {
  if(!("All" %in% behavior)) {
    data <- data %>% 
      filter(Behavior %in% behavior)
    plot_title_behavior<- stringr::str_glue("{paste0(behavior, collapse=', ')} behaviors")
  } else{
    plot_title_behavior<-"all behaviors"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  ggplot(data, aes(x=gap, y=likely_todo, label=Behavior)) + geom_text(size=3.5) + 
    labs(y="% Likely", x="\u2206 ('Should' - 'Do') AVG %", title="Likelihood vs. Gap") + 
    geom_vline(xintercept=40, linetype="dashed", linewidth=.6) +
    scale_x_continuous(limits=c(20,55),expand = c(0,0)) + 
    scale_y_continuous(limits=c(50,100),expand = c(0,0)) + 
    theme( plot.title=element_text(hjust=0.5))
}

##### Profile Table Selection Function
profile_sum_table<-function(data, trait = "All") {
  if(!("All" %in% trait)) {
    data <- subset(data, select = c(trait))
    table_title_behavior<- stringr::str_glue("{paste0(trait, collapse=', ')} traits")
  } else{
    table_title_behavior<-"all traits"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  return(data)
}

##### Profile Counts Plot Selection Function
profile_count_plot<- function(profile, trait = "All", behavior = "All", significant = "Yes") {

if(trait =="All") {
  
if(significant == "Yes") {  
    
  if(!("All" %in% behavior)) {
    profile<-profile %>%
      filter(Behavior %in% behavior)
    signi<-subset(profile, significant=="yes")
    signi<-type.convert(signi, as.is=TRUE)
    
  } else {
    
    plot_title_behavior<-"all behaviors"
    signi<-subset(profile, significant=="yes")
    signi<-type.convert(signi, as.is=TRUE)
  }
  
  if (nrow(profile)==0) {
    return(NULL)
  }
  ggplot(data=profile, mapping = aes(x=Behavior, y=count)) + geom_point(aes(col=name)) + scale_color_hue(c=45, l=30) + 
    geom_point(data=signi, mapping = aes(x=Behavior, y=count), color="red", size=4, pch=8 , na.rm=TRUE) + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 30, vjust=0.8))+ 
    labs(title="Number of Questions Answered by Subgroup", subtitle="Sensitivity to Model")
  
} else {
  if(!("All" %in% behavior)) {
    profile<-profile %>%
      filter(Behavior %in% behavior)
    
  } else {
    
    plot_title_behavior<-"all behaviors"
  }
  
  if (nrow(profile)==0) {
    return(NULL)
  }
  ggplot(data=profile, mapping = aes(x=Behavior, y=count)) + geom_point(aes(col=name)) + scale_color_hue(c=45, l=30) + 
    theme(legend.position = "none", axis.text.x = element_text(angle = 30, vjust=0.8))+ 
    labs(title="Number of Questions Answered by Subgroup")
}
} else {
  if(trait=="Gender") {
    gender<-c("female","male","other")
    profile<-subset(profile, name %in% gender)
  }
  else if(trait =="AlertsHelp") {
    profile<-profile[profile$name %like% "alert",]
  }
  else if(trait == "SmokeCommon") {
    profile<-profile[profile$name %like% "smoke",]
  }
  else if(trait == "HoursCanImpact") {
    profile<-profile[profile$name %like% "hours",]
  }
  else if(trait == "PossibleToReduce") {
    profile<-profile[profile$name %like% "pos",]
  }
  else if(trait == "GeneralHealth") {
    profile<-profile[profile$name %like% "genhealth",]
  }
  else if(trait == "ActivityLevel") {
    profile<-profile[profile$name %like% "act",]
  }
  else if(trait =="Age") {
    profile<-profile[profile$name %like% "age",]
  }
  else if(trait =="Education") {
    education<-c("high_school_orless", "tech", "bachelors_ormore")
    profile<-subset(profile, name %in% education)
  }
  else {
    plot_title_trait<-"all traits"
  }
  if(significant == "Yes") {  
    
    if(!("All" %in% behavior)) {
      profile<-profile %>%
        filter(Behavior %in% behavior)
      signi<-subset(profile, significant=="yes")
      signi<-type.convert(signi, as.is=TRUE)
      
    } else {
      
      plot_title_behavior<-"all behaviors"
      signi<-subset(profile, significant=="yes")
      signi<-type.convert(signi, as.is=TRUE)
    }
    
    if (nrow(profile)==0) {
      return(NULL)
    }
    ggplot(data=profile, mapping = aes(x=Behavior, y=count)) + geom_point(aes(col=name), size = 5) + scale_color_hue(c=70, l=60) + 
      geom_point(data=signi, mapping = aes(x=Behavior, y=count), size=7, pch=8 , na.rm=TRUE) + 
      theme(axis.text.x = element_text(angle = 30, vjust=0.8))+ 
      labs(title="Number of Questions Answered by Subgroup", subtitle="Sensitivity to Model")
    
  } else {
    if(!("All" %in% behavior)) {
      profile<-profile %>%
        filter(Behavior %in% behavior)
      
    } else {
      
      plot_title_behavior<-"all behaviors"
    }
    
    if (nrow(profile)==0) {
      return(NULL)
    }
    ggplot(data=profile, mapping = aes(x=Behavior, y=count)) + geom_point(aes(col=name), size = 5) + scale_color_hue(c=70, l=60) + 
      theme(axis.text.x = element_text(angle = 30, vjust=0.8))+ 
      labs(title="Number of Questions Answered by Subgroup")
}  
}
}
##### Barriers Barplots
barriers_barplot<-function(data, barriers = "All", behavior = "All", count = "Counts") {
  if(count == "Counts") {
  if(!("All" %in% barriers)) {
    data <- data %>% 
      filter(Barrier %in% barriers)
    plot_title_barrier<- stringr::str_glue("{paste0(barriers, collapse=', ')} barriers")
  } else{
    plot_title_barrier<-"all barriers"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  
  if(!("All" %in% behavior)) {
  data<-data %>%
    dplyr::filter(Behavior %in% behavior)
  } else {
    plot_title_behavior<-"all barriers"
  }
  if (nrow(data)==0) {
    return(NULL)
  }
  
  
  label_count<-data  %>% group_by(Barrier) %>% dplyr::summarise(n=n())
  label_count$barrier<-NA
  
  ggplot(data, aes(c(Behavior), fill=Barrier)) + 
    geom_bar(aes(y=after_stat(count)), width = .5) +
    geom_text(stat="count", aes(label=after_stat(count)), position=position_stack(vjust=0.5)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,NA)) + 
    coord_flip() +  theme(axis.title.x=element_blank(),
                          axis.title.y=element_blank(),plot.title=element_text(hjust=0.5, size = 12), legend.position = "bottom", 
                          legend.title = element_text(size=8), legend.text = element_text(size=7), 
                          legend.key.size=unit(3, "mm")) + guides(fill=guide_legend(nrow=1, reverse=TRUE)) +
    labs(y="Count", title = "Barriers to Behavior (\u2206 > 40%)")
  
  } else {
    if(!("All" %in% barriers)) {
      data <- data %>% 
        dplyr::filter(Barrier %in% barriers)
      plot_title_barrier<- stringr::str_glue("{paste0(barriers, collapse=', ')} barriers")
    } else{
      plot_title_barrier<-"all barriers"
    }
    if(nrow(data)==0) {
      return(NULL)
    }
    
    if(!("All" %in% behavior)) {
      data<-data %>%
        dplyr::filter(Behavior %in% behavior)
    } else {
      plot_title_behavior<-"all barriers"
    }
    if (nrow(data)==0) {
      return(NULL)
    }
    
    ggplot(data, aes(Behavior, fill=Barrier)) + geom_bar(position="fill", width = .6) + scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x = element_text(angle = 30, vjust=0.8), plot.title=element_text(hjust=0.5),axis.title.x=element_blank(),
            axis.title.y=element_blank(), legend.position = "bottom", 
            legend.title = element_text(size=8), legend.text = element_text(size=7), 
            legend.key.size=unit(3, "mm")) + guides(fill=guide_legend(nrow=1, reverse=TRUE)) + labs(title = "Barriers to Behavior")
  }
  
}

###### Barrier Extreme Percent Selection Function
barriers_extreme_percents<-function(data, barriers = "All", behavior = "All", extreme = "All") {
 
    if(!("All" %in% barriers)) {
      data <- data %>% 
        dplyr::filter(Barrier %in% barriers)
      plot_title_barrier<- stringr::str_glue("{paste0(barriers, collapse=', ')} barriers")
    } else{
      plot_title_barrier<-"all barriers"
    }
    if(nrow(data)==0) {
      return(NULL)
    }
    
    if(!("All" %in% behavior)) {
      data<-data %>%
        dplyr::filter(Behavior %in% behavior)
    } else {
      plot_title_behavior<-"all barriers"
    }
    if (nrow(data)==0) {
      return(NULL)
    }
    

    if(!("All" %in% extreme)) {
      data <- data %>% 
        dplyr::filter(Different %in% extreme)
      plot_title_extreme<- stringr::str_glue("{paste0(extreme, collapse=', ')} extreme")

    } else{
      plot_title_extreme<-"all percents"

    }
    if(nrow(data)==0) {
      return(NULL)
    }
if ("All" %in% extreme) {
  
   ggplot(data, aes(x=Behavior, y=Percents, color = Barrier, shape=Different), size=6) + 
    geom_point() +
    scale_shape_manual(values = c(16, 8, 9), labels = c("Not Different", "Larger", "Smaller")) + 
    theme(axis.text.x = element_text(angle = 30, vjust=0.8), plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5), legend.title=element_blank()) + 
    labs(title="Extreme Percent Difference in Barriers by Behavior", 
         subtitle="Extremes based on percents falling outside 95% confidence interval of t test")
} else {
  ggplot(data, aes(x=Behavior, y=Percents, color = Barrier, shape=Different), size=6) + 
    geom_point() +
    scale_shape_manual(values = c(16, 8, 9), labels = c(extreme)) + 
    theme(axis.text.x = element_text(angle = 30, vjust=0.8), plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5), legend.title=element_blank()) + 
    labs(title="Extreme Percent Difference in Barriers by Behavior", 
         subtitle="Extremes based on percents falling outside 95% confidence interval of t test")
    }
}

##### CI table plot selection function
CI_plot<-function(data, behavior = "All") {
  if(!("All" %in% behavior)) {
    data <- data %>% 
      dplyr::filter(Behavior %in% behavior)
    plot_title_behavior<- stringr::str_glue("{paste0(behavior, collapse=', ')} behaviors")
  } else{
    plot_title_behavior<-"all behaviors"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  ggplot(data, aes(x=as.factor(Version), y=Fit, color=Version)) +
    geom_errorbar(aes(ymin=Lower, ymax=Upper), width = .4) +
    geom_point(aes(x=as.factor(Version), y=Fit), pch=15, size = 2) + 
    facet_wrap(~Behavior) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank()) + 
    labs(title="\u2206 ('Should'-'Do') by Version and Behavior",y="\u2206 (AVG %)", 
         subtitle = "Estimated Average Fit with Average 95% Confidence Interval")
  


}

##### Version Model Summary Selection Function
model_summary<-function(data, behavior = "All") {
  if(!("All" %in% behavior)) {
    data <- data %>% 
      dplyr::filter(Behavior %in% behavior)
    table_title_behavior<- stringr::str_glue("{paste0(behavior, collapse=', ')} behaviors")
  } else{
    table_title_behavior<-"all behaviors"
  }
  if(nrow(data)==0) {
    return(NULL)
  }
  return(data)
}


##### Map Function
count_map<-function(map1, counts = "State", data2=state_count, data3=zip_count) {
  if(counts == "State"){
    my_breaks <- c(200, 1000, 10000, 300000)
    ggplot(map1, aes(fill = n)) + geom_sf() + theme_void() + 
      scale_fill_gradientn(colours=rev(magma(6)),
                           name="Number of Users",
                           na.value = "grey100", 
                           trans = "log",
                           breaks = my_breaks, labels = my_breaks) + 
      coord_sf(xlim=c(-155, -60), ylim = c(18,60)) + labs(title = "Number of Users by State") +
      theme(legend.position = "bottom",legend.text = element_text(size=7, angle = 45),legend.title = element_text(size=8, vjust=1))
  } else {
    my_breaks <- c(10, 50, 100, 200, 500)
    ggplot(map1) + geom_sf() + theme_void() + 
      geom_point(data3, mapping = aes(x=lng, y=lat,group = ZipCode, color = n)) + 
      coord_sf(xlim=c(-155, -60), ylim = c(18,60)) + 
      scale_color_gradientn(colours=rev(magma(6)),
                            name="Number of Users",
                            na.value = "grey100", 
                            trans = "log",
                            breaks = my_breaks, labels = my_breaks) + labs(title = "Number of Users by Zipcode")+
      theme(legend.position = "bottom",legend.text = element_text(size=7, angle = 45),legend.title = element_text(size=8, vjust=1))
  }
}
############################################################################
#################             Set Up Dashboard           ###################
############################################################################

ui<- dashboardPage(
  dashboardHeader(title="Smoke Smarts Statistics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploratory Data Analysis", tabName="exploratory", icon=icon("fire", lib="glyphicon")),
      menuItem("Profile Traits", tabName="traits",icon=icon("user")),
      menuItem("Behavior Barriers", tabName="barriers", icon=icon("bell")),
      menuItem("Model Results", tabName = "models", icon = icon("lightbulb")),
      menuItem("User Distribution Map",tabName = 'map', icon = icon("person"))
    )
  ),
  dashboardBody(
    tabItems(
########################## Summary Statistics ###############################
            tabItem(tabName="exploratory",
              h2("Magnitude of Change"),
              fluidPage(
                titlePanel("Summary Statistics"),
              
                    selectInput(
                      inputId = "select_behavior",
                      label = "Select Behavior",
                      choices = c(
                        "All",
                        "ASH",
                        "ASTHMA",
                        "AVOID",
                        "HEALTHPRO",
                        "HEART",
                        "HEPA",
                        "IAQ",
                        "N95",
                        "ORANGEAQI",
                        "RECIRC",
                        "WARNINGS",
                        "WINDOWS"
                      ), 
                      selected = "All",
                      multiple = TRUE
                    ),
                fluidRow( 
                  column(6, tableOutput("stats")),
                   column(6, plotOutput("magnitude"))
              ),
              br(),
                plotOutput("likely"),
              br(),
                 plotOutput("plot1")
                )
              
       ),

##################### Profile Traits ####################################### 
     tabItem(tabName="traits",
              h2("Profile Traits of Participants"), 
              fluidPage(
                titlePanel("Profile Traits Summary"),
                fluidRow(
                  column(2,selectInput(
                  inputId = "select_trait",
                  label = "Select Profile Trait",
                  choices = c(
                    "All",
                    "Gender",
                    "Age",
                    "Education",
                    "AlertsHelp",
                    "SmokeCommon",
                    "HoursCanImpact",
                    "PossibleToReduce",
                    "GeneralHealth",
                    "ActivityLevel"
                  ), 
                  selected = "All",
                  multiple = FALSE
                ),
                selectInput(
                  inputId = "select_signi",
                  label = "Highlight Significant Values",
                  choices = c(
                    "Yes",
                    "No"
                  ),
                  selected = "Yes",
                  multiple = FALSE
                )
                ),
                column(10,tableOutput("profile_stats"))),
                plotOutput("prof_count")
              )
      ),

####################  Barriers ##############################
      tabItem(tabName = "barriers",
              h2("Behavior Barriers"),
              fluidPage(
                titlePanel("Barriers to Behavior"),
                fluidRow(
                  column(6,selectInput(
                 inputId = "select_barrier",
                 label = "Select Barrier",
                 choices = c(
                   "All",
                   "Cost (monetary)",
                   "Effort/time",
                   "Forgetting",
                   "No barriers",
                   "Not enough benefit",
                   "Other"
                 ),
                 selected="All",
                 multiple = TRUE
                )),
                column(6, selectInput(
                  inputId = "select_count",
                  label = "Select Type of Summary",
                  choices = c(
                    "Counts",
                    "Percents"
                  ),
                  selected = "Counts",
                  multiple = FALSE
                )
                )),
                plotOutput("plot2"),
                br(),
                fluidRow(
                  column(2, selectInput(
                    inputId = "select_extreme",
                    label = "Select Extreme",
                    choices = c(
                      "All",
                      " ",
                      "Larger",
                      "Smaller"
                    ),
                    selected = "All",
                    multiple = TRUE
                    )),
                  column(10, plotOutput("plot3"))
              )
           )
      ),
########################### Models #############################
      tabItem(tabName = "models",
           h2("Model Results"),
           fluidPage(
             titlePanel("Summary of Model Results"),
             plotOutput("CIplot"),
             br(),
            fluidRow(
              column(1),
             box(title = "Model Coefficient Output", width = 4, status = "primary", 
                 solidHeader = TRUE, tableOutput("ver_model")),
              column(6, offset = 0, br(), br(),
                    box(title="Table Explanation", background = "light-blue", width = 6,collapsible = TRUE, p("This table shows the coefficient output from the model for each behavior. 
                         The intercept is the estimated gap between should and do. 
                         The negative and postive variables are the estimated coefficients 
                         for the model. A positive coefficient indicates that the estimated 
                         gap would increase the estimated gap while a negative number would 
                         decrease the estimated gap. The frame variable is the difference 
                         between the positive and negative coefficients. If the frame variable 
                         is positive, it indicates that the coefficient of the positive version 
                         is larger than the negative version."))))

            )
      ),
######################### Map ###########################
           tabItem(tabName = "map",
                   h2("Distribution of Users in the United States"),
           fluidPage(
               column(2, selectInput(
                 inputId = "select_distribution",
                 label = "Select Level of Distribution",
                 choices = c(
                   "State",
                   "Zipcode"
                 ),
                 selected = "State",
                 multiple = FALSE
               )),
               column(10, plotOutput("plotmap")))))
          )
     )


server<-function(input,output) {
  output$stats<-renderTable(
    summary_table(table_1s, input$select_behavior), bordered = TRUE, striped = TRUE
  )
 
  output$magnitude<- renderPlot(
    magnitude(table_1s, behavior = input$select_behavior)
  )
   
  output$likely<- renderPlot(
    likelihood_plot(table_1s, behavior = input$select_behavior)
  )
  
  output$plot1<- renderPlot(
    version_boxplot(version_gap, behavior = input$select_behavior)
  )
  
  output$profile_stats<-renderTable(
    profile_sum_table(profile_sum_stats, input$select_trait), bordered = TRUE, striped = TRUE
  )
  
  output$prof_count<-renderPlot(
    profile_count_plot(profile_counts, input$select_trait, input$select_behavior, input$select_signi)
  )
  
  output$plot2<-renderPlot(
    barriers_barplot(bar_long, input$select_barrier, input$select_behavior, input$select_count)
  )
  
  output$plot3<-renderPlot(
    barriers_extreme_percents(percents_long, input$select_barrier, input$select_behavior, input$select_extreme)
  )
  
  output$CIplot<-renderPlot(
    CI_plot(CItable, input$select_behavior)
  )
  
  output$ver_model<-renderTable(
    model_summary(estDiff, input$select_behavior), bordered = TRUE, striped = TRUE
  )
  
  output$plotmap<-renderPlot(
    count_map(states, input$select_distribution), height = 600, width = 800
  )
  
}

shinyApp(ui=ui, server=server)
