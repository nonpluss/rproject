library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(ggplot2)
library(readxl)

# data for enrollment, course, map tab
intake<- read.csv("universities-intake-enrolment-and-graduates-by-course.csv")
intake$intake <- gsub(",","",intake$intake)
intake$enrolment<-gsub(",","",intake$enrolment)
intake$graduates <- gsub(",","",intake$graduates)
intake$intake <- as.numeric(intake$intake)
intake$enrolment <- as.numeric(intake$enrolment)
intake$graduates <- as.numeric(intake$graduates)

df<-data.frame(name=c('National University of Singapore','Nanyang Technological University','Singapore Management University',
                      'Singapore University of Technology and Design',"Singapore Institute of Technology",
                      "Singapore University of Social Sciences","NUS Bukit Timah Campus"),
               latitude=c(1.296643,1.34472,1.29667,1.341313,1.3012,1.3282,1.3190),
               longitude=c(103.776398,103.68139,103.84972,103.96376,103.7804,103.7762,103.8172),
               address=c("21 Lower Kent Ridge Road, Singapore 119077","50 Nanyang Avenue Nanyang Technological University Singapore 639798",
                         "81 Victoria Street Singapore 188065","8 Somapah Road Singapore 487372",
                         "10 Dover Drive Singapore 138683","463 Clementi Road. Singapore 599494.",
                         "469 Bukit Timah Road, Singapore 259756"),
               link=c("https://nus.edu.sg/oam/contact-us/getting-to-nus","https://www.ntu.edu.sg/life-at-ntu/visiting-ntu",
                      "https://www.smu.edu.sg/smupatronsday/get-there",'https://www.sutd.edu.sg/About/Contact-Us/Getting-Here',
                      "https://www.singaporetech.edu.sg/connect","https://www.suss.edu.sg/about-suss/resources/campus-location-and-facilities",
                      "https://nus.edu.sg/osa/student-services/ssc/bukit-timah-campus/getting-to-btc"))

mrt<- read.csv("mrt_lrt_data.csv")
mrt$line<-factor(mrt$line,levels=c('red','green','purple','orange','blue','grey'))

# data for cca tab
uni_ccas <- read.csv("University_CCAs.csv")
schools <- unique(uni_ccas[,"school"])
schools <- as.vector(schools)
tags <- unique(uni_ccas[,"tags"])
tags <- as.vector(tags)

#data for GES tab
GES <- read.csv("Uni GES data.csv")
colnames(GES)[1]<-"University"
unis <- unique(GES[,"University"])
degrees <- GES$Degree

# data for the gender tab
uni_gender <- as.data.frame(read_excel("gender_data.xlsx", sheet = "Combined Data"))
schools_gender <- unique(uni_gender[,"School"])
schools_gender <- as.vector(schools_gender)
faculty <- na.omit(unique((uni_gender[,"Faculty"])))

# Define UI for application that draws a histogram
ui <- navbarPage("Singapore Universities Guide",
                 
                 tabPanel("Overview",
                          mainPanel(
                            tabsetPanel(type='tabs',
                                        tabPanel("Enrollment Info",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectInput(inputId = 'year',label='Select Year',
                                                                 choices=c(2005:2019)),
                                                     helpText("Choose a year to view the intake, enrollment and graduates figures by course.
                          Hover over the interactive graph for more information and scroll below for individual charts.")
                                                   ),
                                                   
                                                   mainPanel(
                                                     plotlyOutput(outputId = 'overview',width="150%",height="600px"),
                                                     plotOutput(outputId = 'intake',width = '150%'),
                                                     plotOutput(outputId = 'enrollment',width='150%'),
                                                     plotOutput(outputId = 'graduates', width ='150%')))
                                        ),
                                        tabPanel("Map",
                                                 mainPanel(
                                                   leafletOutput(outputId = 'map',width='200%',height='500px'))
                                        ),
                                        tabPanel("Links",
                                                 mainPanel(uiOutput("tab")))
                            )
                          )
                 ),
                 
                 
                 tabPanel("Course Information",
                          mainPanel(
                            tabsetPanel(type='tabs',
                                        tabPanel("Graduate Employment Survey Data (2020)",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectInput(inputId = "uni_name",
                                                                 label = "Choose a University",
                                                                 choices = unis),
                                                     selectInput(inputId='degree',label='Degree Programme',
                                                                 choices= degrees)
                                                   ),
                                                   mainPanel(
                                                     tableOutput("check")))),
                                        tabPanel('Gender Ratio',
                                                 sidebarLayout(
                                                   
                                                   sidebarPanel(
                                                     
                                                     selectInput(inputId = "School",
                                                                 label = "Choose a University",
                                                                 choices = schools_gender),
                                                     selectInput(inputId = "Faculty",
                                                                 label = "Faculty",
                                                                 choices = faculty)
                                                   ),
                                                   mainPanel(
                                                     tabPanel("Table", tableOutput(outputId = 'gender_table')),
                                                     tabPanel("Gender Charts", plotOutput(outputId = 'C'))
                                                     
                                                   )
                                                 )
                                        )
                            )
                          )
                 ),
                 
                 tabPanel('University CCAs',
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              selectInput(inputId = "university_name",
                                          label = "Choose a University",
                                          choices = schools),
                              selectInput(inputId = "cca_tag",label="CCA Category", choices=NULL)
                              
                            ),
                            mainPanel(
                              tableOutput("view"))))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$coursetable <- renderTable(
    uni_courses %>% filter(University == input$unis)
  )
  
  data <- reactive({
    intake %>% filter(sex=='MF') %>% filter(year==input$year)
  })
  
  output$intake <-renderPlot( {
    #should i make input$year reactive?
    ggplot(data(),aes(x=course,y=intake)) + geom_bar(stat='identity',fill='lightblue') + 
      scale_y_continuous() +
      geom_text(aes(label=intake),vjust = -0.1) +
      theme(axis.text.x = element_text(angle=90)) + 
      labs(title=paste0(input$year," Total Intake")) +
      xlab('Courses') + ylab("Intake")
  })
  
  output$enrollment <- renderPlot({
    ggplot(data(),aes(x=course,y=enrolment)) + geom_bar(stat='identity',fill='lightblue') + 
      geom_text(aes(label=enrolment),vjust= -0.1) +
      theme(axis.text.x = element_text(angle=90)) + 
      labs(title=paste0(input$year," Total Enrollment")) +
      xlab("Courses") +ylab("Enrollment")
  })
  
  output$graduates <- renderPlot({
    ggplot(data(),aes(x=course,y=graduates)) + geom_bar(stat='identity',fill='lightblue') + 
      geom_text(aes(label= graduates),vjust = -0.1) +
      theme(axis.text.x = element_text(angle=90)) + 
      labs(title=paste0(input$year," Total Graduates"))+
      xlab("Courses") +ylab("Graduates")
  })
  
  output$overview <-renderPlotly({
    ggplotly(ggplot(data(),aes(x=course)) + geom_point(aes(y=intake,size=intake),col='blue') +
               geom_line(aes(y=intake,group=1),col='blue') +
               geom_point(aes(y=enrolment,size=intake),col='green') + geom_line(aes(y=enrolment,group=2),col='green') +
               geom_point(aes(y=graduates,size=intake),col='red') + geom_line(aes(y=graduates,group=3),col='red') +
               theme(axis.text.x = element_text(angle=90,size=7)) +
               labs(title='Overview of intake,enrolment and graduates by course',
                    subtitle = 'Hover over points for more information') +
               ylab('Count'))
  })
  
  output$map <- renderLeaflet({
    colorFactors <- colorFactor(c('red','green','purple','orange','blue','black'),domain=mrt$line)
    leaflet() %>% addTiles() %>% addMarkers(data=df, lng=~longitude, lat=~latitude,
                                            popup=paste("Name:",df$name,"<br>",
                                                        "Address:",df$address,"<br>",
                                                        "Link:",df$link)) %>%
      addCircleMarkers(lng=mrt$lng,lat=mrt$lat,popup=mrt$station_name,color=colorFactors(mrt$line),
                       radius=3,fillOpacity = 1)
  })
  
   url1<-a("NTU",href="https://www.ntu.edu.sg/")
  url2<-a("NUS",href="https://www.nus.edu.sg/")
  url3<-a("SMU",href="https://www.smu.edu.sg/")
  url4<-a("SUTD",href="https://www.sutd.edu.sg/")
  url5<-a("SUSS",href="https://www.suss.edu.sg/")
  url6<-a("SIT",href="https://www.singaporetech.edu.sg/undergraduate-programmes")
  output$tab <- renderUI({
    
   tagList("Nanyang Technological University",url1,
           "National University of Singapore",url2,
           'Singapore Management University',url3,
           "Singapore University of Technology and Design",url4,
           "Singapore University of Social Sciences",url5,
           "Singapore Institute of Technology",url6)
    
  })
  
  observe({
    
    x <- uni_ccas %>% filter(school == input$university_name) %>% select(tags)
    updateSelectInput(session,"cca_tag","CCA Category", choices= unique(x))
    
  })
  
  output$view <- renderTable({
    
    uni_ccas %>% filter(school==input$university_name) %>% filter(tags == input$cca_tag) %>% select(titles, tags, school)
  })
  
  observe({
    
    y <- GES %>% filter(University == input$uni_name) %>% select(Degree)
    updateSelectInput(session,"degree","Degree Programme", choices= unique(y))
    
  })
  
  output$check <- renderTable(
    GES %>% filter(University == input$uni_name) %>% filter(Degree== input$degree) %>% select(Employed, In.Full.Time.Permanent.Employment, Mean.Basic.Monthly.Salary, Median.Basic.Monthly.Salary)
  )
  
  #gender table component
  
  observe({
    
    z <- uni_gender %>% filter(School == input$School) %>% select(Faculty)
    updateSelectInput(session, "Faculty", "Faculty", choices = unique(z))
    
    
  })
  
  output$gender_table <- renderTable({
    uni_gender %>% filter(School==input$School) %>% filter(Faculty == input$Faculty) %>% select(Course, Male, Female, Total)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
