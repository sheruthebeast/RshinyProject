############################    Code of HR Dashboard    ########################
################################################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(DT)








# Define UI for application 
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "HR Dashboard",
                  tags$li(class="dropdown",tags$a(href='https://www.linkedin.com/in/sachin-wadhwa-6b700ba4/',icon("linkedin"),"My Profile",target="_blank"))),
  dashboardSidebar(sidebarMenu(
    menuItem("Employee Overview", tabName = "first", icon = icon("dashboard")),
    menuItem("Employee Details", tabName = "third", icon = icon("dashboard")),
    menuItem("Details", tabName = "second", icon = icon("database"),badgeLabel = "new",
             badgeColor = "green")
    
  )),
  dashboardBody(# h1("Employee Overview"),
    
    
    tabItems(
      #first tab COntent 
      tabItem(
        tabName = "first",
        #   h2("Employee Overview"),
        fluidRow(
          
          infoBoxOutput("sum_HeadCount",width=3),
          infoBoxOutput("sum_Hires",width=3),
          infoBoxOutput("sum_Terminations",width=3),
          infoBoxOutput("divide_turn_over",width=3)
        ),
        selectInput(inputId = "FilterYear",
                    label = "Year",
                    choices = c("ALL",sort(unique(Emp_Transf$Year))),
                    selected = 2018),
        fluidRow(
          
          box(title="HeadCount By Dept",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot",height = 300)),
          box(title="HeadCount By Position",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot1",height = 300))
        ),
        fluidRow(
          
          box(title="HeadCount By AgeGroup",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot2",height = 300)),
          box(title="HeadCount By PayTypeID",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot3",height = 300)),
        ),
        fluidRow(
          
          box(title="Trend of New Hires and Terminations",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot4",height = 300))
          
        )
      ),
      
      
      
      #third tab COntent 
      tabItem(
        tabName = "third",
       # fluidRow(box(h2("Personal Information", align = "center"),width = 12,background = "light-blue")),
       fluidRow(box(h2("Personal Information", align = "center"),width = 12)),
        fluidRow(
          valueBoxOutput("personal_info",width=3),
          valueBoxOutput("date_of_birth",width=3),
          valueBoxOutput("hire_date",width=3),
          valueBoxOutput("edu_cation",width=3)
        ),
        fluidRow(box(h2("Current Position", align = "center"),width = 12)),
        fluidRow(
          valueBoxOutput("dept",width=4),
          valueBoxOutput("position",width=4),
          valueBoxOutput("contract_type",width=4)
        ),
       fluidRow(
         
         box(selectInput(inputId = "SelectEmployee",
                     label = "Select Employee",
                     choices =  c("ALL", sort(unique(Emp_Transf$EmpFullName))),
                     selected = "Christy Zhu"),width = 3),
        valueBoxOutput("Avg_Score",width=3)
                ),
      
        fluidRow(
          
          box(title="Employee Engagement By Survey",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot5",height = 300)),
          box(title="Key Metrics of Employee Engagement",status="primary",solidHeader = TRUE,collapsible = TRUE,
              plotOutput("plot6",height = 300))
        ),
       fluidRow(
         
         box(title="Sick Leave and Vacation Leave By Year",status="primary",solidHeader = TRUE,collapsible = TRUE,
             plotOutput("plot7",height = 300))
       )
      ),
      
      # Second Tab Content
      tabItem(tabName = "second",
              #   h2("Detailed Page"),
              radioButtons("type","Format type:",
                           choices=c("Excel (CSV)","Text (TSV)","Doc")),
              downloadButton('downloadData','Download'),
              tableOutput('table')
              
      )
    ))
)




server <- function(input, output, session) {
  
  


  
 
  
  
  ################################   1st Tab DataSet #####################
  ##############################################################################
  Emp_Transf = Emp_Transf
 
   #info1 - Total Head Count Data
  info1 <- reactive({
    req(input$FilterYear)

      Emp_Transf %>% filter(Year %in% input$FilterYear) %>% mutate(HeadCount=sum(HeadCount)) %>% select(HeadCount) %>% distinct()
  })
  
  #info2 - Total Hires Data
  info2 <- reactive({
    req(input$FilterYear)
    Emp_Transf %>% filter(Year %in% input$FilterYear) %>% mutate(Hires=sum(Hires)) %>% select(Hires) %>% distinct()
  })

  #info3 - Total Termination Data
  info3 <- reactive({
    req(input$FilterYear)
    Emp_Transf %>% filter(Year %in% input$FilterYear) %>% mutate(Terminations=sum(Termination)) %>% select(Terminations) %>% distinct()
  })
 
  
  
  #summarise HeadCount by Department data
  data <- reactive({
    req(input$FilterYear)
    HiresByDept <-
      Emp_Transf %>% filter(Year %in% input$FilterYear) %>% group_by(Department) %>% summarise(HeadCount =
                                                                                                 sum(HeadCount))
  })
  
  #update SelectInput Dyanmically
  observe({
    updateSelectInput(session, "FilterYear",  choices =  sort(unique(Emp_Transf$Year)),selected = 2018)
  })
  
  
  #summarise HeadCOunt by Position data
  data1 <- reactive({
    req(input$FilterYear)
    HiresByPosition <-
      Emp_Transf %>% filter(Year %in% input$FilterYear) %>% group_by(Position) %>% summarise(HeadCount =
                                                                                               sum(HeadCount))
  })
  
  #summarise HeadCount by AgeGroup data
  data2 <- reactive({
    req(input$FilterYear)
    HiresByPosition <-
      Emp_Transf %>% filter(Year %in% input$FilterYear) %>% group_by(AgeGroup) %>% summarise(HeadCount =
                                                                                               sum(HeadCount))
  })
  
  
  #summarise HeadCount by PaytypeID data
  data3 <- reactive({
    req(input$FilterYear)
    HiresByPayTypeID <-
      Emp_Transf %>% filter(Year %in% input$FilterYear) %>% group_by(PayTypeID) %>% summarise(HeadCount =
                                                                                                sum(HeadCount))
    
  })
  
  
  
  #summarise Trend of Hires and Termination data
  data4 <- reactive({
    #  req(input$FilterYear)
    HiresByPayTypeID <-
      #  Emp_Transf %>% group_by(Year) %>% summarize_at(c("Hires","Termination"),sum,na.rm=TRUE)
      Emp_Transf %>% group_by(Year) %>% summarize_at(c("Hires","Termination"),sum,na.rm=TRUE) %>%  pivot_longer(cols = c(Termination,Hires),
                                                                                                                names_to = "Attribute",
                                                                                                                values_to = "Value")
    
  })
  
  
  
  
  ################################   2nd Tab DataSet #####################
  ##############################################################################
  
  #summarise data by Year and Avg Employee Engagement Score
  data5 <- reactive({
    if(input$SelectEmployee!="ALL"){
    req(input$SelectEmployee)
    HiresByDept <-
      Emp_Transf %>%  
     filter(EmpFullName %in% input$SelectEmployee) %>% 
      group_by(YearC) %>% summarise(Score =round(mean(Score),2))
    }
    else{
      req(input$SelectEmployee)
      HiresByDept <-
        Emp_Transf %>%  
        group_by(YearC) %>% summarise(Score =round(mean(Score),2))
    }
  })
  
  #update SelectInput Dyanmically
  observe({
    updateSelectInput(session, "SelectEmployee", choices =  c("ALL", sort(unique(Emp_Transf$EmpFullName))),selected = "Christy Zhu")
  })
  
  
  
  #summarise data by Engagement and Score
  data6 <- reactive({
    if(input$SelectEmployee!="ALL"){
    req(input$SelectEmployee)
    HiresByDept <-
      Emp_Transf %>%  
      filter(EmpFullName %in% input$SelectEmployee) %>% 
      group_by(Engagement) %>% summarise(Score =    round(mean(Score),2))   
    }
    else{
      req(input$SelectEmployee)
      HiresByDept <-
        Emp_Transf %>%  
        group_by(Engagement) %>% summarise(Score =    round(mean(Score),2)) 
    }
  })
  
  
  #summarise Sick Leave and Vacation Leave By Year
  data8 <- reactive({
    if(input$SelectEmployee!="ALL"){
    req(input$SelectEmployee)
    HiresByDept <-
      Emp_Transf %>% filter(EmpFullName %in% input$SelectEmployee)  %>% group_by(YearC) %>% summarize_at(c("SickLeave","VacationLeave"),sum,na.rm=TRUE) %>%  pivot_longer(cols = c(SickLeave,VacationLeave),
                                                                                                                                                                                        names_to = "Attribute",
                                                                                                                                                                                        values_to = "Value")
    }
    else{
      req(input$SelectEmployee)
      HiresByDept <-
        Emp_Transf %>%  group_by(YearC) %>% summarize_at(c("SickLeave","VacationLeave"),sum,na.rm=TRUE) %>%  pivot_longer(cols = c(SickLeave,VacationLeave),
                                                                                                                                                                            names_to = "Attribute",
                                                                                                                                                                            values_to = "Value")
    }
    })
  
  # Info-Employee Full Name Filtered
  data7 <- reactive({
    req(input$SelectEmployee)
  })
  
  # info- Date of Birth Filtered
  data9 <- reactive({
  Emp_Transf %>%
     filter(Emp_Transf$EmpFullName %in% input$SelectEmployee) %>% select(DateOfBirthTransf) %>% distinct()
 })
  
  # info -Date of Hire Filtered
  data10 <- reactive({
    Emp_Transf %>%
      filter(Emp_Transf$EmpFullName %in% input$SelectEmployee) %>% select(HireDateTransf) %>% distinct()
  })
  
  # info -Education Filtered
  data11 <- reactive({
    Emp_Transf %>%
      filter(Emp_Transf$EmpFullName %in% input$SelectEmployee) %>% select(Education) %>% distinct()
  })
  
  
  
  # info -Department Filtered
  data12 <- reactive({
    Emp_Transf %>%
      filter(Emp_Transf$EmpFullName %in% input$SelectEmployee) %>% select(Department) %>% distinct()
  })
  
  # info -Position Filtered
  data13 <- reactive({
    Emp_Transf %>%
      filter(Emp_Transf$EmpFullName %in% input$SelectEmployee) %>% select(Position) %>% distinct()
  })
  
  # info -Contract Type Filtered
  data14 <- reactive({
    Emp_Transf %>%
      filter(Emp_Transf$EmpFullName %in% input$SelectEmployee) %>% select(PayTypeID) %>% distinct()
  })

  
  
  ################################   1st Tab Dashboard #####################
  ##############################################################################
  
  #Plot 1 Head count by Department
  output$plot <- renderPlot({
    g <- ggplot(data(), aes(x = HeadCount, y = Department))
    g + geom_bar(stat = "identity",fill="steelblue")+
      geom_text(aes(label=HeadCount), vjust=-0.1,color="black", size=3.5)+theme_minimal()
  
    
  })
  
  #Plot 2 Head count by Position
  output$plot1 <- renderPlot({
    g <- ggplot(data1(), aes(y = HeadCount, x = Position))
    g + geom_bar(stat="identity",fill="red")+coord_flip()+
      geom_text(aes(label=HeadCount), vjust=-0.1, size=3.5)+theme_minimal()
    
  })
  
  #Plot 3 Head count by Age Group
  output$plot2 <- renderPlot({
    g <- ggplot(data2(), aes(y = HeadCount, x = AgeGroup))
    g + geom_bar(stat="identity",fill="green",width = 0.6)+coord_flip()+
      geom_text(aes(label=HeadCount), vjust=-0.1, size=3.5)+theme_minimal()
    
  })
  
  
  
  #Plot 4 pie chart headCount by Paytype ID
  
  
  library(ggpubr)
  
  output$plot3 <- renderPlot({
    
    ggdonutchart(data3(), "HeadCount", label = "PayTypeID",
                 #   lab.pos = "in", lab.font = "white",
                 fill = "PayTypeID", color = "white",
                 palette = c("#00AFBB", "#E7B800", "#FC4E07"))
  })
  
  
  #trend of Hires and termination with time
  
  
  output$plot4 <- renderPlot({
    ggplot(data=data4(), aes(x=Year, y=Value, group=Attribute)) +
      geom_line(aes(color=Attribute))+
      geom_point(aes(color=Attribute))+
      theme_classic()+
      scale_color_manual(values=c('#999999','#E69F00'))
  })
  
  
  # infoboxes

  # Infobox 1 -Total Head Count  
  output$sum_HeadCount<-renderInfoBox({
    infoBox(title="Total HeadCount",
            value=info1(),
            fill = TRUE,
            color = "blue")
  })
  
  # Infobox 2 -Total Hires
  output$sum_Hires<-renderInfoBox({
    infoBox(title="Total Hires",
            value=info2(),
            fill = T,
            color="green",
            icon=icon("user-plus"))
  })
  
  # Infobox 3 -Total Terminations
  
  output$sum_Terminations<-renderInfoBox({
    infoBox(title="Total Terminations",
            value=info3(),
            fill=T,
            color = "orange",
            icon=icon("user-times")
    )
  })
  
  # Infobox 4 -Turn over Ratio
  output$divide_turn_over<-renderInfoBox({
    turnOver=paste(round((sum(Emp_Transf$Termination)/sum(Emp_Transf$HeadCount))*100,1),"%",sep = " ")
    if(turnOver>0){
      infoBox(title="Turn Over Ratio",
              value=paste(round((info3()/info1())*100,2),"%",sep=""),
              fill=T,
              color = "lime",
              icon=icon("arrow-up")
      )
    }else
    {

      infoBox(title="Turn Over Ratio",
              value=paste(round((info3()/info1())*100,2),"%",sep=""),
              fill=T,
              color = "lime",
              icon=icon("arrow-down")
      )
    }

  })
  

  
  
  ################################   2nd Tab Dashboard Code #####################
  ##############################################################################
  

  
  # infoboxes on EmployeeName
  output$personal_info<-renderValueBox({
    valueBox(paste0(data7()),
             "Employee Name",
          #   icon = icon("thumbs-up", lib = "glyphicon"),
             color = "yellow")
  })
  
 # infobox  date of birth filtered
  output$date_of_birth<-renderValueBox({
    valueBox(value=data9(),
             "Date of Birth",
            # icon = icon("thumbs-down", lib = "glyphicon"),
             color = "olive")
  })
  
  # infobox  date of hire filtered
  output$hire_date<-renderValueBox({
    valueBox(value=data10(),
             "Hire Date",
            # icon = icon("thumbs-down", lib = "glyphicon"),
             color = "maroon")
  })
  
  # infobox  education filtered
  output$edu_cation<-renderValueBox({
    valueBox(value=data11(),
             "Education",
             # icon = icon("thumbs-down", lib = "glyphicon"),
             color = "orange")
  })
  

  
  # infobox  Department filtered
  output$dept<-renderValueBox({
    valueBox(value=data12(),
             "Department",
             # icon = icon("thumbs-down", lib = "glyphicon"),
             color = "red")
  })
  
  # infobox  Position filtered
  output$position<-renderValueBox({
    valueBox(value=data13(),
             "Position",
             # icon = icon("thumbs-down", lib = "glyphicon"),
             color = "blue")
  })
  
  # infobox  Contract Type filtered
  output$contract_type<-renderValueBox({
    valueBox(value=data14(),
             "PayTypeID",
             # icon = icon("thumbs-down", lib = "glyphicon"),
             color = "teal")
  })
  
  #infoboxes- Average EMployee Score
  output$Avg_Score<-renderValueBox({
    valueBox(paste0(round(mean(Emp_Transf$Score),2)),
             "Average Employee Score",
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "navy")
  })
  
  
  #Plot 1 - Employee Engagement by Survey 
  output$plot5 <- renderPlot({
    g <- ggplot(data5(), aes(y = Score, x = YearC))
    g + geom_bar(stat="identity",fill="red",width = 0.2)+
      geom_text(aes(label=Score), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+theme_minimal()
    #scale_fill_brewer(palette="Paired")
   #  geom_text(aes(label=Score), vjust=-0.1, size=3.5)+theme_minimal()
    
  })
  
  #Plot 2 - Key Metrics of Employee Engagement
  output$plot6 <- renderPlot({
    g <- ggplot(data6(), aes(y = Score, x = Engagement))
    g + geom_bar(stat="identity",fill="blue",width=0.5)+coord_flip()+
      geom_text(aes(label=Score), vjust=-0.1, size=3.5)+theme_minimal()
    
  })
  
  
  #Plot 3 - Sick Leave and Vacation Leave By Year
  output$plot7 <- renderPlot({
    g <- ggplot(data8(), aes(y = Value, x = YearC,fill=Attribute))
    g + geom_bar(stat="identity", position=position_dodge(),width = 0.4)+
      geom_text(aes(label=Value), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+theme_minimal()
     
    # geom_text(aes(label=Value), vjust=-0.1, size=3.5)
    
  })
  
  
  
  #####################     Plotting table 3rd Tab            ##################
  ##############################################################################
  
  
  # Plotting table 3rd Tab
  
  output$table <- renderTable({
    Emp_Transf
  })
  
  #choosing the file extension  to download
  fileext<- reactive({
    switch (input$type,
            
            "Excel (CSV)"="csv","Text (TSV)"="txt","Doc"="doc"
    )
    
  })
  
  # downloading file
  output$downloadData<-downloadHandler(
    
    filename = function(){
      
      paste(Emp_Transf,fileext(),sep=",")
    },
    
    content=function(file){
      sep<- switch (input$type,
                    
                    "Excel (CSV)"="csv","Text (TSV)"="txt","Doc"="doc"
      )
      
      write.table(Emp_Transf,file,sep = sep,row.names = FALSE)
      
      
    }
    
    
  )
  
}

# Run the application
shinyApp(ui,   server)
