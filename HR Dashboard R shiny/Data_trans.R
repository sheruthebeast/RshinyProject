# HR Dashboard Project
library(tidyverse)
library(dplyr)
library(tidyr)
library(sqldf)
library(RH2)
#Emp_Transf=transform(Emp_Transf,HireDate=as.Date(HireDate))
#EEngagement_Tranf<-EEngagement %>% 
pivot_longer(cols = 'Involved In Work (1-5)':'Satisfaction At Work(1-5)',
             names_to = "Engagement",
             values_to = "Score")
Emp_Transf=sqldf('Select distinct E.EmpFullName,E.DateOfBirth,E.HireDate,E.Gender
              ,E.PayTypeID,E.TermDate,D.Department,
              AG.AgeGroup,EDU.Education,P.Position,
			  EE.Engagement, 
              EE.SCore,
              case when A.AbsenceType="Uncertified Sick Leave" then
             (A.AbsenceEndDate-A.AbsenceStartDate)/(3600*24) else 0 end SickLeave,
              case when A.AbsenceType="Holiday" then
             (A.AbsenceEndDate-A.AbsenceStartDate)/(3600*24) else 0 end VacationLeave,
              case when E.HireDate is not null and  E.TermDate is NULL then 1 else 0 end HeadCount,
              case when E.HireDate is not null or  E.TermDate is not NULL then 1 else 0 end Hires,
              case when E.TermDate is not NULL then 1 else 0 end Termination
              from
              Emp E  
              left join
              Department D on E.DeptID=D.DeptID
              left join 
              AgeGroup AG on E.AgeGroup=AG.AgeGroupID
              left join 
              Education EDU on E.EduID=EDU.EduID
              left join 
              Position P on E.PositionID=P.PositionID
              left join 
              EEngagement_Tranf EE on EE.EmpID=E.EmpID
              left join 
              Absence A on A.EmpID=E.EmpID
              ')
Emp_Transf$YearC <- as.character(strftime(Emp_Transf$HireDate,format="%Y"))
Emp_Transf$DateOfBirthTransf <- as.character(strftime(Emp_Transf$DateOfBirth,format="%Y-%m-%d"))
Emp_Transf$HireDateTransf<- as.character(strftime(Emp_Transf$HireDate,format="%Y-%m-%d"))
Emp_Transf$Year <- as.numeric(as.character(strftime(Emp_Transf$HireDate,format="%Y")))
Emp_Transf %>% group_by(Year) %>% summarize_at(c("Hires","Termination"),sum,na.rm=TRUE) %>%  pivot_longer(cols = c(Termination,Hires),
                                                                                                          names_to = "Attribute",
                                                                                                          values_to = "Value")

Emp_Transf %>% group_by(YearC) %>% summarize_at(c("SickLeave","VacationLeave"),sum,na.rm=TRUE) %>%  pivot_longer(cols = c(SickLeave,VacationLeave),
                                                                                                          names_to = "Attribute",
                                                                                                          values_to = "Value")


