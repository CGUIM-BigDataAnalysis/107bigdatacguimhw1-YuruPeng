library(jsonlite)
library(dplyr)
x103Monthly_average_salary <- read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/103Monthly average salary.csv")
X106Monthly_average_salary <- read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/106Monthly average salary.csv")

x103Monthly_average_salary$大職業別<-gsub("、","_",x103Monthly_average_salary$大職業別)

x103Monthly_average_salary[x103Monthly_average_salary=="—"]<-NA
x103Monthly_average_salary[x103Monthly_average_salary=="…"]<-NA
X106Monthly_average_salary[X106Monthly_average_salary=="—"]<-NA
X106Monthly_average_salary[X106Monthly_average_salary=="…"]<-NA

x103Monthly_average_salary[5:14]<-as.numeric(unlist(x103Monthly_average_salary[5:14]))
X106Monthly_average_salary[4:14]<-as.numeric(unlist(X106Monthly_average_salary[4:14]))

colnames(x103Monthly_average_salary)[3:14] <- paste("2014", colnames(x103Monthly_average_salary[,c(3:14)]),sep = "_")
colnames(X106Monthly_average_salary)[3:14] <- paste("2017", colnames(X106Monthly_average_salary[,c(3:14)]),sep = "_")

New_Education_Salary<-full_join(x103Monthly_average_salary,X106Monthly_average_salary,by="大職業別")


College_Salary<-New_Education_Salary[,c(2,11,24)]
College_Salary$diff<-College_Salary[3]/College_Salary[2]


knitr::kable(head(College_Salary))


