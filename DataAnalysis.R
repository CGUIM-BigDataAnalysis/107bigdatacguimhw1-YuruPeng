library(jsonlite)
library(dplyr)
x103Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/103Monthly average salary.csv")
X104Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/104Monthly average salary.csv")
X105Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/105Monthly average salary.csv")
X106Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/106Monthly average salary.csv")


x103Monthly_average_salary$大職業別<-
  gsub("、","_",x103Monthly_average_salary$大職業別)
X104Monthly_average_salary$大職業別<-
  gsub("、","_",X104Monthly_average_salary$大職業別)
X105Monthly_average_salary$大職業別<-
  gsub("、","_",X105Monthly_average_salary$大職業別)

x103Monthly_average_salary[x103Monthly_average_salary=="—"]<-NA
x103Monthly_average_salary[x103Monthly_average_salary=="…"]<-NA
X104Monthly_average_salary[X104Monthly_average_salary=="—"]<-NA
X104Monthly_average_salary[X104Monthly_average_salary=="…"]<-NA
X105Monthly_average_salary[X105Monthly_average_salary=="—"]<-NA
X105Monthly_average_salary[X105Monthly_average_salary=="…"]<-NA
X106Monthly_average_salary[X106Monthly_average_salary=="—"]<-NA
X106Monthly_average_salary[X106Monthly_average_salary=="…"]<-NA

x103Monthly_average_salary[5:14]<-as.numeric(unlist(x103Monthly_average_salary[5:14]))
X104Monthly_average_salary[4:14]<-as.numeric(unlist(X104Monthly_average_salary[4:14]))
X105Monthly_average_salary[4:14]<-as.numeric(unlist(X105Monthly_average_salary[4:14]))
X106Monthly_average_salary[4:14]<-as.numeric(unlist(X106Monthly_average_salary[4:14]))

colnames(x103Monthly_average_salary)[3:14] <- paste("Y2014", colnames(x103Monthly_average_salary[,c(3:14)]),sep = "_")
colnames(X106Monthly_average_salary)[3:14] <- paste("Y2017", colnames(X106Monthly_average_salary[,c(3:14)]),sep = "_")

New_Education_Salary<-full_join(x103Monthly_average_salary,X106Monthly_average_salary,by="大職業別")


College_Salary<-New_Education_Salary[,c(2,11,24)]
College_Salary$diff<-College_Salary$Y2017_大學-薪資/College_Salary$Y2014_大學-薪資


knitr::kable(head(College_Salary))


College_Salary_Filter<-filter(College_Salary, College_Salary[,3]>College_Salary[,2]) #106>103


College_Salary_order<-College_Salary_Filter[order(-College_Salary_Filter$diff),] #排序由大到小

head(College_Salary_order,10) #Show前10比

filter(College_Salary_order,College_Salary_order$diff>1.05)

knitr::kable(table(sapply(strsplit(Bigger_Than_5P$大職業別,"-"), '[', 1)))


College_Salary103<-x103Monthly_average_salary[,c(2,12)]
College_Salary104<-X104Monthly_average_salary[,c(2,12)]
College_Salary105<-X105Monthly_average_salary[,c(2,12)]
College_Salary106<-X106Monthly_average_salary[,c(2,12)]


College_Salary_103order<-College_Salary103[order(College_Salary103$"Y2014_大學-女/男"),]
colnames(College_Salary_103order)[1] <- "103"
manbtwoman103<-College_Salary_103order$"103"

College_Salary_104order<-College_Salary104[order(College_Salary104$"大學-女/男"),]
colnames(College_Salary_104order)[1] <- "104"
manbtwoman104<-College_Salary_104order$"104"

College_Salary_105order<-College_Salary105[order(College_Salary105$"大學-女/男"),]
colnames(College_Salary_105order)[1] <- "105"
manbtwoman105<-College_Salary_105order$"105"

College_Salary_106order<-College_Salary106[order(College_Salary106$"Y2017_大學-女/男"),]
colnames(College_Salary_106order)[1] <- "106"
manbtwoman106<-College_Salary_106order$"106"

manbtwoman103106<-
  data.frame("Y103"=manbtwoman103,"Y104"=manbtwoman104,"Y105"=manbtwoman105,"Y106"=manbtwoman106)

knitr::kable(head(manbtwoman103106,10))

wCollege_Salary_103order<-College_Salary103[order(-College_Salary103$"Y2014_大學-女/男"),]
colnames(wCollege_Salary_103order)[1] <- "103"
womanbtman103<-filter(wCollege_Salary_103order,wCollege_Salary_103order$'大學-女/男'>=100)

wCollege_Salary_104order<-College_Salary104[order(-College_Salary104$"大學-女/男"),]
colnames(wCollege_Salary_104order)[1] <- "104"
womanbtman104<-filter(wCollege_Salary_104order,wCollege_Salary_104order$'大學-女/男'>=100)

wCollege_Salary_105order<-College_Salary105[order(-College_Salary105$"大學-女/男"),]
colnames(wCollege_Salary_105order)[1] <- "105"
womanbtman105<-filter(wCollege_Salary_105order,wCollege_Salary_105order$'大學-女/男'>=100)

wCollege_Salary_106order<-College_Salary106[order(-College_Salary106$"Y2017_大學-女/男"),]
colnames(wCollege_Salary_106order)[1] <- "106"
womanbtman106<-filter(wCollege_Salary_106order,wCollege_Salary_106order$'大學-女/男'>=100)

womanbtman103106<-
  cbind.fill(womanbtman103[1],womanbtman104[1],womanbtman105[1],womanbtman106[1],fill=NA)


