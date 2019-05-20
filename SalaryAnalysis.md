107-2 大數據分析方法 作業一
================
彭鈺茹

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和106年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 3.5.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.5.3

``` r
library(rowr)
```

    ## 
    ## Attaching package: 'rowr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     coalesce, count

``` r
x103Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/103Monthly average salary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X104Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/104Monthly average salary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X105Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/105Monthly average salary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X106Monthly_average_salary <- 
  read_csv("C:/Users/yuru/Desktop/A17000000J-020066-Qod/106Monthly average salary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
x103Monthly_average_salary$"大職業別"<-
  gsub("、","_",x103Monthly_average_salary$"大職業別")
X104Monthly_average_salary$"大職業別"<-
  gsub("、","_",X104Monthly_average_salary$"大職業別")
X105Monthly_average_salary$"大職業別"<-
  gsub("、","_",X105Monthly_average_salary$"大職業別")

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

New_Education_Salary<-
  full_join(x103Monthly_average_salary,X106Monthly_average_salary,by="大職業別")


College_Salary<-New_Education_Salary[,c(2,11,24)]
College_Salary$diff<-
  College_Salary$"Y2017_大學-薪資"/College_Salary$"Y2014_大學-薪資"


knitr::kable(head(College_Salary))
```

| 大職業別                                      | Y2014\_大學-薪資 | Y2017\_大學-薪資 |      diff|
|:----------------------------------------------|:----------------:|:----------------:|---------:|
| 工業及服務業部門                              |       27193      |       28446      |  1.046078|
| 工業及服務業部門-專業人員                     |       30449      |       32108      |  1.054485|
| 工業及服務業部門-技術員及助理專業人員         |       27383      |       28647      |  1.046160|
| 工業及服務業部門-事務支援人員                 |       25664      |       26781      |  1.043524|
| 工業及服務業部門-服務及銷售工作人員           |       25745      |       26644      |  1.034919|
| 工業及服務業部門-技藝\_機械設備操作及組裝人員 |       25704      |       26842      |  1.044273|

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
College_Salary_Filter<-filter(College_Salary, College_Salary[,3]>College_Salary[,2]) 
College_Salary_order<-College_Salary_Filter[order(-College_Salary_Filter$diff),] 
knitr::kable(head(College_Salary_order,10))
```

| 大職業別                                  | Y2014\_大學-薪資 | Y2017\_大學-薪資 |      diff|
|:------------------------------------------|:----------------:|:----------------:|---------:|
| 其他服務業-技術員及助理專業人員           |       24688      |       27929      |  1.131278|
| 住宿及餐飲業-服務及銷售工作人員           |       22564      |       25486      |  1.129498|
| 用水供應及污染整治業-技術員及助理專業人員 |       27944      |       31560      |  1.129402|
| 專業\_科學及技術服務業-專業人員           |       29977      |       33384      |  1.113654|
| 其他服務業-技藝\_機械設備操作及組裝人員   |       24222      |       26880      |  1.109735|
| 營造業-服務及銷售工作人員                 |       27164      |       30125      |  1.109005|
| 其他服務業-專業人員                       |       29000      |       32000      |  1.103448|
| 資訊及通訊傳播業-專業人員                 |       28839      |       31817      |  1.103263|
| 不動產業-專業人員                         |       30637      |       33632      |  1.097758|
| 教育服務業-事務支援人員                   |       22334      |       24471      |  1.095684|

### 提高超過5%的的職業有哪些?

``` r
Bigger_Than_5P<-filter(College_Salary_order,College_Salary_order$diff>1.05)
knitr::kable(Bigger_Than_5P)
```

| 大職業別                                            | Y2014\_大學-薪資 | Y2017\_大學-薪資 |      diff|
|:----------------------------------------------------|:----------------:|:----------------:|---------:|
| 其他服務業-技術員及助理專業人員                     |       24688      |       27929      |  1.131278|
| 住宿及餐飲業-服務及銷售工作人員                     |       22564      |       25486      |  1.129498|
| 用水供應及污染整治業-技術員及助理專業人員           |       27944      |       31560      |  1.129402|
| 專業\_科學及技術服務業-專業人員                     |       29977      |       33384      |  1.113654|
| 其他服務業-技藝\_機械設備操作及組裝人員             |       24222      |       26880      |  1.109735|
| 營造業-服務及銷售工作人員                           |       27164      |       30125      |  1.109005|
| 其他服務業-專業人員                                 |       29000      |       32000      |  1.103448|
| 資訊及通訊傳播業-專業人員                           |       28839      |       31817      |  1.103263|
| 不動產業-專業人員                                   |       30637      |       33632      |  1.097758|
| 教育服務業-事務支援人員                             |       22334      |       24471      |  1.095684|
| 住宿及餐飲業-技術員及助理專業人員                   |       25633      |       28009      |  1.092693|
| 專業\_科學及技術服務業-技藝\_機械設備操作及組裝人員 |       26211      |       28595      |  1.090954|
| 運輸及倉儲業-技藝\_機械設備操作及組裝人員           |       28087      |       30618      |  1.090113|
| 其他服務業-事務支援人員                             |       23863      |       26007      |  1.089846|
| 教育服務業-服務及銷售工作人員                       |       24491      |       26668      |  1.088890|
| 用水供應及污染整治業                                |       27456      |       29834      |  1.086611|
| 用水供應及污染整治業-專業人員                       |       31444      |       34107      |  1.084690|
| 資訊及通訊傳播業                                    |       27055      |       29198      |  1.079209|
| 支援服務業-服務及銷售工作人員                       |       24166      |       26001      |  1.075933|
| 藝術\_娛樂及休閒服務業-技藝\_機械設備操作及組裝人員 |       24895      |       26768      |  1.075236|
| 資訊及通訊傳播業-事務支援人員                       |       25276      |       27156      |  1.074379|
| 教育服務業                                          |       24027      |       25784      |  1.073126|
| 營造業-專業人員                                     |       30580      |       32785      |  1.072106|
| 專業\_科學及技術服務業                              |       27663      |       29648      |  1.071757|
| 支援服務業-技藝\_機械設備操作及組裝人員             |       25365      |       27169      |  1.071122|
| 住宿及餐飲業                                        |       24646      |       26398      |  1.071087|
| 住宿及餐飲業-技藝\_機械設備操作及組裝人員           |       24823      |       26585      |  1.070983|
| 電力及燃氣供應業-服務及銷售工作人員                 |       26013      |       27837      |  1.070119|
| 運輸及倉儲業-技術員及助理專業人員                   |       28974      |       30993      |  1.069683|
| 運輸及倉儲業-事務支援人員                           |       25886      |       27685      |  1.069497|
| 醫療保健服務業-技術員及助理專業人員                 |       28465      |       30392      |  1.067697|
| 專業\_科學及技術服務業-技術員及助理專業人員         |       27195      |       29016      |  1.066961|
| 支援服務業-技術員及助理專業人員                     |       26933      |       28696      |  1.065459|
| 用水供應及污染整治業-服務及銷售工作人員             |       28736      |       30593      |  1.064623|
| 礦業及土石採取業-技藝\_機械設備操作及組裝人員       |       26647      |       28367      |  1.064548|
| 用水供應及污染整治業-技藝\_機械設備操作及組裝人員   |       26087      |       27758      |  1.064055|
| 服務業部門-技藝\_機械設備操作及組裝人員             |       26722      |       28376      |  1.061897|
| 教育服務業-技術員及助理專業人員                     |       25675      |       27250      |  1.061344|
| 服務業部門-專業人員                                 |       30806      |       32632      |  1.059274|
| 運輸及倉儲業                                        |       28143      |       29808      |  1.059162|
| 資訊及通訊傳播業-技術員及助理專業人員               |       27288      |       28902      |  1.059147|
| 醫療保健服務業-技藝\_機械設備操作及組裝人員         |       27409      |       29028      |  1.059068|
| 用水供應及污染整治業-事務支援人員                   |       25430      |       26924      |  1.058750|
| 金融及保險業-事務支援人員                           |       29070      |       30771      |  1.058514|
| 服務業部門-技術員及助理專業人員                     |       27882      |       29504      |  1.058174|
| 藝術\_娛樂及休閒服務業-事務支援人員                 |       23602      |       24970      |  1.057961|
| 藝術\_娛樂及休閒服務業                              |       25204      |       26614      |  1.055943|
| 營造業-事務支援人員                                 |       24892      |       26256      |  1.054797|
| 工業及服務業部門-專業人員                           |       30449      |       32108      |  1.054485|
| 服務業部門-事務支援人員                             |       25554      |       26930      |  1.053847|
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員       |       27253      |       28717      |  1.053719|
| 教育服務業-專業人員                                 |       25722      |       27101      |  1.053612|
| 專業\_科學及技術服務業-服務及銷售工作人員           |       26245      |       27649      |  1.053496|
| 服務業部門                                          |       27258      |       28715      |  1.053452|
| 其他服務業                                          |       24232      |       25517      |  1.053029|
| 製造業-專業人員                                     |       30035      |       31612      |  1.052505|
| 工業部門-專業人員                                   |       30215      |       31775      |  1.051630|
| 資訊及通訊傳播業-服務及銷售工作人員                 |       25995      |       27296      |  1.050048|

### 主要的職業種別是哪些種類呢?

``` r
knitr::kable(table(sapply(strsplit(Bigger_Than_5P$"大職業別","-"), '[', 1)))
```

| Var1                   |  Freq|
|:-----------------------|-----:|
| 工業及服務業部門       |     1|
| 工業部門               |     1|
| 不動產業               |     1|
| 支援服務業             |     3|
| 用水供應及污染整治業   |     6|
| 住宿及餐飲業           |     4|
| 其他服務業             |     5|
| 服務業部門             |     5|
| 金融及保險業           |     1|
| 專業\_科學及技術服務業 |     5|
| 教育服務業             |     5|
| 資訊及通訊傳播業       |     5|
| 運輸及倉儲業           |     4|
| 電力及燃氣供應業       |     2|
| 製造業                 |     1|
| 營造業                 |     3|
| 醫療保健服務業         |     2|
| 藝術\_娛樂及休閒服務業 |     3|
| 礦業及土石採取業       |     1|

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
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
```

| Y103                                                | Y104                                              | Y105                                          | Y106                                          |
|:----------------------------------------------------|:--------------------------------------------------|:----------------------------------------------|:----------------------------------------------|
| 礦業及土石採取業-技藝\_機械設備操作及組裝人員       | 電力及燃氣供應業-技藝\_機械設備操作及組裝人員     | 不動產業-技藝\_機械設備操作及組裝人員         | 電力及燃氣供應業-技藝\_機械設備操作及組裝人員 |
| 教育服務業-技藝\_機械設備操作及組裝人員             | 教育服務業-服務及銷售工作人員                     | 醫療保健服務業-專業人員                       | 營造業-服務及銷售工作人員                     |
| 其他服務業-技術員及助理專業人員                     | 礦業及土石採取業-技術員及助理專業人員             | 用水供應及污染整治業-事務支援人員             | 其他服務業-事務支援人員                       |
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員       | 礦業及土石採取業-技藝\_機械設備操作及組裝人員     | 營造業-事務支援人員                           | 電力及燃氣供應業-技術員及助理專業人員         |
| 礦業及土石採取業-服務及銷售工作人員                 | 礦業及土石採取業                                  | 不動產業-事務支援人員                         | 其他服務業                                    |
| 營造業                                              | 其他服務業-事務支援人員                           | 營造業                                        | 住宿及餐飲業-技藝\_機械設備操作及組裝人員     |
| 教育服務業-事務支援人員                             | 營造業-技藝\_機械設備操作及組裝人員               | 營造業-專業人員                               | 營造業                                        |
| 教育服務業                                          | 用水供應及污染整治業-技藝\_機械設備操作及組裝人員 | 資訊及通訊傳播業-技藝\_機械設備操作及組裝人員 | 教育服務業-專業人員                           |
| 藝術\_娛樂及休閒服務業-技藝\_機械設備操作及組裝人員 | 營造業                                            | 不動產業-服務及銷售工作人員                   | 運輸及倉儲業-事務支援人員                     |
| 其他服務業                                          | 教育服務業                                        | 其他服務業                                    | 其他服務業-技術員及助理專業人員               |

### 哪些行業女生薪資比男生薪資多?

``` r
wCollege_Salary_103order<-College_Salary103[order(-College_Salary103$"Y2014_大學-女/男"),]
colnames(wCollege_Salary_103order)[1] <- "103"
womanbtman103<-filter(wCollege_Salary_103order,wCollege_Salary_103order$'Y2014_大學-女/男'>=100)

wCollege_Salary_104order<-College_Salary104[order(-College_Salary104$"大學-女/男"),]
colnames(wCollege_Salary_104order)[1] <- "104"
womanbtman104<-filter(wCollege_Salary_104order,wCollege_Salary_104order$'大學-女/男'>=100)

wCollege_Salary_105order<-College_Salary105[order(-College_Salary105$"大學-女/男"),]
colnames(wCollege_Salary_105order)[1] <- "105"
womanbtman105<-filter(wCollege_Salary_105order,wCollege_Salary_105order$'大學-女/男'>=100)

wCollege_Salary_106order<-College_Salary106[order(-College_Salary106$"Y2017_大學-女/男"),]
colnames(wCollege_Salary_106order)[1] <- "106"
womanbtman106<-filter(wCollege_Salary_106order,wCollege_Salary_106order$'Y2017_大學-女/男'>=100)

womanbtman103106<-
  cbind.fill(womanbtman103[1],womanbtman104[1],womanbtman105[1],womanbtman106[1],fill=NA)

knitr::kable(head(womanbtman103106,10))
```

| X103                                    | X104                                                | X105                                        | X106                                                |
|:----------------------------------------|:----------------------------------------------------|:--------------------------------------------|:----------------------------------------------------|
| 礦業及土石採取業-技術員及助理專業人員   | 專業\_科學及技術服務業-技藝\_機械設備操作及組裝人員 | 金融及保險業-專業人員                       | 資訊及通訊傳播業-服務及銷售工作人員                 |
| 用水供應及污染整治業-服務及銷售工作人員 | 用水供應及污染整治業-服務及銷售工作人員             | 礦業及土石採取業-服務及銷售工作人員         | 礦業及土石採取業-技術員及助理專業人員               |
| 營造業-服務及銷售工作人員               | 不動產業-技藝\_機械設備操作及組裝人員               | 用水供應及污染整治業-服務及銷售工作人員     | 用水供應及污染整治業-服務及銷售工作人員             |
| NA                                      | 醫療保健服務業-服務及銷售工作人員                   | 教育服務業-服務及銷售工作人員               | 資訊及通訊傳播業-技藝\_機械設備操作及組裝人員       |
| NA                                      | 其他服務業-專業人員                                 | 醫療保健服務業-技藝\_機械設備操作及組裝人員 | 金融及保險業-技藝\_機械設備操作及組裝人員           |
| NA                                      | NA                                                  | 藝術\_娛樂及休閒服務業-技術員及助理專業人員 | 不動產業-專業人員                                   |
| NA                                      | NA                                                  | NA                                          | 不動產業-服務及銷售工作人員                         |
| NA                                      | NA                                                  | NA                                          | 不動產業-技藝\_機械設備操作及組裝人員               |
| NA                                      | NA                                                  | NA                                          | 專業\_科學及技術服務業-技藝\_機械設備操作及組裝人員 |

研究所薪資差異
--------------

以106年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#這是R Code Chunk
```

我有興趣的職業別薪資狀況分析
----------------------------

### 有興趣的職業別篩選，呈現薪資

``` r
#這是R Code Chunk
```

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
#這是R Code Chunk
```
