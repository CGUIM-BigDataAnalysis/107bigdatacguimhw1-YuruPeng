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
x103Monthly_average_salary<-
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
X106Monthly_average_salary<-
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
x103Monthly_average_salary$大職業別<-
  gsub("、","_",x103Monthly_average_salary$大職業別)

colnames(x103Monthly_average_salary)[3:14]<-
  paste("2014",colnames(x103Monthly_average_salary[,c(3:14)]),sep = "_")
colnames(X106Monthly_average_salary)[3:14]<-
  paste("2017",colnames(X106Monthly_average_salary[,c(3:14)]),sep = "_")

New_Education_Salary<-
  full_join(x103Monthly_average_salary,X106Monthly_average_salary,by="大職業別")
New_Education_Salary[New_Education_Salary=="—"]<-NA
New_Education_Salary[New_Education_Salary=="…"]<-NA

College_Salary<-New_Education_Salary[,c(2,11,24)]
College_Salary$diff<-
  as.numeric(unlist(College_Salary[,3]))/as.numeric(unlist(College_Salary[,2]))
knitr::kable(head(College_Salary))
```

| 大職業別                                      | 2014\_大學-薪資 | 2017\_大學-薪資 |      diff|
|:----------------------------------------------|:----------------|:----------------|---------:|
| 工業及服務業部門                              | 27193           | 28446           |  1.046078|
| 工業及服務業部門-專業人員                     | 30449           | 32108           |  1.054485|
| 工業及服務業部門-技術員及助理專業人員         | 27383           | 28647           |  1.046160|
| 工業及服務業部門-事務支援人員                 | 25664           | 26781           |  1.043524|
| 工業及服務業部門-服務及銷售工作人員           | 25745           | 26644           |  1.034919|
| 工業及服務業部門-技藝\_機械設備操作及組裝人員 | 25704           | 26842           |  1.044273|

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
#這是R Code Chunk
```

### 提高超過5%的的職業有哪些?

``` r
#這是R Code Chunk
```

### 主要的職業種別是哪些種類呢?

``` r
#這是R Code Chunk
```

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#這是R Code Chunk
```

### 哪些行業女生薪資比男生薪資多?

``` r
#這是R Code Chunk
```

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
