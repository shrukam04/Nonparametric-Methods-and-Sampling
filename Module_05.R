#Name: Shruti B. Kamble
#Course: Intermediate Analytics
#Title:Nonparametric Methods and Sampling
#Date: 05/16/2021

installAllPackages <- function(Pkgs){
  new.Pkgs <- Pkgs[!(Pkgs %in% installed.packages()[, "Package"])]
  if (length(new.Pkgs)) 
    install.packages(new.Pkgs, dependencies = TRUE)
  null<- sapply(Pkgs, require, character.only = TRUE)
  return(TRUE)
}
packages <- c(
  "dplyr", "psych", "plyr", "reshape2", "rlang",  "patchwork","scales", "grid", "UsingR", "tidyr", "tidyverse",
  "readxl","ggplot2","ggpubr", "RColorBrewer","rmarkdown","formatR", "plotly","gmodels","knitr","kableExtra",
  "base64","base64enc","enc","EDA","hrbrthemes","ggcorrplot","corrplot", "Publish", "magrittr","PerformanceAnalytics",
  "forecast","xfun","htmltools","funModeling","corrgram","dummies","lsmeans","imputeTS","leaps","lmSubsets","sjPlot",
  "gtsummary","ROCR","InformationValue","caTools","coefplot","mltools","caret","BSDA"
)

if(installAllPackages(packages)) 
  cat("Done, Installed and imported All the required packages and libraries for this assignment") else 
    cat("Some Error in installation")
  
#State the hypotheses and identify the claim.
#Find the critical value(s)
#Compute the test value.
#Make the decision.
#Summarize the results. 

#Section 13-2

#Game Attendance
#An athletic director suggests the median number for the paid attendance at 20 local football games is 3000. 
#The data for a random sample are shown. At α = 0.05, is there enough evidence to reject the claim? 
#If you were printing the programs for the games, would you use this figure as a guide?
  
#6210	3150	2700	3012	4875
#3540	6127	2581	2642	2573
#2792	2800	2500	3700	6030
#5437	2758	3490	2851	2720

Game_Attendance <- c(6210,3150,2700,3012,4875,3540,6127,2581,2642,2573,
                2792,2800,2500,3700,6030,5437,2758,3490,2851,2720)

kbl(Game_Attendance) %>%
  kable_material_dark(c("striped", "hover"))%>%
  kable_classic_2(full_width = F,
                  position = "center") %>%
  row_spec(0, bold = T, color = "Black", background = "white") %>%
  scroll_box(width = "50%", 
             height = "500px")

#H0: The median of football games for paid attendance is equal to 3000
#H1: The median is not 3000 as stated in null hypotheses

#Critical value
n <- 20
k <- round(((n-1)/2)-0.95*(sqrt(n)))
cat("Critical value = ", k)

#test value
test_value <- SIGN.test(Game_Attendance, md = 3000, conf.level=0.95)
cat("Test_value = ", test_value$statistic)


---------------------------------------------------------------------------------------------------------

#Lottery Ticket Sales
#A lottery outlet owner hypothesizes that she sells 200 lottery tickets a day. 
#She randomly sampled 40 days and found that on 15 days she sold fewer than 200 tickets. 
#At α = 0.05, is there sufficient evidence to conclude that the median is below 200 tickets?

#H0: The median of lottery tickets is below 200 ticket
#H1: The median of lottery ticket is not same as stated in null hypotheses

#Critical Value
alpha = 0.05
SigL = 1- alpha
n = 40        #number of days selected
k = round(((n-1)/2)-(SigL/2)*(sqrt(n))) 
cat("Critical value = ", k)

#Test value
Test_value <- binom.test(x=15,n=40, alternative="less")
cat("Test_value = ", Test_value$statistic)

----------------------------------------------------------------------------------------------------------
  
#Section 13-3
#Lengths of prison sentences
#A random sample of men and women in prison was asked to give the length of sentence each received for a certain type of crime. 
#At α = 0.05, test the claim that there is no difference in the sentence received by each gender. The data (in months) are shown here.
 #Males	  8	12	6	14	22	27	3	2	2	
                           #  2	4	6	
#Females	7	5	  2	3	  21	26	3	9	4	
                            # 0			

#Males  	19	15	13		
#Females	17	23	12	11	16

Prison_Sentence <- data.frame(Sentence = c(8,12,6,14,22,27,32,24,26,19,15,13,
                                             7,5,2,3,21,26,30,9,4,17,23,12,11,16),
                           Gender = c(rep("Male", 12), rep("Female", 14)))
Prison_Sentence

#H0:There is a difference in the sentence received by each gender
#H1:There is no difference in the sentence received by each gender

#critical value
n <- table(Prison_Sentence$Gender)
Critical_value <- qwilcox(m=n[1],n=n[2],p=0.05)
cat("Critical value = ", Critical_value)

#test value
a <- stats::wilcox.test(Prison_Sentence$Sentence ~ Prison_Sentence$Gender, , alternative = "less", conf.level = 0.95)
cat("Test value = ", a$statistic)

-------------------------------------------------------------------------------------------------------------------

#Winning Ball Games
#For the years 1970–1993 the National League (NL) and the American League (AL) (major league baseball) were each divided into two divisions: East and West. 
  #Below are random samples of the number of games won by each league’s Eastern Division. 
  #At α = 0.05, is there sufficient evidence to conclude a difference in the number of wins?
  
#NL	89	96	88	101	90	91	92	96	108	100	95	
#AL	108	86	91	97	100	102	95	104	95	89	88	101

Baseball <- data.frame(League = c(rep("NL", 11), rep("AL", 12)),
                       Games  = c(89,96,88,101,90,91,92,96,108,100,95,
                                  108,86,91,97,100,102,95,104,95,89,88,101)
                       )

#H0: There is no difference in the number of wins
#H1: There is a difference in the number of wins

#Critical Value
n <- table(Baseball$League)
Critical_Value <- qwilcox(m=n[1],n=n[2],p=0.05)
cat("critical value = ", Critical_Value)

#test value
x <- stats::wilcox.test(Games ~ League, alternative="two.sided", conf.level=0.95, data=Baseball)
cat("Computed test value = ", x$statistic)


------------------------------------------------------------------------------------------------------

#Section 13-4
#Use Table K to determine whether the null hypothesis should be rejected.
  
#ws = 13, n = 15, α = 0.01, two- tailed, k= 16
#ws = 32, n = 28, α = 0.025, one-tailed, k= 112 
#ws = 65, n = 20, α = 0.05, one-tailed,  k= 60
#ws = 22, n = 14, α = 0.10, two-tailed,  k=26
  
#H0: There is NO Difference 
#H1: There is a Difference 

#ws = 13, n = 15, α = 0.01, two- tailed, k= 16 | Fail to Reject
#ws = 32, n = 28, α = 0.025, one-tailed, k= 112 | Fail to Reject 	
#ws = 65, n = 20, α = 0.05, one-tailed,  k= 60 | Reject
#ws = 22, n = 14, α = 0.10, two-tailed,  k=26 | Fail to Reject 

-----------------------------------------------------------------------------------------------------

#Section 13-5
#Mathematics Literacy Scores
  
#Through the Organization for Economic Cooperation and Development (OECD), 15-year-olds are tested in member countries in mathematics, reading, and science literacy. 
  #Listed are randomly selected total mathematics literacy scores (i.e., both genders) for selected countries in different parts of the world. Test, using the Kruskal-Wallis test, to see if there is a difference in means at α = 0.05.

#Western Hemisphere	Europe	Eastern Asia
#527	520	523
#406	510	547
#474	513	547
#381	548	391
#411	496	549

#H0: There is no difference in means of mathematical literacy in all the three locations. 
#H1: There is difference in means of mathematical literacy in all the three locations. 


Literacy_Score <- data.frame(Scores = c(527,406,474,381,411,520,510,513,548,496,523,547,547,391,549),
                          Regions = c(rep("Western_Hemisphere", 5), rep("Europe", 5),rep("Eastern Asia", 5))) 

#Critical Value
ls_Critical_value <- qchisq(p = 0.05, df = 14)
cat("Computed critical value = ", ls_Critical_value)

#test value
ls_Test_Value <- kruskal.test(Scores ~ Regions , data = Literacy_Score ) 
cat("Computed test value = ", ls_Test_Value$statistic)


------------------------------------------------------------------------------------------------------------
#Section 13-6
#Subway and commuter Rail Passengers
#Six cities are randomly selected, and the number of daily passenger trips (in thousands) for subways and commuter rail service is obtained. 
  #At α = 0.05, is there a relationship between the variables? Suggest one reason why the transportation authority might use the results of this study.
  
#City	1	2	3	4	5	6
#Subway	845	494	425	313	108	41
#Rail	39	291	142	103	33	38
  
Passengers_data <- data.frame( City = c(1,2,3,4,5,6),
                          Subway = c(845,494,425,313,108,41),
                          Rail = c(39,291,142,103,33,38))

#1. Spearman rank correlation coefficient
correlation_coef <- cor(Passengers_data$Subway,Passengers_data$Rail,  method = "spearman")
cat("Computed Correlation value = ", correlation_coef)

#2.State the hypotheses.
#H0: There is a existing relationship between the two variables 
#H1: There is no relationship between the two variables 

#3. Find the critical value. Use α = 0.05.
n <- length(Passengers_data)
passengers_data_Critical <- sqrt((n-2)/(1-(correlation_coef*correlation_coef)))
cat("critical value = ", passengers_data_Critical)


----------------------------------------------------------------------------------------------------------
#Section 14-3
#Prizes is caramel corn boxes  
#A caramel corn company gives four different prizes, one in each box. They are placed in the boxes at random. 
  #Find the average number of boxes a person needs to buy to get all four prizes. (40)
  
  
  #Possible outcomes
sample <- 40
prize <- 4
prize
prob_mas <- 1/4
lambda <- (1/prize)
lambda 
sample_mean = rep(NA, sample)

 #determining probability
pop_mean       <- 1./lambda
Avg_mean  <- mean(sample_mean)
  cat("Population mean = ", pop_mean, " Averaged sample mean =  ", Avg_mean, "\n")
  
  n = 40 
  ncol = 1
  Simulationdata <-matrix(list(), nrow=n, ncol=1)
  Simulationdata

  stdev   <- sqrt(((1/lambda)^2)/sample) 
  stdev
  
  numS = 40 
  newdata1 <- matrix(list() , nrow = numS, ncol =1)
  numtowin <- vector()
  for(j in 1:numS){
    data <- vector()
    a=0
    b=0
    c=0
    d=0
    while(a*b*c*d==0){
      dat<- sample(1:4,1)
      if(dat==1){
        a=1
        data <- append(data,"a")
      } else if (dat==2){
        b=1
        data <- append(data,"b")
      } else if (dat==3){
        c=1
        data <- append(data,"c")
      } else {
        d=1
        data <- append(data,"d")
      }
    }
    Simulationdata[[j,1]] <- data
    numtowin <- append(numtowin,length(data))
  }
  sprintf("Total number of boxes bought for %d wins is %d ",numS, sum(numtowin))
  
  sprintf("The average number of boxes a person must buy to get all four prizes is %d "
          , ceiling(sum(numtowin)/numS))
  
  datatable<-as.table(Simulationdata)
  datatable

-----------------------------------------------------------------------------------------------------------------
  
#Lottery Winner
#To win a certain lotto, a person must spell the word big. 
  #Sixty percent of the tickets contain the letter b, 30% contain the letter i, and 10% contain the letter g. 
  #Find the average number of tickets a person must buy to win the prize. (30)
  
samples <- 30
data_points <- 3
b = 60/100
b
i = 30/100
i
g = 10/100
g
lambda <- (1/data_points)
cat("Lambda value is = ", lambda)
sample_means = c()


#Determine the probability of each outcome</i>
number =30
Simulationdata <-matrix(list(), nrow=number, ncol=1)
Selected <- vector()
set.seed(123)
for(j in 1:number){
  data <- vector()
  b=0; i=0; g=0
  while(b*i*g==0){
    dat<- sample(0:9,1)
    if(dat<=5){
      b=1
      data <- append(data,"b")
    } else if (dat<=8){
      i=1
      data <- append(data,"i")
    } else {
      g=1
      data <- append(data,"g")
    }
  }
  Simulationdata[[j,1]] <- data
  Selected <- append(Selected,length(data))
}
cat("Total number of lottery brought", sum(Selected) ," and Average number of tickets a person must buy" ,sum(Selected)/number )
  
  


  
  



  




