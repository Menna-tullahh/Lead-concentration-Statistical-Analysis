################################################################
#Implemented Functions Section "from scratch"
summarize_data<-function(df){
  for (i in 1:ncol(df)){
    if(class(df[,i])=="numeric" || class(df[,i])=="integer" ){
      s<-cat(sprintf(paste(
        "\ncolumn: %s \nmean= %f \nmedian= %f \nmin= %f",
        " \nmax= %f \nQ1,Q3= %s, %s\n________",sep = "")
        ,colnames(df)[i]
        ,mean(df[,i],na.rm = TRUE)
        ,median(df[,i],na.rm = TRUE)
        ,min(df[,i],na.rm = TRUE)
        ,max(df[,i],na.rm = TRUE)
        ,as.character(quantile(df[,i],na.rm = TRUE,c(0.25,0.75))[1])
        ,as.character(quantile(df[,i],na.rm = TRUE,c(0.25,0.75))[2])
        
      ))
      cat(s)
    }} }

detect_outliers<-function(df){
  for (i in names(df)){
    counter=0
    if(class(df[,i])=="numeric" || class(df[,i])=="integer" ){
       Q1=quantile(df[,i],na.rm = TRUE,c(0.25,0.75))[1]
       Q3=quantile(df[,i],na.rm = TRUE,c(0.25,0.75))[2]
       IQR=Q3-Q1
       for(j in df[,i]){
       if(!is.na(j)){
       if(j>(Q3+(1.5*IQR))||j<(Q1-(1.5*IQR))){
       counter=counter+1}}}
       cat(names(df[i]),"\t Outliers: ",counter,"\n")
       
    }} }

conf_interval_2<-function(df,col,col2,gender,con){
  if(con=="95%"){z=1.95996}
  if(con=="90%"){z=1.64485}
  if(con=="99%"){z=2.57583}
  m<-mean(df[df[,col]==gender,][,col2],na.rm=T)
  # standard error: standard deviation/root(n)
  se<-sd(df[df[,col]==gender,][,col2],na.rm=T)/sqrt(length(df[df[,col]==gender,][,col2]))
  #%95 formula: (mean-(z*se),mean+(z*se))
  low_b<-m-(z*se)
  up_b<-m+(z*se)
  cat("\n",con," Confidence Interval (",gender,"): ",c(low_b,up_b))
  
}

################################################################

#0.	Data reading
#Read in the data-set and analyze the skim through the data variables.
lead_df<-get(load("lead.RData"))

################################################################

#1.	Descriptive statistics

str(lead_df)

#Area, Sex, Lead_type and Exposed columns are such examples for
#Categorical data. However, they are represented as "numeric" datatypes
# So, this kind of data can be mapped into "object/string" datatypes

cat_data<-c("Area", "Sex", "Lead_type","Exposed")

#Creating Factors of these data
#Converting Loop
for (i in cat_data){lead_df[,i]<-factor(lead_df[,i])}

#Assuming "1" for Males and "2" for Females
levels(lead_df$Sex)=c("Male","Female")

#Counting the frequency of the unique values of these columns
#For the categorical variable existing, calculate a frequency table
for (i in cat_data){cat(i);print(table(lead_df[i]));cat("\n")}


# Summarizing data "using summary()".
summary(lead_df)

#Another way "From Scratch"
summarize_data(lead_df)


#For calculating correlation coefficient there are 3 methods:

# "Pearson"/Assumptions: 
#Normality/no outliers/linear relationship/homoscedasticity/continuous

# "Spearman"/Assumptions:
#Monotonic relationship "more robust to outliers"/ordinal

# "Kendall"/Assumptions: 
#similar to Spearman "More robust"

#Scatter-Plots for checking relationships

plot(lead_df$Ld72,lead_df$MAXFWT,xlab="Ld72"
     ,ylab="MAXWT",main="Scatterplot")

plot(lead_df$Ld73,lead_df$MAXFWT,xlab="Ld73"
     ,ylab="MAXWT",main="Scatterplot")

#checking normality
# H0: assumes that the data is Normally distributed "p-val>0.05"
# Ha: assumes that the data is Not Normally distributed "p-val<0.05"

#"Not Normal (the 3 columns)"
shapiro.test(lead_df$Ld72)
shapiro.test(lead_df$Ld73)
shapiro.test(lead_df$MAXFWT)

#Normality assumption failed
#So,"Spearman" method is more appropriate in this case "Kendall too"

#the correlation coefficient (MAXWT and Ld72) 
#"Output: negative correlation/weak cor./weak relationship/less than 0.5"
#"Negative relationship: when x increase, y decrease and vice versa"
cor(lead_df$MAXFWT,lead_df$Ld72,use="complete.obs",method = "spearman")


#the correlation coefficient (MAXWT and Ld73)
#"Output: negative correlation/weak cor./weak relationship/less than 0.5"
cor(lead_df$MAXFWT,lead_df$Ld73,use="complete.obs",method="spearman")

################################################################

# 2. Graphs

# bar chart of a categorical variable for the gender (Sex parameter)
barplot(table(lead_df$Sex), xlab="Gender",ylab="Frequency")

# bar chart graph with mean MAXWT in  males and females 
barplot(tapply(lead_df$MAXFWT,list(lead_df$Sex),mean,na.rm=T),
        xlab="Gender",ylab="Mean MAXWT")

# histograms of a continuous variable: "age" as well as "MAXWT"

#It seems to be NOT normally distributed 
hist(lead_df$Age,xlab="Age",main="Distribution of Age")

#It seems to be NOT normally distributed  
hist(lead_df$MAXFWT,xlab="MAXFWT",main="Distribution of MAXFWT")


# scatter-plot of 2 continuous variables 
# Ld72 and MAXWT, and add the regression lines for each gender
plot(lead_df$Ld72,lead_df$MAXFWT,xlab="Ld72"
     ,ylab="MAXWT",main="Scatterplot",pch=18)

colors<-c("red","blue")
for(i in 1:length(levels(lead_df$Sex))){
  points(lead_df$Ld72[lead_df$Sex==levels(lead_df$Sex)[i]],
         lead_df$MAXFWT[lead_df$Sex==levels(lead_df$Sex)[i]],col=colors[i],pch=18)
  
#Adding the two regression lines for each gender
  abline(lm(lead_df$MAXFWT[lead_df$Sex==levels(lead_df$Sex)[i]]~
              lead_df$Ld72[lead_df$Sex==levels(lead_df$Sex)[i]]
            
  ),col=colors[i])}

legend("topright",pch = 18,
       legend=c("Males","Females")
       ,col = c("red","blue"))

# Make a box-plot of age
# Box-plots are very helpful for the following:
#"Testing homoscedasticity/checking variation/detecting outliers"
boxplot(lead_df$Age,main="Age")

# separate box-plots per Ld72 and per Ld73(as.factors)
boxplot(Age~cut(Ld72,4),data=lead_df,main="Boxplot per Ld72",xlab ="Ld72")
boxplot(Age~cut(Ld73,4),data=lead_df,main="Boxplot per Ld73",xlab ="Ld73")


################################################################

#3.	Outlier detection
#Exploring the data for any existing outliers

#creating a list of Numeric columns' names 
numeric_col=c()
for (i in names(lead_df)){
  if(class(lead_df[,i])=="integer" || class(lead_df[,i])=="numeric")
  numeric_col <- c(numeric_col,i)}

#(What do you think?) "Question"
#Answer:
#Generating a box-plot of these Numeric columns "discarding Id column"
#Output: we can see that there are some outliers in "iqf/Ld72/MAXFWT" columns
boxplot(lead_df[numeric_col[2:7]],main="Boxplot (outliers detection)")


#Implemented Outlier Detector "from scratch/More accurate way"
#for counting outliers in each numeric column in the data-frame
detect_outliers(lead_df)
################################################################

#4. Testing 4.	Testing for normality/ homoscedasticity 

library(car)


#FIRST METHOD BY VISUALIZING THE DATA USING HISTOGRAM AND CHECK THE NORMAILITY
hist(lead_df[lead_df$Sex == "Male",]$MAXFWT, main='Males MAXFWT') 
hist(lead_df[lead_df$Sex == "Female",]$MAXFWT, main='Female MAXFWT') 
hist(lead_df$Age, main='Age') 
hist(lead_df$MAXFWT, main='MAXFWT') 
hist(lead_df$Ld72, main='Ld72') 
hist(lead_df$Ld73, main='Ld73') 


#SECOND METHOD BY USING STATSTICAL TEST USING SHAPIRO TEST.
# H0: assumes that the data is Normally distributed "p-val>0.05"
# Ha: assumes that the data is Not Normally distributed "p-val<0.05"

shapiro.test(lead_df[lead_df$Sex == "Male",]$MAXFWT)#NOT NORMALLY DISTRBUTED
shapiro.test(lead_df[lead_df$Sex == "Female",]$MAXFWT)#NORMALLY DISTRBUTED
shapiro.test(lead_df$Age)#NOT NORMALLY DISTRBUTED

shapiro.test(lead_df$MAXFWT)#NOT NORMALLY DISTRBUTED
shapiro.test(lead_df$Ld72)#NOT NORMALLY DISTRBUTED
shapiro.test(lead_df$Ld73)#NOT NORMALLY DISTRBUTED





#bartlett test works on only the normally distributed data
lead_F=lead_df$Sex == "Female"
bartlett.test(lead_df$MAXFWT ~ lead_F, data=lead_df)



#BOXPLOT
boxplot(MAXFWT~Sex,data=lead_df)
boxplot(Age~Sex,data=lead_df)
boxplot(Ld72~Sex,data=lead_df)
boxplot(Ld73~Sex,data=lead_df)


#LEVENE TEST
# H0: Equal Variance "p-val>0.05"
# Ha: Not equal variance "p-val<0.05"

leveneTest(MAXFWT~Sex,data=lead_df)#VARIANCE IS EQUAL
leveneTest(Age~Sex,data=lead_df)#VARIANCE IS EQUAL
leveneTest(Ld72~Sex,data=lead_df)#VARIANCE IS EQUAL
leveneTest(Ld73~Sex,data=lead_df)#VARIANCE IS EQUAL



################################################################
#5.	Statistical Inference

gender_conf_interval<-function(df,col,col2,alpha,gender){
  #Step 1: Calculate the mean
  mean.one<-mean(df[df[,col]==gender,][,col2],na.rm=T)
  sample.n <- length(df[df[,col]==gender,][,col2])
  sample.sd <- sd(df[df[,col]==gender,][,col2],na.rm=T)
  
  #Step 2: Calculate the standard error of the mean
  sample.se <- sample.sd/sqrt(sample.n)
  cat("\nStandard error: ",sample.se,"\n")
  
  #Step 3: Find the t-score that corresponds to the confidence level
  # 95% C.I alpha = 0.05
  # 90% C.I alpha = 0.1
  # 99 C.I alpha = 0.01
  
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  cat("t-score: ",t.score,"\n")
  
  #Step 4. Calculate the margin of error and construct the confidence interval
  # For Gender 1 "Males"
  # For Gender 2 "Females"
  
  margin.error <- t.score * sample.se
  lower.bound <- mean.one - margin.error
  upper.bound <- mean.one + margin.error
  cat(gender,": (",lower.bound,",",upper.bound,")","\n ")
  
}


#Calling the function "loop"
alphas<-c(0.1,0.05,0.01)
conf_l<-c("90% Confidence","95% Confidence","99% Confidence")
gender<-c("Male","Female")

for(i in 1:length(alphas)){
  cat(conf_l[i],"\n----------------------\n")
  for(j in 1:length(gender)){
  gender_conf_interval(lead_df,"Sex","MAXFWT",alphas[i],gender[j])}
  cat("\n----------------------\n")}




#Another way/approach "using another implemented function"
conf_ll<-c("90%","95%","99%")
for(i in 1:length(alphas)){
  cat(conf_l[i],"\n----------------------\n")
  for(j in 1:length(gender)){
    conf_interval_2(lead_df,"Sex","MAXFWT",gender[j],conf_ll[i])}
  cat("\n----------------------\n")}



#Visualization
library(infer)
library(tibble)
##Male
male_sample <- tibble(
  male = c(lead_df[lead_df$Sex == "Male",]$MAXFWT)
)

##
bootstrap_distribution <- male_sample %>% 
  specify(response = male) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")
visualize(bootstrap_distribution)

percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci

visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = percentile_ci)


###Female
female_sample <- tibble(
  female = c(lead_df[lead_df$Sex == "Female",]$MAXFWT)
)

##
bootstrap_distribution <- female_sample %>% 
  specify(response = female) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")
visualize(bootstrap_distribution)

percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci

visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = percentile_ci)


################################################################

library(dunn.test)
library(report)
library(multcomp)
library(report)

#6- Hypothesis Testing

#.	We hypothesis that MAXWT is different between male vs female. Assuming normality and homoscedasticity, can you test this hypothesis using statistical hypothesis framework

#a- We state the research question:
#Does MAXWT is different between male vs female?

#b- We convert the research question to a statistical one: 
#Does the mean of the group male differs from the mean of the group females in MAXWT?

#c- We state the null and alternative hypothesis:
#- Null hypothesis: there is no difference between the 2 groups
#- Alternative hypothesis: there is a difference between the 2 groups

#d- we calculate p-value: if normality and homoscedasticity are assumed
#two-sample t-test will be used 

t.test(MAXFWT~Sex, data=lead_df, var.equal = TRUE)
#p-value = 0.2364, which is  greater than the alpha(=0.05)
#our result is not significant and we do not have enough evidence to reject the null hypothesis
#thus, we do not have evidence to say that MAXFWT is different between male and female

#---------------------------------------------------------------------------------------------------


#.	Assess whether the previous test assumptions have been meet for the test.

#check normality of male
qqnorm(lead_df[lead_df$Sex == "Male",]$MAXFWT, main='Males MAXFWT')
qqline(lead_df[lead_df$Sex == "Male",]$MAXFWT) # the line has deviation from data points
hist(lead_df[lead_df$Sex == "Male",]$MAXFWT, main='Males MAXFWT') #not normally distributed
shapiro.test(lead_df[lead_df$Sex == "Male",]$MAXFWT)#p-value = 0.005127 which is lower than the alpha
#so we have enough evidence to reject the null hypothesis
#so it is not normal

#check normality of females
qqnorm(lead_df[lead_df$Sex == "Female",]$MAXFWT, main='Female MAXFWT')
qqline(lead_df[lead_df$Sex == "Female",]$MAXFWT) # the line has no deviation from data points
hist(lead_df[lead_df$Sex == "Female",]$MAXFWT, main='Female MAXFWT') #almost normally distributed
shapiro.test(lead_df[lead_df$Sex == "Female",]$MAXFWT)#p-value = 0.2299 which is greater than the alpha
#so we do not have enough evidence to reject the null hypothesis
#so it is normal

#check variance of Sex
#var.test(MAXFWT~Sex, data=lead)
leveneTest(MAXFWT~Sex, data=lead_df)#better to use levene because it does not assume normality and more robust
#p-value = 0.1778, it is greater than the alpha 
#so we do not have enough evidence to reject the null hypothesis
#so it is equal variance

#thus, data is not normal and homoscedasticity so not all assumptions are met
#it is better to use Mann Whitney rank based test
wilcox.test(MAXFWT~Sex, data=lead_df)
#p-value = 0.1399, which is  greater than the alpha(=0.05)
#our result is not significant and we do not have enough evidence to reject the null hypothesis
#thus, we do not have evidence to say that MAXFWT is different between male and female

#---------------------------------------------------------------------------------------------------


#.	We hypothesis that MAXWT is "lower" in the group receiving Ld72 > 40  compared to the control Ld72 =< 40. Can you test this hypothesis assuming heteroscedasiticy 

#a- We state the research question:
#Does MAXWT is "lower" in the group receiving Ld72 > 40  compared to the control Ld72 =< 40?

#b- We convert the research question to a statistical one: 
#Does the mean of the group ld72>40 lower than the mean of the group ld72=<40 in MAXWT?

#c- We state the null and alternative hypothesis:
#- Null hypothesis: MAXWT is not "lower" in the group receiving Ld72 > 40  compared to the control Ld72 =< 40
#- Alternative hypothesis: MAXWT is "lower" in the group receiving Ld72 > 40  compared to the control Ld72 =< 40

#d- we will test normality
#check normality of receiving 
qqnorm(lead_df[lead_df$Ld72 > 40,]$MAXFWT)
qqline(lead_df[lead_df$Ld72> 40 ,]$MAXFWT) # the line has deviation from data points
hist(lead_df[lead_df$Ld72 > 40,]$MAXFWT) #not normally distributed
shapiro.test(lead_df[lead_df$Ld72 > 40,]$MAXFWT)#p-value = 0.005391 which is lower than the alpha
#so we have enough evidence to reject the null hypothesis
#so it is not normal

#check normality of control
qqnorm(lead_df[lead_df$Ld72 <= 40,]$MAXFWT)
qqline(lead_df[lead_df$Ld72 <= 40,]$MAXFWT) # the line has deviation from data points
hist(lead_df[lead_df$Ld72 <= 40,]$MAXFWT) #not normally distributed
shapiro.test(lead_df[lead_df$Ld72 <= 40,]$MAXFWT)#p-value = 0.009785 which is lower than the alpha
#so we have enough evidence to reject the null hypothesis
#so it is not normal


#e- we calculate p-value: if heteroscedasiticy  are assumed, and being not normal
#- Mann Whitney rank based test will be used
wilcox.test(lead_df[lead_df$Ld72 >40,]$MAXFWT, lead_df[lead_df$Ld72 <= 40,]$MAXFWT,alternative = "less")
#p-value = 0.003036, which is  smaller than the alpha(=0.05)
#then we have the evidence to reject the null hypothesis
#thus we have enough evidence to say that there the group receiving Ld72 > 40 is lower than the control Ld72 =< 40.

#---------------------------------------------------------------------------------------------------

#.	Assess the previous test assumption

#check variance of Ld72
#var.test(MAXFWT~Sex, data=lead)
leveneTest(MAXFWT~as.factor(Ld72), data=lead_df)
#better to use levene because it does not assume normality and more robust
#F value = 0.7382, it is greater than the alpha 
#so we do not have enough evidence to reject the null hypothesis
#so it is equal variance

#not normal and equal variance, so the assumptions are not met 
#Mann Whitney rank based test is used


#---------------------------------------------------------------------------------------------------

#.	We hypothesis that MAXWT is different between the different Lead types with the different genders 
#(i.e. 4 groups male_leadtype1, male_leadtype2, female_leadtype1, female_leadtype2). 
#Can you perform comparison between the different groups, after assessing the assumptions and 
#performing post-hoc testing (assuming normality and homoscedasticity).

#a- We state the research question:
#Is MAXWT different between the different Lead types with the different genders 

#b- We convert the research question to a statistical one: 
#Does the mean of the group MAXWT differs from the mean of the group Lead types and gender?

#c- We state the null and alternative hypothesis:
#- Null hypothesis: there is no difference between groups
#- Alternative hypothesis: there is a difference between groups


#d- check normality
#check normality of male lead type 1
lead_1=lead_df[lead_df$Lead_type=="1",]
lead_1_m=lead_1[lead_1$Sex=="Male",]
qqnorm(lead_1_m$MAXFWT, main='Male Lead_type1 MAXFWT')
qqline(lead_1_m$MAXFWT) # the line has deviation from data points
hist(lead_1_m$MAXFWT, main='Male Lead_type1 MAXFWT') #not normally distributed
shapiro.test(lead_1_m$MAXFWT)
#p-value = 0.01024 which is lower than the alpha
#so we have enough evidence to reject the null hypothesis
#so it is not normal

#check normality of male lead type 2
lead_2=lead_df[lead_df$Lead_type=="2",]
lead_2_m=lead_2[lead_2$Sex=="Male",]
qqnorm(lead_2_m$MAXFWT, main='Male Lead_type2 MAXFWT')
qqline(lead_2_m$MAXFWT) # the line has deviation from data points
hist(lead_2_m$MAXFWT, main='Male Lead_type2 MAXFWT') #not normally distributed
shapiro.test(lead_2_m$MAXFWT)
#p-value = 0.02975 which is lower than the alpha
#so we have enough evidence to reject the null hypothesis
#so it is not normal

#check normality of female lead type 1
lead_1_f=lead_1[lead_1$Sex=="Female",]
qqnorm(lead_1_f$MAXFWT, main='Female Lead_type1 MAXFWT')
qqline(lead_1_f$MAXFWT) # the line has no deviation from data points
hist(lead_1_f$MAXFWT, main='Female Lead_type1 MAXFWT') # normally distributed
shapiro.test(lead_1_f$MAXFWT)
#p-value = 0.1573 which is higher than the alpha
#so we do not have enough evidence to reject the null hypothesis
#so it is normal

#check normality of female lead type 2
lead_2_f=lead_2[lead_2$Sex=="Female",]
qqnorm(lead_2_f$MAXFWT, main='Female Lead_type2 MAXFWT')
qqline(lead_2_f$MAXFWT) # the line has deviation from data points
hist(lead_2_f$MAXFWT, main='Female Lead_type2 MAXFWT') # normally distributed
shapiro.test(lead_2_f$MAXFWT)
#p-value = 0.6139 which is higher than the alpha
#so we do not have enough evidence to reject the null hypothesis
#so it is normal

#so we will consider the data not normal


#e- we calculate p-value: If not normal kruskal wallis will be used
#kruskal = kruskal.test(MAXFWT~Lead_type,data=lead_df)
#kruskal
#p-value = 0.001322 which is  greater than the alpha(=0.05)
#our result is not significant and we do not have enough evidence to reject the null hypothesis
#thus, we do not have evidence to say that MAXFWT is different between different gender and different lead types



#Prepare The Four Groups "samples"
#Get the MAXFWT values of each sample, then put the groups into a list

#male lead type 1
g1_m<-lead_1_m$MAXFWT

#female lead type 1
g1_f<-lead_1_f$MAXFWT

#male lead type 2
g2_m<-lead_2_m$MAXFWT

#female lead type 2
g2_f<-lead_2_f$MAXFWT


kruskal=kruskal.test(list(g1_m,g1_f,g2_m,g2_f))

kruskal

#p-value = 0.004795 which is  smaller than the alpha(=0.05)
#our result is below significant and we have enough evidence to reject the null hypothesis
#thus, we have evidence to say that MAXFWT is different between different gender and different lead types



#if normality and homoscedasticity are assumed
#anova will be used 



AnovaModel<- aov(MAXFWT ~  Sex * Lead_type, data= lead_df)

summary(AnovaModel)
#the f-value of the sex is 0.20764 , which is  greater than the alpha(=0.05)
#our result is not significant and we do not have enough evidence to reject the null hypothesis
#thus, we do not have evidence to say that MAXFWT is different between different gender

#the f-value of the lead type is  0.00195, which is  lower than the alpha(=0.05)
#our result is significant and we have enough evidence to reject the null hypothesis
#thus, we have evidence to say that MAXFWT is different between different lead types

#the f-value of the sex and lead type is 0.11060, which is  greater than the alpha(=0.05)
#our result is not significant and we do not have enough evidence to reject the null hypothesis
#thus, we do not have evidence to say that MAXFWT is different between different gender and different lead type

#posthoc
PairComp <- TukeyHSD(aov(MAXFWT ~  Sex * Lead_type, data= lead_df))
PairComp
#tukey results in 6 compinations, each has 4 columns which are 
#1- difference between the mean of both cases
#2- lower and upper: they are the confidence interval the model is is 95% confident 
# that true population mean of the 2 groups lies in this interval
#3- p-adjust: it is corrected p-value 

#p adj of Male:2-Male:1(0.0036208) and Male:2-Female:1(0.0044057) are less than the alpha(=0.05)
#our result is significant and we have enough evidence to reject the null hypothesis
#thus, male lead type 2 with male lead type 1 is different in MAXFWT
#and male lead type 2 and female lead type 1 is different in MAXFWT
#besides that the confidence interval in these 2 cases doesnt have the 0 value
#and that also indicates the low p adjusted 

#the rest are greater than the alpha(=0.05) so they are not different in MAXFWT
#and the 0 value lies in the confidence interval and that indicates the high p-adjusted as well.

plot(PairComp)

#lead_df$Lead_type = as.integer(lead_df$Lead_type)

#summary(glht(AnovaModel, linfct =mcp(Sex = "Tukey")))
#plot(glht(AnovaModel, linfct =mcp(Sex = "Tukey")))
#pairwise.t.test(lead_df$MAXFWT, lead_df$Lead_type, p.adjust.method = "bonferroni")

#---------------------------------------------------------------------------------------------------

################################################################

#7.	Linear model
#.	Fit a linear regression to the data and interpret the regression coefficient
#we chose the 1st hypotheses

plot(lead_df$Ld72, lead_df$MAXFWT)

lead.regression <- lm(MAXFWT~Ld72 , data= lead_df)
summary(lead.regression)

abline(lead.regression, col="purple")

#.	Calculate and interpret a 95% confidence interval of the regression slope (bonus)
confint(lead.regression, 'Ld72', level=0.95)

################################################################







