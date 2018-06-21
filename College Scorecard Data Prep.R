# Project: College Scorecard Data
# Data available at: https://collegescorecard.ed.gov/data/

# Libraries
library(readr)
library(Hmisc)
library(plyr)
library(dplyr)
library(ggplot2)
require(stats)
library(tree)
library(rpart)
library(rpart.plot)

filepath = "G:/My Drive/Personal Projects/College-Scorecard-Data/"

# Importing 
df1 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED1996_97_PP.csv", sep =""), stringsAsFactors = FALSE)
df2 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED1997_98_PP.csv", sep =""), stringsAsFactors = FALSE)
df3 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED1998_99_PP.csv", sep =""), stringsAsFactors = FALSE)
df4 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED1999_00_PP.csv", sep =""), stringsAsFactors = FALSE)
df5 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2000_01_PP.csv", sep =""), stringsAsFactors = FALSE)
df6 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2001_02_PP.csv", sep =""), stringsAsFactors = FALSE)
df7 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2002_03_PP.csv", sep =""), stringsAsFactors = FALSE)
df8 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2003_04_PP.csv", sep =""), stringsAsFactors = FALSE)
df9 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2004_05_PP.csv", sep =""), stringsAsFactors = FALSE)
df10 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2005_06_PP.csv", sep =""), stringsAsFactors = FALSE)
df11 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2006_07_PP.csv", sep =""), stringsAsFactors = FALSE)
df12 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2007_08_PP.csv", sep =""), stringsAsFactors = FALSE)
df13 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2008_09_PP.csv", sep =""), stringsAsFactors = FALSE)
df14 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2009_10_PP.csv", sep =""), stringsAsFactors = FALSE)
df15 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2010_11_PP.csv", sep =""), stringsAsFactors = FALSE)
df16 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2011_12_PP.csv", sep =""), stringsAsFactors = FALSE)
df17 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2012_13_PP.csv", sep =""), stringsAsFactors = FALSE)
df18 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv", sep =""), stringsAsFactors = FALSE)
df19 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv", sep =""), stringsAsFactors = FALSE)
df20 = read.csv(paste(filepath, "CollegeScorecard_Raw_Data/MERGED2015_16_PP.csv", sep =""), stringsAsFactors = FALSE)

df1$Year = 1996
df2$Year = 1997
df3$Year = 1998
df4$Year = 1999
df5$Year = 2000
df6$Year = 2001
df7$Year = 2002
df8$Year = 2003
df9$Year = 2004
df10$Year = 2005
df11$Year = 2006
df12$Year = 2007
df13$Year = 2008
df14$Year = 2009
df15$Year = 2010
df16$Year = 2011
df17$Year = 2012
df18$Year = 2013
df19$Year = 2014
df20$Year = 2015

df = rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19, df20)
rm(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19, df20)

# I set up backups so that if I mess up the data frame, I can just set my df equal to the backup df I made
backup = df
df = backup

# Subsetting columns
df = df[,c("OPEID",
           "Year",
           "INSTNM",
           "CITY",
           "STABBR",
           "ZIP",
           "PREDDEG",
           "HIGHDEG",
           "CONTROL",
           "REGION",
           "LOCALE2",
           "ADM_RATE",
           "SATVR75",
           "SATMT75",
           "SATWR75",
           "SATVRMID",
           "SATMTMID",
           "SATWRMID",
           "ACTCM75",
           "ACTEN75",
           "ACTMT75",
           "ACTWR75",
           "ACTCMMID",
           "ACTENMID",
           "ACTMTMID",
           "ACTWRMID",
           "SAT_AVG",
           "DISTANCEONLY",
           "UGDS",
           "UG",
           "CURROPER",
           "COSTT4_A",
           "TUITIONFEE_IN",
           "TUITIONFEE_OUT",
           "TUITFTE",
           "AVGFACSAL",
           "PFTFAC",
           "RET_FT4",
           "CUML_DEBT_P75",
           "CUML_DEBT_P25",
           "AGE_ENTRY",
           "FEMALE",
           "MARRIED",
           "VETERAN",
           "FIRST_GEN",
           "FAMINC",
           "MD_FAMINC")]

# Creating a vector of labels
labels = c("OPEID",
           "Year",
           "Institution name",
           "City",
           "State postcode",
           "ZIP code",
           "'Predominant undergraduate degree awarded
           0 Not classified
           1 Predominantly certificate-degree granting
           2 Predominantly associate's-degree granting
           3 Predominantly bachelor's-degree granting
           4 Entirely graduate-degree granting'",
           "'Highest degree awarded
           0 Non-degree-granting
           1 Certificate degree
           2 Associate degree
           3 Bachelor's degree
           4 Graduate degree'",
           "Control of institution",
           "Region (IPEDS)",
           "Degree of urbanization of institution",
           "Admission rate",
           "75th percentile of SAT scores at the institution (critical reading)",
           "75th percentile of SAT scores at the institution (math)",
           "75th percentile of SAT scores at the institution (writing)",
           "Midpoint of SAT scores at the institution (critical reading)",
           "Midpoint of SAT scores at the institution (math)",
           "Midpoint of SAT scores at the institution (writing)",
           "75th percentile of the ACT cumulative score",
           "75th percentile of the ACT English score",
           "75th percentile of the ACT math score",
           "75th percentile of the ACT writing score",
           "Midpoint of the ACT cumulative score",
           "Midpoint of the ACT English score",
           "Midpoint of the ACT math score",
           "Midpoint of the ACT writing score",
           "Average SAT equivalent score of students admitted",
           "Flag for distance-education-only education",
           "Enrollment of undergraduate certificate/degree-seeking students",
           "Enrollment of all undergraduate students",
           "Flag for currently operating institution, 0=closed, 1=operating",
           "Average cost of attendance (academic year institutions)",
           "In-state tuition and fees",
           "Out-of-state tuition and fees",
           "Net tuition revenue per full-time equivalent student",
           "Instructional expenditures per full-time equivalent student",
           "Average faculty salary",
           "Proportion of faculty that is full-time",
           "First-time, full-time student retention rate at four-year institutions",
           "Cumulative loan debt at the 75th percentile",
           "Cumulative loan debt at the 25th percentile",
           "Average age of entry",
           "Share of female students",
           "Share of married students",
           "Share of veteran students",
           "Share of first-generation students",
           "Average family income in real 2015 dollars",
           "Median family income in real 2015 dollars")

# Labeling the variables
for(x in 1:ncol(df)){
  label(df[,x]) = labels[x]
}

# Changing numeric columns to numerics
for(x in 7:ncol(df)){
  df[,x] = as.numeric(df[,x])
} 

# Removing rows without an admit rate
df = df[!is.na(df$ADM_RATE),]

# Changing the control to words; 1 = Public, 2 = Private Non-Profit, 3 = Private For-Profit
for(x in 1:nrow(df)){
  if(df[x, 'CONTROL'] == 1){
    df[x, 'CONTROL'] = "Public"
  }else if(df[x, 'CONTROL'] == 2){
    df[x, 'CONTROL'] = "Private Non-Profit"
  }else if(df[x, 'CONTROL'] == 3){
    df[x, 'CONTROL'] = "Private For-Profit"
  }
}
df$CONTROL = factor(df$CONTROL, levels = c("Public", "Private Non-Profit", "Private For-Profit"))


########## Plots ##########

# Base Plots

plot(df$TUITIONFEE_OUT, df$TUITFTE) # Out of state tuition increase does not affect net revenue per student much
plot(df$TUITFTE, df$ADM_RATE) # Net revenue per student is roughly the same for all admission rates. Note that for net revenue outliers, admission rate is high

# average admission rate by year
yearly_adm_rate <- df %>% group_by(Year) %>% summarise(avg_adm_rate = mean(ADM_RATE))
plot(yearly_adm_rate$Year, yearly_adm_rate$avg_adm_rate) 
reg = lm(avg_adm_rate ~ Year, data = yearly_adm_rate)
abline(reg, col="blue")

# average SAT by year
yearly_sat <- df %>% group_by(Year) %>% summarise(avg_sat = mean((na.omit(SAT_AVG))))
plot(yearly_sat$Year, yearly_sat$avg_sat) # General downward trend in admission rates, with a notable downward spike in 2010
reg = lm(avg_sat ~ Year, data = yearly_sat)
abline(reg, col="blue")

# COSTT4_A (Average Cost of Attendance)
# Does not have data before 2009, adjust to make graph start at 2009.
subsetData = select(.data = df, c(Year, COSTT4_A))
yearly_cost <- df %>% group_by(Year) %>% summarise(avg_cost = mean((na.omit(COSTT4_A))))
plot(yearly_cost$Year, yearly_cost$avg_cost)
reg = lm(avg_cost ~ Year, data = yearly_cost)
abline(reg, col="blue")

##### GGPlots #####

# Plot 1: Control of University vs. Admission Rate, Boxplot
ggplot(df, aes(CONTROL, ADM_RATE)) +
  geom_boxplot() + 
  labs(title = "Control of University vs. Admission Rate", x = "Control of University", y = "Boxplot of Admission Rates") 

# Highest admission rates is private for-profit schools, then public, then private non-profit has the lowest.
# Not much difference between public & private non-profit schools
# Public & private non-profit have a lot of outliers (selective schools)

# Plot 2: Average SAT vs. Admission Rate, Scatter Plot + Best Fit Line
ggplot(df, aes(SAT_AVG, ADM_RATE)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE) + 
  labs(title = "Average SAT vs. Admission Rate", x = "Average SAT", y = "Admission Rate") 

# Overall, as average sat increases, schools are more selective (lower admin rates)
# Notably, there is a mass of data points below the regression line, which we speculate might be students who excel in other areas not related to SAT testing,
# This might be indicative of athletic/artistic potential
# It appears that there linear relationship overlaid by a circular set of data points

# Plot 3: Average Admission Rate by Average SAT
summaryDf = as.data.frame(merge(yearly_adm_rate, yearly_sat))

ggplot(summaryDf, aes(avg_sat, avg_adm_rate)) +
  geom_smooth(method = "lm") + 
  labs(title="Average Admission Rate by Average SAT",x="Average SAT",y="Average Admission Rate") +
  geom_text(aes(label=summaryDf$Year))

# There is a clear divide between pre-2010 and 2010+, SAT scores increased and admission rates decreased.
# Notably, 2010 had very low admission rates, which makes sense based on the previous plot, admission rate vs. year
# 2010 seems to be some sort of transitional period since there were such low admission rates and high SAT scores. Maybe people scrambled to study because of low admit rates?

# Plot 4: Midpoint of ACT vs. Admission Rate
ggplot(df, aes(ACTCMMID, ADM_RATE)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE) + 
  labs(title = "Midpoint of ACT Scores vs. Admission Rate", x = "ACT Midpoint", y = "Admission Rate") 

# Similar to the SAT plot, the plot looks like a circle overlaid ontop of a linear relationship,
# which again pulls the regression line to a more horizontal slope

# Plot 5: Family Income vs. Admission Rate, Scatter Plot + Best Fit Line
ggplot(df, aes(FAMINC, ADM_RATE)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE)

# Overall, as family income increases, admission rate decreases.
# This may be a phenomenon where the wealthier families apply to more selective schools.
# But note, there is a lot of variation and this is a general trend.

# Plot 6: Control of Institution vs. Average Debt at the 75th Percentile 
p75 = ggplot(df, aes(CONTROL, CUML_DEBT_P75)) +
  geom_bar(aes(fill = df$CONTROL), stat = "identity", position = "dodge", show.legend = FALSE) +
  coord_cartesian(ylim=c(0,50000)) +
  labs(title="Control of Institution vs. Average Debt at the 75th Percentile",x="Control of Institution",y="Average Debt for the 75th Percentile") 

p25 = ggplot(df, aes(CONTROL, CUML_DEBT_P25)) +
  geom_bar(aes(fill = df$CONTROL),stat = "identity", position = "dodge", show.legend = FALSE) +
  coord_cartesian(ylim=c(0,50000)) +
  labs(title="Control of Institution vs. Average Debt at the 25th Percentile",x="Control of Institution",y="Average Debt for the 25th Percentile") 

grid.arrange(p25, p75, nrow=1)

# In general, those who attend public school have less debt than those who attend private school.
# At the 75th percentile, they are all fairly close in height a.k.a have a shorter range.