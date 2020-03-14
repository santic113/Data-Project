#Import Libraries Needed for Analysis. Dig into h2o package issue.
library(tidyverse)
library(xgboost)
library(skimr)
library(GGally)
library(plotly)
library(viridis)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(ggcorrplot)
library(rpart.plot)
library(corrgram)
#You will need to download 64bit version of Java, use the following link: https://www.oracle.com/java/technologies/javase-jdk8-downloads.html
library(h2o)
library(ggplot2)
library(ggthemes)
library(psych)
library(scales)
library(treemap)
library(treemapify)
library(repr)
library(cowplot)
library(magrittr)
library(ggpubr)
library(RColorBrewer)
library(plotrix)
library(ggrepel)
library(forcats)
library(reshape2)
library(caTools)
library(tree)
library(rattle)
options(repr.plot.width=8, repr.plot.height=6)
options(warn=-1)

#Uploading our Data into R via a CSV file
df <- read.csv("C:/Users/santi/Documents/R Kaggle Data/IBM_Attrition_Data_V2.csv")
head(df)

#This variable will be ised for tranining and testing.
original_df <- df

#Viewing the data in a table.
View(df)

#Getting a summary of our data.
df %>% glimpse

#Understanding the Distribution of our data.
options(repr.plot.width=8, repr.plot.height=4)

#Attrition Count. Simple Yes/No Chart
attritions_number <- df %>% group_by(Attrition) %>% summarise(Count=n()) %>%
  ggplot(aes(x=Attrition, y=Count)) + geom_bar(stat="identity", fill="orange", color="grey40") + theme_bw() + coord_flip() +
  geom_text(aes(x=Attrition, y=0.01, label= Count),
            hjust=-0.8, vjust=-1, size=3,
            colour="black", fontface="bold",
            angle=360) + labs(title="Employee Attrition (Amount)", x="Employee Attrition",y="Amount") + theme(plot.title=element_text(hjust=0.5))


attrition_percentage <- df %>% group_by(Attrition) %>% summarise(Count=n()) %>%
  mutate(pct=round(prop.table(Count),2) * 100) %>%
  ggplot(aes(x=Attrition, y=pct)) + geom_bar(stat="identity", fill = "dodgerblue", color="grey40") +
  geom_text(aes(x=Attrition, y=0.01, label= sprintf("%.2f%%", pct)),
            hjust=0.5, vjust=-3, size=4,
            colour="black", fontface="bold") + theme_bw() + labs(x="Employee Attrition", y="Percentage") +
  labs(title="Employee Attrition (%)") + theme(plot.title=element_text(hjust=0.5))

#Plotting out our Results Above
plot_grid(attritions_number, attrition_percentage, align="h", ncol=2)

#Average age by Gender
avg.age <- df %>% select(Gender,Age) %>% group_by(Gender) %>% summarize(avg=mean(Age))
avg.age

#Age Distribution by Gender
# Let's look closely at the distribution of the Age of our employees
# Why? Unlike the older generation, millenials tend to switch workplaces more and thus that could
# be an explanation of why we have the current levels of attrition
options(repr.plot.width=8, repr.plot.height=6)

dat_text <- data.frame(
  label = c("Mean = 37.33 \n Years Old", "Mean = 36.65 \n Years Old"),
  Gender = c("Female", "Male")
)

#Distribution facet wrap
gender.dist <- df %>% select(Gender, Age) %>% filter(Gender == 'Male' | Gender== "Female") %>%
  filter(!is.na(Age)) %>% group_by(Gender) %>%
  ggplot(aes(x=Age)) + geom_density(aes(fill=Gender), alpha=0.8, show.legend=FALSE) + facet_wrap(~Gender) + theme_minimal() +
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) + labs(title="Age Distribution") +
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("#F781F3", "#819FF7")) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust   = -0.1,
    vjust   = -1
  )

overall.dist <- df %>% select(Gender, Age) %>% filter(!is.na(Age)) %>%
  ggplot(data=df, mapping=aes(x=Age)) + geom_density(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) +  theme_minimal() + labs(x="Overall Age") +
  annotate("text", label = "Mean = 36.92 Years Old", x = 50, y = 0.03, color = "black")


plot_grid(gender.dist, overall.dist, nrow=2)

# Boxplot with attrition in the X-axis and Job Satisfaction in the y-Axis
options(repr.plot.width=8, repr.plot.height=6)

#Box plot of satisfaction by Gender and attrition. Our measure is Jobsatisfaction
box.attrition <- df %>% select(Attrition, JobSatisfaction, Gender) %>%
  ggplot(aes(x=Attrition, y=JobSatisfaction, fill=Attrition)) + geom_boxplot(color="black") + theme_minimal() + facet_wrap(~Gender) +
  scale_fill_manual(values=c("#FA5858", "#9FF781"))

# Distribution of Job Satisfaction
dist.satisfaction <- df %>% select(JobSatisfaction) %>%
  ggplot(aes(x=JobSatisfaction)) + geom_density(color="#013ADF", fill="#81BEF7", trim=TRUE) + theme_tufte() + xlim(range(c(1,4)))

plot_grid(box.attrition, dist.satisfaction, nrow=2)

#Monthly Income by Gender
Income_by_Gender <- ggplot(df, aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + geom_boxplot() +
  scale_fill_manual(values=c("#F5A9F2", "#5882FA")) + scale_color_manual(values=c("#FE2EF7", "#5858FA")) +
  coord_flip() + labs(title="Are there any Gender Disparities in Income?")

Income_by_Gender

#Average Income and Presence by Department
options(repr.plot.width=10, repr.plot.height=8)

gender.income <- df %>% select(Gender, MonthlyIncome) %>% group_by(Gender) %>% summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
  ggplot(aes(x=Gender, y=avg_income)) + geom_bar(stat="identity", fill="#2E9AFE", width=0.5) +
  geom_text(aes(x=Gender, y=0.01, label= paste0("$ ", avg_income)),
            hjust=-2, vjust=0, size=3,
            colour="black", fontface="bold",
            angle=360) + labs(title="Average Salary by Gender", x="Gender",y="Salary") + coord_flip() +
  theme_minimal() + theme(plot.title=element_text(size=14, hjust=0.5))

# # How many people work in each department by gender
gender.department <- df %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x=reorder(Department, -amount), y=amount, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("pink", "lightblue")) +
  labs(title="Number of Employees \n
by Department",x="Department", y="Number of employees")

departments <- df %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x="", y=amount, fill=Department), show.legend=FALSE, width=) + geom_bar(stat="identity", position="dodge") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5), aspect.ratio=1) +
  labs(title="Number of Employees \n
by Department") + coord_polar() + scale_fill_manual(values=c("#FE642E", "#0080FF","#00FF40"))

plot_grid(gender.income, gender.department, departments, ncol=2, nrow=2)

# Distribution of Number of Companies Worked by Attrition and Age
# We want to see if young people have worked in more companies than the older generation
# This might prove that the millenials tend to be more picky with regards to jobs than the older generation.
options(repr.plot.width=8, repr.plot.height=7)

# First we must create categoricals variables based on Age
df$Generation <- ifelse(df$Age<37,"Millenials",
ifelse(df$Age>=38 & df$Age<54,"Generation X",
 ifelse(df$Age>=54 & df$Age<73,"Boomers","Silent" )))

# Let's see the distribution by generation now
generation.dist <- df %>% select(Generation, NumCompaniesWorked, Attrition) %>%
  ggplot() + geom_boxplot(aes(x=reorder(Generation, NumCompaniesWorked, FUN=median),
                              y=NumCompaniesWorked, fill=Generation)) +
  theme_tufte() + facet_wrap(~Attrition) +
  scale_fill_brewer(palette="RdBu") + coord_flip() +
  labs(title="Knowing Past Generations",x="Generation", y="Number of Companies Previously Worked") +
  theme(legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                   size=0.5, linetype="solid",
                                                                   colour ="black")) + theme(strip.background = element_blank(), strip.text.x = element_blank(),
                                                                                             plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                                                                                             axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                                                             axis.title=element_text(colour="white"))

# 2.69
overall.avg <- df %>% select(Generation, NumCompaniesWorked) %>% summarize(avg_ov=mean(NumCompaniesWorked))


# Let's find the Average Numbers of Companies worked by Generation
avg.comp <- df %>% select(Generation, NumCompaniesWorked, Attrition) %>% group_by(Generation, Attrition) %>%
  summarize(avg=mean(NumCompaniesWorked)) %>%
  ggplot(aes(x=Generation, y=avg, color=Attrition)) +
  geom_point(size=3) + theme_tufte() +  # Draw points
  geom_segment(aes(x=Generation,
                   xend=Generation,
                   y=min(avg),
                   yend=max(avg)),
               linetype="dashed",
               size=0.1,
               color="white") +
  labs(title="",
       subtitle="Behavioral Difference between Generations",
       y="Average Number of Companies worked for",
       x="Generation") +
  coord_flip() + scale_color_manual(values=c("#58FA58", "#FA5858")) +
  theme(legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                   size=0.5, linetype="solid",
                                                                   colour ="black")) + theme(strip.background = element_blank(), strip.text.x = element_blank(),
                                                                                             plot.title=element_text(hjust=0.5, color="white"),plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
                                                                                             axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                                                             axis.title=element_text(colour="white"))


plot_grid(generation.dist, avg.comp, nrow=2)


options(repr.plot.width=8, repr.plot.height=5)

conti_df <- as.data.frame.matrix(table(df$Generation, df$Attrition))

conti_df$groupSum <- rowSums(conti_df)
conti_df$xmax <- cumsum(conti_df$groupSum)
conti_df$xmin <- conti_df$xmax - conti_df$groupSum
# The groupSum column needs to be removed; don't remove this line
conti_df$groupSum <- NULL

conti_df$Generation <- rownames(conti_df)

melt_df <- melt(conti_df, id.vars = c("Generation", "xmin", "xmax"), variable.name = "Attrition")

df_melt <- melt_df %>%
  group_by(Generation) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))


index <- df_melt$xmax == max(df_melt$xmax)
df_melt$yposn <- df_melt$ymin[index] + (df_melt$ymax[index] - df_melt$ymin[index])/2


df_melt$xposn <- df_melt$xmin + (df_melt$xmax - df_melt$xmin)/2

# geom_text for ages (i.e. the x axis)

p1<- ggplot(df_melt, aes(ymin = ymin,
                         ymax = ymax,
                         xmin = xmin,
                         xmax = xmax,
                         fill = Attrition)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c("#98fb98", "#ff6347")) +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"),
        legend.text=element_text(color="#595959",size=12),
        plot.subtitle=element_text(color="white", size=10, face= 'italic', hjust=0.5),
        legend.title=element_text(color="#595959"),
        legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                   size=0.5, linetype="solid",
                                                                   colour ="black"))

p1 +
  geom_text(aes(x = xposn, label = Generation),
            y = 0.15, angle = 90,
            size = 3, hjust = -1.22,
            show.legend = FALSE) + labs(title="Mosaic Plot", subtitle="Understanding both levels and frequencies of Attrition by Generation",
                                        x="Share of Employees", y="Frequency") +
  theme(plot.title=element_text(hjust=0.5))

#Attrition by Educational Level
options(repr.plot.width=8, repr.plot.height=4)

# Give names for the different education levels.
df$Educational_Levels <-  ifelse(df$Education == 1, "Without College D.",
                                 ifelse(df$Education == 2 , "College D.",
                                        ifelse(df$Education == 3, "Bachelors D.",
                                               ifelse(df$Education == 4, "Masters D.", "Phd D."))))

# I want to know in terms of proportions if we are loosing key talent here.
edu.level <- df %>% select(Educational_Levels, Attrition) %>% group_by(Educational_Levels, Attrition) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=fct_reorder(Educational_Levels,n), y=n, fill=Attrition, color=Attrition)) + geom_bar(stat="identity") + facet_wrap(~Attrition) +
  coord_flip() + scale_fill_manual(values=c("#2EF688", "#F63A2E")) + scale_color_manual(values=c("#09C873","#DD1509")) +
  geom_label(aes(label=n, fill = Attrition), colour = "white", fontface = "italic") +
  labs(x="", y="Number of Employees", title="Attrition by Educational Level") + theme_wsj() +
  theme(legend.position="none", plot.title=element_text(hjust=0.5, size=14))

edu.level

#Average Income by Department
# Let's determine if income was a major factor when it came to leaving the company.
# Let's start by taking the average monthly income of people who left the company and people who stayed in the company
# Group by department
options(repr.plot.width=8, repr.plot.height=5)


avg.income <- df %>% select(Department, MonthlyIncome, Attrition) %>% group_by(Attrition, Department) %>%
  summarize(avg.inc=mean(MonthlyIncome)) %>%
  ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~Attrition) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values=c("lightgreen", "tomato2")) +
  labs(y="Average Income", x="Department", title="Average Income by Department \n and Attrition Status") +
  geom_text(aes(x=Department, y=0.01, label= paste0("$ ", round(avg.inc,2))),
            hjust=-0.5, vjust=0, size=3,
            colour="black", fontface="bold",
            angle=90)
avg.income

#Determining Satisifaction by Income
options(repr.plot.width=8, repr.plot.height=5)

# Turn the column to factor: One because it should not be considered an integer
# Two: Will help us sort in an orderly manner.
df$JobSatisfaction <- as.factor(df$JobSatisfaction)

high.inc <- df %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% group_by(JobSatisfaction, Attrition) %>%
  summarize(med=median(MonthlyIncome)) %>%
  ggplot(aes(x=fct_reorder(JobSatisfaction, -med), y=med, color=Attrition)) +
  geom_point(size=3) +
  geom_segment(aes(x=JobSatisfaction,
                   xend=JobSatisfaction,
                   y=0,
                   yend=med)) + facet_wrap(~Attrition) +
  labs(title="Is Income a Reason for Employees to Leave?",
       subtitle="by Attrition Status",
       y="Median Income",
       x="Level of Job Satisfaction") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6), plot.title=element_text(hjust=0.5), strip.background = element_blank(),
        strip.text = element_blank()) +
  coord_flip() + theme_minimal() + scale_color_manual(values=c("#58FA58", "#FA5858")) +
  geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med,2))),
            hjust=-0.5, vjust=-0.5, size=4,
            colour="black", fontface="italic",
            angle=360)
high.inc

#Income and the Level of Attrition:
options(repr.plot.width=8, repr.plot.height=7)

per.sal <- df %>% select(Attrition, PercentSalaryHike, MonthlyIncome) %>%
  ggplot(aes(x=PercentSalaryHike, y=MonthlyIncome)) + geom_jitter(aes(col=Attrition), alpha=0.5) +
  theme_economist() + theme(legend.position="none") + scale_color_manual(values=c("#58FA58", "#FA5858")) +
  labs(title="Income and its Impact on Attrition") + theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                                                           axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                           axis.title=element_text(colour="white"))

perf.inc <- df %>% select(PerformanceRating, MonthlyIncome, Attrition) %>% group_by(factor(PerformanceRating), Attrition) %>%
  ggplot(aes(x=factor(PerformanceRating), y=MonthlyIncome, fill=Attrition)) + geom_violin() + coord_flip() + facet_wrap(~Attrition) +
  scale_fill_manual(values=c("#58FA58", "#FA5858")) + theme_economist() +
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
        plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"),
        legend.text=element_text(color="white")) +
  labs(x="Performance Rating",y="Monthly Income")


plot_grid(per.sal, perf.inc, nrow=2)

#Average and Percent Difference of Daily Rates:
# daily rates by JobRole
options(repr.plot.width=10, repr.plot.height=7)

daily_r <- df %>% select(JobRole, Attrition, DailyRate) %>% group_by(Attrition, JobRole) %>%
  ggplot(aes(x=JobRole, y=DailyRate, color=Attrition)) + facet_wrap(~Attrition) + coord_flip() + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5, size=10), plot.background=element_rect(fill="#FFF1E0")) +
  stat_summary(fun.y=mean, fun.ymin = min, fun.ymax = max) + scale_color_manual(values=c("#58FA58", "#FA5858")) +
  labs(title="Daily Rates by Job Role")


# What's the difference between in Dailyrates by attrition and jobrole status.
attrition_daily <- df %>% select(JobRole, Attrition, DailyRate) %>% group_by(JobRole) %>% filter(Attrition == "Yes") %>%
  summarize(avg_attrition=mean(DailyRate))


noattrition_daily <- df %>% select(JobRole, Attrition, DailyRate) %>% group_by(JobRole) %>% filter(Attrition == "No") %>%
  summarize(avg_noattrition=mean(DailyRate))

# (No Attrition daily - Attrition Daily) / No Attrition Daily
colors <- c("#316D15C", "#16D12C", "#B2D116", "#FEBE5D", "#FE9F5D", "#F86E2E", "#F8532E", "#FA451D", "#FA1D1D")

combined_df <- merge(attrition_daily, noattrition_daily)
colourCount = length(unique(combined_df$JobRole))

percent_diff <- combined_df %>% mutate(pct_diff=round(((avg_noattrition - avg_attrition)/avg_noattrition),2) * 100) %>%
  ggplot(aes(x=reorder(JobRole,pct_diff), y=pct_diff, fill=JobRole)) + geom_bar(stat="identity") + coord_flip() + theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set2"))(colourCount)) +
  theme(plot.title=element_text(hjust=0.5, size=10), plot.background=element_rect(fill="#FFF1E0"), legend.position="none") +
  labs(x="JobRole", y="Percent Difference (%)", title="Percent Difference Charged by Day") +
  geom_label(aes(label=paste0(pct_diff, "%")), colour = "white", fontface = "italic", hjust=0.2)

plot_grid(daily_r, percent_diff, nrow=2)

#Level of Attrition by Overtime Status
df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100)

options(repr.plot.width=10, repr.plot.height=5)


overtime_percent <- df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>%
  ggplot(aes(x="", y=pct, fill=OverTime)) +
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  theme_tufte() + scale_fill_manual(values=c("#2EFE64", "#FE2E2E")) +
  geom_label(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), colour = "white",  fontface = "italic")+
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
        plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"),
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid", colour ="black")) +
  labs(title="Level of Attrition by Overtime Status", subtitle="In Percent", x="", y="")


overtime_number <- df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>%
  ggplot(aes(x=OverTime, y=n, fill=OverTime)) + geom_bar(stat="identity") + scale_fill_manual(values=c("#BCF5A9", "#F5BCA9")) +
  geom_label(aes(label=paste0(n)), fill="#FFF9F5", colour = "black", fontface = "italic") +
  labs(title="Level of Attrition by Overtime Status", subtitle="In Numbers", x="Overtime Status", y="Number of Employees") + theme_minimal() +
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
        plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"),
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid",
                                         colour ="black"))
plot_grid(overtime_percent, overtime_number)

# Create a TreeMap with the number of Employees by JobRole
library(tree)
role.amount <- df %>% select(JobRole) %>% group_by(JobRole) %>% summarize(amount=n()) %>%
  ggplot(aes(area=amount, fill=JobRole, label=JobRole)) +  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(legend.position = "none") +
  labs(
    title = "Major Job Roles Inside the Organization",
    caption = "The area of each tile represents the number of
employees by type of job role.",
    fill = "JobRole"
  )

role.amount

# Median Salary by JobRole
# Highest percentage of attrition by JobRole
options(repr.plot.width=8, repr.plot.height=5)

# Median Salary
job.sal <- df %>% select(JobRole, MonthlyIncome) %>% group_by(JobRole) %>% summarize(med=median(MonthlyIncome), avg=mean(MonthlyIncome))


p1 <- ggplot(job.sal, aes(x=reorder(JobRole,-med), y=med)) +  geom_bar(stat="identity", width=.5, fill="#FE9A2E") +
  labs(title="Salary by Job Role",
       subtitle="Median",
       x="Job Role",
       y="Median Income") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))


p2 <- ggplot(job.sal, aes(x=reorder(JobRole,-avg), y=avg)) +  geom_bar(stat="identity", width=.5, fill="#BE81F7") +
  labs(title="Salary by Job Role",
       subtitle="Mean",
       x="Job Role",
       y="Mean Income") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))


plot_grid(p1, p2, ncol=2)

#Attrition by Job Role
# The Funnel with the Attrition Rates by Job Role
options(repr.plot.width=10, repr.plot.height=6)
attr.job <- df %>% select(JobRole, Attrition) %>% group_by(JobRole, Attrition) %>% summarize(amount=n()) %>%
  mutate(pct=round(prop.table(amount),2) * 100) %>% arrange(pct)

nofunc <- colorRampPalette(c("#A9F5A9", "#58FA58", "#01DF01"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))

yes.attr <- attr.job %>% filter(Attrition == "Yes") %>% arrange(JobRole)
no.attr <- attr.job %>% filter(Attrition == "No") %>% arrange(JobRole)

par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, labels = unique(attr.job$JobRole),
                       top.labels=c("No","","Yes"), main = "Attrition by Job Role",
                       gap=30, show.values = T, rxcol = yesfunc(9), lxcol = nofunc(9)))

# Create a Categorical Value for Years with Current Manager
# Create a Categorical Value for RelationShip Satisfaction (We will use facet_wrap here)
# Then we will use income as our Y-Axis
options(repr.plot.width=8, repr.plot.height=7)

df$CatYearsManager <- ifelse(df$YearsWithCurrManager <= 1, "Recently Hired",
                             ifelse(df$YearsWithCurrManager > 1 & df$YearsWithCurrManager <= 4, "2-4 Years hired", "Long Established Manager"))


# Determine what is the Average Relationship Satisfaction with the Recently Hired Managers
rel.sat <- df %>% select(CatYearsManager, RelationshipSatisfaction, Attrition) %>% group_by(CatYearsManager, Attrition) %>%
  summarize(avg.sat=mean(RelationshipSatisfaction)) %>%
  ggplot(aes(x=fct_reorder(CatYearsManager,-avg.sat), y=avg.sat, fill=Attrition)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~Attrition) +
  geom_text(aes(x=CatYearsManager, y=0, label= paste0(round(avg.sat,2))),
            hjust=-0.5, vjust=-0.5, size=4,
            colour="black", fontface="italic",
            angle=360) + coord_flip() + theme_bw() +
  theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(), plot.title=element_text(hjust=0.5),
        axis.text.y = element_text(angle = 55)) +
  labs(x="Years with Current Manager",y="Average Satisfaction Score", title="Dealing with Current Managers") +
  scale_fill_manual(values=c("#58FA58", "#FA5858"))


# Create a Distribution by Attrition of the RelationShip Satisfaction
rel.dist <- df %>% select(RelationshipSatisfaction, Attrition) %>% group_by(Attrition) %>%
  ggplot(aes(x=RelationshipSatisfaction, y=..density..)) + geom_density(aes(fill=Attrition)) + facet_wrap(~Attrition) +
  theme_bw() + theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank()) +
  scale_fill_manual(values=c("#58FA58", "#FA5858"))


plot_grid(rel.sat, rel.dist, nrow=2)


# Environment Satisfaction let's use the changes by JobRole
options(repr.plot.width=8, repr.plot.height=5)

env.attr <- df %>% select(EnvironmentSatisfaction, JobRole, Attrition) %>% group_by(JobRole, Attrition) %>%
  summarize(avg.env=mean(EnvironmentSatisfaction))

ggplot(env.attr, aes(x=JobRole, y=avg.env)) + geom_line(aes(group=Attrition), color="#58ACFA", linetype="dashed") +
  geom_point(aes(color=Attrition), size=3) +  theme_economist() + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=90),
                                                                        plot.background=element_rect(fill="#FFF1E0")) +
  labs(title="Working Environment", y="Average Environment Satisfaction", x="Job Position") + scale_color_manual(values=c("#58FA58", "#FA5858"))

#Is there Work Life Balance
options(repr.plot.width=8, repr.plot.height=4)

attritions <- df %>% filter(Attrition == "Yes")

attritions$WorkLifeBalance <- as.factor(attritions$WorkLifeBalance)

by.department <- attritions %>% select(Department, WorkLifeBalance) %>% group_by(Department, WorkLifeBalance) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=fct_reorder(WorkLifeBalance, -count), y=count, fill=Department)) + geom_bar(stat='identity') + facet_wrap(~Department) +
  theme_economist() + theme(legend.position="bottom", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#FFF1E0")) +
  scale_fill_manual(values=c("#FA5882", "#819FF7", "#FE2E2E")) +
  geom_label(aes(label=count, fill = Department), colour = "white", fontface = "italic") +
  labs(title="Is there a Work Life Balance Environment?", x="Work and Life Balance", y="Number of Employees")

by.department

# Digging into Research and Development
r.d <- df %>% select(Department, WorkLifeBalance, Attrition) %>%
  filter(Department == "Research & Development" & WorkLifeBalance == 1 | WorkLifeBalance == 2) %>%
  group_by(Attrition) %>% summarize(count=n())

# Determine the average distance of people who did not quit the organization.
# Then use this number as an anchor to create a column for the employees that quit.
# Let's see what number of employees that quit are above the average distance of employees that didn't quit.
options(repr.plot.width=8, repr.plot.height=4)

no.attritions <- df %>% filter(Attrition == "No")

# Average distance of employees that didn't quit.
med.distance <- no.attritions %>% select(DistanceFromHome) %>% summarize(med.dist=round(median(DistanceFromHome), 2))

attritions$Median_Distance <- ifelse(attritions$DistanceFromHome < 7, "Below Average", "Above Average")


# Distribution of both Distance from Work Status
dist <- attritions %>% select(Median_Distance, DistanceFromHome) %>%
  ggplot(aes(x=DistanceFromHome, fill=Median_Distance)) + geom_density() + facet_wrap(~Median_Distance) + theme_minimal() +
  scale_color_manual(values=c("#2EFE64", "#FA5858")) + scale_fill_manual(values=c("#F6CED8", "#ECF6CE")) +
  theme(legend.position="bottom", plot.background=element_rect(fill="#FFF1E0")) +
  geom_vline(aes(xintercept=7),
             color="black", linetype="dashed", size=1) +
  annotate("text", label = "Median = 7", x = 15, y = 0.17, color = "black")

dist

p1 <- attritions %>% select(Median_Distance) %>% group_by(Median_Distance) %>% summarize(count=n()) %>%
  ggplot(aes(x=Median_Distance, y=count, color=Median_Distance, fill=Median_Distance)) + geom_bar(stat="identity", position="dodge") +  theme_minimal() +
  theme(legend.position="none") + scale_fill_manual(values=c("#FA5858", "#819FF7")) +
  geom_label(aes(label=count, fill = Median_Distance), colour = "white", fontface = "italic") +
  scale_color_manual(values=c("#DF0101", "#013ADF")) + labs(x="Distance from Work Status")

p2 <- attritions %>% select(Median_Distance) %>% group_by(Median_Distance) %>% summarize(count=n()) %>%
  mutate(pct=round(prop.table(count),2) * 100) %>%
  ggplot(aes(x=Median_Distance, y=pct, color=Median_Distance, fill=Median_Distance)) + geom_bar(stat="identity") + theme_minimal() +
  theme(legend.position="none") +
  geom_label(aes(label=pct, fill = Median_Distance), colour = "white", fontface = "italic")  + scale_fill_manual(values=c("#FA5858", "#819FF7")) +
  scale_color_manual(values=c("#DF0101", "#013ADF")) + labs(x="Distance from Work Status", y="Percentage (%)")

plot_grid(p1, p2, ncol=2)


# Let's see what the Average MonthlyIncome is for those who have stockoptionlevels and for those who don't.
# First let's see how many employees have Stockoption levels.
options(repr.plot.width=10, repr.plot.height=7)

stockoption <- df %>% select(StockOptionLevel, Attrition) %>% group_by(StockOptionLevel, Attrition) %>% summarize(n=n())  %>%
  ggplot(aes(x=reorder(StockOptionLevel, -n), y=n, fill=factor(StockOptionLevel))) + geom_bar(stat="identity") + coord_flip() +
  facet_wrap(~Attrition) + theme_economist() + scale_fill_manual(values=c("#DF0101", "#F5A9A9", "#BEF781", "#04B404")) +
  guides(fill=guide_legend(title="Stock Option \n Level")) +
  theme(legend.position="none", plot.background=element_rect(fill="#0D7680"), plot.title=element_text(hjust=0.5, color="white"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"),
        strip.text.x = element_text(color="white"),
        legend.text=element_text(color="white"))  +
  geom_label(aes(label=n, fill = factor(StockOptionLevel)), colour = "white", fontface = "italic", hjust=0.55) +
  labs(title="Number of Employees", x="StockOptionLevel", y="Amount")

# Average income by StockOption using the geom_line()
income_stockoption <- df %>% select(StockOptionLevel, MonthlyIncome, Attrition) %>% group_by(StockOptionLevel, Attrition) %>%
  ggplot(aes(x=MonthlyIncome)) + geom_area(aes(fill=factor(StockOptionLevel)), stat ="bin", bins=100, alpha=0.8) + facet_wrap(~Attrition) +
  theme_economist() +   scale_fill_manual(values=c("#DF0101", "#F5A9A9", "#BEF781", "#04B404")) +
  guides(fill=guide_legend(title="Stock Option \n Level")) +
  theme(legend.position="bottom", plot.background=element_rect(fill="#0D7680"), plot.title=element_text(hjust=0.5, color="white"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"),
        strip.text.x = element_text(color="white"),
        legend.text=element_text(color="black"),
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid",
                                         colour ="black"))


plot_grid(stockoption, income_stockoption, nrow=2)


options(repr.plot.width=8, repr.plot.height=5)


# Percent Attrition by Work Life Balance
work_bal_cnt <- df %>% select(Attrition, BusinessTravel, WorkLifeBalance) %>% group_by(Attrition, BusinessTravel) %>%
  summarize(count=n()) %>% mutate(pct=round(prop.table(count),2) * 100) %>%
  ggplot(aes(x=Attrition, y=count, fill=BusinessTravel, color=Attrition)) + geom_bar(stat='identity') + facet_wrap(~BusinessTravel) +
  geom_label(aes(label=count, fill = BusinessTravel), colour = "white", fontface = "italic")  + theme_minimal() + theme(legend.position="none") +
  scale_fill_manual(values=c("#00dbdb", "#00db6e", "#fa8072")) +
  scale_color_manual(values=c("#808080", "#808080")) + labs(title="Attrition by Business Travel of Employees",
                                                            x="Attrition", y="Number of Employees") + coord_flip() +
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#FFF1E0"))



work_bal_pct <- df %>% select(Attrition, BusinessTravel, WorkLifeBalance) %>% group_by(Attrition, BusinessTravel) %>%
  summarize(count=n()) %>% mutate(pct=round(prop.table(count),2) * 100) %>%
  ggplot(aes(x=Attrition, y=pct, fill=BusinessTravel, color=Attrition)) + geom_bar(stat='identity') + facet_wrap(~BusinessTravel) + theme_minimal() +
  theme(legend.position="none") +
  geom_label(aes(label=paste0(pct, "%"), fill = BusinessTravel), colour = "white", fontface = "italic")  +
  scale_fill_manual(values=c("#00dbdb", "#00db6e", "#fa8072")) +
  scale_color_manual(values=c("#808080", "#808080")) + labs(x="Attrition", y="Percentage (%)") + coord_flip() +
  theme(plot.background=element_rect(fill="#FFF1E0"))

plot_grid(work_bal_cnt, work_bal_pct, nrow=2)


# # Let's have a better understanding about each feature through a correlation plot
options(repr.plot.width=10, repr.plot.height=7)

nums <- select_if(df, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="square",
           colors = c("tomato2", "white", "#01A9DB"),
           title="Correlogram Employee Attritions",
           ggtheme=theme_minimal())

#Bi-Variate Analysis
options(repr.plot.width=10, repr.plot.height=8)

# Positive Correlations
p1 <- df %>% select(TotalWorkingYears, MonthlyIncome) %>%
  ggplot(aes(x=TotalWorkingYears, y=MonthlyIncome)) + geom_point(colour = "#F2DFCE", alpha=1/2) + geom_smooth(method="loess",color="#EE4037") +
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
                            plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) +
  labs(title="Positive Correlation", subtitle="Monthly Income vs Working Years")

p2 <-  df %>% select(PerformanceRating, PercentSalaryHike) %>%
  ggplot(aes(x=factor(PerformanceRating), y=PercentSalaryHike)) + geom_boxplot(colour = "#FE642E", fill="#A9D0F5") +
  geom_jitter(color="#F2DFCE",alpha=1/3)  +
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
                            plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) +
  labs(title="Possitive Correlation", subtitle="Percent Salary Hike vs Performance Rating", x="Performance Rating")

# Years with Current Manager, Years since Last Promotion
p3 <-  df %>% select(YearsWithCurrManager, YearsSinceLastPromotion) %>%
  ggplot(aes(x=factor(YearsWithCurrManager), y=YearsSinceLastPromotion)) + geom_boxplot(colour = "#FE642E", fill="#A9D0F5") +
  geom_jitter(color="#F2DFCE",alpha=1/3) + geom_smooth(method='loess',aes(group=1),color='#EE4037',lty=2,size=.5) +
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
                            plot.title=element_text(hjust=0.5, color="white"),plot.subtitle=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) +
  labs(title="Possitive Correlation", subtitle="Years since Last Promotions vs Years with Current Manager", x="Years with Current Manager")

# Age and Monthly Income
p4 <-  df %>% select(Age, MonthlyIncome) %>%
  ggplot(aes(x=Age, y=MonthlyIncome)) + geom_point(colour = "#F2DFCE", alpha=1/2) + geom_smooth(method="loess", color="#EE4037") +
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(),
                            plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) +
  labs(title="Positive Correlation", subtitle="Monthly Income vs Age")

plot_grid(p1, p2, p3,p4, ncol=2, nrow=2)

#Splitting out the Data
set.seed(142)
# # I personally prefer to shuffle my data before splitting.
original_df <- original_df[sample(nrow(original_df)),]

# Let's encode the ordinal variables
original_df$BusinessTravel = factor(original_df$BusinessTravel,
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                                    labels = c(1, 2, 3))



# Changing the datatype from integer to factors from the ordinal variables.
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel",
          "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction",
          "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")

original_df[cols] <- lapply(original_df[cols], factor)

# Delete unecessary columns
cols <- c("Over18", "EmployeeNumber", "EmployeeCount")

original_df[cols] <- NULL


# Splitting our data
trainIndex <- createDataPartition(original_df$Attrition, p=0.8,
                                  list=FALSE, times=1)

train <- original_df[trainIndex,]
test <- original_df[-trainIndex,]



# Checking that both the training and testing sets have the same label proportions.
prop_train <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_train
prop_test

options(repr.plot.width=10, repr.plot.height=8)

rpart.tree <- rpart(Attrition ~ ., data=train)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")

# Complicated DecisionTree, Is there a way to determine variable importance?
var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() +
  theme_minimal() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(),
                          plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
                          axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                          axis.title=element_text(colour="white"),
                          legend.background = element_rect(fill="#FFF9F5",
                                                           size=0.5, linetype="solid",
                                                           colour ="black")) + scale_fill_manual(values = colorRampPalette(brewer.pal(24, "Set2"))(colorCount)) +
  geom_label(aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) +
  labs(title="Feature Importance for our Decision Tree Model", x="Features", y="Importance")

feature_importance

#Confusion Matrix
options(repr.plot.width=8, repr.plot.height=6)

predictions <- predict(rpart.tree, test, type="class")
conf_df <- data.frame(table(test$Attrition, predictions))

ggplot(data =  conf_df, mapping = aes(x = predictions, y = Var1)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "#F3F781", high = "#58FA82") +
  theme_economist() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(),
                            plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white"),
                            legend.background = element_rect(fill="#FFF9F5",
                                                             size=0.5, linetype="solid",
                                                             colour ="black")) +
  labs(title="Confusion Matrix", y="Attrition Status", x="Predictions")

# Pruning reduces the size of decision trees by removing parts of the tree that do not provide power to classify instances

prune.rpart.tree <- prune(rpart.tree, cp=0.02) # pruning the tree
plot(prune.rpart.tree, uniform=TRUE, branch=0.6)
text(prune.rpart.tree, all=TRUE, use.n=TRUE)

library(partykit)

rparty.tree <- as.party(rpart.tree)
rparty.tree

# options(repr.plot.width=12, repr.plot.height=12)
# plot(rparty.tree)
options(repr.plot.width=12, repr.plot.height=12)

fancyRpartPlot(rpart.tree)

colnames(df)

# Delete the following columns
delete <- c("Generation", "Educational_Levels", "CatYearsManager")
df[delete] <- NULL
head(df)

h2o.init()

# Putting the original dataframe into an h2o format
h2o_df <- as.h2o(df)

# Splitting into training, validation and testing sets
split_df <- h2o.splitFrame(h2o_df, c(0.7, 0.15), seed=12)

# Obtaining our three types of sets into three separate values
h2o_train <- h2o.assign(split_df[[1]], "train")
h2o_validation <- h2o.assign(split_df[[2]], "validation")
h2o_test <- h2o.assign(split_df[[2]], "test")

h2o.describe(h2o_train)

# Establish X and Y (Features and Labels)
y <- "Attrition"
x <- setdiff(names(h2o_train), y)

auto_ml <- h2o.automl(
  y = y,
  x = x,
  training_frame = h2o_train,
  leaderboard_frame = h2o_validation,
  project_name = "Attrition",
  max_models = 10,
  seed = 12
)

# Check for the top models
top_models <- auto_ml@leaderboard
print(top_models)

# Get the best model
# Our aim is to determine the feature importance
model_id <- as.data.frame(top_models$model_id)[,1]
best_family <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_id, value=TRUE)[1])
obtain_model <- h2o.getModel(best_family@model$metalearner$name)

# How important is each model to the StackEnsemble
h2o.varimp(obtain_model)

#Best ML Model to use
options(repr.plot.width=8, repr.plot.height=4)

GBM <- h2o.getModel(grep('GBM', model_id, value = TRUE)[1])

# Examine the variable importance of the top XGBoost model
# XGBoost can show the feature importance as oppose to the stack ensemble
h2o.varimp(GBM)
h2o.varimp_plot(obtain_model)

# We can also plot the base learner contributions to the ensemble.
h2o.varimp_plot(GBM)
