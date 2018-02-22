
library(dplyr)
library(ggplot2)
library(openxlsx)

Master_sheet <- read.xlsx("assignment.xlsx", sheet = 1)
Data_sheet <- read.xlsx("assignment.xlsx", sheet = 2)

head(Master_sheet)
summary(Master_sheet)
colnames(Master_sheet)


head(Data_sheet)


sapply(Data_sheet,function(x)sum(is.na(x)))
#output 

sapply(Master_sheet,function(x)sum(is.na(x)))




#converting all district names to upper case

Data_sheet$District <- toupper(Data_sheet$District) 

# removing whitespaces
Data_sheet$District <- (str_trim(Data_sheet$District))

# ANANTAPUR and ANANTHAPUR are same --> ANANTAPUR
#CHITTOOR and CHITTOROR are same --> CHITTOOR
r = Data_sheet[, "District"] == "ANANTHAPUR"
Data_sheet[r,"District"] = "ANANTAPUR"

#CHITTOOR and CHITTOROR are same --> CHITTOOR
r = Data_sheet[, "District"] == "CHITTOROR"
Data_sheet[r,"District"] = "CHITTOOR"

table(Data_sheet$District)

##1. Compare Master Sheet with Data Sheet in the Assignment Work Book take UDISE Code 
#as unique code for every school and fill enrollment for Data Sheet from Master Sheet
COUNT = 1
for (x in Data_sheet$School.UDISE.Code){
r =  Master_sheet[,"SCHOOL.UDISE.CODE"] == x 
Data_sheet[COUNT,"Enrollment"] <- Master_sheet[r,"Enrolment"]
COUNT = COUNT +1 
}


#2.Compare Master Sheet with Data Sheet in the Assignment Work Book take UDISE Code as
#unique code for every school and fill School Category for Data Sheet from Master Sheet

colnames(Data_sheet)
COUNT = 1
for(x in Data_sheet$School.UDISE.Code){
  r = Master_sheet[,"SCHOOL.UDISE.CODE"] == x
  Data_sheet[COUNT, "School_Category"] <- Master_sheet[r, "SCHOOL.CATEGORY"]
  COUNT = COUNT +1 
}

#3. Visualize the DATA Sheet portraying Parameters like how many schools per District, Total 
#Impact in terms of enrollment per district, How many Primary , how many primary with Upper Primary,
#how many upper primary with secondary schools overall (School Category)  
colnames(Data_sheet)

# How many schools per district 
table(Data_sheet$District)
Schools_District <- as.factor(Data_sheet$District)


#barplot(table(Schools_District))
#plot(table(Schools_District))
#histogram(Schools_District)

ggplot(data = Data_sheet, aes(x = Data_sheet$District)) +
  xlab("District")+
  ggtitle("No. of Schools per district") +
  stat_count() +
  theme(axis.text.x = element_text(size  = 10,angle = 45, hjust = 1,vjust = 1))
  

#Total Impact in terms of enrollment per district
Enrol_District <- Data_sheet %>% 
                  group_by(District) %>%
                  summarise(Total_Enrollments = sum(Enrollment))

Enrol_District = as.data.frame(Enrol_District)


ggplot(data = Enrol_District, aes(x=factor(Enrol_District$District), y = Enrol_District$Total_Enrollments))+
  geom_bar(stat = 'identity')+
  xlab("District")+
  ylab("Total No. of Enrollments")+
  ggtitle("No. of Enrollments per district") +
  theme(axis.text = element_text(size = 10, angle = 45, hjust = 1, vjust = 1))

# Districtwise School categories 

(School_cat_district <- Data_sheet %>%
                       group_by(District) %>%
                       summarise(Primary = sum(School_Category == '1-Primary'),
                        Primary_Upper_Primary = sum(School_Category == "2-Primary with Upper Primary"),
                        Upper_Prim_Secondary = sum(School_Category=="7-Upper Pr. and Secondary")                        ) 
  )
School_cat_district = as.data.frame(School_cat_district)
ggplot(data = School_cat_district, aes(x=factor(School_cat_district$District), y = School_cat_district$Primary))+
  geom_bar(stat = 'identity')+
  xlab("District")+
  ylab("Count")+
  ggtitle("District wise Primary Schools") +
  theme(axis.text = element_text(size = 10, angle = 45, hjust = 1, vjust = 1))

ggplot(data = School_cat_district, aes(x=factor(School_cat_district$District), y = School_cat_district$Primary_Upper_Primary))+
  geom_bar(stat = 'identity')+
  xlab("District")+
  ylab("Count")+
  ggtitle("District wise Primary with upper Primary Schools") +
  theme(axis.text = element_text(size = 10, angle = 45, hjust = 1, vjust = 1))

ggplot(data = School_cat_district, aes(x=factor(School_cat_district$District), y = School_cat_district$Upper_Prim_Secondary))+
  geom_bar(stat = 'identity')+
  xlab("District")+
  ylab("Count")+
  ggtitle("District wise Upper Primary & Secondary Schools") +
  theme(axis.text = element_text(size = 10, angle = 45, hjust = 1, vjust = 1))

