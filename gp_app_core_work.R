#This script is written by Anna Khoo for BBC Shared Data Unit
#September 15, 2020.
#Project Github https://github.com/BBC-Data-Unit/gp_face_to_face_appointments/

#GP appointment data to administrative level is at NHS Digital: https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice.
#NB:  Sheets for January and February 2019 are missing row 13. 
#format must be made identical to all other sheets for script to run. 

#load libraries
pacman::p_load(tidyverse, janitor, zoo, readxl)

#find all files in the data folder, make a list
filenames <- list.files(path = "D:/R/Anna R scripts/Experiments/gpapp_update/data")

#from that list, batch load all files found into a list of dataframes
  #working directory must match
setwd("D:/R/Anna R scripts/Experiments/gpapp_update/data")
  #target specific sheet in each workbook in list
gps_raw_data <- lapply(filenames, function(x) read_excel(path= x, sheet = "Table 3c"))

#create bespoke function to clean imported sheets into desired format
tidy_gps <- function(x){
  #take source and add as new column
  #manual stage here to add in month data otherwise lost in join
  #filenames and this cell are only indicator of source sheet
  x$source <- x[2,1]
  #removing first four rows
  x <- x[-1,]
  x <- x[-1,]
  x <- x[-1,]
  x <- x[-1,]
  x <- x %>% 
    row_to_names(1) %>% 
    remove_empty("cols") %>% 
    filter(Type=="CCG") %>%
    #don't need Type anymore
    select(-1) %>% 
    #tidying up the source col
    rename("source"=12) %>%
    #source column is a tibble so need to unnest
    #(or list-col will be a huge headache to work with)
    unnest(cols=c(source)) %>%
    #unnesting makes a mess and a load of duplicate rows:
    distinct()
  return(x)
}

#run tidying function over all dataframes in dataframe list object gps_raw_data and write into single dataframe
data <- do.call(rbind,lapply(seq_along(gps_raw_data), function(x) tidy_gps(gps_raw_data[[x]])))

#alternatively, to preserve names(gps_raw_data), use sapply with simplify=FALSE
#gps_raw_data <- sapply(filenames, function(x) read_excel(path= x, sheet = "Table 3c"), simplify = FALSE)

#alternatively, to create an index column, use bind_rows()
#bind_rows can normally take names(list) and return as index but we've wrapped everything in an lapply that wipes it
#data <- bind_rows(sapply(names(gps_raw_data), function(x) tidy_gps(gps_raw_data[[x]])), .id="id")
#we'll manually create a source col for a work around, it's that or pre-order all the files...


#clean up source col:

unique(data$source)

data$source <- gsub("Table 3c: Appointments by Mode, at National, Regional, STP and CCG level, England, ","", data$source)
data$source <- gsub("Table 3c: Appointments by Mode, at National, Regional, Sub-Regional, STP and CCG level, England, ","", data$source)
data$source <- gsub("Table 3c: Appointments by Mode, at National, Regional, Regional Local Office, STP and CCG level, England, ","", data$source)
data$source <- gsub("20203", "2020", data$source)
data$source <- gsub("20193", "2019", data$source)

#analysis and reshaping

#checks
#names(data)
#str(data)

#convert applicable columns to numeric
convert_cols <- c(4:11)
library(magrittr)
#double pipe from magrittr (assign <-  and pipe %>% on same object) is most legible here
data[,convert_cols] %<>% lapply(function(x) as.numeric(as.character(x)))

#write out imported data for checks
#setwd("D:/R/Anna R scripts/Experiments/gpapp_update")
#write_csv(data, "gp_app_data_ak_raw.csv")


#create by mode sheet

#options(scipen=999)

data_by_mode <- data %>%
  pivot_longer(cols=7:11,
               names_to="app_mode",
               values_to="app_count") %>% 
  #calc percentage
  mutate(mode_percent=(app_count/Total)*100)

data_in_person <- data %>%
  mutate(app_in_person=(`Face-to-Face`+`Home Visit`)) %>% 
  mutate(app_not_in_person=(Telephone+`Video/Online`))

names(data_in_person)

#reshaping
tidy_inc_total <- data_in_person %>%
  select(1:3,12,6,11,13,14) %>% 
  pivot_longer(cols=5:8,
               names_to="app_in_person?",
               values_to="app_count") %>% 
  separate(source, c("month", "year"), " ")

pivot_person <- tidy_inc_total  %>% 
  group_by(month, year, `app_in_person?`) %>% 
  summarise(total_app=sum(app_count, na.rm=T)) %>% 
  filter(year=="2020")

#plot
pivot_person$month <- factor(pivot_person$month, levels= month.name)
plot_in_person <- ggplot(pivot_person, aes(x = month, y = total_app, group=`app_in_person?`, colour = `app_in_person?`)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1", "#1380F4", "#333333"))
plot_in_person

##Year-on-year data

comparable_months=c("January", "February", "March", "April", "May", "June", "July")

data_by_mode <- data_by_mode %>% 
  separate(source, c("month", "year"), " ")

years_face_to_face <- data_by_mode %>%
  group_by(month, year, app_mode) %>%
  summarise(total_app=sum(app_count, na.rm=T)) %>%
  filter(app_mode=="Face-to-Face") %>% 
  filter(month %in% comparable_months) %>% 
  select(-app_mode)

years_face_to_face$month <- factor(years_face_to_face$month, levels= month.name)
  
plot_yr_on_year <- ggplot(years_face_to_face, aes(x=month, y = total_app, group=year, colour=year)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  labs(title = "Face-to-face GP appointments",
       subtitle = "Number of times GPs met patients in England")
plot_yr_on_year

###handed over to BBC England Data Unit for further formatting###

############BELOW_SDU ONLY#########################


#create sheets to share:

names(data_by_mode)

all_data <- data_by_mode %>% 
  select(`NHS Area Code`, `ONS Code`, Name, source, app_mode, app_count, Total, mode_percent) %>% 
  rename(CCG_Name=Name, appointment_type=app_mode, appointment_type_count=app_count, all_appointments=Total, appointment_type_percent=mode_percent) %>% 
  separate(source, c("month", "year"), " ") %>% 
  mutate(month=factor(month, levels= month.name)) %>% 
  arrange(CCG_Name, month)

face_to_face_app <- all_data %>% 
  filter(appointment_type=="Face-to-Face") %>% 
  select(-appointment_type, -all_appointments, -year) %>%
  rename(number=appointment_type_count, percent=appointment_type_percent) %>% 
  pivot_wider(names_from=month, 
              values_from=c(number, percent))

#creating conditional formatting flag for unknown app>10%
unknown_app <- all_data %>% 
  filter(appointment_type=="Unknown", year=="2020") %>% 
  select(-appointment_type, -all_appointments, -year, -appointment_type_count) %>%
  rename(percent=appointment_type_percent) %>%
  mutate(flag=percent>10) %>% 
  pivot_wider(names_from=month, 
              values_from=c(flag, percent)) %>% 
  #now we know the test is working, drop percent cols:
  select(1:10)

flagged_sheet <- left_join(face_to_face_app, unknown_app, by=c("NHS Area Code", "ONS Code", "CCG_Name"))

require(openxlsx)
export_list <- list("Face-to-Face_appointments" = flagged_sheet, "full_data" = all_data)
getwd()
setwd("D:/R/Anna R scripts/Experiments/gpapp_update")
write.xlsx(export_list, file = "gp_appointments.xlsx",keepNA = TRUE)


#draft only for total year on year:

comparable_months=c("January", "Februrary", "March", "April", "May", "June", "July")
year_pivot <- tidy_inc_total  %>% 
  group_by(month, year, `app_in_person?`) %>%
  summarise(total_app=sum(app_count, na.rm=T)) %>% 
  filter(`app_in_person?`=="Total") %>% 
  filter(month %in% comparable_months)

year_pivot$month <- factor(year_pivot$month, levels= month.name)
plot_yr_on_year <- ggplot(year_pivot, aes(x=month, y = total_app, group=year, colour=year)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333")
plot_yr_on_year

