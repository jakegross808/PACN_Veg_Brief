##===================== TITLE =========================
##...................................................##
##...US National Park Service........................##
##...Pacific Island Inventory & Monitoring Network...##
##...................................................##
##...Jacob Gross 01/22/2021..........................##
##...................................................##
##...Briefing report.................................##
##...FTPC - Focal Terrestrial Plant Communities......##
##...................................................##
##....... Understory Cover Stats and Graphs..........##
##...................................................##


## Used "01_FTPC_DB_20201203.r" to prepare .csv files


#.-----------------------------------------------------
#   Packages  ---- 
#.......................................................
#library(gridExtra)
#library(ggrepel)
#library(tidytext)
library(lubridate)
library(here)
library(tidyverse)

#.-----------------------------------------------------
#   Custom Functions  ---- 
#.......................................................

site.filter <- function(x, UC = "All", C = "All", SF = "All"){
  y <- x %>%
    mutate(S_Year = year(Start_Date)) %>%
    # Create column for Sample Cycle 
    mutate(S_Cycle = case_when(S_Year <= 2014 ~ "1",
                               S_Year >= 2015 & S_Year < 2020 ~ "2",
                               S_Year > 2020 ~ "3")) %>%
    # Remove QA Plots
    filter(QA_Plot == FALSE) %>%
    # Apply site filters
    {if(C!="All") filter(.,Community == C) else .} %>%
    {if(SF!="All") filter(.,Sampling_Frame == SF) else .} %>%
    {if(UC!="All") filter(.,Unit_Code == UC) else .} 
  
  return(y)
  
}

add.stats <- function(.data, .summary_var, ...){
  # dataset needs to have columns: "Unit_Code, Sampling_Frame, Strata"
  # column is the dataset$column that shows the change between two cycles
  .summary_var <- enquo(.summary_var)
  #.group_vars <- enquos(...)
  
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(NPLOTS = n(),
                     MEAN_CHG = round(mean(!!.summary_var),3),
                     MED_CHG = round(median(!!.summary_var),3),
                     MIN_CHG = round(min(!!.summary_var),3),
                     MAX_CHG = round(max(!!.summary_var),3),
                     SD_CHG = sd(!!.summary_var),
                     ERR_CHG = qt(0.975,df=NPLOTS-1)*(SD_CHG/sqrt(NPLOTS)),
                     L_CHG = MEAN_CHG - ERR_CHG,
                     R_CHG = MEAN_CHG + ERR_CHG,
                     MEAN_1 = round(mean(`1`),3),
                     MED_1 = round(median(`1`),3),
                     MIN_1 = round(min(`1`),3),
                     MAX_1 = round(max(`1`),3),
                     SD_1 = sd(`1`),
                     ERR_1 = qt(0.975,df=NPLOTS-1)*(SD_1/sqrt(NPLOTS)),
                     L_1 = MEAN_1-ERR_1,
                     R_1 = MEAN_1+ERR_1,
                     MEAN_2 = round(mean(`2`),3),
                     MED_2 = round(median(`2`),3),
                     MIN_2 = round(min(`2`),3),
                     MAX_2 = round(max(`2`),3),
                     SD_2 = sd(`2`),
                     ERR_2 = qt(0.975,df=NPLOTS-1)*(SD_2/sqrt(NPLOTS)),
                     L_2 = MEAN_2-ERR_2,
                     R_2 = MEAN_2+ERR_2,) %>%
    tidyr::pivot_longer(
      # period (.) means match any character
      # asterisk (*) means match zero or more times
      # use () to distinguish groups
      # therefore the following breaks apart two words separated by underscore (_)
      cols = MEAN_CHG:R_2,
      names_to = c(".value", "S_Cycle"), 
      names_pattern = "(.*)_(.*)") %>%
    dplyr::arrange(S_Cycle)
  
}

#.-----------------------------------------------------
#   Loading Data  ---- 
#.......................................................

list.files()
list.files("data/FTPC_Export")

# Select last folder in FTPC_Export
DB_download <- tail(list.files("data/FTPC_Export"), n=1)
print(DB_download)


# Read tables
Presence <- read_csv(here("data/FTPC_Export", DB_download, "Presence.csv"))
Event <- read_csv(here("data/FTPC_Export", DB_download, "Event.csv"))

# Add folders for tables (tbls) and figures (figs), if not already created.
figs <- here("tbls")
tbls <- here("figs")

dir.create(tbls, showWarnings = FALSE)
dir.create(figs, showWarnings = FALSE)


#.-----------------------------------------------------
#   Apply Site Filter Function ---- 
#.......................................................
# User selected subsets
levels(as.factor(Event$Unit_Code))
uc <- "HAVO"

levels(as.factor(Event$Community))
c <- "All"

levels(as.factor(Event$Sampling_Frame))
sf <- "All"


Event_filter <- site.filter(Event, uc, c, sf) %>%
  select(S_Year, S_Cycle, Unit_Code, Sampling_Frame, 
         Plot_Type, Plot_Number, Plot_ID, Event_ID)


Presence_filter <- site.filter(Presence, uc, c, sf)
