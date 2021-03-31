#===================== FTPC Large Trees =============
#...................................................#
#...US National Park Service........................#
#...Pacific Island Inventory & Monitoring Network...#
#...................................................#
#...Jacob Gross 04/29/2020..........................#
#...................................................#
#...Briefing report.................................#
#...FTPC - Focal Terrestrial Plant Communities......#
#...................................................#
#........ Vegetation Lrg Trees Stats and Graphs.....#
#...................................................#
#===================================================#


#'* NOTE - Before running this script: * 
#'* Export FTPC data from database using script 01_FTPC_DB_YYYYMMDD *
#'* All PACN vegetation R scrips are located on I drive: *
#'* I:\vital_signs\05_focal_terr_plant_communities\Documents\R_scripts *


#.-----------------------------------------------------
#   Packages  ---- 
#.......................................................
library(tidytext)
library(lubridate)
library(here)
library(tidyverse)


#.-----------------------------------------------------
#   DATA  ---- 
#.......................................................

# list database exports available in folder "FTPC_Export". 
list.files("data/FTPC_Export")
#' If function returns *character(0)*, then folder 'FTPC_Export' is empty or 
#' missing, and the rest of the script will not work. To fix, first run 
# script (01_FTPC_DB_YYYYMMDD.R) within project folder. 

# select latest (last) FTPC export
DB_download <- tail(list.files("data/FTPC_Export"), n=1) 
##DB_download <- "FTPC_2020-04-09_1633" # option to manually select folder
print(DB_download) # print version being used



# Data Tables to Load
Sm_Trees <- read_csv(here("data/FTPC_Export", DB_download, "Sm_Trees.csv"))
##sPresent <- read_csv(here("FTPC_Export", DB_download,"Presence.csv"))
Event <- read_csv(here("data/FTPC_Export", DB_download,"Event.csv"))



# Add folders for tables (tbls) and figures (figs), if not already created.
figs <- here(DB_download, "tbls")
tbls <- here(DB_download, "figs")

dir.create(tbls, showWarnings = FALSE)
dir.create(figs, showWarnings = FALSE)

#.-----------------------------------------------------
#   Subset options  ---- 
#.......................................................


## Set to "YES" for all years, "NO" if you only want the last survey
## NOTE: option "NO" is not currently supported in script
## All_years<-"YES" 

# Set filter parameters, e.g. filter "Wet forest" in "HAVO"
levels(as.factor(Event$Community))
C <- "All" #Can query "All" or select Community

levels(as.factor(Event$Sampling_Frame))
SF <- "Mangrove Forest" #Can query "All" or select Sampling Frame 

levels(as.factor(Event$Unit_Code))
UC <- "All" #Can query "All"  "AMME" "HALE" "HAVO" "KAHO" "KALA" "NPSA" "WAPA"

# set range of years to query
YS <- 2010:2019 # for specific year see example -> 2013:2013 

# QA Plots
QA <- "FALSE" #Can query "All"  "TRUE"  "FALSE"



#.-----------------------------------------------------
#   Sampling Events  ---- 
#.......................................................


# Create new columns and filter Events according to subset options selected above
Event_filter <- Event %>%
  # pull out year from column "Start_Date" and put in new column "S_Year"
  mutate(S_Year = year(Event$Start_Date)) %>%
  # create column called "S_Cycle" to group by sampling cycle 
  # 2010-2014 = 1; 2015-2019 = 2; 2021-2025 = 3; etc.  
  mutate(S_Cycle = case_when(S_Year <= 2014 ~ "1",
                             S_Year >= 2015 & S_Year < 2020 ~ "2",
                             S_Year > 2020 ~ "3")) %>%
  # filter according to selections made from above in "Subset options"
  {if(UC!="All") filter(.,Unit_Code == UC) else .} %>%
  {if(C!="All") filter(.,Community == C) else .} %>%
  {if(SF!="All") filter(.,Sampling_Frame == SF) else .} %>%
  filter(QA_Plot == QA) %>%
  # drop unneeded columns from original "Event" table
  select(S_Year, S_Cycle, Unit_Code, Sampling_Frame, 
         Plot_Type, Plot_Number, Plot_ID, Event_ID)
# display total number of fixed and rotational plots in subset
table(Event_filter$Plot_Type)


# Plot totals Fixed and Rotational seperate 
plot_totals <- Event_filter %>%
  group_by(S_Cycle, S_Year, Unit_Code, Sampling_Frame, Plot_Type) %>%
  summarise(plot_ss = n()) # plot_ss = plot sample size
plot_totals


# Plot totals Fixed and Rotationals combined
plot_totals_all <- Event_filter %>%
  group_by(S_Cycle, S_Year, Unit_Code, Sampling_Frame) %>%
  summarise(plot_ss = n()) # plot_ss = plot sample size
plot_totals_all


# Which plots are paired matches between years 
fixed_plots <- Event_filter %>%
  filter(Plot_Type=="Fixed") %>%  
  select(-S_Year,-Plot_Type,-Event_ID) %>%
  mutate(Sampled = TRUE) %>%
  complete(S_Cycle, # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           nesting(Unit_Code, Sampling_Frame, Plot_Number),  
           fill = list(Sampled = FALSE))

# If fixed plots without 2 pairs (non-paired) exsist display here
fixed_plots[fixed_plots$Sampled==FALSE,]

# Fixed plots sampled during Cycle 1 and Cycle 2
plots_f_match <- fixed_plots %>%
  pivot_wider(names_from = S_Cycle, values_from = Sampled) %>%
  filter(`1` == TRUE & `2` == TRUE) 
plots_f_match    

paste("Number of Paired Plots =", nrow(plots_f_match))


#.-----------------------------------------------------
#   Prep Data  ---- 
#.......................................................

Sm_Trees <- plots_f_match %>%
  #filter only records matching plots_f_match 
  inner_join(Sm_Trees, 
             by = c("Unit_Code", "Sampling_Frame", 
                    "Plot_Number", "Plot_ID")) %>%
  mutate(S_Year = year(Start_Date)) %>%
  # create column called "S_Cycle" to group by sampling cycle 
  # 2010-2014 = 1; 2015-2019 = 2; 2021-2025 = 3; etc.  
  mutate(S_Cycle = case_when(S_Year <= 2014 ~ "1",
                             S_Year >= 2015 & S_Year < 2020 ~ "2",
                             S_Year > 2020 ~ "3")) %>%
  select(S_Cycle, Sampling_Frame, Plot_Number, DBH, Status, Count, Code, Name)

 

         
#.-----------------------------------------------------
#   Small Tree Density  ---- 
#.-----------------------------------------------------

Sm_Trees_pp <- Sm_Trees %>%
  mutate(Plot = Plot_Number) %>%
  filter(Code == "BRUGYM") %>%
  # Very few "Dead" small trees
  filter(Status == "Live") %>%
  group_by(S_Cycle, Sampling_Frame, Plot, DBH, Status, Code, Name) %>%
  summarise(Trees_pp = sum(Count)) %>%
  mutate(Trees_ha = Trees_pp*40)

Sm_Trees_pp$Plot<-as.factor(Sm_Trees_pp$Plot)
Sm_Trees_pp$S_Cycle<-as.factor(Sm_Trees_pp$S_Cycle)

Sm_Trees_pp %>%
  ggplot(aes(x=Plot, y=Trees_pp, fill=S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=Trees_pp), vjust=-0.25, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") + #"Paired"
  #scale_fill_manual(values = c("#736F6E", "#000000")) +
  facet_grid(vars(DBH)) +
  labs(title="Bruguiera gymnorrhiza",
       x ="Plot", y = "Count", fill = "Cycle") 

ltrees <- Lg_tree_sp %>%
  ungroup() %>%
  mutate(Trees_ha = indiv*10) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Size = ">10") %>%
  select(S_Cycle, Sampling_Frame, Plot, Code, Name, Size, Trees_ha)
trees <- Sm_Trees_pp %>%
  ungroup() %>%
  mutate(Size = DBH) %>%
  select(S_Cycle, Sampling_Frame, Plot, Code, Name, Size, Trees_ha) %>%
  mutate(Plot = as.factor(Plot)) %>%
  bind_rows(ltrees)

trees %>%
  mutate(Size = fct_relevel(Size, ">10","5<10","1<5")) %>%
  ggplot(aes(x=Plot, y=Trees_ha, fill=S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  #geom_text(aes(label=Trees_ha), vjust=1.6, color="white",
  #          position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") + #"Paired"
  #scale_fill_manual(values = c("#736F6E", "#000000")) +
  facet_grid(vars(Size), scales = "free") +
  labs(title="Bruguiera gymnorrhiza",
       x ="Plot", y = "Trees / ha", fill = "Cycle") 


# Change ----
trees_Chg <- trees %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           Sampling_Frame, Plot, Name, Code, Size,  
           fill = list(Trees_ha = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = Trees_ha) %>%
  mutate(Trees_ha_chg = round(`2` - `1`, 2))

Tree_Slope_X <- trees_Chg %>%
  #filter(`1` > 0 | `2` > 0) %>%
  #mutate(Plot = Plot_Number) %>%
  #mutate(Understory = str_sub(Strata,-1,-1)) %>%
  mutate(Direction = case_when(Trees_ha_chg < 0 ~ "DECREASE",
                               Trees_ha_chg >= 0  ~ "INCREASE")) %>%
  #mutate(code_lab = case_when(`1` >= 5 ~ Code,
  #                            `2` >= 5 ~ Code,
  #                            TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) 

Tree_Slope_X %>%
  mutate(Size = fct_relevel(Size, ">10","5<10","1<5")) %>%
  ggplot() +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Direction), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Tree_Slope_X$`1`, Tree_Slope_X$`2`)))) +
  facet_grid(vars(Size), vars(Plot), scales = "free", labeller = label_both) +
  geom_text_repel(label=Tree_Slope_X$Code,
                  y=Tree_Slope_X$`1`, x=rep(1, NROW(Tree_Slope_X)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2014", x=1, y=1.1*(max(Tree_Slope_X$`1`, Tree_Slope_X$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2019", x=2, y=1.1*(max(Tree_Slope_X$`1`, Tree_Slope_X$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_manual(values = c("#CC0000", "#009900"))

















# Add missing values
Lg_tree_pp_zeros <- plots_f_match %>%
  mutate(Dead = TRUE,
         Live = TRUE) %>%
  pivot_longer(cols = c(Dead, Live),
               names_to = "Status", 
               values_to = "foo") %>%
  pivot_longer(cols = c(`1`, `2`),
               names_to = "S_Cycle") %>%
  select(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Status) %>%
  full_join(Lg_trees_pp) %>%
  mutate(Trees_pp = replace_na(Trees_pp, 0)) %>%
  mutate(Trees_ha = Trees_pp*10) %>%  
  mutate(Year = case_when(S_Cycle == "1" ~ '2014',
                          S_Cycle == "2" ~ '2019')) 

#write_csv(Lg_tree_pp_zeros, "Lg_tree_pp_zeros.csv")

#  .... Density: Trees per plot ----
Lg_tree_pp_zeros %>%
  filter(Status == "Live") %>%
ggplot(aes(Trees_ha, fill = Year)) +
  geom_density(alpha = 0.3) 


#  .... Stripchart: Trees/ha ----
Lg_tree_pp_zeros %>%
  mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=Year, y=Trees_ha, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.1)) +
  geom_point(aes(fill=Year), shape = 21, stroke = 0.5, size=3, color = "black", position=position_dodge(width=0.1)) +
  xlab('Year') +
  ylab('Trees/ha') +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4"), guide=FALSE) + 
  #theme_bw() +
  facet_grid(cols = vars(Status)) 


#  .... Bar: Trees per plot----
Lg_tree_pp_zeros %>%
  mutate(foo = fct_reorder2(Trees_pp, as.factor(Plot_Number))) %>%
  ggplot(aes(x = as.factor(Plot_Number), 
             y = foo, fill = S_Cycle)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette="Paired") +
  scale_x_reordered() +
  labs(x ="Plot", y = "Trees", fill = "Sample Cycle") +
  facet_wrap( ~Status, scales = 'free_x', dir = "h")



# CHANGE
Lg_tree_pp_chg <- Lg_tree_pp_zeros %>%
  select(-Trees_ha, -Year) %>%
  #group_by(add = TRUE, Year) %>%
  pivot_wider(names_from = S_Cycle, values_from = Trees_pp) %>%
  mutate(Trees_pp_chg = `2` - `1`) 

plot_to_ha <- function(x, na.rm = FALSE) (x * 10)
Lg_tree_ha_chg <- Lg_tree_pp_chg %>%
  mutate_at(vars(`1`:Trees_pp_chg), plot_to_ha) %>%
  filter(Status == "Live")

dens_mean <- mean(Lg_tree_ha_chg$Trees_pp_chg) 
dens_med <- median(Lg_tree_ha_chg$Trees_pp_chg)

#  .... Density: Trees per plot ----
Lg_tree_ha_chg %>%
  filter(Status=="Live") %>%
  ggplot(aes(Trees_pp_chg)) +
  geom_density(alpha = 0.3) +
  labs(x = "difference in Trees/ha") +
  geom_vline(xintercept=dens_mean, size=1, color="red") +
  geom_vline(xintercept=dens_med, size=0.5, color="black", linetype = "dashed")

#  .... Bar: chg in trees per plot----
Lg_tree_pp_chg %>%
  ggplot(aes(x = reorder_within(as.factor(Plot_Number), -Trees_pp_chg, Status), y = Trees_pp_chg)) +
  geom_bar(stat="identity") +
  scale_x_reordered() +
  facet_wrap( ~Status, scales = 'free_x', dir = "h")
  #facet_grid(cols = vars(Status))

#  .... Stripchart: Trees/ha ----
Lg_tree_ha_chg %>%
  mutate(Status = fct_rev(Status)) %>%
  #filter(Status == "Live") %>%
  ggplot(aes(x = Sampling_Frame, y=Trees_pp_chg)) +
  geom_jitter(width = 0.05) +
  ylab('Difference in Trees/ha') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_summary(fun.y=median, geom="point", shape=95,
               size=8, color="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1) +
  facet_grid(cols = vars(Status)) 

# STATS

Lg_tree_pp_stats_wide <- Lg_tree_pp_chg %>%
  group_by(Unit_Code, Sampling_Frame, Status) %>%
  dplyr::summarise(NPLOTS = n(),
                   MEAN_CHG = round(mean(Trees_pp_chg),3),
                   MED_CHG = round(median(Trees_pp_chg),3),
                   MIN_CHG = round(min(Trees_pp_chg),3),
                   MAX_CHG = round(max(Trees_pp_chg),3),
                   SD_CHG = sd(Trees_pp_chg),
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
                   R_2 = MEAN_2+ERR_2,)


Lg_tree_pp_stats_long <- Lg_tree_pp_stats_wide %>%
  pivot_longer(
    cols = MEAN_CHG:R_2,
    names_to = c(".value", "S_Cycle"),
    # period (.) means match any character
    # asterisk (*) means match zero or more times
    # use () to distinguish groups
    # therefore the following breaks apart two words separated by underscore (_)
    names_pattern = "(.*)_(.*)") %>%
  arrange(S_Cycle) 

plot_to_ha <- function(x, na.rm = FALSE) (x * 10)
Lg_tree_ha_stats <- Lg_tree_pp_stats_long %>%
  mutate_at(vars(MEAN:R), plot_to_ha)

# .... Bar: mean trees per ha ----

Lg_tree_ha_stats %>%
  filter(Status == "Live") %>%
  mutate(Year = case_when(S_Cycle == "1" ~ '2014',
                          S_Cycle == "2" ~ '2019',
                          S_Cycle == "CHG" ~ 'Paired Change')) %>%
  ggplot(aes(x = Year, y = MEAN, fill = Year)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Trees/ha") +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "firebrick3")) 
ggsave(here("figs","Mean_trees_per_ha_change.png")) 


#.-----------------------------------------------------
#   Basal Area  ---- 
#.-----------------------------------------------------

Lg_tree_BA_s <- Lg_tree %>%
  #filter(Code != "HIBTIL") %>%
  select(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Status, DBH) 
 

# Add missing values
Lg_tree_BA_zeros <- plots_f_match %>%
  mutate(Dead = TRUE,
         Live = TRUE) %>%
  pivot_longer(cols = c(Dead, Live),
               names_to = "Status", 
               values_to = "foo") %>%
  pivot_longer(cols = c(`1`, `2`),
               names_to = "S_Cycle") %>%
  select(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Status) %>%
  full_join(Lg_tree_BA_s) %>%
  mutate(DBH = replace_na(DBH, 0)) 

write_csv(Lg_tree_BA_zeros, "Lg_tree_BA_zeros.csv")
# Add Basal Area Calculation  
Lg_tree_BA <- Lg_tree_BA_zeros %>%
  mutate(BAm2 = pi*DBH^2/40000) %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Status, Plot_Number) %>%
  summarise(indiv = n(),
            BA_pp = sum(BAm2)) %>%
  mutate(BAha = BA_pp*10)

#  .... Density: Basal Area per plot ----
Lg_tree_BA %>%
  filter(Status == "Live") %>%
  ggplot(aes(BA_pp, fill = S_Cycle)) +
  geom_density(alpha = 0.3) 


#  .... Bar: Basal Area per plot----
Lg_tree_BA %>%
  ggplot(aes(x = as.factor(Plot_Number), 
             y = BA_pp, fill = S_Cycle)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette="Paired") +
  scale_x_reordered() +
  labs(x ="Plot", y = "Trees", fill = "Sample Cycle") +
  facet_wrap( ~Status, scales = 'free_x', dir = "h")


# CHANGE 
Lg_tree_BA_chg <- Lg_tree_BA %>%
  select(-BAha, -indiv) %>%
  pivot_wider(names_from = S_Cycle, values_from = BA_pp) %>%
  mutate(Trees_BA_chg = `2` - `1`)


#  .... Bar: chg in BA per plot----
Lg_tree_BA_chg %>%
  ggplot(aes(x = reorder_within(as.factor(Plot_Number), -Trees_BA_chg, Status), y = Trees_BA_chg)) +
  geom_bar(stat="identity") +
  scale_x_reordered() +
  facet_wrap( ~Status, scales = 'free_x', dir = "h")
 


# STATS 
Lg_tree_BA_chg_wide <- Lg_tree_BA_chg %>%
  group_by(Unit_Code, Sampling_Frame, Status) %>%
  dplyr::summarise(NPLOTS = n(),
                   MEAN_CHG = round(mean(Trees_BA_chg),3),
                   MED_CHG = round(median(Trees_BA_chg),3),
                   MIN_CHG = round(min(Trees_BA_chg),3),
                   MAX_CHG = round(max(Trees_BA_chg),3),
                   SD_CHG = sd(Trees_BA_chg),
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
                   R_2 = MEAN_2+ERR_2,)


Lg_tree_BA_chg_long <- Lg_tree_BA_chg_wide %>%
  pivot_longer(
    cols = MEAN_CHG:R_2,
    names_to = c(".value", "S_Cycle"),
    # period (.) means match any character
    # asterisk (*) means match zero or more times
    # use () to distinguish groups
    # therefore the following breaks apart two words separated by underscore (_)
    names_pattern = "(.*)_(.*)") %>%
  arrange(S_Cycle) 

Lg_tree_BA_ha_stats <- Lg_tree_BA_chg_long %>%
  mutate_at(vars(MEAN:R), plot_to_ha)


# .... Bar: mean total cover chg ----

Lg_tree_BA_ha_stats %>%
  filter(Status == "Live") %>%
  mutate(Year = case_when(S_Cycle == "1" ~ '2014',
                          S_Cycle == "2" ~ '2019',
                          S_Cycle == "CHG" ~ 'Paired Change')) %>%
  ggplot(aes(x = Year, y = MEAN, fill = Year)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = expression("Basal Area ("~m^2/"ha)")) +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "firebrick3")) 
ggsave(here("figs","Mean_Basal_Area_Change.png")) 













Lg_trees_BA %>%
  filter(Status == "Live") %>%
  ggplot(aes(BA_pp, fill = S_Cycle)) +
  geom_density(alpha = 0.5) 

BA_chg <- BA %>%
  ungroup() %>%
  complete(S_Cycle, Status, Plot,   # AMAZING FUNCTION!!! Complete a data frame with missing combinations of factors
           nesting(Unit_Code, Sampling_Frame),  # nesting = find only the combinations that occur in the data
           fill = list(BAha = 0)) %>%           #supplies a single value to use instead of NA
  select(-S_Year, -indiv, -BA_PLOT) %>%
  pivot_wider(names_from = S_Cycle, values_from = BAha) %>%
  mutate(CHG = `2` - `1`) %>%
  mutate(PCT_CHG = round((CHG/`1`)*100))

#THIS WILL BE USEFUL FOR EASILY ADDING FUTURE CYCLES
#In future create Function here to Shorten and reuse 'summarise' code 
BA_mean <- BA_chg %>%
  group_by(Status, Unit_Code, Sampling_Frame) %>%
  dplyr::summarise(NPLOTS = n(),
            MEAN0 = round(mean(CHG),3),
            MED0 = round(median(CHG),3),
            MIN0 = round(min(CHG),3),
            MAX0 = round(max(CHG),3),
            SD0 = sd(CHG),
            ERR0 = qt(0.975,df=NPLOTS-1)*(SD0/sqrt(NPLOTS)),
            LEFT0 = MEAN0-ERR0,
            RIGHT0 = MEAN0+ERR0,
            MEAN1 = round(mean(`1`),3),
            MED1 = round(median(`1`),3),
            MIN1 = round(min(`1`),3),
            MAX1 = round(max(`1`),3),
            SD1 = sd(`1`),
            ERR1 = qt(0.975,df=NPLOTS-1)*(SD1/sqrt(NPLOTS)),
            LEFT1 = MEAN1-ERR1,
            RIGHT1 = MEAN1+ERR1,
            MEAN2 = round(mean(`2`),3),
            MED2 = round(median(`2`),3),
            MIN2 = round(min(`2`),3),
            MAX2 = round(max(`2`),3),
            SD2 = sd(`2`),
            ERR2 = qt(0.975,df=NPLOTS-1)*(SD2/sqrt(NPLOTS)),
            LEFT2 = MEAN2-ERR2,
            RIGHT2 = MEAN2+ERR2,
            )

BA_mean2 <- BA_mean %>%
  pivot_longer(
    cols = MEAN0:RIGHT2,
    names_to = c(".value", "year"), 
    names_pattern = "(.*)(.)"
    ) %>%
  arrange(Status, year)

BA_chg_mean <- BA_mean2 %>%
  filter(year == 0) 
  
BA_mean_yrs <- BA_mean2 %>%
  filter(year != 0)

ggplot(BA_mean_yrs, aes(x=year, y=MEAN, group=Status, color=Status)) + #Graph to add to for future analysis
  geom_errorbar(aes(ymin=LEFT, ymax=RIGHT), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()
ggsave(here("figs","BA_mean_linegraph.png"))

png(here::here("figs", "BA_mean_noHIBTIL.png"),width=3000,height=1500, res=200)
p <- ggplot(data=BA_mean_yrs, aes(x=Status, y=MEAN, fill=interaction(year, Status))) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_errorbar(aes(ymin=LEFT, ymax=RIGHT), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=MEAN), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_manual(values = c("#736F6E", "#000000", "#A6CEE3" , "#1F78B4")) +
  labs(title="Basal Area per ha (HIBTIL removed)",
       x ="Status", y = "Basal Area / ha", fill = "Year") #+
p 
dev.off()
p

png(here::here("figs", "BA_chg_mean_noHIBTIL.png"),width=3000,height=1500, res=200)
p <- ggplot(data=BA_chg_mean, aes(x=Status, y=MEAN, fill=interaction(year, Status))) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0)) +
  geom_errorbar(aes(ymin=LEFT, ymax=RIGHT), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=MEAN), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_manual(values = c("#000000" , "#1F78B4")) +
  labs(title="Change in Basal Area (HIBTIL removed)",
       x ="Status", y = "Basal Area / ha", fill = "Year") 
p 
dev.off()
p

# BAR CHARTS: All Species ---------------------------------------------------------
#
# LIVE Tree Count 
#
Lg_tree_spp <- Lg_tree %>%
  mutate(Plot = Plot_Number) %>%
  filter(Status == "Live") %>%
  #filter(Plot_Number == 1) %>%
  group_by(S_Year, S_Cycle, Unit_Code, Sampling_Frame, Plot, 
           #Quad, 
           Name, Code, Life_Form, Nativity, 
           #Status
  ) %>%
  summarise(indiv = n()) #%>%

png(here::here("figs", "Lg_tree_spp_all_count.png"),width=3000,height=1500, res=200)
Lg_tree_spp$Plot<-as.factor(Lg_tree_spp$Plot)
Lg_tree_spp$S_Year<-as.factor(Lg_tree_spp$S_Year)
p <- ggplot(data=Lg_tree_spp, aes(x=Code, y=indiv, fill=S_Year)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=indiv), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") + #"Paired"
  #scale_fill_manual(values = c("#736F6E", "#000000")) +
  labs(title="Large Trees (>10 cm DBH, alive)",
       x ="Species", y = "Count", fill = "Year") +
  facet_grid(vars(Plot), labeller = label_both)
p 
dev.off()
p
#ggsave("AMME_Large_Tree_Spp_Count.png")


png(here::here("figs", "Lg_tree_spp_all_count_free.png"),width=3000,height=1500, res=200)
Lg_tree_spp$Plot<-as.factor(Lg_tree_spp$Plot)
Lg_tree_spp$S_Year<-as.factor(Lg_tree_spp$S_Year)
p <- ggplot(data=Lg_tree_spp, aes(x=Code, y=indiv, fill=S_Year)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=indiv), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") + #"Paired"
  #scale_fill_manual(values = c("#736F6E", "#000000")) +
  labs(title="Large Trees (>10 cm DBH, alive)",
       x ="Species", y = "Count", fill = "Year") +
  facet_grid(,vars(Plot),scales = "free", space = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p 
dev.off()
p
#ggsave("AMME_Large_Tree_Spp_Count_free.png")

#
# Dead Tree Count 
#
Lg_tree_spp_d <- Lg_tree %>%
  mutate(Plot = Plot_Number) %>%
  filter(Status == "Dead") %>%
  #filter(Plot_Number == 1) %>%
  group_by(S_Year, S_Cycle, Unit_Code, Sampling_Frame, Plot, 
           #Quad, 
           Name, Code, Life_Form, Nativity, 
           #Status
  ) %>%
  summarise(indiv = n()) #%>%

png(here::here("figs", "Lg_tree_spp_all_count_d.png"),width=3000,height=1500, res=200)
Lg_tree_spp_d$Plot<-as.factor(Lg_tree_spp_d$Plot)
Lg_tree_spp_d$S_Year<-as.factor(Lg_tree_spp_d$S_Year)
p <- ggplot(data=Lg_tree_spp_d, aes(x=Code, y=indiv, fill=S_Year)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=indiv), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  #scale_fill_brewer(palette="Paired") + #"Paired"
  scale_fill_manual(values = c("#736F6E", "#000000")) +
  labs(title="Large Dead Trees (>10 cm DBH, dead)",
       x ="Species", y = "Count", fill = "Year") +
  facet_grid(vars(Plot), labeller = label_both)
p 
dev.off()
p
#ggsave("AMME_Large_Tree_Spp_Count_d.png")

# BAR CHARTS: <select sp.> ---------------------------------------------------------
#
# LIVE Tree Count 
#
Lg_tree_sp <- Lg_tree %>%
  filter(Status == "Live") %>%
  filter(Code == "BRUGYM") %>% # Pick individual Species (ex. BRUGYM)
  group_by(S_Year, S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           #Quad, 
           Name, Code, Life_Form, Nativity, 
           #Status
  ) %>%
  summarise(indiv = n()) #%>%

Lg_tree_sp$Plot_Number<-as.factor(Lg_tree_sp$Plot_Number)
Lg_tree_sp$S_Year<-as.factor(Lg_tree_sp$S_Year)
ggplot(data=Lg_tree_sp, aes(x=Plot_Number, y=indiv, fill=S_Year)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=indiv), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") + #"Paired"
  #scale_fill_manual(values = c("#736F6E", "#000000")) +
  labs(title="Bruguiera gymnorrhiza, Large Trees (>10 cm DBH, alive)",
       x ="Plot", y = "Count", fill = "Year") 
ggsave(here("figs", "BRUGYM_Large_Tree_Sp_Count.png"))

#
# DEAD Tree Count
#
Lg_tree_sp_d <- Lg_tree %>%
  filter(Status == "Dead") %>%
  filter(Code == "BRUGYM") %>%
  group_by(S_Year, S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           #Quad, 
           Name, Code, Life_Form, Nativity, 
           #Status
           ) %>%
  summarise(indiv = n()) #%>%

Lg_tree_sp_d$Plot_Number<-as.factor(Lg_tree_sp_d$Plot_Number)
Lg_tree_sp_d$S_Year<-as.factor(Lg_tree_sp_d$S_Year)
ggplot(data=Lg_tree_sp_d, aes(x=Plot_Number, y=indiv, fill=S_Year)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=indiv), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  #scale_fill_brewer(palette="Paired") + #"Paired"
  scale_fill_manual(values = c("#736F6E", "#000000")) +
  labs(title="Bruguiera gymnorrhiza, Dead Standing Trees (>10 cm DBH)",
          x ="Plot", y = "Count", fill = "Year") 
ggsave(here("figs", "BRUGYM_Large_Tree_Sp_Count_Dead.png"))




# Change ---------------------------------------------------------
#



Tree_count_spp1 <- Lg_tree %>%
  filter(S_Cycle == 1)

Tree_count_spp2 <- Lg_tree %>%
  filter(S_Cycle == 2) %>%
  full_join(Tree_count_spp1, by = c("Unit_Code", "Sampling_Frame", "Plot"
                                    #"Quad", 
                                    #"Name", "Code", "Life_Form", "Nativity",  
                                    #"Status" 
                                    )) %>%
  rename(indiv.2 = indiv.x) %>%
  rename(indiv.1 = indiv.y) %>%
  replace_na(list(indiv.2015 = 0, indiv.1 = 0)) %>%
  mutate(indiv_chg = round(indiv.2 - indiv.1,2)) %>%
  mutate(pct_chg = round(indiv_chg / indiv.1 * 100,2))


min.fun <- function(x) {
  return(x == min(x))
}

max.fun <- function(x) {
  return(x == max(x))
}


Tree_mm <- Tree_count_spp2 %>%
  #filter(Status == "Dead") %>%
  group_by(Sampling_Frame) %>%
  mutate(min = ifelse(min.fun(pct_chg), Plot_Number, as.numeric(NA))) %>%
  mutate(max = ifelse(max.fun(pct_chg), Plot_Number, as.numeric(NA))) %>%
  arrange(min,max)
write_csv(Tree_mm,here("tbls", "Tree_outliers_AMME.csv"))

#Manual Edits
# Tree_mm[3, "min"] <- NA
# Tree_mm[5, "min"] <- NA
# Tree_mm[19, "max"] <- 11

#Tree_mm[which(Tree_mm$Plot_Number==4), "max"] <- 4
#Tree_mm[which(Tree_mm$Plot_Number==11), "max"] <- 11
#Tree_mm[which(Tree_mm$Plot_Number==12), "max"] <- 12
#Tree_mm[which(Tree_mm$Plot_Number==13), "min"] <- NA

# Scatterplot
g <- ggplot(Tree_mm, aes(indiv.1, indiv.2, colour = Sampling_Frame)) +
  geom_point() + 
  geom_abline() + #geom_smooth(method="lm", se=F) +
  ggrepel::geom_label_repel(
    aes(label = min, fill = factor(Sampling_Frame)),
    color = 'black',
    size = 2,
    show.legend = FALSE,
    nudge_x = 8,
    nudge_y = -8
  ) +
  ggrepel::geom_label_repel(
    aes(label = max, fill = factor(Sampling_Frame)),
    color = 'black',
    size = 2,
    show.legend = FALSE,
    nudge_x = -8,
    nudge_y = 8
  ) +
  scale_y_continuous(name="# of large trees (2nd Cycle)", breaks=seq(0,160,10)) +
  scale_x_continuous(name="# of large trees (1st Cycle)", breaks=seq(0,160,10)) +
  labs(subtitle="1st vs. 2nd Monitoring",
       title="AMME Large Trees", 
       caption="20200123")
ggsave(here("figs", "AMME_Large_Tree_Compare.png"))
g


#Tree View - match by nearest DBH --------------------------------------------


Tree_view <- Lg_tree %>%
  select(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, DBH) %>% #group_by
  right_join(plots_f_match, by = c("Unit_Code", "Sampling_Frame", "Plot_Number")) %>%
  #filter(Sampling_Frame == "Wet Forest" & Plot_Number == 7) %>%
  arrange(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, -DBH) %>%
  mutate(Name = fct_explicit_na(Name, na_level = "Unknown")) #%>%
  #m.df$value <- cut_width(m.df$value, width=2, boundary=0)
  
#BY QUAD
Tree_view1 <- Tree_view %>%
  filter(S_Cycle == 1) %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity) %>% 
  arrange(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, -DBH) %>%
  mutate(TID = row_number()) %>%
  select(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, DBH, TID) #%>%

Tree_view2 <- Tree_view %>%
  filter(S_Cycle == 2) %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity) %>% 
  arrange(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, -DBH) %>%
  mutate(TID = row_number()) %>%
  select(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, DBH, TID) #%>%

Tree_vs <- merge(Tree_view1, Tree_view2, by=c("Unit_Code", "Sampling_Frame", "Plot_Number", "Quad", 
                                              "Name", "Status", "Code", "Nativity", "TID"), all =T)

#write.csv(Tree_vs,paste("Tree_vs_Tree_quad.csv",sep=""))

# Tree_view1 <- Tree_view %>%
#   filter(S_Cycle == 1) %>%
#   group_by(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity) %>% 
#   mutate(TID = row_number()) %>%
#   select(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, TID, -DBH)  
# 
# 
# Tree_view2 <- Tree_view %>%
#   filter(S_Cycle == 2) %>%
#   group_by(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity) %>% 
#   mutate(TID = row_number()) %>%
#   select(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, TID, -DBH) 

Tree_view1 <- select(Tree_view1, -DBH)
Tree_view2 <- select(Tree_view2, -DBH)
total1 <- anti_join(Tree_view1, Tree_view2) %>%
  mutate(Recorded = "1st Cycle Only")
total2 <- anti_join(Tree_view2, Tree_view1) %>%
  mutate(Recorded = "2nd Cycle Only")

missing_by_quad <- total1 %>%
  rbind(total2) %>%
  mutate(only_record = case_when(TID==1 ~ 1,TID!=1 ~ 0)) %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, Recorded, only_record)  %>%
  summarise(number_missing = n())
missing_by_quad2  <- missing_by_quad %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Quad, Name, Status, Code, Nativity, Recorded) %>%
  summarize(number_missing = sum(number_missing)) %>%
  left_join(missing_by_quad) %>%
  replace_na(list(only_record = 1)) %>%
  mutate(only_record=replace(only_record, only_record==1, "YES")) %>%
  mutate(only_record=replace(only_record, only_record==0, "")) 

write.csv(missing_by_quad2,paste("missing_lg_trees_quad.csv",sep=""))

#BY Plot
Tree_view1 <- Tree_view %>%
  filter(S_Cycle == 1) %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity) %>% #No Quad
  arrange(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity, -DBH) %>%
  mutate(TID = row_number()) %>%
  select(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity, DBH, TID) #%>%
  
Tree_view2 <- Tree_view %>%
  filter(S_Cycle == 2) %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity) %>% #No Quad
  arrange(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity, -DBH) %>%
  mutate(TID = row_number()) %>%
  select(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity, DBH, TID) #%>%

Tree_vs <- merge(Tree_view1, Tree_view2, by=c("Unit_Code", "Sampling_Frame", "Plot_Number", "Name", 
                                           "Status", "Code", "Nativity", "TID"), all =T)

write.csv(Tree_vs,paste("Tree_vs_Tree.csv",sep=""))


Tree_view1 <- select(Tree_view1, -DBH)
Tree_view2 <- select(Tree_view2, -DBH)
total1 <- anti_join(Tree_view1, Tree_view2) %>%
  mutate(Recorded = "1st Cycle Only")
total2 <- anti_join(Tree_view2, Tree_view1) %>%
  mutate(Recorded = "2nd Cycle Only")

missing_by_plot <- total1 %>%
  rbind(total2) %>%
  mutate(only_record = case_when(TID==1 ~ 1,TID!=1 ~ 0)) %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity, Recorded, only_record)  %>%
  summarise(number_missing = n())
missing_by_plot2  <- missing_by_plot %>%
  group_by(Unit_Code, Sampling_Frame, Plot_Number, Name, Status, Code, Nativity, Recorded) %>%
  summarize(number_missing = sum(number_missing)) %>%
  left_join(missing_by_plot) %>%
  replace_na(list(only_record = 1)) %>%
  mutate(only_record=replace(only_record, only_record==1, "YES")) %>%
  mutate(only_record=replace(only_record, only_record==0, "")) 
    
write.csv(missing_by_plot2,paste("missing_lg_trees_plot.csv",sep=""))
  

