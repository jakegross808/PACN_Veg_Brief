#===================== FTPC Large Trees =============
#...................................................#
#...US National Park Service........................#
#...Pacific Island Inventory & Monitoring Network...#
#...................................................#
#...Jacob Gross 04/01/2021..........................#
#...................................................#
#...Briefing report.................................#
#...FTPC - Focal Terrestrial Plant Communities......#
#...................................................#
#........ Vegetation: Lrg, Small, Seedling Trees....#
#...................................................#
#===================================================#


#'* NOTE - Before running this script: * 
#'* Export FTPC data from database using script 01_FTPC_DB_YYYYMMDD *
#'* All PACN vegetation R scrips are located on I drive: *
#'* I:\vital_signs\05_focal_terr_plant_communities\Documents\R_scripts *


#.-----------------------------------------------------
#   Packages  ---- 
#.......................................................
library(gridExtra)
library(ggrepel)
library(tidytext)
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
Lg_Trees <- read_csv(here("data/FTPC_Export", DB_download, "Lg_Trees.csv"))
Seedlings <- read_csv(here("data/FTPC_Export", DB_download, "Seedlings.csv"))
Event <- read_csv(here("data/FTPC_Export", DB_download,"Event.csv"))

# Add folders for tables (tbls) and figures (figs), if not already created.
figs <- here(DB_download, "tbls")
tbls <- here(DB_download, "figs")

dir.create(tbls, showWarnings = FALSE)
dir.create(figs, showWarnings = FALSE)

#.-----------------------------------------------------
#   Apply Site Filter Function ---- 
#.......................................................
# User selected subsets
levels(as.factor(Event$Unit_Code))
uc <- "AMME"

levels(as.factor(Event$Community))
c <- "All"

levels(as.factor(Event$Sampling_Frame))
sf <- "All"


Event_filter <- site.filter(Event, uc, c, sf) %>%
  select(S_Year, S_Cycle, Unit_Code, Sampling_Frame, 
         Plot_Type, Plot_Number, Plot_ID, Event_ID)


Sm_Trees_filter <- site.filter(Sm_Trees, uc, c, sf)
Lg_Trees_filter <- site.filter(Lg_Trees, uc, c, sf)
Seedlings_filter <- site.filter(Seedlings, uc, c, sf)

#.-----------------------------------------------------
#   Paired Plots ---- 
#.......................................................

# display total number of fixed and rotational plots in subset
table(Event_filter$Plot_Type)

# Plot totals Fixed and Rotational separate 
plot_totals <- Event_filter %>%
  group_by(S_Cycle, S_Year, Unit_Code, Sampling_Frame, Plot_Type) %>%
  summarise(plot_ss = n(), .groups = 'drop')
#plot_ss = plot sample size
plot_totals 

# Plot totals Fixed and Rotationals combined
plot_totals_all <- Event_filter %>%
  group_by(S_Cycle, S_Year, Unit_Code, Sampling_Frame) %>%
  summarise(plot_ss = n(), .groups = 'drop')
# plot_ss = plot sample size
plot_totals_all

# Which plots are paired matches between years 
fixed_plots <- Event_filter %>%
  filter(Plot_Type=="Fixed") %>%  
  select(-S_Year,-Plot_Type,-Event_ID) %>%
  mutate(Sampled = TRUE) %>%
  complete(S_Cycle, # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           nesting(Unit_Code, Sampling_Frame, Plot_Number, Plot_ID),  
           fill = list(Sampled = FALSE))

# If fixed plots without 2 pairs (non-paired) exist display here
fixed_plots[fixed_plots$Sampled==FALSE,]

# Fixed plots sampled during Cycle 1 and Cycle 2
plots_f_match <- fixed_plots %>%
  pivot_wider(names_from = S_Cycle, values_from = Sampled) %>%
  filter(`1` == TRUE & `2` == TRUE) 
plots_f_match    

paired_plots <- nrow(plots_f_match) 
paste("Number of Paired Plots =", paired_plots)


#.-----------------------------------------------------
#   Fixed Plot Subset ---- 
#.......................................................

Lg_Trees_Fixed <- plots_f_match %>%
  left_join(Lg_Trees_filter,
            by = c("Unit_Code", "Sampling_Frame", 
                   "Plot_Number", "Plot_ID"))

Sm_Trees_Fixed <- plots_f_match %>%
  left_join(Sm_Trees_filter,
            by = c("Unit_Code", "Sampling_Frame", 
                   "Plot_Number", "Plot_ID"))

Seedling_Fixed <- plots_f_match %>%
  left_join(Seedlings_filter,
            by = c("Unit_Code", "Sampling_Frame", 
                   "Plot_Number", "Plot_ID"))

# Prep datasets before merging (row bind)
Lg_Trees_Select <- Lg_Trees_Fixed %>%
  select(S_Cycle, S_Year, Unit_Code, Sampling_Frame, Plot_Number, Life_Form, Quad, 
         Status, Height, Height_Dead, Boles, DBH, DBH_Basal, Vigor, Fruit_Flower, 
         Rooting, Shrublike_Growth, Resprouts, Code, Name, Nativity) %>%
  mutate(Size = ">10") %>%
  mutate(Count = 1)

Sm_Trees_Select <- Sm_Trees_Fixed %>%
  mutate(Size = DBH) %>%
  mutate(DBH = case_when(DBH == "1<5" ~ 1,
                         DBH == "5<10" ~ 5)) 

Seedling_Select <- Seedling_Fixed %>%
  mutate(Size = DBH) %>%
  mutate(DBH = 0) 

Trees_Select <- Lg_Trees_Select %>%
  bind_rows(Sm_Trees_Select) %>%
  bind_rows(Seedling_Select) %>%
  select(names(Lg_Trees_Select)) %>%
  mutate(Plot = as.factor(Plot_Number)) %>%
  mutate(S_Cycle = as.factor(S_Cycle)) %>%
  select(-Plot_Number) %>%
  mutate(Size = fct_relevel(Size, ">10","5<10","1<5", "<1")) %>%
  mutate(Life_Form = case_when(Size == "<1" ~ "Seedlings",
                             Size == "1<5" ~ "Small Trees",
                             Size == "5<10" ~ "Medium Trees",
                             Size == ">10" ~ "Large Trees")) %>%
  mutate(Life_Form = fct_relevel(Life_Form, "Large Trees","Medium Trees","Small Trees", "Seedlings"))

table(Trees_Select$Life_Form)

#.......................................................
#   Lump Spp ---- 
#....................................................... 

Trees <- Trees_Select

# Check Codes/Species
table(Trees$Code)
table(Trees$Name)

# Get unique names
name_code <- Trees %>%
  group_by(Nativity, Name, Code, Life_Form) %>%
  summarize(s.count = sum(Count))

# # # If IDs are uncertain for some species lump by "Code" here:
# Cover <- Cover %>%
#   # Lifeforms:
#   #mutate(Life_form=replace(Life_form, Code=="CIBSP.", "Tree Fern")) %>%
#   
#   # Species:
#   # Epipremnum pinnatum is accepted name ITIS, Bishop, and is introduced/naturalized (cultivar Aureum which originated in the Solomon Islands? (Wagner))  
#   mutate(Code=replace(Code, Code=="EPISP.", "EPIPIN")) %>%
#   mutate(Name=replace(Name, Name=="Epipremnum  sp.", "Epipremnum pinnatum")) %>%

# "Trees" ----
# Dataset is ready for analysis

#.-----------------------------------------------------
#   ***** Sections *****  ----
#......................................................

# Sections start with most general (total trees) and proceed 
#  to most specific (species x plot) 

# ""Trees" dataframe is the main dataset used at beginning of 
#   each following sections: 

# 1) Total Density          (Tot_Dens)
# 2) Nativity Density       (Nat_Dens)
# 3) Species Density        (Spp_Dens)


#.-----------------------------------------------------
# 3)  Species Density  ---- 
#.-----------------------------------------------------

Spp_Dens <- Trees %>%
  #'*Check if should include Dead trees or not!* 
  filter(Status == "Live") %>%
  filter(Code == "BRUGYM") %>%
  group_by(S_Cycle, S_Year, Sampling_Frame, Plot, Life_Form, Size, Status, Code, Name) %>%
  summarise(count_pp = sum(Count)) %>%
  # Calculate trees per ha for each size category
  mutate(count_ha = case_when(Life_Form == "Seedlings" ~ count_pp*100,
                              Life_Form == "Small Trees" ~ count_pp*40,
                              Life_Form == "Medium Trees" ~ count_pp*40,
                              Life_Form == "Large Trees" ~ count_pp*10
                              )) %>%
  droplevels()

Spp_Dens %>%
  ggplot(aes(x=Plot, y=count_ha, fill=S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single", padding = 0))+
  geom_text(aes(label=count_ha), vjust=-0.25, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") + #"Paired"
  #scale_fill_manual(values = c("#736F6E", "#000000")) +
  facet_grid(vars(Life_Form), scales = "free") +
  labs(title="Bruguiera gymnorrhiza",
       x ="Plot", y = "Trees / ha", fill = "Cycle") 


# Change ----
Spp_Dens_Chg <- Spp_Dens %>%
  ungroup() %>%
  select(-count_pp, -S_Year) %>%
  complete(S_Cycle, # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           Sampling_Frame, Name, Code, nesting(Life_Form, Size), Plot, Status,
           fill = list(count_ha = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = count_ha) %>%
  mutate(count_ha_chg = round(`2` - `1`, 2)) 

# Calculate range for count_ha_chg so that it can be plotted correctly in Jitter plot.
Spp_Dens_Chg_range <- Spp_Dens_Chg %>%
  group_by(Sampling_Frame, Name, Life_Form, Size) %>%
  summarize(y_range = max(abs(count_ha_chg))) %>%
  ungroup()
# Add range column to Chg dataset  
Spp_Dens_Chg <- Spp_Dens_Chg %>%
  inner_join(Spp_Dens_Chg_range)

# Prep Data for use in slope plot
Spp_Dens_Chg_Slope <- Spp_Dens_Chg %>%
  mutate(Direction = 
           case_when(count_ha_chg <= 0 ~ "DECREASE",
                     count_ha_chg > 0  ~ "INCREASE")) %>%
  mutate(Direction = as.factor(Direction)) 


Spp_Dens_Chg_Slope %>%
  ggplot() +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Direction), size=.75, show.legend=T) + 
  facet_grid(Life_Form ~ Plot, scales = "free", 
             labeller = labeller(Plot = label_both, Life_Form = label_value)) +
  theme(panel.spacing = unit(1, "lines")) +
  guides(color=guide_legend("")) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  ggtitle(expression(italic("Bruguiera  gymnorrhiza"))) +
  xlab("Year") + ylab("Live trees / ha") +
  scale_x_continuous("Year", breaks = c(1,2), label = c("2014","2019")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_manual(values = c("#333333", "#1B9E77")) # Non-native color = "#D95F02"


# Summary Stats ----

# Use custom function at top of script to add stats to dataset
Spp_Dens_Stats <- add.stats(
  .data =  Spp_Dens_Chg,
  .summary_var = count_ha_chg,
  Sampling_Frame, Life_Form)


# ....... BAR TOTAL MEANS ----

Spp_Dens_Stats %>%
  #filter(S_Cycle != "CHG") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Live trees per ha", x = "Sample Cycle") +
  scale_fill_brewer(palette="Accent") +
  facet_wrap(vars(Life_Form), scales = "free")


#........JITTER PLOT ----
# Total Cover Change jitter plot
p2 <- Spp_Dens_Chg %>%
  ggplot(aes(x =Sampling_Frame, y = count_ha_chg, label = Plot)) +
  geom_blank(aes(y = y_range)) +
  geom_blank(aes(y = -y_range)) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  geom_jitter(width = 0.05) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "red") +
  labs(y = "Change (trees / ha)") +
  facet_wrap(vars(Life_Form), nrow = 1, scales = "free") +
  # geom_text_repel(force=1, point.padding=unit(1,'lines'),
  #                 hjust=1, size = 3,
  #                 direction='x',
  #                 nudge_y=0.1,
  #                 segment.size=0.7) +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
p2

#........STRIP CHRT PAIR -----
p1 <- Spp_Dens %>%
  select(-count_pp) %>%
  ungroup() %>%
  mutate(S_Year = as.factor(S_Year)) %>%
  complete(nesting(S_Cycle, S_Year), # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           Sampling_Frame, Name, Code, nesting(Life_Form, Size), Plot, Status,
           fill = list(count_ha = 0)) %>%
  ggplot(aes(x=S_Year, y=count_ha, group=Plot)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('') +
  ylab('Live trees / ha') +
  facet_wrap(vars(Life_Form), scales = "free", nrow = 1)
p1

grid.arrange(p1, p2, nrow = 2, top = "Bruguiera  gymnorrhiza" 
             #heights = c(2, 1.5)
             )



