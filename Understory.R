##===================== TITLE =========================
##...................................................##
##...US National Park Service........................##
##...Pacific Island Inventory & Monitoring Network...##
##...................................................##
##...J. Gross & L. Moore 06/09/2021..................##
##...................................................##
##...Briefing report.................................##
##...FTPC - Focal Terrestrial Plant Communities......##
##...................................................##
##....... Understory Cover Stats and Graphs..........##
##...................................................##


#'* NOTE - Before running this script for first time: * 
#'* Export FTPC data from database using script 01_FTPC_DB_YYYYMMDD *
#'* All PACN vegetation R scrips are located on I drive: *
#'* 'I:\vital_signs\05_focal_terr_plant_communities\Documents\R_scripts' *


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
#   Loading Data  ---- 
#.......................................................

list.files()
list.files("data/FTPC_Export")

# Select last folder in FTPC_Export
DB_download <- tail(list.files("data/FTPC_Export"), n=1)
print(DB_download)


# Read tables
Cover_High <- read_csv(here("data/FTPC_Export", DB_download, "Species_coverage_High.csv"))
Cover_Low <- read_csv(here("data/FTPC_Export", DB_download, "Species_coverage_Low.csv"))
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
uc <- "HALE"

levels(as.factor(Event$Community))
c <- "All"

levels(as.factor(Event$Sampling_Frame))
sf <- "Subalpine Shrubland"


Event_filter <- site.filter(Event, uc, c, sf) %>%
  select(S_Year, S_Cycle, Unit_Code, Sampling_Frame, 
         Plot_Type, Plot_Number, Plot_ID, Event_ID)
  
  
Cover_High_filter <- site.filter(Cover_High, uc, c, sf)
Cover_Low_filter <- site.filter(Cover_Low, uc, c, sf)

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

Cov_Fixed_Low <- plots_f_match %>%
  left_join(Cover_Low_filter,
             by = c("Unit_Code", "Sampling_Frame", 
                    "Plot_Number", "Plot_ID"))

Cov_Fixed_High <- plots_f_match %>%
  left_join(Cover_High_filter,
             by = c("Unit_Code", "Sampling_Frame", 
                    "Plot_Number", "Plot_ID"))


Cover_Fixed <- bind_rows(UNDERSTORY2 = Cov_Fixed_High, 
                           UNDERSTORY1 = Cov_Fixed_Low, .id = "Strata") %>%
  mutate(Strata = as.factor(Strata)) %>%
  mutate(Strata = fct_rev(Strata)) %>%
  mutate(Plot_Number = as.factor(Plot_Number)) %>%
  mutate(S_Cycle = as.factor(S_Cycle)) %>%
  filter(is.na(Dead) | Dead == FALSE) %>%  
  select(S_Cycle, S_Year, Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, 
         Transect, Point, Strata, Nativity, Life_form, Code, Name,
         Center_X_Coord, Center_Y_Coord, UTM_Zone, Datum)

#.-----------------------------------------------------
#   Leaflet ---- 
#.......................................................

# Cover currently only has UTM coordinates so have to convert to lat long-----
library(leaflet)
library(terra)

points <- Cover_Fixed %>%
  select(Plot_Number, Center_X_Coord, Center_Y_Coord) %>%
  rename(x = Center_X_Coord) %>%
  rename(y = Center_Y_Coord) %>%
  distinct() 

points.matrix <- points %>%
  select(-Plot_Number) 

points.matrix <- as.matrix(points.matrix)   
v <- vect(points.matrix, crs="+proj=utm +zone=04 +datum=NAD83  +units=m")
y <- project(v, "+proj=longlat +datum=WGS84")


plots.xy <- geom(y)[, c("x", "y")]

plots.xy <- as_tibble(plots.xy) %>%
  bind_cols(points) %>%
  rename(lng = x...1) %>%
  rename(lat = y...2)
  
#------------------------------------------------------------------------

leaflet(plots.xy) %>% 
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(
    radius = 8,
    color = "navy",
    stroke = FALSE, fillOpacity = 0.4,
    label = plots.xy$Plot_Number,
    labelOptions = labelOptions(noHide = T, 
                                direction = "center", 
                                textOnly = T,
                                style = list("color" = "white"))
  )

#.......................................................
#   Lump Spp & Update Spp Info---- 
#....................................................... 

Cover <- Cover_Fixed

#'* If IDs are uncertain for some species lump by "Code" here: *

# Check Codes/Species
table(Cover_Fixed$Code)
table(Cover_Fixed$Name)

# Get unique names
name_code <- Cover %>%
  group_by(Nativity, Name, Code, Life_form) %>%
  summarize(n = n())


#Cover <- Cover %>%
  # Lifeforms:
  #mutate(Life_form=replace(Life_form, Code=="CIBSP.", "Tree Fern")) %>%
  
  # # Species:
  # # Epipremnum pinnatum is accepted name ITIS, Bishop, and is introduced/naturalized (cultivar Aureum which originated in the Solomon Islands? (Wagner))  
  # mutate(Code=replace(Code, Code=="EPISP.", "EPIPIN")) %>%
  # mutate(Name=replace(Name, Name=="Epipremnum  sp.", "Epipremnum pinnatum")) %>%
  # # The majority of Mucuna sp. is likely 'M. ginantea', but other spp. are possible. 
  # mutate(Code=replace(Code, Code=="MUCSPP", "MUCGIG2")) %>%
  # mutate(Name=replace(Name, Name=="Mucuna  gigantea", "Mucuna  sp.")) %>%
  # # The majority of Ipomoea sp. is likely 'I. violacea', but other spp. are possible. 
  # mutate(Code=replace(Code, Code=="IPOVIO", "IPOSP.")) %>%
  # mutate(Name=replace(Name, Name=="Ipomoea  violacea", "Ipomoea  sp.")) %>%
  # # The majority of Ficus sp. is likely 'F. prolixa' & 'F.tinctoria, but other spp. are possible. 
  # mutate(Code=replace(Code, Code=="FICPRO", "FICSP.")) %>%
  # mutate(Name=replace(Name, Name=="Ficus  prolixa", "Ficus  sp.")) %>%
  # mutate(Code=replace(Code, Code=="FICTIN1", "FICSP.")) %>%
  # mutate(Name=replace(Name, Name=="Ficus  tinctoria", "Ficus  sp.")) %>%
  # # The majority of Nephrolepis sp. is likely 'N. hirsutula', but other spp. are possible. 
  # mutate(Code=replace(Code, Code=="NEPHIR", "NEPSP.")) %>%
  # mutate(Name=replace(Name, Name=="Nephrolepis  hirsutula", "Nephrolepis  sp.")) 
  
   
 Cover <- Cover %>%
   # Change Code column: "FESRUB" to "POAPRA" (Cycle 1 only)
   mutate(Code = case_when(
     Code == "FESRUB" & S_Cycle == 1 & Plot_Number == 3 ~ "POAPRA",
     TRUE ~ Code)) %>%
   # Change Name column: "Festuca  rubra" to "Poa  pratensis" (Cycle 1 only)
   mutate(Name = case_when(
     Name == "Festuca  rubra" & S_Cycle == 1 & Plot_Number == 3 ~ "Poa  pratensis",
     TRUE ~ Name)) 
   
 
#   mutate(Code=replace(Code, Code=="SADSOU", "SADSP.")) %>%
#   mutate(Name=replace(Name, Name=="Sadleria  souleyetiana", "Sadleria  sp.")) %>%
#   mutate(Code=replace(Code, Code=="SADCYA", "SADSP.")) %>%
#   mutate(Name=replace(Name, Name=="Sadleria  cyatheoides", "Sadleria  sp.")) %>%
#   mutate(Code=replace(Code, Code=="SADPAL", "SADSP.")) %>%
#   mutate(Name=replace(Name, Name=="Sadleria  pallida", "Sadleria  sp."))
# 
# Cover <- Cover %>%
#   mutate(Code=replace(Code, Code=="STECAL", "STESP.")) %>%
#   mutate(Name=replace(Name, Name=="Stenogyne  calaminthoides", "Stenogyne  sp."))
# 
# Cover <- Cover %>%
#   mutate(Life_form=replace(Life_form, Code=="MELSP.", "Tree")) %>%
#   mutate(Code=replace(Code, Code=="MELSP.", "MELCLU")) %>%
#   mutate(Name=replace(Name, Name=="Melicope  sp.", "Melicope  clusiifolia"))
# 
# Cover <- Cover %>%
#   mutate(Code=replace(Code, Code=="ASPSPH", "ASPSP.2")) %>%
#   mutate(Name=replace(Name, Name=="Asplenium  sphenotomum", "Asplenium  sp.")) 


# Check if Lumped Correctly
name_code_lump <- Cover %>%
  group_by(Nativity, Name, Code, Life_form) %>%
  summarize(n = n())

name_code_lump %>%
  group_by(Name) %>% 
  filter(n()>1)

# If same species being lumped occurs at same point, lump records 
#  together into one hit, so not to double count.

Cover <- Cover %>%
  distinct(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Strata, Nativity, Code, .keep_all = TRUE) 

#.......................................................
#   Remove Nativity Unknown?---- 
#.......................................................

#'*If appropriate, remove records that show Nativity == Unknown*
 
table(Cover$Nativity)

# Display "Unknown" Nativity species
Cover %>%
  filter(Nativity == "Unknown") %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Code, Name) %>%
  summarise(n = n())


# Remove if needed...
# Cover <- Cover %>%  
#  filter(Nativity != "Unknown") 

# If Nativity is wrong update here:

Cover <- Cover %>%
  # #Non-natives:
   mutate(Nativity=replace(Nativity, Code=="Unk_Grass1", "Non-Native")) #%>%
  # mutate(Nativity=replace(Nativity, Name=="Epipremnum pinnatum", "Non-Native")) %>% 
  # mutate(Nativity=replace(Nativity, Name=="Stachytarpheta  sp.", "Non-Native")) %>%
  # #Natives:
  # mutate(Nativity=replace(Nativity, Name=="Hernandia  sp.", "Native")) %>%
  # mutate(Nativity=replace(Nativity, Name=="Ipomoea  sp.", "Native")) %>%
  # mutate(Nativity=replace(Nativity, Name=="Nephrolepis  sp.", "Native")) %>%
  # mutate(Nativity=replace(Nativity, Name=="Thelypteris sp.", "Native")) %>%
  # mutate(Nativity=replace(Nativity, Name=="Mucuna  sp.", "Native")) %>%
  # mutate(Nativity=replace(Nativity, Name=="Ficus  sp.", "Native"))

# Lifeform updates:
Cover %>%
  group_by(Life_form, Nativity) %>%
  summarize(n = n())

Cover %>%
  filter(is.na(Life_form)) %>%
  group_by(Name, Code) %>%
  summarize(n = n())

Cover <- Cover %>%
  mutate(Life_form=replace(Life_form, Code=="PINPAT", "Tree")) %>%
  mutate(Life_form=replace(Life_form, Code=="RYTSEM", "Grass")) %>%
  mutate(Life_form=replace(Life_form, Code=="SANHAL", "Tree"))

# lf.update <- read_csv(here("data", "AMME_lifeform_update.csv")) %>%
#   mutate(LF_update = Life_form) %>%
#   select(-Name, -n, -Life_form) 

# Cover <- Cover %>% 
#   left_join(lf.update, by = "Code") %>%
#   mutate(Life_form = case_when(is.na(Life_form) ~ LF_update,
#                                TRUE ~ Life_form))

Cover %>%
  group_by(Life_form, Nativity) %>%
  summarize(n = n())


# NA updates:
Cover %>%
  filter(is.na(Nativity)) 

Cover <- Cover %>%
  drop_na(Transect)

Cover %>%
  filter(is.na(Nativity)) 

# "Cover" ----
# Dataset is ready for analysis


#.-----------------------------------------------------
#   ***** Sections *****  ----
#......................................................

# Sections start with most general (total cover) and proceed 
#  to most specific (species x plot) 

# ""Cover" dataframe is the main dataset used at beginning of 
#   each following sections: 

# 1) Total % cover            (Tot_Cov)
# 2) Nativity Total % cover   (Nat_Cov)
# 3) Nativity Richness        (Nat_Rich)
# 4) Species % Cover          (Spp_Cov)
# 5) Species Presence         (Spp_Pres)


#.-----------------------------------------------------
# 1) Total % cover ---- 
#.......................................................

# Total can be greater Than 100%
Tot_Cov <- Cover %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Strata) %>%
  # Count hits at each cover point:
  summarise(Hits_All_Sp = n(), .groups = 'drop')  %>%
  # But don't count record if entire plot had no hits: (e.g. transect is NA )
  mutate(Hits_All_Sp = replace(Hits_All_Sp, is.na(Transect), 0)) %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata) %>%
  #Total hits at each point for each strata for entire plot 
  # (can be > 300 points or >100%)
  summarise(tot_pct_cov = (sum(Hits_All_Sp)) / 300 * 100, .groups = 'drop')

#........DENSITY PLOT ---- 
means <- Tot_Cov %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Strata) %>%
  summarise(mean = mean(tot_pct_cov), .groups = 'drop') %>%
  mutate(mean = round(mean,1))

 
ggplot(Tot_Cov, aes(x = tot_pct_cov, fill = S_Cycle)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = means, aes(xintercept = mean, color= S_Cycle)) +
  geom_text(data = means, aes(x = mean, label =mean), 
            y = 0.01, angle = 90, vjust = -0.2, size = 3) +
  scale_fill_brewer(palette="Accent") +
  scale_color_brewer(palette="Accent") +
  facet_grid(rows = vars(Strata), cols = vars(Sampling_Frame)) +
  xlim(0, max(Tot_Cov$tot_pct_cov) + 20)

#........BAR COV/PLOT# ----
Tot_Cov %>%
  ggplot(aes(x = reorder_within(Plot_Number, desc(tot_pct_cov), Strata), y = tot_pct_cov, fill = S_Cycle)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette="Accent") +
  facet_wrap(vars(Strata), dir = "v", scales = "free_x") +
  ylab("Total % Cover") + xlab("Plot Number") +
  scale_x_reordered()

# Change ----
Tot_Cov_Chg <- Tot_Cov %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           nesting(Unit_Code, Sampling_Frame, Plot_Number, Strata),  
           fill = list(tot_pct_cov = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = tot_pct_cov) %>%
  mutate(tot_pct_cov_chg = round(`2` - `1`, 2))

#........BAR CHG/PLOT# ----
Tot_Cov_Chg %>%
  mutate(direction = case_when(tot_pct_cov_chg > 0 ~ "Pos",
                               tot_pct_cov_chg < 0 ~ "Neg" )) %>%
  ggplot(aes(x = reorder_within(Plot_Number, desc(tot_pct_cov_chg), Strata), y = tot_pct_cov_chg, fill = direction)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("#CC0000", "#009900")) +
  #scale_fill_brewer(palette="Accent") +
  #facet_grid(rows = vars(Strata), cols = vars(Sampling_Frame)) +
  facet_wrap(vars(Strata), dir = "v", scales = "free_x") +
  scale_x_reordered() +
  xlab("Plot Number") + ylab("Change in Total % Cover") +
  theme(legend.position = "none")

# Calculate range for count_ha_chg so that it can be plotted correctly in Jitter plot.
Tot_Cov_Chg_range <- Tot_Cov_Chg %>%
  group_by(Sampling_Frame, Strata) %>%
  summarize(y_range = max(abs(tot_pct_cov_chg))) %>%
  ungroup()
# Add range column to Chg dataset  
Tot_Cov_Chg <- Tot_Cov_Chg %>%
  inner_join(Tot_Cov_Chg_range)

#........PAIRED PLOT ----
Tot_Cov %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=S_Cycle, y=tot_pct_cov, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Total % Cover') +
  #scale_fill_brewer(palette="Accent") +
  #scale_color_brewer(palette="Accent") + 
  #theme_bw() +
  facet_grid(rows = vars(Strata))

#........JITTER PLOT ----
# Total Cover Change jitter plot
Tot_Cov_Chg %>%
  ggplot(aes(x =Sampling_Frame, y = tot_pct_cov_chg)) +
  geom_jitter(width = 0.05) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "red") +
  labs(x = "", y = "Change in % Cover") +
  facet_wrap(vars(Strata), dir = "v") 


# Summary Stats ----

# Use custom function at top of script to add stats to dataset
Tot_Cov_Stats <- add.stats(
  .data =  Tot_Cov_Chg,
  .summary_var = tot_pct_cov_chg,
  Unit_Code, Sampling_Frame, Strata)


# ....... BAR TOTAL MEANS ----

Tot_Cov_Stats %>%
  #filter(S_Cycle != "CHG") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Total % Cover", x = "Sample Cycle") +
  scale_fill_brewer(palette="Accent") +
  facet_grid(rows = vars(Strata), cols = vars(Sampling_Frame))

#........JITTER PLOT ----
# Total Cover Change jitter plot
p2 <- Tot_Cov_Chg %>%
  ggplot(aes(x =Sampling_Frame, y = tot_pct_cov_chg, label = Plot_Number)) +
  geom_blank(aes(y = y_range)) +
  geom_blank(aes(y = -y_range)) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  geom_jitter(width = 0.05) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "red") +
  labs(y = "Change (% Cover)") +
  facet_wrap(vars(Strata), nrow = 1, scales = "free") +
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
p1 <- Tot_Cov %>%
  #select(-count_pp) %>%
  ungroup() %>%
  #mutate(S_Year = as.factor(S_Year)) %>%
  complete(nesting(S_Cycle), # Complete a data frame with missing combinations of factors 
           # nesting = find only the combinations that occur in the selected factors
           Sampling_Frame, nesting(Strata), Plot_Number,
           fill = list(tot_pct_cov = 0)) %>%
  ggplot(aes(x=S_Cycle, y=tot_pct_cov, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('') +
  ylab('% Cover') +
  facet_wrap(vars(Strata), scales = "free", nrow = 1)
p1

#........STRIP/JITTER MULTI -----
grid.arrange(p1, p2, nrow = 2, top = "Total Cover" 
             #heights = c(2, 1.5)
)

# ....... Bar: SF compare % cover ----
#' *notes*
# Compare total percent cover of plots to other sampling frames.
#' *Note: subset options may need adjusted to add other sampling frames to graph*

# Tot_Cov_Stats %>%
#   filter(S_Cycle == '2') %>%
#   mutate(highlight = case_when(Unit_Code == "AMME" ~ 'AMME',
#                                Unit_Code != "AMME" ~ 'Other')) %>%
#   ggplot(aes(x = reorder_within(Sampling_Frame, -MEAN, Strata), 
#              y = MEAN, 
#              fill = highlight)) +
#   geom_bar(stat="identity") + #, position = position_dodge()
#   geom_errorbar(aes(ymin=L, ymax=R), width=.2) + #,position=position_dodge(.9)
#   labs(x = "Sampling Frame", y = "% Cover") +  
#   scale_x_reordered() +
#   theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.33)) +
#   guides(fill=FALSE) +
#   scale_fill_manual(values = c("#FF3300", "#736F6E")) +
#   facet_wrap( ~Strata, scales = 'free_x', dir = "v")


 

#.-----------------------------------------------------
# 2) Nativity Total % cover ----
#.......................................................

# Can Total Greater Than 100%

# Calculate Total Percent Cover for Native vs. Non-native
Nat_Cov <- Cover %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Strata, Nativity) %>%
  # Count hits at each cover point:
  summarise(Hits_All_Nat = n(), .groups = 'drop')  %>%
  # But don't count record if entire plot had no hits: (e.g. transect is NA )
  mutate(Hits_All_Nat = replace(Hits_All_Nat, is.na(Transect), 0)) %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, Nativity) %>%
  #Total hits at each point for each strata for entire plot 
  # (can be > 300 points or >100%)
  summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop')

#  ........ BAR COV/PLOT----
Nat_Cov %>%
  ggplot(aes(x = reorder_within(Plot_Number, -tot_pct_cov, within = Nativity), 
             y = tot_pct_cov, fill = S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_brewer(palette="Accent") +
  facet_grid(rows = vars(Strata), cols = vars(Nativity), scales = "free") +
  #facet_wrap(vars(Strata), dir = "v", scales = "free_x") +
  scale_x_reordered() +
  xlab("Plot Number")


# ...Change ----

# Calculate Change in Total Percent Cover for Native & Non-Native Frequency
Nat_Cov_Chg <- Nat_Cov %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, Nativity,  
           fill = list(tot_pct_cov = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = tot_pct_cov) %>%
  mutate(tot_pct_cov_chg = round(`2` - `1`, 2))  

#  ........ BAR CHG/PLOT----
Nat_Cov_Chg %>%
  mutate(Plot_Number = reorder_within(Plot_Number, -tot_pct_cov_chg, 
                                      list(Strata, Nativity))) %>%
  ggplot(aes(x = Plot_Number, y = tot_pct_cov_chg, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Strata + Nativity, scales = "free_x") +
  scale_fill_brewer(palette="Dark2") +
  scale_x_reordered() +
  xlab("Plot Number") + ylab("Change in % Cover")


#........QUAD NAT COVER----
plt <- max(c(abs(max(Nat_Cov_Chg$tot_pct_cov_chg)), 
                 abs(min(Nat_Cov_Chg$tot_pct_cov_chg))))

Nat_Cov_Chg %>%
  filter(Strata == "UNDERSTORY1") %>%
  select(-`1`, -`2`) %>%
  pivot_wider(names_from = Nativity, values_from = tot_pct_cov_chg) %>%
  ggplot(aes(x = Native, y = `Non-Native`, label = Plot_Number)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "#1B9E77", alpha = .25) + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "#D95F02", alpha = .25) +
  geom_point(color = "black", shape = 19, size = 5) +
  geom_text(vjust = 0.4, color = "white", size = 3, 
            fontface = "bold", show.legend = FALSE) +
  #geom_text_repel() +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  xlim(min(-plt),max(plt)) +
  ylim(max(plt), min(-plt)) +
  facet_wrap(vars(Strata), dir = "v") +
  ylab("Change in Non-Native Cover") +
  xlab("Change in Native Cover") 

   
# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Nat_Cov_Stats <- add.stats(
  .data =  Nat_Cov_Chg,
  .summary_var = tot_pct_cov_chg,
  Unit_Code, Sampling_Frame, Strata, Nativity)


#........BAR YEARLY MEANS----
Nat_Cov_Stats %>%
  filter(S_Cycle != "CHG") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "% Cover") +
  facet_wrap(vars(Strata, Nativity), scales = "free_x") +
  scale_fill_brewer(palette="Dark2") +
  xlab("Sample Cycle") +
  theme(legend.title = element_blank())


#........BAR CHG----
Nat_Cov_Stats %>%
  filter(S_Cycle == "CHG") %>%
  #filter(Strata == "UNDERSTORY1") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in Total % Cover") +
  facet_wrap(vars(fct_rev(Strata))) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank())

#ggsave(here("figs", "bar_mean_cov_chg_nativity.png"))  


#........JITTER PLOT ----

# Jitter plot change (stand alone)
Nat_Cov_Chg %>%
  ggplot(aes(x =Sampling_Frame, y = tot_pct_cov_chg)) +
  geom_jitter(width = 0.05) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "red") +
  facet_grid(vars(Strata), vars(Nativity))  

# Calculate range for nat__cov_chg so that zeros will line up.
Nat_Cov_Chg_range <- Nat_Cov_Chg %>%
  group_by(Sampling_Frame, Nativity, Strata) %>%
  summarize(y_range = max(abs(tot_pct_cov_chg))) %>%
  ungroup()
# Add range column to Chg dataset  
Nat_Cov_Chg <- Nat_Cov_Chg %>%
  inner_join(Nat_Cov_Chg_range)

# Jitter plot change (for multichart)
Nat_Cov_Chg_jitter <- Nat_Cov_Chg %>%
  ggplot(aes(x =Sampling_Frame, y = tot_pct_cov_chg, label = Plot_Number)) +
  geom_blank(aes(y = y_range)) +
  geom_blank(aes(y = -y_range)) +
  geom_hline(yintercept=0.0, linetype = "dashed", color = "gray", size = 1) +
  geom_jitter(width = 0.05) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, color = "red") +
  labs(y = "Change (% Cover)") +
  facet_wrap(vars(Strata, Nativity), nrow = 1, scales = "free") +
  # geom_text_repel(force=1, point.padding=unit(1,'lines'),
  #                 hjust=1, size = 3,
  #                 direction='x',
  #                 nudge_y=0.1,
  #                 segment.size=0.7) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
Nat_Cov_Chg_jitter

#........STRIP CHRT PAIR -----

Nat_Cov_strip <- Nat_Cov %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=S_Cycle, y=tot_pct_cov, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Total % Cover') +
  #scale_fill_brewer(palette="Accent") +
  #scale_color_brewer(palette="Accent") + 
  #theme_bw() +
  facet_wrap(vars(Strata, Nativity), nrow = 1, scales = "free")
  #facet_grid(cols = vars(Nativity), rows = vars(Strata))
Nat_Cov_strip

#........STRIP/JITTER MULTI -----
grid.arrange(Nat_Cov_strip, Nat_Cov_Chg_jitter, nrow = 2, top = "Nativity Cover" 
             #heights = c(2, 1.5)
)



#.-----------------------------------------------------
# 3) Nativity Richness ----
#.......................................................

Nat_Rich <- Cover %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, Nativity) %>%
  summarise(richness = n_distinct(Code))

#........STRIP CHRT PAIR -----


Nat_Rich %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=S_Cycle, y=richness, group=Plot_Number)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Understory Richness') +
  #scale_fill_brewer(palette="Accent") +
  #scale_color_brewer(palette="Accent") + 
  #theme_bw() +
  facet_grid(cols = vars(Nativity), rows = vars(Strata)) 

# ...Change ----

# Calculate Change in Total Percent Cover for Native & Non-Native Frequency
Nat_Rich_Chg <- Nat_Rich %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, Nativity,  
           fill = list(richness = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = richness) %>%
  mutate(richness_chg = round(`2` - `1`, 2))

#  ........STRIP CHRT CHG----
Nat_Rich_Chg %>%
  ggplot(aes(x = Sampling_Frame, y=richness_chg)) +
  geom_jitter(width = 0.05) +
  ylab('Change in richness') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_summary(fun=mean, geom="point", shape=95,
               size=8, color="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 1) +
  facet_grid(cols = vars(Nativity), rows = vars(Strata)) 


#........QUAD NAT RICH----
plt.r <- max(c(abs(max(Nat_Rich_Chg$richness_chg)), 
             abs(min(Nat_Rich_Chg$richness_chg))))

Nat_Rich_Chg %>%
  select(-`1`, -`2`) %>%
  pivot_wider(names_from = Nativity, values_from = richness_chg) %>%
  ggplot(aes(x = Native, y = `Non-Native`)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "green", alpha = .25) + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "red", alpha = .25) +
  geom_point() +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  xlim(min(-plt.r),max(plt.r)) +
  ylim(max(plt.r), min(-plt.r)) +
  facet_wrap(vars(Strata), dir = "v") +
  ylab("Change in Non-Native Richness") +
  xlab("Change in Native Richness") 

# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Nat_Rich_Stats <- add.stats(
  .data =  Nat_Rich_Chg,
  .summary_var = richness_chg,
  Unit_Code, Sampling_Frame, Strata, Nativity)

#........BAR YEARLY MEANS----
Nat_Rich_Stats %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "richness") +
  facet_wrap(vars(Strata, Nativity), scales = "free_x") +
  scale_fill_brewer(palette="Dark2") 

#........BAR CHG----
nrs <- Nat_Rich_Stats %>%
  filter(S_Cycle == "CHG") %>%
  filter(Nativity != "Unknown") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in richness") +
  facet_wrap(vars(fct_rev(Strata))) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")
nrs
#ggsave(here("figs", "bar_mean_nat_rich_chg.png")) 
#grid.arrange(ncs, nrs, nrow = 1)

#.-----------------------------------------------------
# 4) Lifeform Total % cover ----
#.......................................................
#'* Optional Filter  * 
 Forms_Cov_Filter <- Cover %>%
  # Take 'Nativity' and 'Life_form' to make plural category (ex. 'Native Shrubs') 
   mutate(NLF = paste0(Nativity, " ", Life_form, "s")) #%>%
#   filter(Plot_Number %in% c(2,4,6,9)) %>%
#   filter(Nativity != "Unknown")
  #drop_na(Name)

# Calculate Total Percent Cover for Native vs. Non-native Lifeforms
Forms_Cov <- Forms_Cov_Filter %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Strata, Nativity, Life_form, NLF) %>%
  # Count hits at each cover point:
  summarise(Hits_All_Forms = n(), .groups = 'drop')  %>%
  # But don't count record if entire plot had no hits: (e.g. transect is NA )
  mutate(Hits_All_Forms = replace(Hits_All_Forms, is.na(Transect), 0)) %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, Nativity, 
           Life_form, NLF) %>%
  #Total hits at each point for each strata for entire plot 
  # (can be > 300 points or >100%)
  summarise(tot_pct_cov = (sum(Hits_All_Forms)) / 300 * 100, .groups = 'drop')

# Calculate lifeform Cover (same as lines above) - but combine Strata 1 & 2
Forms_Cov_1a2 <- Forms_Cov_Filter %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Transect, Point, Nativity, Life_form, NLF) %>%
  summarise(Hits_All_Forms = n_distinct(n()), .groups = "drop") %>% #remove dbl counts
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Nativity, Life_form, NLF) %>%
  #Total hits at each point for each strata for entire plot 
  # (cannot be greater than 100%)
  summarise(tot_pct_cov = (sum(Hits_All_Forms)) / 300 * 100, .groups = 'drop')

#  ........ BAR COV/PLOT----
Forms_Cov %>%
  ggplot(aes(x = reorder_within(Plot_Number, -tot_pct_cov, within = NLF), 
             y = tot_pct_cov, fill = S_Cycle)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_brewer(palette="Accent") +
  facet_grid(rows = vars(Strata), cols = vars(NLF), scales = "free") +
  #facet_wrap(vars(Strata), dir = "v", scales = "free_x") +
  scale_x_reordered() +
  xlab("Plot Number")


# ...Change ----

# Calculate Change in Total Percent Cover for Native & Non-Native Frequency
Forms_Cov_Complete <- Forms_Cov %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, Strata, 
                            Nativity, Life_form, NLF),  
           fill = list(tot_pct_cov = 0)) 

Forms_Cov_Chg <- Forms_Cov_Complete %>%
  pivot_wider(names_from = S_Cycle, values_from = tot_pct_cov) %>%
  mutate(tot_pct_cov_chg = round(`2` - `1`, 2))  

# Calculate Change (Strata 1&2 Combined)
Forms_Cov_Complete_1a2 <- Forms_Cov_1a2 %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, 
                            Nativity, Life_form, NLF),  
           fill = list(tot_pct_cov = 0)) 

Forms_Cov_Chg_1a2 <- Forms_Cov_Complete_1a2 %>%
  pivot_wider(names_from = S_Cycle, values_from = tot_pct_cov) %>%
  mutate(tot_pct_cov_chg = round(`2` - `1`, 2))  



#  ........ BAR CHG/PLOT----
Forms_Cov_Chg %>%
  mutate(Plot_Number = reorder_within(Plot_Number, -tot_pct_cov_chg, 
                                      list(Strata, Nativity, Life_form))) %>%
  ggplot(aes(x = Plot_Number, y = tot_pct_cov_chg, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Strata + Life_form, scales = "free_x") +
  scale_fill_brewer(palette="Dark2") +
  scale_x_reordered() +
  xlab("Plot Number") + ylab("Change in % Cover")


#........JITTER PLOT ----
Forms_Cov_Chg %>%
  #filter(Plot_Number %in% c(2,4,6,9)) %>%
  ggplot(aes(x =Sampling_Frame, y = tot_pct_cov_chg, color=Nativity)) +
  geom_jitter(width = 0.05) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "red") +
  facet_grid(vars(Strata), vars(Life_form)) +
  scale_color_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#........STRIP CHRT PAIR -----
Forms_Cov_Complete %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=S_Cycle, y=tot_pct_cov, 
             group=interaction(Plot_Number, Nativity), 
             color = Nativity)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Total % Cover') +
  scale_color_brewer(palette="Dark2") +
  facet_grid(cols = vars(Life_form), rows = vars(Strata))


# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Forms_Cov_Stats <- add.stats(
  .data =  Forms_Cov_Chg,
  .summary_var = tot_pct_cov_chg,
  Unit_Code, Sampling_Frame, Strata, Nativity, Life_form, NLF)

# Add stats to dataset (Strata 1&2 Combined)
Forms_Cov_Stats_1a2 <- add.stats(
  .data =  Forms_Cov_Chg_1a2,
  .summary_var = tot_pct_cov_chg,
  Unit_Code, Sampling_Frame, Nativity, Life_form, NLF)


#........BAR LF CHG 1a2 ----

fcs.means1a2 <- Forms_Cov_Stats_1a2 %>%
  filter(S_Cycle == "CHG") %>%
  #filter(MEAN > 0.5 | MEAN < -0.5) %>%
  mutate(tot_pct_cov_chg = MEAN) %>%
  mutate(ERR = case_when(NPLOTS < 4 ~ NA_real_ , TRUE ~ ERR)) %>%
  droplevels() 
  
fcs.NLF_1a2 <- unique(fcs.means1a2$NLF)
fcs.NLF_1a2

Forms_Cov_Chg_1a2 %>%
  filter(NLF %in% fcs.NLF_1a2) %>%
  ggplot(aes(x = reorder(NLF, tot_pct_cov_chg), y = tot_pct_cov_chg, fill = Nativity)) +
  geom_linerange(data = fcs.means1a2, aes(ymin = MEAN - ERR, ymax = MEAN + ERR)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.8),
    shape = 21, size = 1, alpha = 0.5) +
  geom_bar(data = fcs.means1a2, colour = "black", stat="identity", alpha = 0.5) +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  labs(title="", y="Change in % Cover", x="Nativity + Lifeform") +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #scale_color_discrete() +
  #name = NULL, values = c("Specificity" = "black")) +
  theme(legend.title=element_blank()) +
  coord_flip()



#........BAR YEARLY MEANS----
Forms_Cov_Stats %>%
  filter(Nativity != "Unknown") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "% Cover") +
  facet_wrap(vars(Strata, Life_form), scales = "free_x") +
  scale_fill_brewer(palette="Dark2") 


#........BAR CHG----
ncs <- Forms_Cov_Stats %>%
  filter(Nativity != "Unknown") %>%
  filter(S_Cycle == "CHG") %>%
  ggplot(aes(x = S_Cycle, y = MEAN, fill = Nativity)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                position=position_dodge(.9)) +
  labs(y = "Change in Total % Cover") +
  facet_wrap(vars(fct_rev(Strata), Life_form)) +
  scale_fill_brewer(palette="Dark2") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ncs
 


#.-----------------------------------------------------
# 5) Species % Cover ----
#.......................................................

#'* Optional Filter  * 
Spp_Cov_Filter <- Cover #%>%
  #filter(Plot_Number %in% c(2,4,6,9)) %>%
  #drop_na(Name)

# Calculate Total Percent Cover for Native vs. Non-native
Spp_Cov <- Spp_Cov_Filter %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Strata, Nativity, Code, Name, Life_form) %>%
  # Count hits at each cover point (will be '1' for each species)
  summarise(Hits_Sp = n(), .groups = 'drop')  %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, 
           Nativity, Code, Name, Life_form) %>%
  #Total hits at each point for each strata for entire plot 
  # (cannot be greater than 100%)
  summarise(pct_cov_sp = (sum(Hits_Sp)) / 300 * 100, .groups = 'drop') 

# Calculate Spp Cover (same as lines above) - but combine Strata 1 & 2
Spp_Cov_1a2 <- Spp_Cov_Filter %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Nativity, Code, Name, Life_form) %>%
  summarise(Hits_Sp = n_distinct(n()), .groups = "drop") %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Nativity, Code, Name, Life_form) %>%
  #Total hits at each point for each strata for entire plot 
  # (cannot be greater than 100%)
  summarise(pct_cov_sp = (sum(Hits_Sp)) / 300 * 100, .groups = 'drop')
  



# ...Change ----


# Calculate Change in Pct Cover by Species
Spp_Cov_Chg <- Spp_Cov %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, Strata, 
           Nativity, Code, Name, Life_form),
           fill = list(pct_cov_sp = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = pct_cov_sp) %>%
  mutate(pct_cov_sp_chg = round(`2` - `1`, 3))  

# Calculate Change in Pct Cover by Species (Strata 1 and 2 combined)
Spp_Cov_Chg_1a2 <- Spp_Cov_1a2 %>%
  ungroup() %>%
  #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
  complete(S_Cycle, nesting(Unit_Code, Sampling_Frame, Plot_Number, 
                            Nativity, Code, Name, Life_form),
           fill = list(pct_cov_sp = 0)) %>%
  pivot_wider(names_from = S_Cycle, values_from = pct_cov_sp) %>%
  mutate(pct_cov_sp_chg = round(`2` - `1`, 3)) 

#........JITTER PLOT ----
Spp_Cov_Chg %>%
  droplevels() %>%
  ggplot(aes(x =Sampling_Frame, y = pct_cov_sp_chg, color=Name)) +
  geom_jitter(width = 0.05) +
  geom_hline(yintercept=0, linetype = "dashed", color = "gray", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "red") +
  facet_grid(vars(Strata), vars(Life_form)) 


#........STRIP CHRT PAIR -----
Spp_Cov %>%
  #mutate(Status = fct_rev(Status)) %>%
  ggplot(aes(x=S_Cycle, y=pct_cov_sp, 
             group=interaction(Plot_Number, Name), 
             color = Name)) +
  geom_line(size=1, alpha=0.5, position=position_dodge(width=0.2)) +
  geom_point(position=position_dodge(width=0.2)) +
  xlab('Sample Cycle') +
  ylab('Total % Cover') +
  facet_grid(cols = vars(Life_form), rows = vars(Strata))


# ...Summary Stats ----

# Use custom function at top of script to add stats to dataset
Spp_Cov_Stats <- add.stats(
  .data =  Spp_Cov_Chg,
  .summary_var = pct_cov_sp_chg, 
  Unit_Code, Sampling_Frame, Strata, Nativity, Code, Name, Life_form)

# Add stats to dataset (Strata 1&2 Combined)
Spp_Cov_Stats_1a2 <- add.stats(
  .data =  Spp_Cov_Chg_1a2,
  .summary_var = pct_cov_sp_chg,
  Unit_Code, Sampling_Frame, Nativity, Code, Name, Life_form)

#........BAR SPP CHG  ----

p.means <- Spp_Cov_Stats %>%
  filter(S_Cycle == "CHG") %>%
  filter(MEAN > 0.5 | MEAN < -0.5) %>%
  mutate(pct_cov_sp_chg = MEAN) %>%
  mutate(ERR = case_when(NPLOTS < 4 ~ NA_real_ , TRUE ~ ERR))
p.code <- p.means$Code

p <- Spp_Cov_Chg %>%
  filter(Code %in% p.code) %>%
  mutate(Strata = fct_rev(Strata)) %>%
  ggplot(aes(x = reorder(Name, pct_cov_sp_chg), y = pct_cov_sp_chg, fill = Nativity)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.8),
    shape = 21, size = 1, alpha = 0.5) +
  geom_bar(data = p.means, colour = "black", stat="identity", alpha = 0.5) +
  geom_linerange(data = p.means, aes(ymin = MEAN - ERR, ymax = MEAN + ERR)) +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  labs(title="", y="Change in % Cover", x="Species") +
  facet_grid(,vars(Strata)) +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.title=element_blank()) +
  coord_flip()
p
#png("spp_cov_chg.png", width = 800, height = 375)
#plot(p)
#dev.off()


#........BAR SPP CHG 1a2 ----

p.means1a2 <- Spp_Cov_Stats_1a2 %>%
  filter(S_Cycle == "CHG") %>%
  filter(MEAN > 0.5 | MEAN < -0.5) %>%
  mutate(pct_cov_sp_chg = MEAN) %>%
  mutate(ERR = case_when(NPLOTS < 4 ~ NA_real_ , TRUE ~ ERR)) %>%
  droplevels() 
p.code1a2 <- p.means1a2$Code


Spp_Cov_Chg_1a2 %>%
  filter(Code %in% p.code1a2) %>%
  ggplot(aes(x = reorder(Name, pct_cov_sp_chg), y = pct_cov_sp_chg, fill = Nativity)) +
  geom_linerange(data = p.means1a2, aes(ymin = MEAN - ERR, ymax = MEAN + ERR)) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.8),
    shape = 21, size = 1, alpha = 0.5) +
  geom_bar(data = p.means1a2, colour = "black", stat="identity", alpha = 0.5) +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  labs(title="", y="Change in % Cover", x="Species") +
  scale_fill_brewer(palette="Dark2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #scale_color_discrete() +
  #name = NULL, values = c("Specificity" = "black")) +
  theme(legend.title=element_blank()) +
  coord_flip()







#........TREE MAP  ----

# Calculate Total Proportion of Hits
# Calculate Spp Cover (but combine Strata 1 & 2)
Spp_Hits <- Cover %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           Transect, Point, Nativity, Code, Name, Life_form) %>%
  summarise(Hits_Sp = n_distinct(n()), .groups = "drop") %>%
  # group by plot (i.e. remove Transect and Point from grouping variable)
  group_by(S_Cycle, Unit_Code, Sampling_Frame, 
           Nativity, Code, Name, Life_form) %>%
  #Total hits at each point for each strata for entire plot 
  # (cannot be greater than 100%)
  summarise(tot_hits = (sum(Hits_Sp)), .groups = 'drop') %>%
  filter(S_Cycle == "2")

Spp_Hits <- Spp_Hits %>%
  drop_na(Name)
  

library(treemapify)

tree.plot <- ggplot(Spp_Hits, aes(area = tot_hits, subgroup = Nativity, 
                     fill = Nativity, label = Name)) +
  geom_treemap(size = 2) +
  geom_treemap_subgroup_border(colour = "gray30", size = 5, alpha = 0.5) +
  geom_treemap_subgroup_text(place = "bottom", colour = "gray30", alpha = 0.5, 
                             grow = F, size = 20) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "middle",
                    grow = TRUE, reflow = T, min.size = 5) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "none")

tree.plot #if this throws "Error in 1:row_n : argument of length 0", than check
# for NA's in the dataset. 

# Add lifeforms to treemap
tree.plot2 <- ggplot(Spp_Hits, aes(area = tot_hits, subgroup = Nativity, 
                                  subgroup2 = Life_form, fill = Nativity, label = Name)) +
  geom_treemap(size = 1) +
  geom_treemap_subgroup_border(colour = "gray30", size = 5, alpha = 0.5) +
  geom_treemap_subgroup2_border(colour = "black", size = 3) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "middle",
                    grow = F, reflow = T, min.size = 5) +
  geom_treemap_subgroup2_text(
    colour = "black",alpha = 1, fontface = "italic", 
    size = 10, place = "bottomright") +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position="bottom", legend.title = element_blank())

tree.plot2 #if this throws "Error in 1:row_n : argument of length 0", than check
# for NA's in the dataset. 

#png(here("figures","tree_plot_230x160r180.png"), width = 230, height = 160, 
#    units = 'mm', res = 180)
#plot(tree.plot)
#dev.off()

# # Average Cover Treemap
# Spp_Cov %>%
#   filter(S_Cycle == "2") %>%
#   filter(Strata == "UNDERSTORY1") %>%
#   complete(Unit_Code, Sampling_Frame, Plot_Number, nesting(Code, Nativity),
#            fill = list(pct_cov_sp = 0)) %>%
#   group_by(Unit_Code, Sampling_Frame, Code, Nativity) %>%
#   summarise(mean_cov_sp = mean(pct_cov_sp),
#             n = n()) %>%
#   ggplot(aes(area = mean_cov_sp, fill = Nativity, label = Code)) +
#   geom_treemap() +
#   geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
#                     grow = TRUE) +
#   scale_fill_brewer(palette="Dark2")

#........JITTER PLOT SPP  ----
Spp_Cov_Chg %>%
  mutate(code_lab = case_when(pct_cov_sp_chg <= -5 ~ Code,
                              pct_cov_sp_chg >= 5 ~ Code,
                              TRUE ~ "")) %>%
  filter(pct_cov_sp_chg != 0) %>%
  ggplot(aes(x = Plot_Number, y=pct_cov_sp_chg, 
             color = Nativity, label = code_lab)) +
  geom_jitter(width = 0.3) +
  geom_text(size = 3, hjust=1, vjust=0) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette="Dark2") +
  facet_wrap(vars(Strata), dir = "v", scales = "free_x")

# ........SLOPE PLOT ALL ----
library(ggrepel)

Spp_Slope <- Spp_Cov_Chg %>%
  filter(`1` > 0 | `2` > 0) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Understory = str_sub(Strata,-1,-1)) %>%
  mutate(Direction = case_when(pct_cov_sp_chg < 0 ~ "DECREASE",
                               pct_cov_sp_chg >= 0  ~ "INCREASE")) %>%
  mutate(code_lab = case_when(`1` >= 5 ~ Code,
                              `2` >= 5 ~ Code,
                              TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) 


ggplot(Spp_Slope) +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Nativity), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Spp_Slope$`1`, Spp_Slope$`2`)))) +
  facet_grid(vars(Strata), vars(Plot), labeller = label_both) +
  geom_text_repel(label=Spp_Slope$code_lab,
                  y=Spp_Slope$`1`, x=rep(1, NROW(Spp_Slope)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2012", x=1, y=1.1*(max(Spp_Slope$`1`, Spp_Slope$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2017", x=2, y=1.1*(max(Spp_Slope$`1`, Spp_Slope$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_brewer(palette="Dark2") 

# ........SLOPE PLOT = X ----

Spp_Slope_X <- Spp_Cov_Chg %>%
  filter(`1` > 0 | `2` > 0) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Understory = str_sub(Strata,-1,-1)) %>%
  mutate(Direction = case_when(pct_cov_sp_chg < 0 ~ "DECREASE",
                               pct_cov_sp_chg >= 0  ~ "INCREASE")) %>%
  mutate(code_lab = case_when(`1` >= 5 ~ Code,
                              `2` >= 5 ~ Code,
                              TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) %>%
  filter(Plot_Number == "2" |
           Plot_Number == "3" )
#filter(Code == "CIBGLA")


ggplot(Spp_Slope_X) +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Nativity), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)))) +
  facet_grid(vars(Strata), vars(Plot), labeller = label_both) +
  geom_text_repel(label=Spp_Slope_X$code_lab,
                  y=Spp_Slope_X$`1`, x=rep(1, NROW(Spp_Slope_X)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2012", x=1, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2017", x=2, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_brewer(palette="Dark2") 

# ........SLOPE SP = X ----

Spp_Slope_X <- Spp_Cov_Chg %>%
  filter(`1` > 0 | `2` > 0) %>%
  mutate(Plot = Plot_Number) %>%
  mutate(Understory = str_sub(Strata,-1,-1)) %>%
  mutate(Direction = case_when(pct_cov_sp_chg < 0 ~ "DECREASE",
                               pct_cov_sp_chg >= 0  ~ "INCREASE")) %>%
  mutate(code_lab = case_when(`1` >= 5 ~ Code,
                              `2` >= 5 ~ Code,
                              TRUE ~ "")) %>%
  mutate(Direction = as.factor(Direction)) %>%
  filter(Code == "FESRUB")


ggplot(Spp_Slope_X) +
  geom_segment(aes(x=1, xend=2, y=`1`, yend=`2`, 
                   col=Direction), size=.75, show.legend=T) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(x="", y="% Cover") +  
  xlim(.5, 2.5) + ylim(0,(1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)))) +
  facet_grid(vars(Strata), vars(Plot), labeller = label_both) +
  geom_text_repel(label=Spp_Slope_X$code_lab,
                  y=Spp_Slope_X$`1`, x=rep(1, NROW(Spp_Slope_X)), hjust=1.1, size=3, direction = "y") +
  geom_text(label="2012", x=1, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=1.2, size=4.5) +
  geom_text(label="2017", x=2, y=1.1*(max(Spp_Slope_X$`1`, Spp_Slope_X$`2`)), hjust=-0.1, size=4.5) +
  guides(color=guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank(),axis.text.x = element_blank()) +
  scale_color_manual(values = c("#CC0000", "#009900")) 

# ........BAR SP = X ----
sp.x <- "POAPRA"
Spp_Cov %>%
  filter(Code == sp.x) %>%
  ggplot(aes(x = reorder_within(Plot_Number, desc(pct_cov_sp), Strata), y = pct_cov_sp, fill = S_Cycle)) +
  geom_col(position = position_dodge()) +
  ggtitle(sp.x) +
  scale_fill_brewer(palette="Accent") +
  facet_wrap(vars(Strata), dir = "v", scales = "free_x") +
  ylab("Total % Cover") + xlab("Plot Number") +
  scale_x_reordered()

#........TABLE DECLINES ----
declines <- Spp_Cov_Stats %>%
  filter(S_Cycle == "CHG") %>%
  mutate(
    chg_dir = case_when(
      MEAN > 0 ~ "Increase",
      MEAN < 0 ~ "Decrease",
      MEAN == 0 ~ "No Change")) %>%
  group_by(Unit_Code, Sampling_Frame, Strata, Nativity, chg_dir) %>%
  summarize(chg_dir_count = n()) %>%
  add_tally(chg_dir_count) %>%
  mutate(pct_of_species = chg_dir_count/n)

head(declines)


#.-----------------------------------------------------
# 5) Species Presence ----
#.......................................................

Spp_Pres <- Cover %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number,
           Nativity, Code, Name, Life_form) %>%
  summarise(present = n_distinct(Name), .groups = 'drop') %>%
  complete(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, 
           nesting(Nativity, Code, Name, Life_form),
           fill = list(present = 0)) %>%
  group_by(S_Cycle, Unit_Code, Sampling_Frame,
           Nativity, Code, Name, Life_form) %>%
  summarise(plots_pres = sum(present), 
            n = n(), .groups = 'drop')


# ...Change ----

Spp_Pres_Chg <- Spp_Pres %>%
  pivot_wider(names_from = S_Cycle, values_from = plots_pres) %>%
  mutate(presence_chg = round(`2` - `1`, 2)) %>%
  mutate(prop_plts = presence_chg/n)

#........POINT CHG SPP ----
Spp_Pres_Chg %>%
  ggplot(aes(Sampling_Frame, prop_plts, colour = Nativity)) + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1) +
  geom_jitter(width = .3, size=1) +
  #facet_grid(,vars(Strata)) + 
  #geom_count() +
  scale_colour_brewer(palette="Dark2") + 
  labs(title="Change in Landscape Frequency", y="Change in proportion of Plots", x="Sampling Frame", caption="(each point represents one species)") +
  guides(color=guide_legend("Plant Species")) 


#........BAR CHG SPP ----
Spp_Pres_Chg %>%
  filter(presence_chg > 0 | presence_chg < 0) %>%
  ggplot(aes(x = reorder(Name, presence_chg), presence_chg)) +   
  geom_bar(aes(fill = Nativity), 
           position = position_dodge2(width = 0.9, preserve = "single"), 
           stat="identity") +
  geom_hline(yintercept=0, linetype="solid", 
             color = "black", size=0.5) +
  scale_fill_brewer(palette="Dark2") +
  labs(title="", y="Plots (Change in Understory Presence)", x="Species", caption= paste('(',Spp_Pres_Chg$n[1],' paired plots)', sep='')) +
  guides(fill=guide_legend("")) +
  scale_y_continuous(breaks = seq(min(Spp_Pres_Chg$presence_chg),
                                  max(Spp_Pres_Chg$presence_chg), by = 1)) +  #spacebetween tick mark and tick label ('unit')
  coord_flip() 

  

