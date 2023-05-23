source("utils/functions.R")

# Download COW War Data csv files
# Download the most recent for intra-state war, which uses a different format
url = "https://correlatesofwar.org/data-sets/cow-war/"
ref = "Sarkees, Meredith Reid and Frank Wayman (2010). Resort to War: 1816 – 2007. Washington DC: CQ Press."

# Read in tables and get suggested tables for cleaning
intra = fread("ref/INTRA-STATE_State_participants v5.1 CSV.csv")
inter = fread("ref/Inter-StateWarData_v4.0.csv")
extra = fread("ref/Extra-StateWarData_v4.0.csv")
non = fread("ref/Non-StateWarData_v4.0.csv")

type_key = data.frame(
  WarType = c(1:9),
  TypeDesc = c("Inter-state war",
               "Colonial - conflict with colony",
               "State vs. nonstate",
               "Civil war - for central control",
               "Civil war - over local issues",
               "Regional internal",
               "Intercommunal",
               "Non-state war in nonstate territory",
               "Non-state war across state borders")
)

# Quantity outcome should be deaths
# Get lists for:
#   Intra-state (civil) wars (types 4-7)
#   Inter-state wars (type 1)
#   Colonial wars (wars of independence) (type 2)
#   State-nonstate wars (type 3)
#   Non-state wars (type 8-9)


################################################################################
# Intra-state wars
intra_list = intra %>%
  mutate(across(c(StartYr1,EndYr1,StartYr2,EndYr2,StartYr3,EndYr3,
                  StartYr4,EndYr4,
                  TotalBDeaths,SideA,SideB),
                function(x) ifelse(x<0,NA,x)),
         EndYear = unlist(mapply(FUN = max,EndYr1,EndYr2,EndYr3,EndYr4,na.rm=TRUE)),
         EndYear = ifelse(is.infinite(EndYear),NA,EndYear),
         WarType = as.numeric(WarType)) %>%
  group_by(WarNum, WarName, WarType) %>%
  summarize(StartYr = min(StartYr1),
            EndYear = min(EndYear),
            deaths = sum(TotalBDeaths,na.rm=TRUE),
            sidea = paste0(unique(SideA[!is.na(SideA)]),collapse=", "),
            sideb = paste0(unique(SideB[!is.na(SideB)]),collapse=", "),
            .groups = "drop") %>%
  merge(type_key) %>%
  select(-WarType) %>%
  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = "Intra-state (civil) wars",
         Event = WarName,
         `Event description` = paste0(TypeDesc,": ",sidea," vs. ",sideb),
         `Timepoint start` = StartYr,
         `Timepoint end` = EndYear,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-03-23")) %>%
  ungroup() %>%

  select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(intra_list,"output/civil wars.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "G2",
  "Category name" = "Intra-state (civil) wars",
  "Description" = "List of intra-state wars (types 4-7), as defined by COW",
  "Description quantity column 1" = "Total number of combat deaths",
  "Period start" = "1818",
  "Period end" = "2015",
  "How was the period selected" = "Data availability",
  "Collected by" = ref
)

update_category_info_sheet(metadata)



################################################################################
# Inter-state wars
inter_list = inter %>%
  mutate(across(c(StartYear1,EndYear1,StartYear2,EndYear2,
                  BatDeath),
                function(x) ifelse(x<0,NA,x)),
         EndYear = unlist(mapply(FUN = max,EndYear1,EndYear2,na.rm=TRUE)),
         EndYear = ifelse(is.infinite(EndYear),NA,EndYear),
         WarType = as.numeric(WarType)) %>%
  group_by(WarNum, WarName, WarType) %>%
  summarize(StartYr = min(StartYear1),
            EndYear = min(EndYear),
            deaths = sum(BatDeath,na.rm=TRUE),
            States = paste0(unique(StateName),collapse=", "),
            .groups = "drop") %>%
  merge(type_key) %>%
  select(-WarType) %>%
  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = "Inter-state wars",
         Event = WarName,
         `Event description` = paste0(TypeDesc,": ",States),
         `Timepoint start` = StartYr,
         `Timepoint end` = EndYear,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-03-23")) %>%
  ungroup() %>%

  select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(inter_list,"output/interstate wars.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "G3",
  "Category name" = "Inter-state wars",
  "Description" = "List of Inter-state wars (type 1), as defined by COW",
  "Description quantity column 1" = "Total number of combat deaths",
  "Period start" = "1823",
  "Period end" = "2007",
  "How was the period selected" = "Data availability",
  "Collected by" = ref
)

update_category_info_sheet(metadata)


################################################################################
# Colonial wars (type 2)
colonial_list = extra %>%
  filter(WarType == 2) %>%
  mutate(across(c(StartYear1,EndYear1,StartYear2,EndYear2,SideA,SideB,BatDeath),
                function(x) ifelse(x<0,NA,x)),
         EndYear = unlist(mapply(FUN = max,EndYear1,EndYear2,na.rm=TRUE)),
         EndYear = ifelse(is.infinite(EndYear),NA,EndYear),
         WarType = as.numeric(WarType)) %>%
  group_by(WarNum, WarName, WarType) %>%
  summarize(StartYr = min(StartYear1),
            EndYear = min(EndYear),
            deaths = sum(BatDeath,na.rm=TRUE),
            sidea = paste0(unique(SideA[!is.na(SideA)]),collapse=", "),
            sideb = paste0(unique(SideB[!is.na(SideB)]),collapse=", "),
            .groups = "drop") %>%
  mutate(deaths = ifelse(deaths == 0,NA,deaths)) %>%
  merge(type_key) %>%
  select(-WarType) %>%
  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = "Colonial wars",
         Event = WarName,
         `Event description` = paste0(TypeDesc,": ",sidea," vs. ",sideb),
         `Timepoint start` = StartYr,
         `Timepoint end` = EndYear,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-03-23")) %>%
  ungroup() %>%

  select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(colonial_list,"output/colonial wars.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "G4",
  "Category name" = "Colonial wars",
  "Description" = "List of wars between a state and its colony (type 2), as defined by COW",
  "Description quantity column 1" = "Total number of combat deaths",
  "Period start" = "1817",
  "Period end" = "2007",
  "How was the period selected" = "Data availability",
  "Collected by" = ref
)

update_category_info_sheet(metadata)


################################################################################
# State-nonstate wars (type 3)
sns_list = extra %>%
  filter(WarType == 3) %>%
  mutate(across(c(StartYear1,EndYear1,StartYear2,EndYear2,SideA,SideB,BatDeath),
                function(x) ifelse(x<0,NA,x)),
         EndYear = unlist(mapply(FUN = max,EndYear1,EndYear2,na.rm=TRUE)),
         EndYear = ifelse(is.infinite(EndYear),NA,EndYear),
         WarType = as.numeric(WarType)) %>%
  group_by(WarNum, WarName, WarType) %>%
  summarize(StartYr = min(StartYear1),
            EndYear = min(EndYear),
            deaths = sum(BatDeath,na.rm=TRUE),
            sidea = paste0(unique(SideA[!is.na(SideA)]),collapse=", "),
            sideb = paste0(unique(SideB[!is.na(SideB)]),collapse=", "),
            .groups = "drop") %>%
  mutate(deaths = ifelse(deaths == 0,NA,deaths)) %>%
  merge(type_key) %>%
  select(-WarType) %>%
  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = "State-nonstate wars",
         Event = WarName,
         `Event description` = paste0(TypeDesc,": ",sidea," vs. ",sideb),
         `Timepoint start` = StartYr,
         `Timepoint end` = EndYear,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-03-23")) %>%
  ungroup() %>%

  select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(sns_list,"output/state nonstate wars.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "G5",
  "Category name" = "State-nonstate wars",
  "Description" = "List of wars between a state and a nonstate (type 3), as defined by COW",
  "Description quantity column 1" = "Total number of combat deaths",
  "Period start" = "1816",
  "Period end" = "2007",
  "How was the period selected" = "Data availability",
  "Collected by" = ref
)

update_category_info_sheet(metadata)


################################################################################
# Non-state wars (type 8-9)
non_list = non %>%
  mutate(across(c(StartYear, EndYear, TotalCombatDeaths,
                  SideA1,SideA2,SideB1,SideB2,SideB3,SideB4,SideB5),
                function(x) ifelse(x<0,NA,x)),
         WarType = as.numeric(WarType),
         deaths = TotalCombatDeaths) %>%
  group_by(WarNum, WarName, WarType, StartYear, EndYear, deaths) %>%
  summarize(sidea = paste(SideA1,SideA2,
                          sep = ", "),
            sideb = paste(SideB1,SideB2,SideB3,SideB4,SideB5,
                          sep=", "),
            .groups = "drop") %>%
  mutate(deaths = ifelse(deaths == 0,NA,deaths),
         across(c(sidea,sideb),gsub,pattern = ", NA",replacement = "")) %>%
  merge(type_key) %>%
  select(-WarType) %>%
  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = "Nonstate wars",
         Event = WarName,
         `Event description` = paste0(TypeDesc,": ",sidea," vs. ",sideb),
         `Timepoint start` = StartYear,
         `Timepoint end` = EndYear,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-03-23")) %>%
  ungroup() %>%

  select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(non_list,"output/nonstate wars.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "G6",
  "Category name" = "Nonstate wars",
  "Description" = "List of wars between nonstate actors (types 8 and 9), as defined by COW",
  "Description quantity column 1" = "Total number of combat deaths",
  "Period start" = "1816",
  "Period end" = "2007",
  "How was the period selected" = "Data availability",
  "Collected by" = ref
)

update_category_info_sheet(metadata)


