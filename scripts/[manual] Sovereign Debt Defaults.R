source("utils/functions.R")

url = "https://www.bankofcanada.ca/2022/08/staff-analytical-note-2022-11/"
cat_name = "Sovereign Debt Defaults"

# Do the cleaning
table = fread("ref/BoC-BoE-Database-2022-08-18.csv") %>%
  select(COUNTRY,COUNTRY_GROUP,YEAR,TOTAL = TOTAL_2022,IMF = IMF_2022,
         IBRD = IBRD_2022, IDA = IDA_2022, Paris_Club = PARIS_CLUB_2022,
         China = CHINA_2022,
         Other_Official_Creditors = OTHER_OFFICIAL_CREDITORS_2022,
         Private_Creditors = PRIVATE_CREDITORS_2022,
         FC_Bank_Loans = FC_BANK_LOANS_2022, FC_Bonds = FC_BONDS_2022,
         LC_Debt = LC_2022) %>%
  filter(COUNTRY != "World") %>%
  # Special characters can mess with the ordering it seems, need to remove
  mutate(COUNTRY = case_when(
    COUNTRY == "Cura\xe7ao" ~ "Curacao",
    COUNTRY == "S\xe3o Tom\xe9 and Pr\xedncipe" ~ "Sao Tome and Principe",
    COUNTRY == "C\xf4te d\x92Ivoire" ~ "Cote d'Ivoire",
    TRUE ~ COUNTRY
  )) %>%
  arrange(COUNTRY,YEAR) %>%
  group_by(COUNTRY) %>%
  mutate(in_default = TOTAL > 0 | grepl("\\*",paste(TOTAL,IMF,IBRD,IDA,Paris_Club,
                                                    China,Other_Official_Creditors,
                                                    Private_Creditors,FC_Bank_Loans,
                                                    FC_Bonds,LC_Debt)),
         entered_default = (lag(in_default == FALSE) & in_default == TRUE),
         exit_default = (lag(in_default == TRUE) & in_default == FALSE)) %>%
  filter(YEAR > 1960) %>%
  mutate(period_counter = cumsum(as.numeric(entered_default|exit_default))) %>%
  mutate(across(c(IMF,IBRD,IDA,Paris_Club,China,Other_Official_Creditors,
                  Private_Creditors,FC_Bank_Loans,FC_Bonds,LC_Debt),
                as.character)) %>%
  pivot_longer(cols = c(IMF,IBRD,IDA,Paris_Club,China,Other_Official_Creditors,
                        Private_Creditors,FC_Bank_Loans,FC_Bonds,LC_Debt),
               names_to = "Creditor", values_to = "Debt") %>%
  mutate(valid_creditor = as.numeric(Debt) > 0 | grepl("\\*",Debt)) %>%
  replace_na(list(valid_creditor = FALSE)) %>%
  filter(in_default == TRUE) %>%
  group_by(COUNTRY,COUNTRY_GROUP,period_counter) %>%
  summarize(year_start = min(YEAR),
            year_end = max(YEAR),
            default = unique(in_default),
            creditors = paste0(unique(Creditor[valid_creditor == TRUE]),
                               collapse=", "),
            max_default = max(as.numeric(TOTAL),na.rm=TRUE),
            .groups = "drop") %>%
  select(-period_counter,-default) %>%
  mutate(Category = cat_name,
         Event = paste0(COUNTRY," - ",year_start),
         `Event description` = paste0(COUNTRY_GROUP,"; ",
                                      COUNTRY," in default with ",
                                      creditors),
         `Timepoint start` = year_start,
         `Timepoint end` = year_end,
         `Quantity outcome 1` = max_default,
         `Reference/link to data` = url,
         `Accessed on` = as.Date("2023-03-30")) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/sovereign debt defaults.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "Continuous stretches of time when countries were in default on 1+ sources of sovereign debt",
  "Description quantity column 1" = "Maximum total amount in default at one point in time across all creditors, in nominal USD",
  "Period start" = "1961",
  "Period end" = "2021",
  "How was the period selected" = "Max range available",
  "Collected by" = "Bank of Canada and Bank of England"
)

update_category_info_sheet(metadata)
