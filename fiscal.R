library(foreign)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gt)
library(janitor)
library(naniar)

# 1. Change in GDP by income group
## IMF Global economic data (WEO, 2020) https://www.imf.org/en/Publications/WEO/weo-database/2020/October
imf_raw <- read.csv("WEOOct2020.csv", sep=";", stringsAsFactors = FALSE)

## Pivot for data processing, select variables of interest
imf <- imf_raw %>% pivot_longer(cols=(starts_with("X")),
                                names_to="year",
                                values_to="value",
                                names_prefix="X") %>% 
  clean_names() %>% 
  select(iso, year, subject_descriptor, value, units, scale) %>% 
  rename(variable = subject_descriptor) 
imf$value[imf$value=="n/a"] <- NA


## Only look at variables related to GDP and from 2005-2022 years
x <- 2005:2022
imf <- imf %>% filter(year %in% x)

gdp <- imf %>% filter(variable %in% c("Gross domestic product, current prices",
                                      "Gross domestic product per capita, current prices"),
                      units == "U.S. dollars") %>% select(-scale, -units) %>% 
  pivot_wider(names_from=variable, values_from=value) %>% as.data.frame 

## Sort Country names by income group
### Country names by income group 
all_groups <- read.csv('CLASS.csv') %>% clean_names() %>% select(economy, code, region, income_group)
income <- all_groups %>% filter(income_group %in% c("High income", "Upper middle income", "Lower middle income", "Low income")) %>% 
  rename(iso=code, country=economy)

### Join gdp and income group
gdp <- left_join(income, gdp, by="iso") %>% 
  clean_names()
gdp$gross_domestic_product_current_prices <- as.numeric(gdp$gross_domestic_product_per_capita_current_prices)

## Calculate gdp growth rate by income group
gdp_income <- gdp %>% group_by(year, income_group) %>% 
  summarise(ave_gdp = ave(na.omit(gross_domestic_product_current_prices))) %>% 
  distinct() %>% 
  group_by(income_group) %>%  
  mutate(year= as.numeric(year)) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = ave_gdp - lag(ave_gdp),
         grt = (diff_growth/diff_year)/ ave_gdp*100) %>% as.data.frame()
## Graph gdp growth rate by income group
ggplot(gdp_income, aes(x=year, y=grt, group=income_group)) + 
  geom_line(aes(colour=income_group)) +
  ylim(-10, 30) # Graph or the calculation method need improvement

## Graph average GDP by income group
ggplot(gdp_income, aes(x=year, y=ave_gdp, group=income_group)) + 
  geom_line(aes(colour=income_group))

###################################
# 2. Analysis of COVID-19 fiscal stimulus provided by governments
## Data from IMF Fiscal Policy Database, Oct 2020  https://www.imf.org/en/Topics/imf-and-covid19/Fiscal-Policies-Database-in-Response-to-COVID-19
raw <- read.csv("fiscal.csv")
clean <- na.omit(raw)
duplicated(clean)

## Global stimulus by income group and type
global <- clean %>% group_by(region) %>% 
  mutate(total_stimulus = spending+liquidity) %>% 
  summarise(global_expenditure = sum(spending),
            global_liquidity = sum(liquidity),
            total_stimulus = sum(total_stimulus)) 
global_byregion <- as.data.frame(global)

ggplot(global_byregion, aes(x=region)) + 
  geom_bar(aes(y=total_stimulus), stat="identity") +
  labs(x = "Region", y = "Total Stimulus (billion USD)")
ggsave("covid19_stimulus_region.png", width = 6, height = 4)

## Total global stimulus 
global_sum <- global_byregion %>% 
  summarise(expenditure = sum(global_expenditure),
            liquidity = sum(global_liquidity)) %>% 
  mutate(total = expenditure + liquidity)
global_sum

### Graph: Covid-19 stimulus by region and type
#### Prep data for graphing
global_long <- global %>% pivot_longer(cols=global_expenditure:total_stimulus, names_to = "type", values_to= "amount") %>%
  as.data.frame()
global_long_nototal <- global_long[which(global_long$type != "total_stimulus"),]

#### Plot
ggplot(global_long_nototal, aes(x=region, y=amount, fill=type)) +
  geom_bar(stat="identity") +
  labs(x="Country by Income", y="Amount(billion USD)") + 
  scale_fill_discrete(name="Type",
                      breaks= c("global_expenditure", "global_liquidity"),
                      labels= c("Expenditure", "Liquidity")) +
  theme_classic()

#### Save as png image
ggsave("covid19_stimulus_region.png", width = 6, height = 4)

#########################################
# 3. Analyze green recovery efforts by country
## Data from Carbon Brief (Coronavirus: Tracking how the world’s ‘green recovery’ plans aim to cut emissions)
## https://www.carbonbrief.org/coronavirus-tracking-how-the-worlds-green-recovery-plans-aim-to-cut-emissions

green_raw <- read.csv("green.csv") %>%
  select(-X, -X.1) # Since the Carbon Brief's Tracker does not provide downloadable raw data, this file was created by the author's manual entry.
green_country <- green_raw %>% group_by(country) %>% 
  summarise(green_stimulus = sum(amount)) %>% arrange(desc(green_stimulus))
green_country
sum(green_country$green_stimulus)

green_country <- as.data.frame(green_country)

## Compare the share of green stimulus in global total COVID-19 stimulus, 
## Join IMF Fiscal Policy Data (clean) with green recovery data (green_country)
## Match income group data with the full dataset
clean <- clean %>% select(-region)
all <- left_join(clean, green_country, by="country")
all <- left_join(all, income, by="country")
all$green_stimulus[is.na(all$green_stimulus)] <- 0
all_global <- all %>% 
  mutate(total_stimulus = spending+liquidity) %>% 
  summarise(global_expenditure = sum(spending),
            global_liquidity = sum(liquidity),
            global_green = sum(green_stimulus),
            total_stimulus = sum(total_stimulus)) %>% 
  mutate(green_perc = round(global_green/total_stimulus*100,2))
all_global

## Percentage of green recovery stimulus in total stimulus by region
all_region <- all %>% 
  group_by(region) %>% 
  mutate(total_stimulus = spending+liquidity) %>% 
  summarise(global_expenditure = sum(spending),
            global_liquidity = sum(liquidity),
            global_green = sum(green_stimulus),
            total_stimulus = sum(total_stimulus)) %>% 
  mutate(stimulus=total_stimulus - global_green,
         green_prc=global_green/total_stimulus*100) %>% 
  select(region, stimulus, global_green, green_prc) %>% as.data.frame() 
all_region

## Create a table that summarizes global stimulus and share of green recovery
total <- all_region %>% 
  summarise(stimulus=sum(stimulus),
            global_green = sum(global_green)) %>% 
  mutate(green_prc = global_green/stimulus*100) %>% 
  as.data.frame()

world <- c("World", 12803, 205, 1.6) ## Need to find a better way to add the world summary to gtable

summary <- rbind(all_region, world) %>% 
  mutate(stimulus = as.numeric(stimulus),
         global_green = as.numeric(global_green),
         green_prc = as.numeric(green_prc))

table <- summary %>% gt() %>% 
  tab_header(title = "COVID-19 Recovery Stimulus by Region",
             subtitle = "in billion USD") %>% 
  fmt_number(columns=vars(
    stimulus, global_green, green_prc), decimals = 2) %>% 
  cols_label(
    region = "Region",
    stimulus = "General Stimulus",
    global_green = "Green Stimulus",
    green_prc = "% of Green") 
table %>% gtsave("stimulus_table.png") 

## Green stimulus by country and sector
green_country_sector <- green_raw %>%
  group_by(country, sector) %>% summarise(amount=sum(amount))

country_sector <- ggplot(green_country_sector, aes(y=reorder(country, amount), x=amount, fill=sector)) + 
  geom_bar(stat="identity") +
  theme(axis.title.x=element_blank()) +
  labs(x="Amount in billion USD",
      title = "Green Stimulus by Country and Sector",
      fill = "Sector") +
  theme_classic() +
  scale_fill_ordinal() +
  theme(axis.title.y=element_blank())
country_sector
ggsave("green_country_sector.png")

## Green stimulus by sector
green_sector <- green_raw %>% 
  group_by(sector) %>% summarise(amount=sum(amount)) %>% 
  as.data.frame()

green_bysector <- ggplot(green_sector, aes(x="", y=amount, fill=sector, label=sector)) +
   geom_bar(stat="identity") +  coord_polar("y", start=0) +
  theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title="Sector breakdown of green stimulus") +
  theme_void() 
green_bysector 

## Green stimulus by income group
### Add income group data to the green dataset
green <- left_join(green_raw, income, by="country") 

### A table that summarizes breakdown of global green recovery effort by income group
green_income_data <- green %>% group_by(income_group) %>%
  summarize(amount=sum(amount)) %>%
  mutate(share=round(amount/sum(amount)*100,2)) %>% 
  arrange(desc(amount)) %>%
  as.data.frame()

table <- green_income_data %>% gt() %>% 
  tab_header(title = "COVID-19 Recovery Stimulus by Income Group",
             subtitle = "in billion USD") %>% 
  cols_label(
    income_group = "Income group",
    amount = "Green recovery stimulus (in USD mil)",
    share = "% of income group")
table %>% gtsave("stimulus_incomegroup.png")


## INTERNATIONAL FINANCIAL INSTITUTIONS

ifi <- read.csv("ifi_tracker.csv",  skip=3, stringsAsFactors=F)
ifi <- ifi[,1:17]
ifi <- ifi %>% rename(Commitment = Institutional.Commitment...M.,
                      Approved = Approved.Value...M.,
                      Disbursed = Disbursed.Value...M.,
                      GDP_cap_nominal = GDP.per.capita..nominal.) %>% 
  clean_names() %>% select(institution, commitment, institution_type, approved, disbursed, sector, green, country_recipient, region, gdp_cap_nominal, income_group) %>% 
replace_with_na(replace = list(commitment="",
                               country_recipient ="",
                               gdp_cap_nominal = c("#NV", ".."),
                               income_group="#NV",
                               region = "")) 
ifi$commitment <- gsub(",", "", ifi$commitment)
ifi$commitment <- as.numeric(ifi$commitment)

# Convert millions into billion USD
ifi_bil <- ifi %>% mutate(commitment = commitment/1000,
                             approved = approved/1000,
                             disbursed = disbursed/1000)

# Commitment by multilateral development banks.
commit_appr <- ifi_bil %>% filter(institution_type %in% c("MDB", "IMF")) %>% 
  select(institution, commitment, approved) %>% 
  group_by(institution) %>% summarise(commitment = sum(na.omit(commitment)),
                                      approved = sum(na.omit(approved))) %>% 
  arrange(desc(commitment)) %>% as.data.frame(commitment)

# Add full Bank names
bank <- read.csv("bank_names.csv")[,1:2]
commit_appr_long <- left_join(commit_appr, bank, by="institution") 

# Green funding by institution
green <-ifi %>% select(institution, commitment, approved, green, sector, income_group) %>% 
  filter(green==TRUE) %>% 
  mutate(commitment = commitment/1000,
         approved = round(approved/1000,3)) %>% 
  group_by(institution) %>% 
  summarise(gr_com=sum(na.omit(commitment)),
            gr_app=sum(na.omit(approved))) %>% 
  select(-gr_com) %>% 
  as.data.frame()

# Join general and green 
all <- left_join(commit_appr_long, green, by="institution") 

# Clean NAs
all[is.na(all)] <- 0

# Add percentage of green and clean NAs
all_prc <- all %>% mutate(prc_green = round(gr_app/approved*100, 2)) %>% 
  select(institution_long, commitment, approved, gr_app, prc_green)
all_prc[is.na(all_prc)] <- 0

# Add total
# 
all_prc <- all_prc %>% bind_rows(
   summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))
 )

# Clean decimals
all_prc$gr_app[all_prc$gr_app == 0] <- "0"
all_prc$prc_green[all_prc$prc_green ==0] <- "0"

table <- gt(all_prc) %>% 
  cols_label(institution_long = "Institution",
            commitment = "Committed",
            gr_app = "Approved Green",
            approved = "Approved",
            prc_green = "% of green") %>% 
  tab_header(title = "COVID-19 Funding by MDBs and the IMF",
             subtitle = "in billion USD") %>% 
  tab_source_note(
    source_note=md("Data from CSIS IFI COVID-19 Response Tracker, CSIS Economics Program, Last Updated October 19, 2020") 
  ) %>% 
  cols_align(align = c("center"), columns= TRUE) %>% 
  fmt_number(
    columns=2:3,
    decimals=1
  ) 
table
gtsave(table, "mbd_funding.png")

# All approved funding by recipient income group
app_region <- ifi %>% filter(institution_type %in% c("MDB", "IMF")) %>% 
  select(institution, approved, green, sector, income_group) %>% 
  group_by(income_group) %>% 
  summarise(amount = sum(na.omit(approved[sector=="General"])),
            gr_amount = sum(na.omit(approved[green==TRUE])),
            hl_amount = sum(na.omit(approved[sector=="Health"]))) %>% 
  mutate(income_group = case_when(is.na(income_group) ~ "General",
                                  TRUE ~ income_group))

app_region_long <- app_region %>% rename(general = amount,
                                         green = gr_amount,
                                         health = hl_amount) %>% 
  pivot_longer(cols=general:health,
               names_to = "sector",values_to = "amount")

ggplot(app_region_long, aes(y=reorder(income_group, amount), x=amount, fill=sector)) +
  geom_bar(stat="identity") + 
  labs(title = "MDB and IMF's stimulus by recipients' income group",
       x="Amount in million USD",
       y=NULL)+
  theme_classic() 
ggsave("mdb_incomegroup.png")

africa <- ifi %>% filter(institution_type %in% c("MDB", "IMF"),
                         region %in% c("Americas", "Africa", "Asia/Oceania", "Eurasia", "Middle East")) %>% 
  mutate(region = case_when(region=="Americas" ~ "Central and \n South Americas",
                            TRUE ~ region)) %>% 
  select(institution, approved, green, sector, region) %>% 
  group_by(region) %>% 
  summarise(amount = sum(na.omit(approved[sector=="General"])),
            gr_amount = sum(na.omit(approved[green==TRUE])),
            hl_amount = sum(na.omit(approved[sector=="Health"]))) 

africa_long <- africa %>% rename(general = amount,
                                         green = gr_amount,
                                         health = hl_amount) %>% 
  pivot_longer(cols=general:health,
               names_to = "sector",values_to = "amount")

ggplot(africa_long, aes(y=reorder(region, amount), x=amount, fill=sector)) +
  geom_bar(stat="identity") + 
  labs(title = "MDB and IMF's stimulus by region",
       x="Amount in million USD",
       y=NULL) +
  theme_classic()
  
ggsave("mdb_region.png")

