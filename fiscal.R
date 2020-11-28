library(foreign)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gt)
library(janitor)
library(naniar)

# IMF Global economic data (WEO, 2020)
imf_raw <- read.csv("WEOOct2020.csv", sep=";", stringsAsFactors = FALSE)

# Pivot for data processing, select variables of interest
imf <- imf_raw %>% pivot_longer(cols=(starts_with("X")),
                                names_to="year",
                                values_to="value",
                                names_prefix="X") %>% 
  clean_names() %>% 
  select(iso, year, subject_descriptor, value, units, scale) %>% 
  rename(variable = subject_descriptor) 
imf$value[imf$value=="n/a"] <- NA


# Only look at variables from 2005-2022 years
x <- 2005:2022
imf <- imf %>% filter(year %in% x)

# GDP
gdp <- imf %>% filter(variable %in% c("Gross domestic product, current prices",
                                      "Gross domestic product per capita, current prices"),
                      units == "U.S. dollars") %>% select(-scale, -units) %>% 
  pivot_wider(names_from=variable, values_from=value) %>% as.data.frame 

# Country names by income group
all_groups <- read.csv('CLASS.csv', sep=";") %>% clean_names()
income <- all_groups %>% filter(group_name %in% c("High income", "Upper middle income", "Lower middle income", "Low income")) %>% 
  select(-group_code) %>% rename(iso=country_code)

# Join gdp and income group
gdp <- left_join(income, gdp, by="iso") %>% 
  clean_names()
gdp$gross_domestic_product_current_prices <- as.numeric(gdp$gross_domestic_product_per_capita_current_prices)

# calculate gdp growth rate by income group
gdp_income <- gdp %>% group_by(year, group_name) %>% 
  summarise(ave_gdp = ave(na.omit(gross_domestic_product_current_prices))) %>% 
  distinct() %>% 
  group_by(group_name) %>%  
  mutate(year= as.numeric(year)) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = ave_gdp - lag(ave_gdp),
         grt = (diff_growth/diff_year)/ ave_gdp*100) %>% as.data.frame()

ggplot(gdp_income, aes(x=year, y=grt, group=group_name)) + 
  geom_line(aes(colour=group_name)) +
  ylim(-10, 30)

ggplot(gdp_income, aes(x=year, y=ave_gdp, group=group_name)) + 
  geom_line(aes(colour=group_name))

raw <- read.csv("fiscal.csv")
names(raw)
clean <- na.omit(raw)
nrow(clean)
duplicated(clean)

global <- clean %>% group_by(region, type) %>% 
  mutate(total_stimulus = spending+liquidity) %>% 
  summarise(global_expenditure = sum(spending),
            global_liquidity = sum(liquidity),
            total_stimulus = sum(total_stimulus)) 
global_byregion <- as.data.frame(global)
global_byregion$prc_gdp <- c(9.3, 6, 1.8)

ggplot(global_byregion, aes(x=region)) + 
  geom_bar(aes(y=total_stimulus), stat="identity") +
  geom_line(aes(y=prc_gdp, color=region)) +
  labs(x = "Region", y = "Total Stimulus (billion USD)")
ggsave("covid19_stimulus_region.png", width = 6, height = 4)

global_sum <- global_byregion %>% 
  summarise(expenditure = sum(global_expenditure),
            liquidity = sum(global_liquidity)) %>% 
  mutate(total = expenditure + liquidity)

global_long <- global %>% pivot_longer(cols=global_expenditure:total_stimulus, names_to = "type", values_to= "amount") %>%
  as.data.frame()
global_long_nototal <- global_long[which(global_long$type != "total_stimulus"),]

ggplot(global_long_nototal, aes(x=region, y=amount, fill=type)) +
  geom_bar(stat="identity") +
  labs(x="Country by Income", y="Amount(billion USD)") + 
  scale_fill_discrete(name="Type",
                      breaks= c("global_expenditure", "global_liquidity"),
                      labels= c("Expenditure", "Liquidity")) +
  theme_classic()

ggsave("covid19_stimulus_region.png", width = 6, height = 4)

green_raw <- read.csv("green.csv")
green_country <- green_raw %>% group_by(country) %>% 
  summarise(green_stimulus = sum(amount)) %>% arrange(desc(green_stimulus))
green_country
sum(green_country$green_stimulus)

green_country <- as.data.frame(green_country)

all <- left_join(clean, green_country, by="country")
all$green_stimulus[is.na(all$green_stimulus)] <- 0
all_global <- all %>% 
  mutate(total_stimulus = spending+liquidity) %>% 
  summarise(global_expenditure = sum(spending),
            global_liquidity = sum(liquidity),
            global_green = sum(green_stimulus),
            total_stimulus = sum(total_stimulus)) %>% 
  mutate(green_perc = round(global_green/total_stimulus*100,2))
all_global

all_global <- all %>% 
  mutate(total_stimulus = spending+liquidity) %>% 
  summarise(global_expenditure = sum(spending),
            global_liquidity = sum(liquidity),
            global_green = sum(green_stimulus),
            total_stimulus = sum(total_stimulus)) %>% 
  mutate(green_perc = round(global_green/total_stimulus*100,2))

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

total <- all_region %>% 
  summarise(stimulus=sum(stimulus),
            global_green = sum(global_green)) %>% 
  mutate(green_prc = global_green/stimulus*100) %>% 
  as.data.frame()
world <- c("World", 12803.39, 205.48, 1.604887)

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

green_country_sector <- green_raw %>% select(-X, -X.1) %>% 
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

green_sector <- green_raw %>% select(-X, -X.1) %>% 
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

