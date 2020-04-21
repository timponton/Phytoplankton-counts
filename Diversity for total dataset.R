library(zoo)
#### Read in data and fix date#####
# read in data
all <- read.csv("All data/Final data/Final cleaned data all.csv")

# fix dates column 
all$Date <- as.Date(all$Date, "%d/%m/%Y")
####

#####adding columns related to dates#####
# add month months and year column column
all <- all %>% 
  mutate(months = months(Date), month = month(Date), year = year(Date))

# coerce year column as factor
all$year <- as.factor(all$year)

all$monthYear <- as.yearmon(paste(all$year, all$month), "%Y %m")

# create month year column but this creates character so FIND SOLUTION
all$yearMonth <- format(as.Date(all$Date))

####



##### Sort data #####

# add index column grouped by data and Time i.e new sample column
all <- all %>% 
  arrange(Date)  %>% 
  group_by(Date, Time) %>% 
  mutate(Id = group_indices())

# Subset necessary columns out
dat <- all %>% 
  select(Id, Date,months, month, year, monthYear, yearMonth, Time, Site, Species, Classification, Cells.L)

# seperating Species column into Genus and species epithet column
dat <- dat %>% 
  separate(Species, into = c("Genus", "species epithet", NA), sep = " ", remove = FALSE)

####


##### Species Richness #####
# Species richness per month for all classes available line graph
dat %>% 
  filter(Classification != "Various") %>% 
  filter(Site == "Primary sump") %>% 
  group_by(month, months, year, monthYear, Classification) %>% 
  summarise(numSp = n_distinct(Species)) %>% 
  ggplot(., aes(x = monthYear, y = numSp, col = Classification)) +
  geom_line(size = 1)

# Bar graph species richness per month of available classes
dat %>% 
  filter(Classification != "Various") %>% 
  filter(Site == "Primary sump") %>% 
  group_by(month, months, year, yearMonth, Classification) %>% 
  summarise(numSp = n_distinct(Species)) %>% 
  ggplot(., aes(x = yearMonth, y = numSp, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge")

 
####
dat %>% 
  filter(Classification != "Various") %>% 
  filter(Site == "Primary sump") %>% 
  group_by(month, months, year, monthYear, Classification) %>% 
  summarise(numSp = n_distinct(Species)) %>% 
  ggplot(., aes(x = reorder(months, month), y = year, fill = numSp)) + 
  geom_tile()

###




##### Species Diversity Shannon per CLASSIFICATION #####
# remove unnec rows
all_shannon <- all %>% 
  filter(Classification != "Various") %>% 
  filter(Site == "Primary sump")


# summarise average cell density with specific groups
## For diveristy by MONTH
all_shannon_month <- all_shannon %>% 
  group_by(Species, month, months, year, yearMonth, monthYear, Classification) %>% 
  summarise(mean_cells = mean(Cells.L, na.rm = TRUE))

# convert fromlong to wide format and convert NA to 0's
all_shannon_month_wide <- all_shannon_month %>% 
  pivot_wider(names_from = Species, values_from = mean_cells, 
              values_fill = list(mean_cells = 0))

# extract group columns
month_group <- all_shannon_month_wide %>% 
  select(1:6)

# shannon diveristy column added to group df
month_group$shannon <- diversity(all_shannon_month_wide[, 7:106], "shannon")

# dataviz per month

# all classifications
ggplot(data = month_group, aes(x = monthYear, y = shannon, col = Classification)) +
  geom_line(size = 1)

# just dino's and diatoms
DinoDiat <- month_group %>% 
  filter(Classification == "Dinoflagellate" | Classification == "Diatom") %>% 
  ggplot(aes(x = monthYear, y = shannon, col = Classification)) +
  geom_line(size = 1)
ggplotly(DinoDiat)

## for diversity by DAY
all_shannon_day <- all_shannon %>% 
  group_by(Date, Species, month, months, year, yearMonth, monthYear, Classification) %>% 
  summarise(mean_cells = mean(Cells.L, na.rm = TRUE))

# convert fromlong to wide format and convert NA to 0's
all_shannon_day_wide <- all_shannon_day %>% 
  pivot_wider(names_from = Species, values_from = mean_cells, 
              values_fill = list(mean_cells = 0))

# extract group columns
day_group <- all_shannon_day_wide %>% 
  select(1:7)

# shannon diveristy column added to group df
day_group$shannon <- diversity(all_shannon_day_wide[, 8:107], "shannon")

# dataviz per day
ggplot(data = day_group, aes(x = Date, y = shannon, col = Classification)) +
  geom_line(size = 1) +
  facet_wrap(~Classification)

# just diatoms (test to see)
day_group %>% 
  filter(Classification == "Diatom") %>% 
  ggplot(., aes(x = Date, y = shannon)) +
  geom_line(size = 1)
####

##### Species Diversity Shannon IN TOTAL #####
# remove unnec rows
all_shannonT <- all %>% 
  filter(Classification != "Various" & Site == "Primary sump")

# SUmmarise for time period (Day)
# COnvert long to wide and replace NA with 0
# extract grouping columns
# do diversity

# summarise average cell density with specific groups
## For diveristy by MONTH
all_shannon_monthT <- all_shannonT %>% 
  group_by(Species, month, months, year, yearMonth, monthYear) %>% 
  summarise(mean_cells = mean(Cells.L, na.rm = TRUE))

# convert fromlong to wide format and convert NA to 0's
all_shannon_month_wideT <- all_shannon_monthT %>% 
  pivot_wider(names_from = Species, values_from = mean_cells, 
              values_fill = list(mean_cells = 0))

# extract group columns
month_groupT <- all_shannon_month_wideT %>% 
  select(1:5)

# shannon diveristy column added to group df
month_groupT$shannon <- diversity(all_shannon_month_wideT[, 6:105], "shannon")

# dataviz per month

monthly <- ggplot(data = month_groupT, aes(x = monthYear, y = shannon)) +
  geom_line(size = 1)
ggplotly(monthly)

## for diversity by DAY
all_shannon_dayT <- all_shannonT %>% 
  group_by(Date, Species, month, months, year, yearMonth, monthYear) %>% 
  summarise(mean_cells = mean(Cells.L, na.rm = TRUE))

# convert fromlong to wide format and convert NA to 0's
all_shannon_day_wideT <- all_shannon_dayT %>% 
  pivot_wider(names_from = Species, values_from = mean_cells, 
              values_fill = list(mean_cells = 0))

# extract group columns
day_groupT <- all_shannon_day_wideT %>% 
  select(1:6)

# shannon diveristy column added to group df
day_groupT$shannon <- diversity(all_shannon_day_wideT[, 7:106], "shannon")

# dataviz per day
day <- ggplot(data = day_groupT, aes(x = Date, y = shannon)) +
  geom_line(size = 1)

ggplotly(day)
# just diatoms (test to see)
day_group %>% 
  filter(Classification == "Diatom") %>% 
  ggplot(., aes(x = Date, y = shannon)) +
  geom_line(size = 1)


library(patchwork)
day/monthly

ggplotly(day)
ggplotly(monthly)

Gday
Gmonth
####





