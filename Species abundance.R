
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
all$yearMonth <- format(as.Date(all$Date), "%Y-%m")

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


ClassMean <- dat %>% 
  filter(Classification != "Various") %>% 
  group_by(month, months, year, monthYear, Classification) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))

ggplot(data = ClassMean, aes(x = monthYear, y = meanCells, col = Classification)) +
  geom_line(size = 1)


di <- dat %>% 
  filter(Classification != "Various") %>% 
  group_by(Date, month, months, year, monthYear, Classification) %>% 
  summarise(DailyMean = mean(Cells.L, na.rm = )) %>% 
  ggplot(., aes(x = Date, y = DailyMean, col = Classification)) +
  geom_line(size = 1)

ggplotly(di)



