# ANALYZING COMMUNITY ECOLOGY DATA

# NON-METRIC MULTI-DIMENSIONAL SCALING (NMDS)#####
## condense info from multidimensional data into a 2D representation or ordination
## in this ordination, the closer two points are, the more similar the corresponding samples are
## with respects to the variables that went into making the NMDS plot


# Load libraries
library(vegan)
library(ggplot2)
library(indicspecies)

# Load data
abun <- read_delim("WORKING DATASHEETS/Abundance matrix.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)
# Loads an abundace datasheet for Koeberg

# make community matrix - extract columns with abundance infomation 
com =abun[,3:ncol(abun)]
# turn data frame into a matrix
m_com = as.matrix(com)


# run the metaMDS command from "vegan
nmds = metaMDS(m_com, distance = "bray")
nmds

## Important to note the "stress" which is roughly the 
## goodness of fit of the NMDS ordination
## Stress value should be less than 0.2

# Plot the NMDS
plot(nmds)

# Export the data from the output to plot the figure in ggplot2

# extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

# add columns to data frame
data.scores$site = abun$SITE
data.scores$veg = abun$VEG
## These will be used to differentiate groups or treatments

head(data.scores) # view the sheet to ensure everything was added correctly


library(ggalt)
library(ggforce)

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 5, aes(colour = veg)) +
  geom_text(aes(label = site), hjust=1, vjust=1) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Vegetation type", y = "NMDS2")   

xx

## Distinctions mean nothing statistical (only observations)
## Clalculate whether samples are statistically different based on groupong = ANOSIM test

# ANALYSIS OF SIMILARITIES (ANOSIM)####
## Similar to an ANOVA but uses a dissimilarity matrix
## determine if the differences between two or more groups are significant

# 
ano = anosim(m_com, abun$VEG, distance ="bray", permutations = 9999)
ano
## Using the VEG column as the grouping variable

# ANOSIM statistic R = Compares the mean of ranked dissimilarities between groups
## to the mean of reanked dissimilarites within groups
## R close to 1.0 = diss between groups
## R close to 0 suggests an even distribtion of high and low ranks
## within and between groups (Higher the R the more dissimilar your groups in terms of community composition)

# Significance value = less than 0.05 is statisticall significant
## and the null hypothesis can be rejected
## There is a difference in species composition between vegetation types

# INDICATOR SPECIES ANALYSIS IN R#####
## Identifies species that are found more often in one treatment group compared to another
## Can use SIMPER, mvabund and Indicator Species Analysis

### Indicator species are:
### “A species whose status provides information on the overall condition of the ecosystem and of other species in that ecosystem. 
### They reflect the quality and changes in environmental conditions as well as aspects of community composition.” - United Nations Environment Programme (1996)

abund = abun[,3:ncol(abun)] # only use the abundance data
veg = abun$VEG # Create a vector with the grouping variable

# Run the indicator species command
inv = multipatt(abund, veg, func = "r.g", control = how(nperm=9999))
summary(inv)

## multipatt is the command use
## results in lists of species that are associated with a particular group of samples
## If your group has more than 2 categories, the code will also identify species that are statisticall more abundant
## in combinations of categories

## stat value for "r.g" function is a "point biserial correlation coefficient"

summary(inv, alpha = 1) # To view all species


## Gives an overview of the species that might be more interstiing to investigate
## use a boxplot to show the differences in distribution of these identified


# CORRELATION HEATMAP#####
## Association between two variables
## Helps to develop predictions about relationships of data

install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
library(ggplot2)

# Load each data frame 

p.rho <- read_excel("ORIGINAL DATASHEET/SNAKE DIETS.xlsx", 
                       sheet = "p.rho")

# make community matrix - extract columns with abundance infomation 
com = p.rho[,2:ncol(p.rho)]
# turn data frame into a matrix
m_com = as.matrix(com)


# Use abundance matrix without grouping variables
cc = cor(com, method = "spearman")

corrplot(cc)

corrplot(cc, tl.col = "black", order = "hclust", hclust.method = "average", addrect = 4, tl.cex = 0.7)

cc_df = as.data.frame(cc) # Change the matrix into a data frame format
cc_df$species = row.names(cc_df) # makes the row names in data frame into a column in the new data frame

library(reshape2) # For data manipulation
ccm = melt(cc_df, id = "species") # convert "wide" format data frame into a "long" format data frame

# Keep the species order the same as the initial excel spreadsheet
ccm$Species <- factor(ccm$species, levels = unique(ccm$species))

# plotting in ggplot

xx = ggplot(ccm, aes(x = variable, y = species)) + 
  geom_tile(aes(fill = value), colour = "grey45") + 
  coord_equal() + 
  scale_fill_gradient(low = "navy", high = "darkorange") + 
  theme(axis.text.y = element_text(face = "bold", colour = "grey25"), 
        legend.title = element_text(size = 10, face = "bold"),legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, face = "bold",colour = "grey25", vjust = 0.5, hjust = 0), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = NA), 
        axis.ticks = element_blank()) + 
  labs(x= "", y = "", fill = "Spearman's Correlation") + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(ccm$OTUs))) 
xx


# STACKED BAR PLOT#####
## community composition data visualization

