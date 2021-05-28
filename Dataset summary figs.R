##### Database summary statistics

setwd("~/Documents/Documents - MacBook Pro/Kelp meta-analysis")
library(tidyverse)

#load dataset
data <- read.csv("kelp dataset.csv")

############ Histogram of study years
sumdata <- data %>%
  group_by(Year, Study) %>%
  summarize(n())

ggplot(sumdata, aes(x=Year)) +
  geom_histogram(binwidth = 1, colour="black", fill="white") 


############ Species 
sumspp <- data %>%
  group_by(Species, Study) %>%
  summarize(n())

species <- sumspp %>%
  group_by(Species) %>%
  summarize("Number.of.studies" = n())

species$Washington <- ifelse(species$Species == "Agarum clathratum" |
                               species$Species == "Costaria costata" |
                               species$Species == "Dictyoneurum californicum" |
                               species$Species == "Dictyoneurum reticulatum" | 
                               species$Species == "Neoagarum fimbriatum" |
                               species$Species == "Alaria marginata" |
                               species$Species == "Lessoniopsis littoralis" |
                               species$Species == "Pleurophycus gardneri" |
                               species$Species == "Pterygophora californica" | 
                               species$Species == "Egregia menziesii" |
                               species$Species == "Hedophyllum nigripes" |
                               species$Species == "Hedophyllum sessile" |
                               species$Species == "Macrocystis pyrifera" |
                               species$Species == "Nereocystis luetkeana" |
                               species$Species == "Postelsia palmaeformis" |
                               species$Species == "Saccharina complanata" |
                               species$Species == "Saccharina latissima" |
                               species$Species == "Cymathaere triplicata" |
                               species$Species == "Laminaria ephemera" |
                               species$Species == "Laminaria longipes" |
                               species$Species == "Laminaria setchellii" |
                               species$Species == "Laminaria sinclairii", "Y", "N")

ggplot(data=species, aes(x=reorder(Species, -Number.of.studies), 
                         y=Number.of.studies, fill = Washington)) +
  geom_bar(stat="identity") + theme_classic() + coord_flip() +
  xlab("Species") + ylab("Number of studies") +
  scale_fill_manual(values=c("grey","red"), 
                    name="Present in\nWashington?",
                    labels=c("No","Yes")) +
  theme(axis.text.y = element_text(face="italic"))

############### 



####### How much data from each study?
########
#count the rows, and average and get sd (maybe histogram? split for t/oa?)
# how many levels did people look at?

