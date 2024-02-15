##### Purpose ####
# This script is to provide an introduction to R coding for beginners


#### Co-Authors ####
# Christina McCosker
# Julia Sunnarborg
# Alice Hotopp
# Dara Yiu


#### Coding Basics ####

# This document is a script, it is essentially a "word doc" full of code in R language that you can run
# Use separate scripts for different analyses, research projects or questions, etc.

# This is a hashtag that makes your line of text a "comment" in R, this is text that you can write to
# annotate your script and take notes - anything you don't want run as code.

# read in data, and name it "bycatch_data" (or change it to something else :)
bycatch_data <- read.csv("introduction_to_r/data/Bycaught Sample Inventory - Subsampled Fall 2023.csv", header = TRUE)

#This imports your data, creating an object in your R environment. 
    #If you can see your environment tab, you should see the new object appear 
#In our case the data is in a dataframe, which looks essentially like a spreadsheet

#### Checking the data ####
# various ways of previewing data  
head(bycatch_data) #see the first 6 lines
tail(bycatch_data) #see the last 6 lines
str(bycatch_data) #view the data structure (e.g., what your columns are and what type of variable they hold)
#View(bycatch_data) #pull up the whole data frame in a new window
summary(bycatch_data) #see a summary of each data column
colnames(bycatch_data) #see the column names

# you can call a specific column with the $ symbol: dataframe_name$column
# looking at unique entries in specific column can be an easy way QA_QC the data for errors & inconsistencies. 
# For example,
unique(bycatch_data$Species) 
  # here I can see that at least one line is missing a species designation.
  # also, PV is entered inconsistently, at least once, its entered as Pv with a lowercase v. 
  # this leaves you with some options. 
  # if you are in the data QAQC process, you can locate these problematic lines in the raw data file and correct them.
  # often, when data is missing for real, we designate something like "unknown" or "missing" or NA to denote that 
    # the data is truly missing and is not a typo or spreadsheet error.
  # if the data spreadsheet is finalized, we can deal with these errors in the code, see below.
unique(bycatch_data$Tag..)
  # at least one entry of Tag data is missing
unique(bycatch_data$Fishery)
  # inconsitent entries will be problematic -- "gillnet", "gillnet  " with a space after, "Gillnet" with a capital G. 
  # the problem is that R will see every different variation a unique entry.
unique(bycatch_data$Length)
  # inconsistent nomenclature here too (usually my column name is "length_cm" and the values are all numeric or not_available)
unique(bycatch_data$Disp)
unique(bycatch_data$Haul..)


#### Data cleanup ####
# we can use some R packages. "tidyverse" is a great set of packages that helps make data manipulation easier
# the first time you use any new package, you have to install it. Install packages by running this line
# install.packages("tidyverse") #or in the "packages" tab to the right. 
# once it is installed, you just have to load it into your workspace each time you open it. 
library(tidyverse) #loading the "tidyverse" package

# if we want to summarise some data, first we have to make some cleaner data frames
# the backwards arrow is creating a new object. Here we are creating a new object called "bycatch_data_clean" that is
  # an exact copy of "bycatch_data"
bycatch_data_clean <- bycatch_data

# we will employ this pipe operator: |> 
# it just means -- take the data, and then subject it to the next command
bycatch_data_clean <- bycatch_data |> 
  mutate(Species = gsub("Pv", "PV", Species)) 
  #here we use the mutate function to manipulate the data frame. We are replacing "Pv" with "PV" in the species column. 
  #we can check if it worked:
  unique(bycatch_data_clean$Species)

# we can string together a whole bunch of piped operations with some common commands
# (i'm assuming for examples sake that anything saying gillnet is gillnet bycatch (can correct this))
bycatch_data_clean <- bycatch_data |> 
  mutate(Species = gsub("Pv", "PV", Species)) |> 
  filter(Species == "PV" | Species == "HG") |> # choose rows that have an entry for species PV OR HG (the line | means OR)
  mutate(Fishery = gsub("Gillnet", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("gillnet ", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("gillnet/anchor sink", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("gillnet/sink fixed", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("100 \\(gillnet\\)", "gillnet", Fishery)) |>  #the parentheses in the code pose an extra problem \
  filter(Fishery != "") # choose rows where fishery does NOT have nothing
#always check that the commands did what you wanted them to do
unique(bycatch_data_clean$Species)
unique(bycatch_data_clean$Fishery)
View(bycatch_data_clean)

#### Making data summaries ####
#lets say we want to create a summary of bycatch from each fishery
fishery_summary <- bycatch_data_clean |> 
  group_by(Species, Fishery) |> 
  summarise(
    count = n()
  )
#and we wnat to visualize that data
ggplot(fishery_summary, aes(x=Fishery, y=count, fill = Species)) +
  geom_bar(stat="identity")
#we can add various elements with the + symbol to make the plot look better
ggplot(fishery_summary, aes(x=Fishery, y=count, fill = Species)) +
  geom_bar(stat="identity", position ="dodge") +
  scale_fill_manual(values = c("skyblue", "magenta"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="number of bycaught seals recorded", title = "seal bycatch by fishery 2023")
  
#Notes about ggplot (this could go wherever or just be talked through)
  #ggplot works by layering elements of a graph, and that makes it very fun and customizable! 
  #There are two essential pieces: ggplot() + geom_something()
  #In the ggplot() parentheses, we identify the object that holds our data, and designate the x and y axes as well as visual elements like what variable we want the colors to correspond to
  #The geom_something element tells it what kind of plot we are making (the geometry)
      #In our case, we are making a bar plot so we give it geom_bar
      #Other options include geom_boxplot, geom_point, or geom_line
      #Some geometries might have additional pieces to fill in
        #Like in our case we add stat="identity" to make the height of our bar be our y variable 
  #We can then keep layering to make the graph extra nice looking
    #We added some grouping to our bars with position = "dodge"
    #We also chose our colors, added a theme (notice the background change), and added labels/titles
