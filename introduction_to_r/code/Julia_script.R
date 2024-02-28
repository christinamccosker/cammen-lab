


bycatch_data <- read.csv("~/Desktop/BycaughtSampleInventorySubsampled-Fall2023.csv", header = TRUE)

str(bycatch_data)

head(bycatch_data) #see the first 6 lines
tail(bycatch_data) #see the last 6 lines
str(bycatch_data) #view the data structure (e.g., what your columns are and what type of variable they hold)
View(bycatch_data) #pull up the whole data frame in a new window
summary(bycatch_data) #see a summary of each data column
names(bycatch_data)
unique(bycatch_data$Species) 
install.packages("tidyverse") #or in the "packages" tab to the right. 
#library(tidyverse)
library(tidyverse)
bycatch_data_clean <- bycatch_data

bycatch_data_clean <- 
  bycatch_data |>
  mutate(Species=gsub("Pv", "PV", Species))

unique(bycatch_data_clean$Species)


bycatch_data_clean <- 
  bycatch_data |> 
  mutate(Species = gsub("Pv", "PV", Species)) |> # what we did above, but now we are adding on!
  filter(Species == "PV" | Species == "HG") |> # choose rows that have an entry for species PV OR HG (the line | means OR)
  mutate(Fishery = gsub("Gillnet", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("gillnet ", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("gillnet/anchor sink", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("gillnet/sink fixed", "gillnet", Fishery)) |> 
  mutate(Fishery = gsub("100 \\(gillnet\\)", "gillnet", Fishery)) |>  #the parentheses in the code pose an extra problem \
  filter(Fishery != "") # choose rows where the fishery column is NOT empty (the ! means NOT)

#plots

#make a summary 
fishery_summary <- 
  bycatch_data_clean |> 
  group_by(Species, Fishery) |> 
  summarise(
    count = n()
  )


ggplot(fishery_summary, aes(x=Fishery, y=count, fill = Species)) +
  geom_bar(stat="identity")

ggplot(fishery_summary, aes(x=Fishery, y=count, fill = Species)) +
  geom_bar(stat="identity", position ="dodge") +
  scale_fill_manual(values = c("skyblue", "magenta"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="number of bycaught seals recorded", title = "seal bycatch by fishery 2023")

#now species by date
species_summary <- 
  bycatch_data_clean %>%
  mutate(Year = str_sub(Bycaught.Date.Format, start=-4, end=-1)) %>%
  group_by(Species, Year) %>%
  summarise(count=n())


plot1 <- ggplot(species_summary, aes(x=Year, y=count, fill = Species)) +
  geom_bar(stat="identity", position ="dodge") +
  scale_fill_manual(values = c("skyblue", "magenta"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="Number of Seals", title = "Bycaught Seals by Species x Year")
plot1




