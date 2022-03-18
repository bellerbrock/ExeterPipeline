require(tidyverse)
require(magrittr)
library(data.table)
# install.packages("WriteXLS")  # for export .xls file to SPbase
library(WriteXLS)

# VARIABLES TO SET
data_directory_to_search = "~/Desktop/GRADER"
trial_name = "21MC010024CUNA03"
plants_harvested_per_plot = 20
person = "Russell"
output_directory = "~/Documents"

# Read in data
data.files  <- list.files(
  path=data_directory_to_search,
  recursive=T,
  pattern=paste(trial_name, ".csv", sep=''),
  full.names=T
)

data <- lapply(data.files, fread, sep=",", fill=TRUE)
data.use <- unique(rbindlist( data , fill=TRUE))

# Write complied unique input to TRIALNAME_unique_raw_data.csv file
setwd(output_directory)
write.csv(data.use, paste(trial_name, "_unique_raw_data.csv", sep=""), row.names = F)

# Add header
colnames(data.use) <- c(
  "plotname",
  "NA2",
  "NA3",
  "concaveside",
  "hourglass",
  "bottleneck",
  "longtapered",
  "wedgeshape",
  "totalbumpsanddips",
  "worstbump",
  "worstdip",
  "tipedge",
  "biggestblackdefect",
  "longestcrack",
  "white",
  "NA4",
  "NA5",
  "NA6",
  "NA7",
  "hollowheart",
  "NA8",
  "colorspots",
  "pointy",
  "largedip",
  "smalldip",
  "estimateddiameter",
  "estimatedweight",
  "estimatedlength",
  "aspectratio",
  "NA9",
  "NA10",
  "NA11",
  "NA12",
  "NA13",
  "NA14",
  "NA15",
  "NA16",
  "NA17",
  "NA18",
  "NA19",
  "NA20",
  "NA21",
  "NA22",
  "NA23",
  "NA24",
  "color1",
  "color1pixels",
  "color2",
  "color2pixels",
  "color3",
  "color3pixels",
  "NA31",
  "NA32",
  "NA33",
  "NA34",
  "NA35",
  "NA36",
  "NA37",
  "NA38",
  "NA39",
  "NA40",
  "NA41",
  "NA42",
  "NA43",
  "NA44",
  "NA45",
  "NA46",
  "NA47",
  "NA48",
  "NA49",
  "NA50",
  "NA51",
  "NA52",
  "NA53",
  "NA54",
  "NA55",
  "NA56",
  "NA57",
  "NA58",
  "NA59",
  "NA60",
  "NA61",
  "NA62",
  "NA63",
  "NA64",
  "time"
)

# Add metric conversions
data.use$diametercm <- as.numeric(data.use$estimateddiameter) * 0.0254 # cm per 1/100 inch
data.use$lengthcm <- as.numeric(data.use$estimatedlength) * 0.0254 # cm per 1/100 inch
data.use$weightg <- as.numeric(data.use$estimatedweight) * 2.8349523 # g per 1/10 ounce

# Add blockiness scores, where blockiness is the ratio of actual volume (from estimated weight) to maximum possible cylindrical volume (from length and diameter)
# 1 = perfectly blockly (all of possible cylindrical volume filled), 0 = infinitely not blocky (none of possible cylindrical volume filled)
data.use$max.volumecc <- pi * (as.numeric(data.use$diametercm)/2)^2 * as.numeric(data.use$lengthcm)  # cyclindrical volume = pi*r2*h
data.use$blockiness <- round(data.use$weightg / data.use$max.volumecc, digits = 2)

# Create an empty dataframe for plot summaries
plot.names = unique(data.use$plotname)                   # list of all plots
descript.results = data.frame(plotname = plot.names )    # create an empty dataframe

# Define a function for color averaging
# average_color <- function(indiv_colors) {
#   # print(indiv_colors)
#   reds<- substr(indiv_colors, 5, 6)
#   greens<- substr(indiv_colors, 7, 8)
#   blues<- substr(indiv_colors, 9, 10)
#   rsquared <- strtoi(reds, base=16)^2
#   gsquared <- strtoi(greens, base=16)^2
#   bsquared <- strtoi(blues, base=16)^2
#   return(
#     rgb(
#       round(sqrt(mean(rsquared))),
#       round(sqrt(mean(gsquared))),
#       round(sqrt(mean(bsquared))),
#       maxColorValue=255
#     )
#   )
# }

# Populate output dataframe with plot summaries
for (i in 1:length(plot.names)) {
  # sort based on defects, size and weight i=1
  ind.plot.all = subset(data.use, plotname == plot.names [i])
  ind.plot.trash = subset(ind.plot.all, estimateddiameter <= 100 | estimatedlength <= 200)
  ind.plot = subset(ind.plot.all, estimateddiameter > 100 & estimatedlength > 200)
  
  ind.plot.mid = subset(ind.plot, estimateddiameter > 100 & estimateddiameter <= 350 &
                          estimatedlength > 200 & estimatedlength <= 900 &
                          estimatedweight <= 220)
  ind.plot.canner = subset(ind.plot.mid,estimatedweight < 50 | estimateddiameter < 200 | estimatedlength < 300 )
  
  ind.plot.cull_no1 = subset(ind.plot, estimatedweight >= 50 & estimatedweight <= 220 &
                               estimateddiameter >= 200 & estimateddiameter <= 350 &
                               estimatedlength >= 300 & estimatedlength <= 900)
  
  #In late2019/early2020 decided that concaveside and hourglass are only reliable cull values
  ind.plot.cull = subset(ind.plot.cull_no1, concaveside >= 45 | hourglass >= 30 &
                           aspectratio <= 135 | aspectratio >= 450)
  
  ind.plot.no1 = subset(ind.plot.cull_no1, concaveside < 45 & hourglass < 30 &
                          aspectratio > 135 & aspectratio < 450)
  #Can ignore defects by turning this on#
  #ind.plot.cull = subset(ind.plot.cull_no1, concaveside >= 9999 | hourglass >= 9999)
  #ind.plot.no1 = ind.plot.cull_no1
  
  ind.plot.no1.5_9.4 = subset(ind.plot.no1, estimatedweight >= 50 & estimatedweight <= 94)
  ind.plot.no1.9.5_14 = subset(ind.plot.no1, estimatedweight >= 95 & estimatedweight <= 140)
  ind.plot.no1.14.1_18 = subset(ind.plot.no1, estimatedweight >= 141 & estimatedweight <= 180)
  ind.plot.no1.18.1_22 = subset(ind.plot.no1, estimatedweight >= 181 & estimatedweight <= 220)
  ind.plot.jumbo = subset(ind.plot, estimatedweight > 220 | estimateddiameter > 350 | estimatedlength > 900)
  ind.plot.jumbo.22_27 = subset(ind.plot.jumbo, estimatedweight <= 270 )
  ind.plot.jumbo.27more = subset(ind.plot.jumbo, estimatedweight > 270 )
  ind.plot.no1.jumbo = rbind(ind.plot.no1,ind.plot.jumbo)
  
  ### Summarize ontology traits
  
  descript.results[i,c("Number of plants harvested per NET plot|CO_331:0000679")] = plants_harvested_per_plot
  descript.results[i,c("Number of storage roots after harvest per NET plot|CO_331:0000233")] = nrow(ind.plot)
  descript.results[i,c("Number of non-commercial storage roots per NET plot|CO_331:0000217")] = nrow(ind.plot.cull)
  descript.results[i,c("Number of commercial storage roots per NET plot|CO_331:0000214")] = nrow(ind.plot) - nrow(ind.plot.cull)
  descript.results[i,c("Length to diameter ratio computation|CO_331:0000779")] = round(mean(ind.plot$aspectratio)/100, digits = 2)
  descript.results[i,c("Total storage root weight per NET plot in kg|CO_331:0000237")] = round(sum(ind.plot$weightg/1000), digits = 2)
  descript.results[i,c("Weight of cull storage roots measuring kg per plot|CO_331:0000612")] = round(sum(ind.plot.cull$weightg/1000), digits = 2)
  descript.results[i,c("Weight of canner storage roots measuring kg per plot|CO_331:0000610")] = round(sum(ind.plot.canner$weightg/1000), digits = 2)
  descript.results[i,c("Weight of total US no. 1 storage roots measuring kg per plot|CO_331:0000609")] = round(sum(ind.plot.no1$weightg/1000), digits = 2)
  descript.results[i,c("Weight of 32 count US no. 1 storage roots measuring kg per plot|CO_331:0000616")] = round(sum(ind.plot.no1.18.1_22$weightg/1000), digits = 2)
  descript.results[i,c("Weight of 40 count US no. 1 storage roots measuring kg per plot|CO_331:0000615")] = round(sum(ind.plot.no1.14.1_18$weightg/1000), digits = 2)
  descript.results[i,c("Weight of 55 count US no. 1 storage roots measuring kg per plot|CO_331:0000614")] = round(sum(ind.plot.no1.9.5_14$weightg/1000), digits = 2)
  descript.results[i,c("Weight of 90 count US no. 1 storage roots measuring kg per plot|CO_331:0000613")] = round(sum(ind.plot.no1.5_9.4$weightg/1000), digits = 2)
  descript.results[i,c("Weight of jumbo storage roots measuring kg per plot|CO_331:0000611")] = round(sum(ind.plot.jumbo$weightg/1000), digits = 2)
  descript.results[i,c("Weight of commercial storage roots per NET plot in kg|CO_331:0000220")] = round(sum(ind.plot$weightg/1000) - sum(ind.plot.cull$weightg/1000), digits = 2)
  descript.results[i,c("Weight of non-commercial storage roots per NET plot in kg|CO_331:0000223")] = round(sum(ind.plot.cull$weightg/1000), digits = 2)
  descript.results[i,c("Percentage of commercial storage root|CO_331:0000682")] = round(100 * ((sum(ind.plot$weightg/1000) - sum(ind.plot.cull$weightg/1000)) / sum(ind.plot$weightg/1000)), digits = 2)
  descript.results[i,c("timestamp")] = strftime(strptime(max(ind.plot.all$time), "%m%d%Y %H:%M:%S"))


  ### Summarize other traits
  
  ## summarize color within plot
  # descript.results[i,c("average_color")] = average_color(ind.plot$color1)
  predominant_color = names(sort(-table(ind.plot$color1)))[1]
  descript.results[i,c("predominant_color")] = paste("#", substr(predominant_color, 5, 10), sep="")
  # descript.results[i,c("red_component")] = strtoi(substr(predominant_color, 5, 6), base=16)
  # descript.results[i,c("green_component")] = strtoi(substr(predominant_color, 7, 8), base=16)
  # descript.results[i,c("blue_component")] = strtoi(substr(predominant_color, 9, 10), base=16)
  
  ## root numbers of individual groups
  # descript.results[i,c("number_all")] = nrow(ind.plot)
  descript.results[i,c("number_trash")] = nrow(ind.plot.trash)
  descript.results[i,c("number_can_cull_no1")] = nrow(ind.plot.mid)
  descript.results[i,c("number_canner")] = nrow(ind.plot.canner)
  descript.results[i,c("number_cull_no1")] = nrow(ind.plot.cull_no1)
  # descript.results[i,c("number_cull")] = nrow(ind.plot.cull)
  descript.results[i,c("number_no1")] = nrow(ind.plot.no1)
  descript.results[i,c("number_no1.18.1_22")] = nrow(ind.plot.no1.18.1_22)
  descript.results[i,c("number_no1.14.1_18")] = nrow(ind.plot.no1.14.1_18)
  descript.results[i,c("number_no1.9.5_14")] = nrow(ind.plot.no1.9.5_14)
  descript.results[i,c("number_no1.5_9.4")] = nrow(ind.plot.no1.5_9.4)
  descript.results[i,c("number_jumbo")] = nrow(ind.plot.jumbo)
  descript.results[i,c("number_jumbo_22_27")] = nrow(ind.plot.jumbo.22_27)
  descript.results[i,c("number_jumbo_27more")] = nrow(ind.plot.jumbo.27more)
  descript.results[i,c("number_no1.jumbo")] = nrow(ind.plot.no1.jumbo)
  
  ## individual roots
  ## mean, SD, median of weight 
  descript.results[i,c("mean_wt_all")] = round(mean(ind.plot$estimatedweight)/10, digits = 2)
  descript.results[i,c("mean_wt_canner")] = round(mean(ind.plot.canner$estimatedweight)/10, digits = 2)
  descript.results[i,c("mean_wt_cull")] = round(mean(ind.plot.cull$estimatedweight)/10, digits = 2)
  descript.results[i,c("mean_wt_no1")] = round(mean(ind.plot.no1$estimatedweight)/10, digits = 2)
  descript.results[i,c("mean_wt_jumbo")] = round(mean(ind.plot.jumbo$estimatedweight)/10, digits = 2)
  descript.results[i,c("mean_wt_no1.jumbo")] = round(mean(ind.plot.no1.jumbo$estimatedweight)/10, digits = 2)
  
  descript.results[i,c("SD_wt_all")] = round(sd(ind.plot$estimatedweight/10), digits = 2)
  descript.results[i,c("SD_wt_canner")] = round(sd(ind.plot.canner$estimatedweight/10), digits = 2)
  descript.results[i,c("SD_wt_cull")] = round(sd(ind.plot.cull$estimatedweight/10), digits = 2)
  descript.results[i,c("SD_wt_no1")] = round(sd(ind.plot.no1$estimatedweight/10), digits = 2)
  descript.results[i,c("SD_wt_jumbo")] = round(sd(ind.plot.jumbo$estimatedweight/10), digits = 2)
  
  descript.results[i,c("median_wt_all")] = median(ind.plot$estimatedweight/10)
  descript.results[i,c("median_wt_canner")] = median(ind.plot.canner$estimatedweight/10)
  descript.results[i,c("median_wt_cull")] = median(ind.plot.cull$estimatedweight/10)
  descript.results[i,c("median_wt_no1")] = median(ind.plot.no1$estimatedweight/10)
  descript.results[i,c("median_wt_jumbo")] = median(ind.plot.jumbo$estimatedweight/10)
  
  ## individual roots
  ## mean, SD, median of diameter
  descript.results[i,c("mean_diameter_all")] = round(mean(ind.plot$estimateddiameter)/100, digits = 2)
  descript.results[i,c("mean_diameter_canner")] = round(mean(ind.plot.canner$estimateddiameter)/100, digits = 2)
  descript.results[i,c("mean_diameter_cull")] = round(mean(ind.plot.cull$estimateddiameter)/100, digits = 2)
  descript.results[i,c("mean_diameter_no1")] = round(mean(ind.plot.no1$estimateddiameter)/100, digits = 2)
  descript.results[i,c("mean_diameter_jumbo")] = round(mean(ind.plot.jumbo$estimateddiameter)/100, digits = 2)
  descript.results[i,c("mean_diameter_no1.jumbo")] = round(mean(ind.plot.no1.jumbo$estimateddiameter)/100, digits = 2)
  
  descript.results[i,c("SD_diameter_all")] = round(sd(ind.plot$estimateddiameter/100), digits = 2)
  descript.results[i,c("SD_diameter_canner")] = round(sd(ind.plot.canner$estimateddiameter/100), digits = 2)
  descript.results[i,c("SD_diameter_cull")] = round(sd(ind.plot.cull$estimateddiameter/100), digits = 2)
  descript.results[i,c("SD_diameter_no1")] = round(sd(ind.plot.no1$estimateddiameter/100), digits = 2)
  descript.results[i,c("SD_diameter_jumbo")] = round(sd(ind.plot.jumbo$estimateddiameter/100), digits = 2)
  
  descript.results[i,c("median_diameter_all")] = median(ind.plot$estimateddiameter/100)
  descript.results[i,c("median_diameter_canner")] = median(ind.plot.canner$estimateddiameter/100)
  descript.results[i,c("median_diameter_cull")] = median(ind.plot.cull$estimateddiameter/100)
  descript.results[i,c("median_diameter_no1")] = median(ind.plot.no1$estimateddiameter/100)
  descript.results[i,c("median_diameter_jumbo")] = median(ind.plot.jumbo$estimateddiameter/100)
  
  ## individual roots
  ## mean, SD, median of length
  descript.results[i,c("mean_length_all")] = round(mean(ind.plot$estimatedlength)/100, digits = 2)
  descript.results[i,c("mean_length_canner")] = round(mean(ind.plot.canner$estimatedlength)/100, digits = 2)
  descript.results[i,c("mean_length_cull")] = round(mean(ind.plot.cull$estimatedlength)/100, digits = 2)
  descript.results[i,c("mean_length_no1")] = round(mean(ind.plot.no1$estimatedlength)/100, digits = 2)
  descript.results[i,c("mean_length_jumbo")] = round(mean(ind.plot.jumbo$estimatedlength)/100, digits = 2)
  descript.results[i,c("mean_length_no1.jumbo")] = round(mean(ind.plot.no1.jumbo$estimatedlength)/100, digits = 2)
  
  descript.results[i,c("SD_length_all")] = round(sd(ind.plot$estimatedlength/100), digits = 2)
  descript.results[i,c("SD_length_canner")] = round(sd(ind.plot.canner$estimatedlength/100), digits = 2)
  descript.results[i,c("SD_length_cull")] = round(sd(ind.plot.cull$estimatedlength/100), digits = 2)
  descript.results[i,c("SD_length_no1")] = round(sd(ind.plot.no1$estimatedlength/100), digits = 2)
  descript.results[i,c("SD_length_jumbo")] = round(sd(ind.plot.jumbo$estimatedlength/100), digits = 2)
  
  descript.results[i,c("median_length_all")] = median(ind.plot$estimatedlength/100)
  descript.results[i,c("median_length_canner")] = median(ind.plot.canner$estimatedlength/100)
  descript.results[i,c("median_length_cull")] = median(ind.plot.cull$estimatedlength/100)
  descript.results[i,c("median_length_no1")] = median(ind.plot.no1$estimatedlength/100)
  descript.results[i,c("median_length_jumbo")] = median(ind.plot.jumbo$estimatedlength/100)
  
  ## individual roots
  ## mean, SD, median of aspectratio
  # descript.results[i,c("mean_LD_all")] = round(mean(ind.plot$aspectratio)/100, digits = 2)
  descript.results[i,c("mean_LD_canner")] = round(mean(ind.plot.canner$aspectratio)/100, digits = 2)
  descript.results[i,c("mean_LD_cull")] = round(mean(ind.plot.cull$aspectratio)/100, digits = 2)
  descript.results[i,c("mean_LD_no1")] = round(mean(ind.plot.no1$aspectratio)/100, digits = 2)
  descript.results[i,c("mean_LD_jumbo")] = round(mean(ind.plot.jumbo$aspectratio)/100, digits = 2)

  descript.results[i,c("SD_LD_all")] = round(sd(ind.plot$aspectratio/100), digits = 2)
  descript.results[i,c("SD_LD_canner")] = round(sd(ind.plot.canner$aspectratio/100), digits = 2)
  descript.results[i,c("SD_LD_cull")] = round(sd(ind.plot.cull$aspectratio/100), digits = 2)
  descript.results[i,c("SD_LD_no1")] = round(sd(ind.plot.no1$aspectratio/100), digits = 2)
  descript.results[i,c("SD_LD_jumbo")] = round(sd(ind.plot.jumbo$aspectratio/100), digits = 2)

  descript.results[i,c("median_LD_all")] = median(ind.plot$aspectratio/100)
  descript.results[i,c("median_LD_canner")] = median(ind.plot.canner$aspectratio/100)
  descript.results[i,c("median_LD_cull")] = median(ind.plot.cull$aspectratio/100)
  descript.results[i,c("median_LD_no1")] = median(ind.plot.no1$aspectratio/100)
  descript.results[i,c("median_LD_jumbo")] = median(ind.plot.jumbo$aspectratio/100)
  
  ## individual roots
  ## mean, SD, median of blockiness
  descript.results[i,c("mean_blockiness_all")] = round(mean(ind.plot$blockiness), digits = 2)
  descript.results[i,c("mean_blockiness_canner")] = round(mean(ind.plot.canner$blockiness), digits = 2)
  descript.results[i,c("mean_blockiness_cull")] = round(mean(ind.plot.cull$blockiness), digits = 2)
  descript.results[i,c("mean_blockiness_no1")] = round(mean(ind.plot.no1$blockiness), digits = 2)
  descript.results[i,c("mean_blockiness_jumbo")] = round(mean(ind.plot.jumbo$blockiness), digits = 2)
  
  descript.results[i,c("SD_blockiness_all")] = round(sd(ind.plot$blockiness), digits = 2)
  descript.results[i,c("SD_blockiness_canner")] = round(sd(ind.plot.canner$blockiness), digits = 2)
  descript.results[i,c("SD_blockiness_cull")] = round(sd(ind.plot.cull$blockiness), digits = 2)
  descript.results[i,c("SD_blockiness_no1")] = round(sd(ind.plot.no1$blockiness), digits = 2)
  descript.results[i,c("SD_blockiness_jumbo")] = round(sd(ind.plot.jumbo$blockiness), digits = 2)
  
  descript.results[i,c("median_blockiness_all")] = median(ind.plot$blockiness)
  descript.results[i,c("median_blockiness_canner")] = median(ind.plot.canner$blockiness)
  descript.results[i,c("median_blockiness_cull")] = median(ind.plot.cull$blockiness)
  descript.results[i,c("median_blockiness_no1")] = median(ind.plot.no1$blockiness)
  descript.results[i,c("median_blockiness_jumbo")] = median(ind.plot.jumbo$blockiness)
  
  ## individual plots
  ## total weight of individual classes
  # descript.results[i,c("weight_all")] = sum(ind.plot$estimatedweight/10)
  # descript.results[i,c("weight_canner")] = sum(ind.plot.canner$estimatedweight/10)
  descript.results[i,c("weight_cull_no1")] = sum(ind.plot.cull_no1$estimatedweight/10)
  # descript.results[i,c("weight_cull")] = sum(ind.plot.cull$estimatedweight/10)
  # descript.results[i,c("weight_no1")] = sum(ind.plot.no1$estimatedweight/10)
  # descript.results[i,c("weight_no1.18.1_22")] = sum(ind.plot.no1.18.1_22$estimatedweight/10)
  # descript.results[i,c("weight_no1.14.1_18")] = sum(ind.plot.no1.14.1_18$estimatedweight/10)
  # descript.results[i,c("weight_no1.9.5_14")] = sum(ind.plot.no1.9.5_14$estimatedweight/10)
  # descript.results[i,c("weight_no1.5_9.4")] = sum(ind.plot.no1.5_9.4$estimatedweight/10)
  # descript.results[i,c("weight_jumbo")] = sum(ind.plot.jumbo$estimatedweight/10)
  descript.results[i,c("weight_jumbo_22_27")] = sum(ind.plot.jumbo.22_27$estimatedweight/10)
  descript.results[i,c("weight_jumbo_27more")] = sum(ind.plot.jumbo.27more$estimatedweight/10)
  # Marketwt = sum(ind.plot$estimatedweight/10) - sum(ind.plot.cull$estimatedweight/10)
  # descript.results[i,c("weight_market")] = Marketwt
  descript.results[i,c("weight_no1.jumbo")] = sum(ind.plot.no1.jumbo$estimatedweight/10)
  
  ## weight percentage of individual classes to overall
  descript.results[i,c("percent_wt_canner")] = round(sum(ind.plot.canner$estimatedweight)/sum(ind.plot$estimatedweight) *100, digits = 0)
  descript.results[i,c("percent_wt_cull_no1")] = round(sum(ind.plot.cull_no1$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_no1")] = round(sum(ind.plot.no1$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_no1.18.1_22")] = round(sum(ind.plot.no1.18.1_22$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_no1.14.1_18")] = round(sum(ind.plot.no1.14.1_18$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_no1.9.5_14")] = round(sum(ind.plot.no1.9.5_14$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_no1.5_9.4")] = round(sum(ind.plot.no1.5_9.4$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_cull")] = round(sum(ind.plot.cull$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_jumbo")] = round(sum(ind.plot.jumbo$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_jumbo_22_27")] = round(sum(ind.plot.jumbo.22_27$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  descript.results[i,c("percent_wt_jumbo_27more")] = round(sum(ind.plot.jumbo.27more$estimatedweight)/sum(ind.plot$estimatedweight)*100, digits = 0)
  
  ## weight percentage of individual classes to marketable weight
  descript.results[i,c("percent_Marketwt_canner")] = round(sum(ind.plot.canner$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_cull_no1")] = round(sum(ind.plot.cull_no1$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_no1")] = round(sum(ind.plot.no1$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_no1.18.1_22")] = round(sum(ind.plot.no1.18.1_22$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_no1.14.1_18")] = round(sum(ind.plot.no1.14.1_18$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_no1.9.5_14")] = round(sum(ind.plot.no1.9.5_14$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_no1.5_9.4")] = round(sum(ind.plot.no1.5_9.4$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_cull")] = round(sum(ind.plot.cull$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_jumbo")] = round(sum(ind.plot.jumbo$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_jumbo_22_27")] = round(sum(ind.plot.jumbo.22_27$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
  descript.results[i,c("percent_Marketwt_jumbo_27more")] = round(sum(ind.plot.jumbo.27more$estimatedweight)/(sum(ind.plot$estimatedweight) - sum(ind.plot.cull$estimatedweight))*100, digits = 0)
   
  ## individual plots
  ## weight percentage of No1 groups to overall No1
  descript.results[i,c("percent_no1wt_no1.18.1_22")] = round(sum(ind.plot.no1.18.1_22$estimatedweight)/sum(ind.plot.no1$estimatedweight), digits = 2)
  descript.results[i,c("percent_no1wt_no1.14.1_18")] = round(sum(ind.plot.no1.14.1_18$estimatedweight)/sum(ind.plot.no1$estimatedweight), digits = 2)
  descript.results[i,c("percent_no1wt_no1.9.5_14")] = round(sum(ind.plot.no1.9.5_14$estimatedweight)/sum(ind.plot.no1$estimatedweight), digits = 2)
  descript.results[i,c("percent_no1wt_no1.5_9.4")] = round(sum(ind.plot.no1.5_9.4$estimatedweight)/sum(ind.plot.no1$estimatedweight), digits = 2)
  
  ## individual plots
  ## weight percentage of jumbo groups to overall jumbo
  descript.results[i,c("percent_jumbowt_jumbo_22_27")] = round(sum(ind.plot.jumbo.22_27$estimatedweight)/sum(ind.plot.jumbo$estimatedweight), digits = 2)
  descript.results[i,c("percent_jumbowt_jumbo_27more")] = round(sum(ind.plot.jumbo.27more$estimatedweight)/sum(ind.plot.jumbo$estimatedweight), digits = 2)
}


# Write spbase-ready output to table and database format plot summary files

outfile <- paste(trial_name, "_database_plot_summaries.csv", sep="")
header = as.matrix(t(c("plot_name", "trait", "value", "timestamp", "person", "location", "number")))
write.table(
  header,
  outfile, sep = ",",
  col.names = FALSE,
  row.names = FALSE,
  quote = TRUE
)
plots <- descript.results$plotname
traits <- colnames(descript.results)
times <- descript.results$timestamp

for (i in 1:nrow(descript.results)) {
  for (j in 2:18) {
    write.table(
      as.matrix(t(c(plots[i], traits[j], descript.results[i,j], times[i], person, "HCR", 1))),
      outfile,
      append = TRUE,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE,
      quote = TRUE
    )
  }
}

spbase <- descript.results[,c(1:18)]
colnames(spbase)[1] <- "observationunit_name"
# write.csv(spbase, paste(trial_name, "_table_plot_summaries.csv", sep=""), row.names = F)
WriteXLS(spbase, ExcelFileName=paste(trial_name, "_table_plot_summaries.xls", sep=""), SheetNames = trial_name)

# Write all output to TRIALNAME_complete_plot_summaries file

write.csv(descript.results, paste(trial_name, "_complete_plot_summaries.csv", sep=""), row.names = F)
