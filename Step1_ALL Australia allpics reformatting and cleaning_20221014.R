
# R Script to format and clean ECL camera trap surveys
# Original work by  Zachary Amir (z.amir@uq.edu.au),
# updates by Nathan Travell (n.paul.travell@gmail.com)

########### overall step 1 explanation placeholder ####
# This step 1 code uses detection records per camera (with a date for each photo taken) and standardises it to fit into later steps
# Key components consist of generating a start and stop date, correcting species names, and any other data cleaning
# The final result will be generating a data table, to be joined to the overall dataset, that has the following columns and data types:
  
#        "camera_id"              chr
#        "camera_start.date"      chr
#        "camera_end.date"        chr
#        "Species"                chr
#        "Photo.Date"             chr
#        "Photo.Time"             chr
#        "Individuals"            int
#        "survey_id"              chr
#        "source"                 chr

########### Change log  ##### 
# 12/10/2022 Initial repurposing of Zach's Asian script and renaming to Australian
# 14/10/2022 addedd option to run species corrections without using taxize, added dynamic date saving for the final dataframe

########### To Do ##### 
# initial run through with the K'gari data
# add in data types for step 1A explanation
# add in step 1B explanation
# see if I can remove the last section

########### Known issues ##### 


##### Dependencies ####

{
rm(list = ls())
library(tidyverse) 
library(plyr)
library(chron)
}

##### Set Working directory ####

#dynamic drop box set up
{
  library(jsonlite)
  #a json file is just how data structures are stored, this script looks in variations of appdata folders for a json file that mentions dropbox
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  #this then extracts the filepath from the above
  file_content<-fromJSON(txt=file_name)$personal
  file_content<-file_content$path
  p = file_content
  # set directory where cleaned captures and metadata are 
  setwd(paste(p,"/CT capture histories database/", sep = "")) # this will not change for everybody
  rm(p,file_content,file_name)
}

#Set up all pics col names to use for all files
{
allpic.cols<- c("camera_id", "Species", "yyyy", "mm", "dd", "hr", "min", "ss",
                "Individuals", "survey_id", "source")
new = data.frame(matrix(NA, nrow = 0, ncol = 11))
names(new) = allpic.cols
}

# Check what has already been included, so we only standardize and update 'new' dataframe with data NOT included in old! 
{
  #this lists the files in the folder that contain camera metadata (rather than hard coding most recent file name)
  latest_cam_metadata_file<-list.files(paste(getwd(),"/Australian clean CT data/", sep = ""),pattern = "ECL and Collaborator standardized and independent all captures_*")
  #only want files with clean titles, not training so we get a list of those file names and remove them
  unwanted_cam_meta_file<-list.files(paste(getwd(),"/Asian clean CT data/", sep = ""),pattern = "*_Training")
  latest_cam_metadata_file<-setdiff(latest_cam_metadata_file,unwanted_cam_meta_file)
  #orders them by most recent date first, then select the first line
  latest_cam_metadata_file<-sort(latest_cam_metadata_file,decreasing=TRUE)[1:1]
  #copies the above into Zach's script
  old<-read.csv(paste("Australian clean CT data/",latest_cam_metadata_file, sep = ""))
  #cleanup
  rm(latest_cam_metadata_file,unwanted_cam_meta_file)
}

##### STEP 1A EXPLANATION ####

# Once the data has been loaded and surveys are confirmed not already processed, it needs to be standardised. 
# At the end of step 1A you should end up with a dataframe with the following columns and datatypes
#            "camera_id"        character
#            "Species"          character
#            "Photo.Date"
#            "yyyy"
#            "mm"
#            "dd"
#            "Photo.Time"
#            "hr"
#            "min"        
#            "ss"
#            "Individuals"
#            "survey_id"       character
#            "source"          character


################ Step 1A: Surveys by Emma Tilley #####

#check if Emma tilley's data has already been standardised, if not - standardise it
{
surv = "Emma_Tilley"

if(surv %in% old$source){
  
  print(paste(surv, "has already been standardized"))
}else{
  ET.allp<- read.csv("Australian Collaborator raw CT data/Emma_Tilley/ECL_camera_trap_records_for_BIOL2015.csv")
  ET.allp$source = "Emma_Tilley"
  ET.allp$Individuals = 1
  
  #quick checks - this is really good data!
  {
  head(ET.allp)
  sort(unique(ET.allp$species)) #a few full stops, and "bush" turkey with both a space and underscore but nothing that won't be picked up by the corrections later
  str(ET.allp)
  range(ET.allp$Photo.Time)
  }
    
  #Create Photo date with correct data type, and split out d,m,y into seperate columns
  {
    # The reason for this is that the capture records are not independent, E.G.
    # kroombit 8/6/21, 2 macropod_sp photos were taken at 1am
    # Mt-superbus 5/1/21 2 red-legged_pademelons were photographed at 3am
  ET.allp$Photo.Date<-as.factor(ET.allp$Photo.Date)
  ET.allp$Photo.Date<-strptime(ET.allp$Photo.Date,format="%d/%m/%y") #defining what is the original format of your date
  ET.allp$Photo.Date<-as.Date(ET.allp$Photo.Date,format="%m/%d/%y") 
  hist(ET.allp$Photo.Date, breaks = unique(ET.allp$Photo.Date))
  ET.allp$dd<-format(as.Date(ET.allp$Photo.Date,format="%m/%d/%y") ,"%d")
  ET.allp$mm<-format(as.Date(ET.allp$Photo.Date,format="%m/%d/%y") ,"%m")
  ET.allp$yyyy<-"2021"
  }
  
  #Create Photo Time with correct data type and split out into seperate columns
  {
    ET.allp$Photo.Time<-as.factor(ET.allp$Photo.Time)
    ET.allp$hr = format(as.POSIXct(ET.allp$Photo.Time,format="%H:%M:%S"),"%H")
    ET.allp$min = format(as.POSIXct(ET.allp$Photo.Time,format="%H:%M:%S"),"%M")
    ET.allp$ss = format(as.POSIXct(ET.allp$Photo.Time,format="%H:%M:%S"),"%S")
    ET.allp$Photo.Time<-chron(times=ET.allp$Photo.Time)
  }

  #check survey lengths - max survey was 95 days, that's fine
  {
    surveylengths<-dplyr::select(ET.allp, survey_id,Photo.Date)
    surveylengths <- surveylengths %>% 
    group_by(survey_id) %>% 
    filter(row_number()==1 | row_number()==n()) %>% 
    ungroup()
    surveylengths= do.call("cbind", split(surveylengths, rep(c(1, 2), length.out = nrow(surveylengths))))
    surveylengths$`2.survey_id`=NULL
    surveylengths$diff=surveylengths$`2.Photo.Date`-surveylengths$`1.Photo.Date`
  }
  
  #think these need to be removed to fit into the entire datastructure (we'll calculate them later anyway..)
  {
  ET.allp$camera_end.date<-NULL
  ET.allp$camera_start.date<-NULL
  }
  
  #rearrange columns and save in the new DF
  {
  ET.allp<-dplyr::select(ET.allp,'camera_id','species','Photo.Date','yyyy', 'mm','dd', 'Photo.Time',
                        'hr', 'min','ss', 'Individuals', 'survey_id', "source")
  new = rbind(new, ET.allp)
  rm(ET.allp, surveylengths,surv)
  }
}
}
################ Step 1A: Surveys K'gari #####

#check if the K'gari data has already been standardised, if not - standardise it
{
  surv = c('BIOL2015 Class of 2022',
           'BIOL2015 Classes 2014, 2017, 2018, 2019, 2021',
           'Mainland QLD cameras 2021',
           'Potoroo fire cameras 2022',
           "QPWS K'gari single longterm camera 2016-2022")

  
  if(surv %in% old$source){
    
    print(paste(surv, "has already been standardized"))
  }else{
    kgari.allp<- read.csv("Australian Collaborator raw CT data/K'gari/Standardized_ALL_BIOL2015_cam_trap_records_20220626.csv")
    
    
    #quick checks - this is really good data!
    {
      head(kgari.allp)
      sort(unique(kgari.allp$Species)) #looks good - a few spp, but that'll change later
      str(kgari.allp)
      range(kgari.allp$Photo.Time)
    }
    
    #Create Photo date with correct data type, and split out d,m,y into seperate columns
    {
      kgari.allp$Photo.Date<-as.factor(kgari.allp$Photo.Date)
      kgari.allp$Photo.Date<-strptime(kgari.allp$Photo.Date,format="%Y-%m-%d") #defining what is the original format of your date
      anyNA(kgari.allp$Photo.Date)
      kgari.allp$Photo.Date<-as.Date(kgari.allp$Photo.Date,format="%m/%d/%y") 
      hist(kgari.allp$Photo.Date, breaks = unique(kgari.allp$Photo.Date))
    }
    
    #Create Photo Time with correct data type and split out into seperate columns
    {
      kgari.allp$Photo.Time<-as.factor(kgari.allp$Photo.Time)
      kgari.allp$Photo.Time<-chron(times=kgari.allp$Photo.Time)
      anyNA(kgari.allp$Photo.Time)
    }
  
    #rearrange columns and save in the new DF
    {
      names(kgari.allp)[names(kgari.allp) == 'Species'] <- "species"
      names(kgari.allp)[names(kgari.allp) == 'Year'] <- "yyyy"
      names(kgari.allp)[names(kgari.allp) == 'Month'] <- "mm"
      names(kgari.allp)[names(kgari.allp) == 'Day'] <- "dd"
      names(kgari.allp)[names(kgari.allp) == 'Hour'] <- "hr"
      names(kgari.allp)[names(kgari.allp) == 'Minutes'] <- "min"
      names(kgari.allp)[names(kgari.allp) == 'Seconds'] <- "ss"
      kgari.allp<-dplyr::select(kgari.allp,'camera_id','species','Photo.Date','yyyy', 'mm','dd', 'Photo.Time',
                             'hr', 'min','ss', 'Individuals', 'survey_id', "source")
      new = rbind(new, kgari.allp)
      rm(kgari.allp,surv)
    }
  }
}

###################### Inspect new and old ####

#dataframe inspection
{
head(new)
dim(new) 
str(new)
}

# make sure numbers are numeric for inspection 
{
new$hr<-as.numeric(new$hr)
new$min<-as.numeric(new$min)
new$ss<-as.numeric(new$ss)
new$dd<-as.numeric(new$dd)
new$mm<-as.numeric(new$mm)
new$yyyy<-as.numeric(new$yyyy)
}

#check for NAs
anyNA(new) 

#check which surveys are being added
{
sort(unique(new$survey_id)) #these are the new data to be added 
sort(unique(old$survey_id)) ## inspect old survey IDs 
setdiff(new$survey_id, old$survey_id) #which surveys are in new and NOT in old.
}

################ Step 1B: Make captures independent #########

#data structure set up
{
data = new
backup = new #save a back-up b/c new gets overwritten here 
colnames(data)
colnames(data)=c('camera_id','Species','Photo.Date','Year', 'Month','Day', 'Photo.Time',
                 'Hour', 'Minutes','Seconds', 'Individuals', 'survey_id', "source")
}

#Remove non-independent records
{
cronologico = T
independenciaTmin = T
limit = 30 # Independence period in minutes. It can be changed
d=data

# Order the species by chronological order per camera
# records of a single species per camera are grouped in chronological order
# Speed! -->

if(cronologico == T | independenciaTmin == T){
  rownames(d) = NULL
  
  speed = d[0,]
  Temp=d[0,]
  
  for(i in unique(d$camera_id)){
    t=subset(d, camera_id == i)
    new = d[0,]
    
    #Species
    for(sp in sort(unique(t$Species))){
      t1=subset(t, Species == sp)
      #Year
      for(y in sort(unique(t1$Year))){
        t2=subset(t1, Year == y)
        
        #Month
        for(m in sort(unique(t2$Month))){
          t3=subset(t2, Month == m)
          #Day
          for(day in sort(unique(t3$Day))){
            t4=subset(t3, Day == day)
            #Hour
            for(h in sort(unique(t4$Hour))){
              t5=subset(t4, Hour == h)
              #minutes
              for(mn in sort(unique(t5$Minutes))){
                t6=subset(t5, Minutes == mn)
                new = rbind(new,t6)
              }
            }
          }
        }
      }
    }
    
    Temp = rbind(Temp, new)
    print(unique(as.character(new$camera_id)))
    
    if(nrow(Temp) > 3000){
      speed = rbind(speed, Temp)
      Temp=d[0,]
      print("Speed!")
    }
    
  }
  speed = rbind(speed, Temp)
  
  DD = speed
  rm(t,t1,t2,t3,t4,t5,t6,i,m,d,h,sp,mn,y,new,Temp, speed)
}
}

#quick checks
{
head(DD)
dim(DD)
rm(cronologico)
rownames(DD) = NULL
head(DD)
d=DD
str(d)
}

# Make the records independent - WARNING: may take a while
{
a = Sys.time()
if(independenciaTmin == T){
  d$type = rep("NA", nrow(d))
  correct = F
  for(i in 2:nrow(d)){
    if(correct == T){
      b = reference
      correct = F
    }else{
      b = i-1
    }
    if(d$Species[i] == d$Species[b] & d$camera_id[i] == d$camera_id[b] &
       d$Year[i] == d$Year[b] & d$Month[i] == d$Month[b] & d$Day[i] == d$Day[b] &
       d$Hour[i] - d$Hour[b] <= 1){
      if(limit == 60){
        if(d$Hour[i] - d$Hour[b] == 0 | (limit - d$Minutes[i]) + d$Minutes[b] <= limit){
          d$type[i] = "Delete"
          reference = b
          correct = T
        }
      }else{
        if(d$Hour[i] - d$Hour[b] == 0){
          if(d$Minutes[i] - d$Minutes[b] <= limit){
            d$type[i] = "Delete"
            reference = b
            correct = T
          }
        }else{
          if(sum(d$Minutes[i],60) - d$Minutes[b] <= limit){
            d$type[i] = "Delete"
            reference = b
            correct = T
          }
        }
      }
    }
    print(paste(i, as.character(correct)))
  }
  rm(i,b,correct,reference)
  d = d[d$type != "Delete",]
  rownames(d) = NULL
}
rm(independenciaTmin, limit, day)
rownames(d) = NULL
b = Sys.time()
b-a # Time difference of 1.6 hours w/ 1,100,000+ records, 0.37 secs for 5473 records (2.43 minutes for 1.1mill?)
rm(a,b)
d$type = NULL
rm(DD,data,backup)
}

################ Step 1C: Standardize species names ############

Captures= d
rm(d)
#write.csv(Captures,"C:/Users/NPTra/Desktop/Captures.csv",row.names = FALSE)
#Captures<-read.csv("C:/Users/NPTra/Desktop/Captures.csv")
Captures$Species<- as.character(Captures$Species)
sort(unique(Captures$Species))

#Standardize the spaces  and remove "sp.", "spp", etc.
Captures$Species = gsub("_", " ", Captures$Species)
Captures$Species <- Captures$Species %>%  
  str_replace_all(c("\xa0" = " ",
                    " spp. " =" sp",
                    " sp." = " sp",
                    " sp" = " sp",
                    " spp" = " sp",
                    " " = "_" ))
#check it's all clean
sort(unique(Captures$Species))

#Calebe's automatic species correction
{
#Change  some records for more scientific terms as per Calebe's recommendation
Captures$Species[Captures$Species == "bat"] = "Chiroptera"
Captures$Species[Captures$Species == "Bat"] = "Chiroptera"
Captures$Species[Captures$Species == "Rodent"] = "Rodentia"
Captures$Species[Captures$Species == "Rodent_"] = "Rodentia"
Captures$Species[Captures$Species == "Snake"] = "Serpentes"

library(taxize)

# Make a species list for standardization
species_list = sort(unique(Captures$Species))

#search in NCBI
#NOTE this takes a while, and will ask for confirmations - you will have to type the correct number in the console below
# type NA if nothing matches
NCBI_uids <- get_ids(species_list, db = 'ncbi', verbose = F)
NCBI <- classification(NCBI_uids)

#Which species were not found?
missing = attr(NCBI_uids$ncbi, "names")[attr(NCBI_uids$ncbi, "match") != "found"] 

#search the missing ones in GBIF
#NOTE this takes a while, and will ask for confirmations - you will have to type the correct number in the console below
GBIF_uids <- get_ids(missing, db = 'gbif', verbose = F) 
GBIF <- classification(GBIF_uids)

#Are there any species still missing?
missing = attr(GBIF_uids$gbif, "names")[attr(GBIF_uids$gbif, "match") != "found"] 
if(length(missing) == 0){rm(missing, species_list)}


### Create a function to facilitate accessing the classification info

extract.classif = function(uids = NULL, classif = NULL, request = c("species","genus","family","order","class")){
  
  require(tidyverse)
  require(taxize)
  
  #warnings
  if(is.null(uids)){ print("Missing argument 1: IDs. Use taxize::get_ids() to obtain the ids"); break }
  if(is.null(classif)){ print("Missing argument 2: Classification. Use taxize::classification() to obtain the classification"); break }
  
  #make a table
  sp = data.frame(user_provided_name = attr(uids[[1]],"names")) %>% 
    cbind(data.frame(uids[[1]])) %>% 
    select(-"class")
  
  t = data.frame(matrix(NA,nrow = nrow(sp),ncol = length(request)))
  colnames(t) = request
  sp = cbind(sp,t)
  rm(t)
  
  #remove NAs
  sp = sp[!is.na(sp$ids),] 
  classif[[1]] =  classif[[1]][!is.na( classif[[1]])] 
  
  #extract
  for(i in sp$ids){ #id loop
    t = classif[[1]][names(classif[[1]]) == i][[1]]
    
    for(a in request){ #request loop
      
      if(length(t$name[t$rank == a]) >0) { #to avoid error. It is not possible to extract species from a family lvl record
        sp[sp$ids == i,colnames(sp) == a] = t$name[t$rank == a]
      }
    }#request loop
  } #id loop
  return(sp)
}

#extract
NCBI_verified = extract.classif(NCBI_uids,NCBI)
GBIF_verified = extract.classif(GBIF_uids,GBIF)

#merge
verified = rbind(NCBI_verified,GBIF_verified)
rm(NCBI_verified,GBIF_verified, GBIF_uids, NCBI_uids, NCBI, GBIF)

#clean
verified = verified %>% 
  select(c("user_provided_name","uri","species","genus","family","order","class"))

#if the taxa was not identified to the species level, than the most precise level is used

verified$binomial_verified = NA
verified$taxonomic_level = NA

for(i in 1:nrow(verified)){
  
  #Make a final verified_binomial column with the most precise taxonomic level found for each given taxa
  if(!is.na(verified$species[i])){
    verified$binomial_verified[i] = verified$species[i]
    verified$taxonomic_level[i] = "species"
    
    #Change species to remove the genus
    verified$species[i] = strsplit(verified$species[i], " ")[[1]][length(strsplit(verified$species[i], " ")[[1]])]
    
  }else if(!is.na(verified$genus[i])){
    verified$binomial_verified[i] = verified$genus[i]
    verified$taxonomic_level[i] = "genus"
  }else if(!is.na(verified$family[i])){
    verified$binomial_verified[i] = verified$family[i]
    verified$taxonomic_level[i] = "family"
  }else if(!is.na(verified$order[i])){
    verified$binomial_verified[i] = verified$order[i]
    verified$taxonomic_level[i] = "order"
  }
}; rm(i)

#clean up verified names table
{
verified$uri<-NULL
verified$species<-NULL
verified$genus<-NULL
verified$family<-NULL
verified$order<-NULL
verified$class<-NULL
verified$taxonomic_level<-NULL
verified<-na.omit(verified)
}

rm(missing, species_list,extract.classif)
}

# Setting up a dynamic dropbox directory
{
  library(jsonlite)
  #a json file is just how data structures are stored, this script looks in variations of appdata folders for a json file that mentions dropbox
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  #this then extracts the filepath from the above
  file_content<-fromJSON(txt=file_name)$personal
  file_content<-file_content$path
  #pulled from Zach's script
  p = file_content
  # set directory where cleaned captures and metadata are 
  setwd(paste(p,"/CT capture histories database/R scripts CT data cleaning",sep = "")) 
  rm(p,file_content,file_name)
}

# Most recent species mapping file
{
  #captures = read.csv("Species mapping_20220906.csv", header = T)
  #this lists the files in the folder that contain camera metadata (rather than hard coding most recent file name)
  latest_mapping_file<-list.files(getwd(),pattern = "Species mapping_*")
  #orders them by most recent date first, then select the first line
  latest_mapping_file<-sort(latest_mapping_file,decreasing=TRUE)[1:1]
  corrections<-read.csv(paste(getwd(),paste("/",latest_mapping_file, sep = ""),sep = ""))
  corrections$Comments<-NULL
  #cleanup
  rm(latest_mapping_file)
}

# Run this if using both Calebe's taxize correction and the manual correction:
# Merge dfs
Captures2<-merge(Captures, verified, by.x = "Species", by.y = "user_provided_name", all.x = TRUE)

#run this if using the manual correction only:
Captures<-merge(Captures, corrections, by.x = "Species", by.y = "OLD", all.x = TRUE)
anyNA(Captures$NEW) # check if any unstandardised species names exist

# Calebe's taxize correction: Grab a list of missing mappings
{
  requiredmappings<-Captures2[is.na(Captures2$binomial_verified),]
  #names(requiredmappings)[names(requiredmappings) == 'Species'] <- 'OLD'
  View(requiredmappings)
  sort(unique(requiredmappings$Species))
}

# manual correction only: Grab a list of missing mappings (this will be empty if anyNA returned FALSE)
{
  requiredmappings<-Captures[is.na(Captures$NEW),]
  #names(requiredmappings)[names(requiredmappings) == 'Species'] <- 'OLD'
  #write.csv(requiredmappings,"C:/Users/NPTra/Desktop/requiredmappings.csv",row.names = FALSE)  #another manual way of adding species
  #newmappings<-read.csv("C:/Users/NPTra/Desktop/requiredmappings.csv")     #another manual way of adding species
  #corrections<-rbind(corrections,newmappings)  #another manual way of adding species
  #rm(newmappings)
  View(requiredmappings)
  sort(unique(requiredmappings$Species))
}

# Manually add new mappings based on NAs presented above - dont just run this - make sure the corrections are relevant!
#manualcorrections<-c("bird","bird","bird","Ursidae_spp","Orthotomus_spp")

# Add these new mappings back to the required mapping list (only if you have any)
for(i in 1:nrow(requiredmappings)){
  requiredmappings$NEW[i] = as.character(manualcorrections[i])
}
rm(manualcorrections,i)

# Add the new rows
{
  corrections<-rbind(requiredmappings,corrections)
  rm(requiredmappings)
}

# Check everything maps with no missing values by removing the joined column and rejoining the mapping list (with your additions above)
{
  Captures$NEW<-NULL
  Captures<-merge(Captures, corrections, by.x = "Species", by.y = "OLD", all.x = TRUE)
  anyNA(Captures$NEW)  #great!
}

# Save the updated file with today's date
{
  library(stringr)
  day<-str_sub(Sys.Date(),-2)
  month<-str_sub(Sys.Date(),-5,-4)
  year<-str_sub(Sys.Date(),-10,-7)
  filename<-paste(paste(paste(paste("Species mapping_",year,sep=""),month,sep=""),day,sep=""),".csv",sep="")
  rm(day,month,year)
  write.csv(corrections,filename,row.names = FALSE)
  rm(corrections,filename)
}

# Replace old species with correct mappings, remove redundant column
{
  Captures$Species<-Captures$NEW
  Captures$NEW<-NULL
  anyNA(Captures$Species) #good
}

################ Step 1D: Generate Camera Start/Stop dates from first/last records ######
#Captures<-read.csv("C:/Users/NPTra/Desktop/Captures.csv")
ecl.allp<- Captures

starts= ecl.allp %>%
  dplyr::select(camera_id, Photo.Date) %>%
  group_by(camera_id) %>%
  dplyr::summarise(sort(Photo.Date)[1])
names(starts)[2]= "camera_start.date"

stops= ecl.allp %>%
  dplyr::select(camera_id, Photo.Date) %>%
  group_by(camera_id) %>%
  dplyr::summarise(max(Photo.Date))
names(stops)[2]= "camera_end.date"

start.stop<- left_join(starts,stops, by = "camera_id")

## calculate how long each cam was active
start.stop$dur = as.numeric(difftime(start.stop$camera_end.date,
                                     start.stop$camera_start.date, units = "days"))
head(start.stop)
summary(start.stop$dur) #some cams are active WAYY too long...
start.stop$camera_id[start.stop$dur > 300]

## Visualize effort via histogram to ensure they are good 
ggplot(start.stop, aes(x = dur))+
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 150)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept = mean(dur)), linetype = "dashed")+
  theme_test()+
  labs(x = "Trap-nights per Camera", y="Frequency")

## Why are there many at zero??
c = start.stop[start.stop$dur == 0,]
sort(unique(c$camera_id)) 

## Long story short, I figured out all these cams were only active for one day!
## therefore, no difference in start/stop dates, and this underestimates our effort. 

## 1 cam active on 1 day == 1 trap night, but if we calculate effort from 
## same start/stop date, then effort == 0!

## Add + 1 to the end date of these problem cams
c$camera_end.date<-as.factor(c$camera_end.date)
c$camera_end.date<-strptime(c$camera_end.date,format="%Y-%m-%d") #defining what is the original format of your date
anyNA(c$camera_end.date)
c$camera_end.date<-as.Date(c$camera_end.date,format="%Y/%m/%d") 
anyNA(c$camera_end.date)
c$camera_end.date = c$camera_end.date + 1

#remove these cams from original start/stops
start.stop = start.stop[!start.stop$camera_id %in% c$camera_id,]
start.stop = rbind(c, start.stop)

#remove duration before merging (it is contained in metadata)
start.stop$dur  = NULL

## merge w/ captures
ecl.allp<- as.data.frame(left_join(start.stop, ecl.allp, by = "camera_id"))
head(ecl.allp)

rm(starts, stops, start.stop)


### Verify that start and stops are good
check = distinct(ddply(ecl.allp, .(camera_id, survey_id), summarise,
                       dur = as.numeric(difftime(camera_end.date, camera_start.date))))
head(check)
sum(check$dur)

## Visualize via histogram
ggplot(check, aes(x = dur))+
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 150)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept = mean(dur)), linetype = "dashed")+
  theme_test()+
  labs(x = "Trap-nights per Camera", y="Frequency")

## Verify no cams w/ zero effort
check[check$dur == 0,] #None!


## Examine and visualize some overall stats from the dataset. 
summary(check$dur) #big stats! 
length(unique(check$survey_id))
length(unique(check$camera_id))

rm(check, c)



###################### Step 1E: Make sure final data set is all good and SAVE! ####
new = ecl.allp

head(new)
dim(new) 
summary(new)
str(new)
sort(table(new$Species))
# must never have ANY NAs
anyNA(new) 

#trim the fat and select only what we need
ecl.allp<- dplyr::select(ecl.allp, c("camera_id", "camera_start.date", "camera_end.date", "Species",
                                     "Photo.Date", "Photo.Time", "Individuals", "survey_id", "source"))
# bind to the old! 
head(old)
head(ecl.allp)

# but make sure all classes match first! 
str(old)
str(ecl.allp) #change these to character 
ecl.allp = ecl.allp %>% 
  mutate(camera_start.date = as.character(camera_start.date),
         camera_end.date = as.character(camera_end.date),
         Photo.Date = as.character(Photo.Date),
         Photo.Time = as.character(Photo.Time))

final = rbind(old, ecl.allp)
anyNA(final)
tail(final)

# Setting up a dynamic dropbox directory for saving as species mapping file changes wd
{
  library(jsonlite)
  #a json file is just how data structures are stored, this script looks in variations of appdata folders for a json file that mentions dropbox
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  #this then extracts the filepath from the above
  file_content<-fromJSON(txt=file_name)$personal
  file_content<-file_content$path
  p = file_content
  # set directory where cleaned captures and metadata are 
  setwd(paste(p,"/CT capture histories database/", sep = "")) # this will not change for everybody
  rm(p,file_content,file_name)
}

# Save the updated file with today's date
{
  library(stringr)
  day<-str_sub(Sys.Date(),-2)
  month<-str_sub(Sys.Date(),-5,-4)
  year<-str_sub(Sys.Date(),-10,-7)
  filename<-paste(paste(paste(paste("Australian clean CT data/ECL and Collaborator standardized and independent all captures_",year,sep=""),month,sep=""),day,sep=""),".csv",sep="")
  rm(day,month,year)
  write.csv(final,filename,row.names = FALSE)
}

####### Send ECL data to collaborators - WHAT DOES THIS DO, CAN WE SCRAP IT? #####

## captures
# dat = read.csv("ECL and Collaborator standardized and independent all captures_20220214.csv")
head(dat)

##susbet for ecl data
dat = dat[dat$source == "ECL",]
sort(table(dat$survey_id))

## remove unnecessary cols
dat$X<- NULL
dat$source<- NULL
dat$camera_start.date<- NULL
dat$camera_end.date<- NULL # will get start/stops from meta
head(dat)

## remove baited danum cams
r = dat[dat$survey_id == "Danum_2020",]
r = r[endsWith(r$camera_id, "_SR"),]
sort(unique(r$camera_id)) 

#remove!
dat = dat[!dat$camera_id %in% r$camera_id,]
rm(r)
head(dat)
str(dat) # should be good to go!


## metadata
# met = read.csv("ECL and Collaborator standardized camera trap deployment data_20220214.csv")
head(met)

## thin meta to match dat
met = met[met$source == "ECL",]
setdiff(met$camera_id, dat$camera_id) # only danum_2020 SR cams in meta --> remove! 

met = met[met$camera_id %in% dat$camera_id,]

## make sure they mathc
setdiff(met$camera_id, dat$camera_id)
setdiff(dat$camera_id, met$camera_id)

# remove unneeded cols
head(met)
met$X.1<- NULL
## I will generate UTMs from their lat long
met$X<- NULL
met$Y<- NULL
met$UTM_zone<- NULL
met$source = NULL

## inspect and save
head(met)
str(met)

head(dat)
str(dat)
 
# write.csv(met, "ECL camera trap deployment data_20220218.csv", row.names = F)
# write.csv(dat, "ECL camera trap records_20220218.csv", row.names = F)




