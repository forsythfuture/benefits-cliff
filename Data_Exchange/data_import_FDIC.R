##################################################################################################################################

# the script imports 2019 data from the FDIC https://economicinclusion.gov/downloads/#yearly to perform an analysis
# on the unbanked population with NC. No data was present to perform an analysis on the underbanked population so
# one should view https://scorecard.prosperitynow.org/data-by-issue#finance/outcome/underbanked-households 2017 results

##################################################################################################################################

# library(survey)
# options("survey.replicates.mse=TRUE")
# options(scipen=999)
library(srvyr)

##################################################################################################################################

### function to take data frame and convert codes to factors based on metadata file

applyFormats <- function(hh){
  
  library(sqldf)
  
  options(gsubfn.engine = "R")
  # metadata folder path
  metadata=read.csv("hh2019/metadata/metadata.csv")
  # code to convert raw codes to factors from metadata
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  #recoding raw codes to metadata labels in r
  levels(metadata$VariableName)<-tolower(trim(levels(metadata$VariableName)))
  
  vars<-sqldf('select distinct VariableName,VarLabel from metadata')
  
  #for each var in meta data conver var in to factor and use labels
  for (i in levels(metadata$VariableName)[levels(metadata$VariableName) %in% names(hh)] )
  {
    print(i)
    if (as.character(subset(metadata,VariableName==i)$Value)!="")
    {
      hh[i]<-factor(unlist(hh[i]),levels=subset(metadata,VariableName==i)$Code,labels=as.character(subset(metadata,VariableName==i)$Value))
      table(hh[i])
    }
  }
  
  hh$h<-hh$hhsupwgt/10000000
  
  return(hh)
  
}

##################################################################################################################################

# example change to file you want to read in

hh<-read.csv("hh2019/hh2019_analys.csv",na.strings=".")
hh=applyFormats(hh)

# pull only NC
hh <- hh %>%
  filter(gestfips == 37)

# go to the metadata folder and open the metadata.csv to view the different variables and their corresponding levels
# race/ethnicity recode
hh <- hh %>%
  mutate(`Race Ethnicity` = if_else(praceeth == 2, "Hispanic/Latino", if_else(praceeth == 6, "White, NH", 
                                                                             if_else(praceeth == 1, "Black/AA, NH", "Other Race"))))

#subset to respondents
#hh=subset(hh, hsupresp=='Respondent') #this does not work, I am not sure what's it purpose

# survey design object
# hh_svy <- svydesign(id = ~1, weights = ~h, data = hh, repweights = hh[,grepl("repwgt.*",names(hh)) & !grepl("repwgt0.*", names(hh))],
#                     type="JKn", scale = 0.025, rscales = rep(1, 160), combined.weights = TRUE)

