################################################################################################

# the primary data and replicate weights were downloaded from https://www.census.gov/programs-surveys/sipp/data/datasets/2018-data/2018.html
# SIPP 2018 Metadata describes the variables, the Users' Guide helps to provide an understanding of SIPP
# and the replicate weights
# the code below helps import and join both the primary data and the replicate weights into one dataset

################################################################################################

# The following code is an example of reading the pipe-delimited Survey of Income and Program Participation (SIPP) 
# 	data into an R dataframe in preparation for analysis. Specifically, this code loads in both the primary data file 
#   and the calendar-year replicate weights file (as opposed to the longitudinal replicate weights). These files are 
#   separate downloads on the SIPP website.
# SIPP data are in person-month format, meaning each record represents one month for a specific person.
#   Unique persons are identified using SSUID+PNUM. Unique households are identified using SSUID+ERESIDENCEID. For 
#   additional guidance on using SIPP data, please see the SIPP Users' Guide at <https://www.census.gov/programs-surveys/sipp/guidance/users-guide.html>
# This code was written in R 4.1.0, and requires the "data.table", "dplyer", and "bit64" packages. 
# Note the 'select' statement in the first use of fread(). Most machines do not have enough memory to read
# 	the entire SIPP file into memory. Use a 'select' statement to read in only the columns you are interested in using. 
#   If you still encounter an out-of-memory error, you must select less columns or less observations.
# Run this code from the same directory as the data.
# This code was written by Adam Smith. Please contact census.sipp@census.gov if you have any questions.

#Load the "data.table", "dplyer", and "bit64" libraries
require("data.table")
require("bit64")
require("dplyr")

#Read in the Primary Data file. Choose only the variables you want to read in in order to save on memory.
#This code assumes that your working directory is the same directory as the data.
ds <- c("pu2018.csv")
pu <- fread(ds, sep = "|", select = c(
  
  #Common case identification variables
  'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
  
  #The base weight
  'WPFINWGT',
  
  #Common demographics variables, including age at time of interview (TAGE)
  #	and monthly age during the reference period (TAGE_EHC)
  'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','TEHC_ST','THHLDSTATUS','RHNUMPER',
  
  #Example additional variables for analysis
  'TPTOTINC','THNETWORTH','THINC_BANK','THVAL_BANK','THINC_BOND','THINC_STMF','TTHR401VAL','TIRAKEOVAL','THINC_OTH','THINC_AST'))

#Make sure all the column names are upper-case
names(pu) <- toupper(names(pu))

#Preview the data
head(pu, 20)

#check some means against the validation xls file to help ensure that the data
#	were read in correctly. Note that the validation xls files do not include all variables.
mean(pu[["TPTOTINC"]], na.rm = TRUE)

#Read in the replicate-weight data. This dataset is small enough that most machines
#	can read the whole file into memory
dw <- c("rw2018.csv")
rw <- fread(dw, sep = "|")

#Make sure all the column names are upper-case
names(rw) <- toupper(names(rw))

#Preview the data
head(rw, 20)

#check some means against the validation xls file to help ensure that the data
#	were read in correctly. Note that the validation xls files do not include all variables.
mean(rw[["REPWGT100"]], na.rm = TRUE)

#Merge primary data and replicate weights on SSUID, PNUM, MONTHCODE, SPANEL, and SWAVE
data <- left_join(pu, rw, by = c("SSUID","PNUM","MONTHCODE","SPANEL","SWAVE"))

#preview the merged data
head(data, 20)

# used https://www2.census.gov/programs-surveys/sipp/Select_approp_wgt_2014SIPPpanel.pdf as a reference
# to calculate annual estimates, used the filters below to do so
data <- data %>%
  filter(ERELRPE %in% c(1,2)) %>%
  filter(MONTHCODE == 12)

# race/ethnicity recode
data <- data %>%
  mutate(`Race Ethnicity` = if_else(EORIGIN == 1, "Hispanic/Latino", if_else(ERACE == 1, "White, NH", 
                            if_else(ERACE == 2, "Black/AA, NH", "Other Race"))))

