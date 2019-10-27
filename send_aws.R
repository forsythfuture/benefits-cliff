###############################################
#
# this script sends datasets and plots to aws
# it will not run correctly without proepr credentials
#
###############################################

library(tidyverse)
library(aws.s3)

put_object("plots/data/income_diff.json",
           object = "plots/data/income_diff.json",
           bucket = "benefits-cliff",
           acl = "public-read")