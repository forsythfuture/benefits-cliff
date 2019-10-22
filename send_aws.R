###############################################
#
# this script sends datasets and plots to aws
# it will not run correctly without proepr credentials
#
###############################################

library(tidyverse)
library(aws.s3)

put_object("plots/benefits_cliff_hc.html",
           object = "plots/benefits_cliff_hc.html",
           bucket = "benefits-cliff",
           acl = "public-read")

