###############################################
#
# this script sends datasets and plots to aws
# it will not run correctly without proepr credentials
#
###############################################

library(tidyverse)
library(aws.s3)

put_object("plots/data/benefits.json",
           object = "data/benefits.json",
           bucket = "benefits-cliff",
           acl = "public-read")

