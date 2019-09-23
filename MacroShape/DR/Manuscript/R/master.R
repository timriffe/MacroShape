# master script to run the others in order.

# Human Mortality Database:
# -you need to become a registered user at www.mortality.org

# First, enter you HMD username and password:
# BE CAREFUL NOT TO SAVE THESE IN THE SCRIPT!
# YOU MIGHT PREFER TO DEFINE THEM IN THE CONSOLE
us <- "HMD username" # change to your HMD username
pw <- "HMD password" # change to your HMD password

# install dependencies if necessary. This may still hit a block
# if for some reason devtools doesn't run properly (maybe due
# to Windows users without Rtools installed). Also HMDHFDplus requires
# XML and ssl and stuff like that, usually already installed, but will choke
# (and give informative error) if missing.
source(file.path("R","prepare_session.R"))

# load custom functions
source(here("R","functions.R"))

# check if HMD already downloaded, set flag. If you already have this 
# file then let's not download it again!
download_HMD <- !file.exists(here("Data","HMDltper.rds"))
# If the file isn't there, then it will be created.
# a new column for VAR added in any case
source(here("R","prepare_data.R"))

# produce figures 2,3,4
source(here("R","figs_2_3_4.R"))

# end

# optionally make subplots to figure 1:
# source(here("R","fig1_subplots.R"))

