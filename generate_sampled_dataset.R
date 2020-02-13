
# run as

# Rscript generate_sampled_dataset.R \
#     input_scraped \
#     input_matches \
#     max_time_minutes \
#     output_csv

# read in the command arguments
args <- commandArgs( trailingOnly = TRUE )
input_scraped <- args[1]
input_matches <- args[2]
max_time_minutes <- as.numeric( args[3] )
output_csv <- args[4]

# read in the data
dat <- read.csv( input_scraped, as.is = TRUE)

# strip off the age and add name to data frame
chop_off_age <- function( name_age ){
    nc <- nchar(name_age)
    # chop until we get to "("
    while( substr(name_age, nc, nc) != "(" ){
        name_age <- substr(name_age, 1, nc-1)
        nc <- nchar(name_age)
    }
    # chop off final portion
    if( substr(name_age,nc-1,nc) == " (" ){
        name_age <- substr(name_age, 1, nc-2)
    } else {
        name_age <- substr(name_age, 1, nc-1)
    }
    return(name_age)
}
name_only <- sapply( dat$name_age, chop_off_age )
dat$full_name <- toupper( name_only )

# move net time to time if time is missing
missing_time <- is.na(dat$time)
dat$time[ missing_time ] <- dat$net_time[ missing_time ]

# convert times to minutes and seconds
time_split <- lapply( strsplit( dat$time, ":" ), as.numeric )
dat$time_minutes <- sapply( time_split, function(x) 60*x[1] + x[2] + x[3]/60 )

# flip first and last for races listing last first.
# this isn't perfect. When there are 3 or or more names,
# not always possible to tell which is first and list
race_data <- read.csv("race_codes.csv",as.is=TRUE)
race_inds <- which( race_data$first_last == "FALSE" )
flip_midd <- race_data$midd[race_inds]
result_inds <- which( dat$midd == flip_midd )
name_split <- strsplit( dat$full_name[result_inds], " " )
name_flip <- lapply( name_split, 
    function(x) paste( x[length(x)], paste(x[1:(length(x)-1)],collapse=" ") ) 
)
dat$full_name[ result_inds ] <- unlist( name_flip )
dat$name_len <- nchar( dat$full_name )

# process the matches in input_matches
# for each row in matches, look for names that 
# matches the second name in the row.
# assign the first name in the row
matches <- read.csv(input_matches, as.is=TRUE, header=FALSE)
dat$match_name <- dat$full_name
for(j in 1:nrow(matches)){
    # update the dataset
    inds <- dat$full_name == matches[j,2]
    dat$match_name[inds] <- matches[j,1]
}

# get indices of all performances better than max_time in 2015 or 2016
# max_time_minutes defined at top of script
sample_years <- c(2015,2016)
fast_inds <- dat$time_minutes < max_time_minutes & dat$year %in% sample_years

# get names of these performers
fast_names <- unique( dat$match_name[fast_inds] )
cat("Number of athletes:\n")
print(length(fast_names))

# get subset of full dataset
subset_inds <- dat$match_name %in% unique( fast_names )
subdat <- dat[ subset_inds, ]

# save the data to csv
ord <- order( subdat$marathon, subdat$date, subdat$time_minutes )
vars <- c("name_age","match_name","full_name","marathon",
    "year","date","time","time_minutes")
write.csv( subdat[ord, vars], file = output_csv, row.names = FALSE )

