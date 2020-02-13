
# merge sampled dataset with shoe dataset
# Rscript sampled_csv shoe_csv output_csv

args <- commandArgs( trailingOnly = TRUE )
#args <- c("men_sampled.csv","men_shoe.csv","men_sampled_shoe.csv")

input_sampled <- args[1]
input_shoe <- args[2]
output_csv <- args[3]

# read in the data
perf_data <- read.csv( input_sampled, as.is = TRUE)
shoe_data <- read.csv( input_shoe, as.is = TRUE)

# join them together
perf_data$vaporfly <- NA
for(j in 1:nrow(perf_data)){
    
    # find the right row in shoe data 
    ind_shoedata <- which( 
        shoe_data$match_name == perf_data$match_name[j] &
        shoe_data$marathon == perf_data$marathon[j] &
        shoe_data$year == perf_data$year[j] 
    )

    # assign vaporfly variable
    perf_data$vaporfly[j] <- shoe_data$vaporfly[ind_shoedata]

    # check for weirdness
    if( length(ind_shoedata) != 1 ){print(j)}
}

# write to csv
write.csv(perf_data, file = output_csv, row.names = FALSE )
