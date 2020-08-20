

# look at average performance with and without shoe
# for each athlete
data_files <- c("men_sampled_shoe.csv","women_sampled_shoe.csv")

pdf("data_explore.pdf",width=10,height=4)
par(family = "serif", mar=c(3.5,4,3,0.5))
layout( matrix(1:2,1,2), widths = c(0.6,0.4))
plot( NA, type="n", xlim = c(1,365*5), ylim = c(0,1),
    axes = FALSE, ann = FALSE)
cols <- c("black","blue")
for(j in 1:2){
    dat <- read.csv( data_files[j], as.is = TRUE )
    x1 <- as.numeric( dat$vaporfly )
    dat$mar_year <- paste( dat$marathon, dat$year )    
    races <- unique( dat$mar_year )
    race_inds <- match( races, dat$mar_year ) 
    dates <- dat$date[race_inds]
    days <- as.Date( dates ) - as.Date( "2015-01-01" )
    num_total <- rep(NA,length(races))
    num_vaporfly <- rep(NA, length(races))
    for(k in 1:length(races)){
        jj <- dat$mar_year == races[k]
        num_vaporfly[k] <- sum( x1[jj] == 1, na.rm = TRUE )
        num_total[k] <- num_vaporfly[k] + sum( x1[jj] == 0, na.rm = TRUE )
    }
    prop <- num_vaporfly/num_total
    prop[is.nan(prop)] <- NA
    points( days, prop, col = cols[j], cex = sqrt(num_total/4) )
}
legend("topleft",legend=c("Women","Men"), pch = 1, col = rev(cols) )
axis(1, at = 365*(0:5), labels = rep("",6), lwd.ticks = 1, lwd = 0 )
axis(1, at = 365*(0.5 + (0:4)), labels = 2015:2019, lwd = 0 )
axis(2, lwd.ticks=1, lwd = 0)
mtext("Adoption of Vaporfly Shoes", side=3, line = 0.5)
mtext("Date", side=1, line = 2.5)
mtext("Proportion wearing Vaporfly Shoes", side=2, line = 2.5)
box()

plot( NA, type="n", xlim = c(125,180), ylim = c(125,180),
    axes = FALSE, ann = FALSE)
cols <- c("black","blue")
for(j in 1:2){
    
    dat <- read.csv(data_files[j], as.is = TRUE)
    x1 <- as.numeric( dat$vaporfly )
    inds <- !is.na(x1)
    x1 <- x1[inds]
    f1 <- dat$match_name[inds]
    y  <- dat$time_minutes[inds]
    unique_runners <- unique(f1[inds])
    n_runners <- length(unique_runners)
    avg_times_no_yes <- matrix(NA, n_runners, 2 )
    one_time_no_yes <- matrix(NA, n_runners, 2 )
    nperf_no_yes <- matrix(NA, n_runners, 2 )
    for(k in 1:n_runners){
        inds_no <- f1 == unique_runners[k] & x1 == 0
        avg_times_no_yes[k,1] <- mean( y[inds_no] )
        nperf_no_yes[k,1] <- sum(inds_no)

        inds_yes <- f1 == unique_runners[k] & x1 == 1
        avg_times_no_yes[k,2] <- mean( y[inds_yes] )
        nperf_no_yes[k,2] <- sum(inds_yes)
    }
    points( avg_times_no_yes[,1], avg_times_no_yes[,2], col = cols[j])
    vapor_better <- avg_times_no_yes[,2] < avg_times_no_yes[,1]
    print(sum(vapor_better,na.rm=TRUE))
    print(sum(!vapor_better,na.rm=TRUE))
    print(mean(vapor_better,na.rm=TRUE))
    
}
axis(1,at=c(130,150,170),
    labels = c("2:10","2:30","2:50"), lwd = 0, lwd.ticks = 1 )
axis(2,at=c(130,150,170),
    labels = c("2:10","2:30","2:50"), lwd = 0, lwd.ticks = 1 )
abline(0,1)
mtext("Non-Vaporfly Times",side=1,line=2.5)
mtext("Vaporfly Times",side=2,line=2.5)
mtext("Marathon Times", side=3, line = 0.5)
box()
dev.off()





# look at average performance at each race
dat <- read.csv(data_files[1], as.is = TRUE)
f2 <- paste( dat$marathon, dat$year )
unique_races <- unique(as.character(f2))
dat <- read.csv(data_files[2], as.is = TRUE)
f2 <- paste( dat$marathon, dat$year )
unique_races <- unique(c(unique_races,as.character(f2)))
n_races <- length(unique_races)
avg_times_men_women <- matrix(NA, n_races, 2 )
sd_times_men_women <- matrix(NA, n_races, 2 )
race_daycount <- rep(NA,n_races)


cols <- c("black","blue")
for(j in 1:2){
    dat <- read.csv(data_files[j], as.is = TRUE)
    x1 <- as.numeric( dat$vaporfly )
    inds <- !is.na(x1)
    x1 <- x1[inds]
    f2 <- paste( dat$marathon, dat$year )[inds]
    y  <- dat$time_minutes[inds]
    day_count <- as.numeric(as.Date(dat$date)-min(as.Date(dat$date)))[inds]
    
    for(k in 1:n_races){
        
        inds_race <- as.character(f2) == unique_races[k]
        avg_times_men_women[k,j] <- mean( y[inds_race] )
        sd_times_men_women[k,j] <- sd( y[inds_race] )
        if(is.na(race_daycount[k])){
            race_daycount[k] <- mean(day_count[inds_race])
        }
    }
}    

cols <- c("black","blue")
plot(NA, type="n", xlim = c(0,365*5), ylim = c(125,180),
    axes = FALSE, ann = FALSE)
for(j in 1:2){
    points(race_daycount,avg_times_men_women[,j],
        col = cols[j])
}
for(j in 1:length(race_daycount)){
    lines( rep(race_daycount[j],2), avg_times_men_women[j,], col = "gray" )
}
axis(1,at=(0:5)*365.25,labels=rep("",6), lwd = 0, lwd.ticks = 1)
axis(1,at=seq(0.5,4.5,by=1)*365.25,lwd=0,lwd.ticks=0,
    labels=2015:2019)
axis(2,at=c(130,150,170),
    labels = c("2:10","2:30","2:50"), lwd = 0, lwd.ticks = 1 )
mtext("Race Date",side=1, line = 2.5)
mtext("Average Times for Each Race",side=3, line = 0.5)
box()

