
args <- commandArgs(trailingOnly = TRUE)
#args <- c("women_sampled_shoe.csv","women_fit.RData")
input_perf_csv <- args[1]
output_rdata <- args[2]

# read in the data
perf_data <- read.csv(input_perf_csv, as.is = TRUE )

# convert to a day count
perf_data$date <- as.Date(perf_data$date)
perf_data$day_count <- as.numeric( perf_data$date - min(perf_data$date) )

# find the non-missing values
not_missing <- !is.na(perf_data$vaporfly)
n <- sum(not_missing)

# define the response
y <- perf_data$time_minutes[not_missing] 

# define the vaporfly variable
int <- rep(1,n)
x1 <- as.numeric( perf_data$vaporfly[not_missing] )
X <- cbind(int,x1)

# runner factor and block matrix
f1 <- as.factor(perf_data$match_name[not_missing])
lev1 <- levels(f1)
lev1 <- levels(f1)
Z1 <- matrix(0, n, length(lev1) )
for(j in 1:ncol(Z1)){ Z1[,j] <- as.numeric( f1 == lev1[j] ) }
ZZ1 <- Z1 %*% t(Z1)

# race factor and block matrix
f2 <- as.factor( paste(perf_data$marathon,perf_data$year)[not_missing] )
lev2 <- levels(f2)
Z2 <- matrix(0, n, length(lev2) )
for(j in 1:ncol(Z2)){ Z2[,j] <- as.numeric( f2 == lev2[j] ) }
ZZ2 <- Z2 %*% t(Z2)

# days
day_count <- perf_data$day_count[not_missing]

# fit the models
fit1 <- lme4::lmer( y ~ x1 + (1|f1) + (1|f2), REML = TRUE )
fit2 <- lme4::lmer( log(y) ~ x1 + (1|f1) + (1|f2), REML = TRUE )

print(summary(fit1))
print(summary(fit2))

vc <- lme4::VarCorr(fit1)
covparms1 <- c(vc$f1,vc$f2,attr(vc,"sc")^2)
vc <- lme4::VarCorr(fit2)
covparms2 <- c(vc$f1,vc$f2,attr(vc,"sc")^2)


# confidence intervals
round( fit1@beta[2] + qnorm(0.95)*sqrt(vcov(fit1)[2,2])*c(-1,1), 3 )
round( fit2@beta[2] + qnorm(0.95)*sqrt(vcov(fit2)[2,2])*c(-1,1), 3 )

# multiplicative effects
1-exp(fit2@beta[2])

# save the results
save(fit1, fit2, y, x1, f1, f2, day_count, X, ZZ1, ZZ2, 
    covparms1, covparms2, file = output_rdata)









