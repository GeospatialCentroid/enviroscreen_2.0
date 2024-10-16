

# data from the original run 
run1 <- list.files("data/run1Products20241015/enviroscreenScore", 
                   pattern = ".csv",
                   full.names = TRUE)
c1 <- read_csv(run1[3])
ct1 <- read_csv(run1[2])
cbg1 <- read_csv(run1[1])

# data from second run 
run2 <- list.files("data/products/enviroscreenScore", 
                   pattern = ".csv",
                   full.names = TRUE)
c2 <- read_csv(run2[3])
ct2 <- read_csv(run2[2])
cbg2 <- read_csv(run2[1])
test <- c2 
test$extra <- NA

# test match 
all.equal(c1,c2)
all.equal(ct1,ct2)
all.equal(cbg1,cbg2)
all.equal(c1,test)
View(c1)
