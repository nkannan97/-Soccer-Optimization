
# read in the dataset
t <- read.csv('/home/adithya/Desktop/SPRING2017/CS524/Project/data/CompleteDataset.csv', stringsAsFactors = FALSE)

# get columns related to player attributes
stats <- t[, which(colnames(t) == "Acceleration"):ncol(t)]
stats <- stats[, -which(colnames(stats) == "Preferred.Positions")]
# replace missing data with 0 (most likely goal keepers without forward playing statistics)
stats[is.na(stats)] <- 0

# look for positions with +/- in their attributes and add/subtract those number
stats2 <- apply(stats, 1, function(x) {
    temp = unlist(x)
    pos = grep("+", temp, fixed = T)
    if(any(pos)) {
        d = strsplit(temp[pos], "+", fixed = T)
        for(i in 1:length(d)) {
            temp[pos[i]] <- sum(as.integer(d[[i]]))
        }
    }
    pos = grep("-", temp, fixed = T)
    if(any(pos)) {
        d = strsplit(temp[pos], "-", fixed = T)
        for(i in 1:length(d)) {
            temp[pos[i]] <- as.integer(d[[i]][1]) - as.integer(d[[i]][2])
        }
    }
    x <- temp
})

# transpose and convert back to dataframe
stats2 <- t(stats2)
stats2 <- as.data.frame(stats2)

# attach back to t dataframe
t[, colnames(stats2)] <- stats2
t <- t[-which(t[,"X"] == "13771"),] # remove T. Wilson because he doesn't have a card on FUT

# change players without a club to  "No Club/Non FIFA"
t[which(t[,"Club"] == ""),"Club"] <- "No Club/Non FIFA"
t <- t[,-1]

# change the values into numbers
values <- t[,"Value"]
values <- sapply(values, function(x) { 
    substring(x, 2, nchar(x))
})
values <-  unname(values)
for(i in 1:length(values)) {
    if(substring(values[i], nchar(values[i])) == "M") {
        println(as.double(substring(values[i], 1, nchar(values[i])-1)))
    } else if (substring(values[i], nchar(values[i])) == "K") {
        
    }
}
values <- unlist(values)




# write to file
write.csv(t, file = '../data/modifiedCompleteDataset.csv')
