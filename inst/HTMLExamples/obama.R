load("~/Books/XMLTechnologies/Data/ElectionResults/countyBoundaries/states.rda")

i = match(names(countyBoundaries), tolower(state.abb), 0)
names(countyBoundaries)[ i != 0] = tolower(state.name[i])
stateWinner = sapply(stateSummaries, function(x) x[[1]][1,1]) 
names(stateWinner) = gsub("-", " ", names(stateWinner))


stateWinner = sapply(stateSummaries, function(x) x[[1]][1,1]) 
names(stateWinner) = gsub("-", " ", names(stateWinner))
stateWinner = 
 c("Rep.", "Dem.")[sapply(states, function(x) { 
                            totals = colSums(x[,1:2])
                            totals[1] > totals[2]
                          }) + 1]
names(stateWinner) = names(states)

names(stateWinner) = gsub("-", " ", names(stateWinner))
zz = map('state', fill = TRUE)
zz$name = gsub(":.*", "", zz$name)
polygon(zz$x, zz$y, col = ifelse(stateWinner[zz$name] == "Dem.", "blue", "red"))
zz = map('state', fill = TRUE)
polygon(zz$x, zz$y, col = ifelse(stateWinner[zz$name] == "Dem.", "blue", "red"))
purple = 
function(blue, green = 0)
{
 colors = cbind(red = (1 - blue), 
                green = rep(green, length(blue)),
                blue =  blue)
 colors[is.na(colors)] = 0
 rgb(colors[,1], colors[,2], colors[,3])
}

percentObama = sapply(states, function(x) sum(x[,1])/sum(colSums(x[,1:2]) ))
names(percentObama) = gsub("-", " ", names(percentObama))
polygon(zz$x, zz$y, col = purple(percentObama[zz$name]))


svg("~/Classes/stat242-08/Lectures/SVG/obama.svg")
map('state')
polygon(zz$x, zz$y, col = purple(percentObama[zz$name]))
dev.off()
