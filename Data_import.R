## Load packages
library(stringr)
library(chron)

## Import dataset (pasted from MySteamGauge.com)
profiledata = read.csv(file = "../../20180726_Steam.txt", sep = "\t", header = T)

# rename columns
names(profiledata) = c("type", "ID", "icon", "title", "minPlayed", "hrsPlayed", "timeToBeat", "priceUSD", "pricePerHour", "releaseDate", "developers", "publishers", "metascore", "win", "mac", "linux", "sizeMB", "sizeGB", "controller", "multiplayer", "genres")

# Re-annotate genres

# Encode release date as such
profiledata$releaseDate = as.Date(profiledata$releaseDate, "%b %d, %Y")

# Save separately min and max time to beat game
#profiledata$timeToBeatMin = 
# extract all digits in min times
minTimes = str_extract(string = profiledata$timeToBeat, pattern = "^.*mm")
minTimes = str_extract_all(minTimes, "[:digit:]*", simplify = T)
#subset only columns with numeric strings
paste(minTimes[,c(6,9)], sep = ":")
# paste in single string

# pass to chron for time formatting
  chron(times. = , format = )

# Save subset of only games actually played
played = profiledata[which(profiledata$minPlayed>10),]

#### Plot basic summaries
library(ggplot2)

# Display time played per game
ggplot(data = profiledata, aes(x = minPlayed, label = title)) +
  geom_density()

ggplot(data = played, aes(x = minPlayed, label = title)) +
  geom_dotplot() + scale_x_log10()

# Display games per time played vs cost
ggplot(data = played, 
  aes(x = priceUSD, y =minPlayed, label = title)) +
  geom_text() + scale_y_log10() +
  geom_smooth( method = "lm")

cor.test(played$minPlayed, played$priceUSD)

# Playtime vs average completion time of played games
ggplot(data = played, 
  aes(x = timeToBeat, y =minPlayed, label = title)) +
  geom_text() + scale_y_log10()

# playtime vs release date
ggplot(data = played, 
  aes(x = releaseDate, y =minPlayed, label = title, col= multiplayer==1)) +
  geom_text() + scale_y_log10() +
  geom_smooth( method = "lm")
