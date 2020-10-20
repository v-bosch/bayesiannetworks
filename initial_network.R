library(dagitty)

# full network is specified below with yet unavailable
# variables commented
g <- dagitty(paste('dag {
  atemp [pos="-0.372,0.106"]
  autumn [pos="-0.282,-0.353"]
  casual [pos="-0.090,0.121"]',
  # daylength [pos="-0.172,-0.169"]',
  # gasprice [pos="0.198,0.029"]
  'holiday [pos="-0.083,-0.220"]
  hum [pos="-0.282,-0.036"]
  mnth [pos="-0.161,-0.407"]
  registered [pos="0.102,0.129"]
  spring [pos="-0.402,-0.356"]
  summer [pos="-0.345,-0.359"]
  temp [pos="-0.458,-0.041"]
  cnt [pos="0.014,0.316"]
  weathersit [pos="-0.264,-0.209"]
  weatherlatent [latent,pos="-0.360,-0.170"]',
  # winddirection [pos="-0.458,-0.169"]
  'windspeed [pos="-0.366,-0.039"]
  winter [pos="-0.471,-0.358"]
  workingday [pos="0.111,-0.221"]
  
  winter -> {weatherlatent}', # winddirection}
  'spring -> {weatherlatent}', # winddirection}
  'summer -> {weatherlatent}', # winddirection}
  'autumn -> {weatherlatent}', # winddirection}
  # winddirection -> temp
  'weatherlatent -> {hum temp weathersit windspeed}
  {hum temp windspeed} -> atemp',
  # mnth -> daylength
  '{atemp workingday holiday} -> casual',
  # daylength -> casual
  '{atemp workingday holiday} -> registered',
  # gasprice -> registered
  '{casual registered} -> cnt
}'))
plot(g)



# read bikeshare data from storage
data <- read.csv("day.csv", header=TRUE)

# convert season variable to 4 binary variables
winter <- as.numeric(data$season == 1)
spring <- as.numeric(data$season == 2)
summer <- as.numeric(data$season == 3)
autumn <- as.numeric(data$season == 4)
data <- cbind(data, winter)
data <- cbind(data, spring)
data <- cbind(data, summer)
data <- cbind(data, autumn)

# convert month variable to continuous ratio variable
mnth.since.launch <- data$yr * 12 + data$mnth + 3
data$mnth <- mnth.since.launch

# add the other variables
daylength <- ...
data <- cbind(data, daylength)

gasprice <- ...
data <- cbind(data, gasprice)

windirection <- ...
data <- cbind(data, winddirection)

# remove all variables not present in model
data <- subset(data, select=-c(instant, yr, dteday, season, weekday))



# perform conditional independence tests
# a small p-value is bad, as that indicates a dependence
# still need to convert these p-values to effect sizes
localTests(g, data)
