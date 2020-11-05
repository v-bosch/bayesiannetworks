library(dagitty)
library(lavaan)
library(lavaanPlot)
library(ppcor)
library(ggplot2)

# full Bayesian network is specified below
g <- dagitty(paste('dag {',
  ####################################
  #            Variables             #
  ####################################                   
  # weather variables
  'atemp [pos="-0.428,-0.318"]
  casual [pos="-0.412,-0.300"]
  daylength [pos="-0.427,-0.346"]
  gasprice [pos="-0.412,-0.363"]
  holiday [pos="-0.386,-0.317"]
  hum [pos="-0.440,-0.292"]
  instant [pos="-0.385,-0.346"]
  rallyprotest [pos="-0.412,-0.278"]
  registered [pos="-0.412,-0.332"]
  temp [pos="-0.439,-0.334"]
  warm [pos="-0.448,-0.365"]
  weathersit [pos="-0.428,-0.289"]
  windspeed [pos="-0.447,-0.317"]
  workingday [pos="-0.387,-0.290"]',
  
  ####################################
  #            Relations             #
  ####################################
  # weather relations
  'warm -> {daylength}',
  '{hum windspeed} <- weathersit',
  'daylength -> temp',
  'windspeed -> hum',
  '{temp windspeed hum} -> atemp',
  
  # gasprice
  '{warm instant} -> gasprice -> registered',
  
  # outcome relations
  '{instant atemp workingday holiday weathersit rallyprotest daylength} -> casual',
  '{instant atemp gasprice workingday holiday weathersit daylength} -> registered',
'}'))
plot(g)


# read bikeshare data from storage
data <- read.csv("day.csv", header=TRUE)

# convert season variable to 4 binary variables
warm <- data$season == 2 | data$season == 3
data <- cbind(data, warm)

# add day length
data$daylength <- 0
L <- 38.8951  # latitude Washington D.C.
for (J in 1:731) 
{
  P = asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(.00860 * (J - 186)))))
  
  D <- 24 - (24 / pi) * 
    acos(
      (sin(0.8333 * pi / 180) + sin(L * pi / 180) * sin(P))
        / 
      (cos(L * pi / 180) * cos(P))
    )
  data$daylength[J] <- D
}

data$workingday <- data$weekday < 5

# add monthly gasoline price
gasprice.per.week <- c(
  3.114, 3.129, 3.141, 3.164, 3.172, 3.165, 3.175, 3.181, 3.209, 3.37, 3.519,
  3.567, 3.562, 3.576, 3.657, 3.757, 3.828, 3.898, 3.981, 4, 3.993, 3.912,
  3.847, 3.805, 3.766, 3.727, 3.672, 3.634, 3.687, 3.738, 3.759, 3.772, 3.738,
  3.67, 3.625, 3.626, 3.666, 3.667, 3.634, 3.567, 3.504, 3.462, 3.498, 3.494,
  3.476, 3.454, 3.454, 3.416, 3.373, 3.338, 3.334, 3.309, 3.313, 3.346, 3.44,
  3.46, 3.494, 3.548, 3.571, 3.627, 3.67, 3.748, 3.78, 3.793, 3.828, 3.882,
  3.924, 3.974, 3.955, 3.93, 3.874, 3.809, 3.764, 3.715, 3.657, 3.603, 3.534,
  3.477, 3.417, 3.384, 3.434, 3.482, 3.564, 3.569, 3.637, 3.725, 3.75, 3.781,
  3.823, 3.86, 3.953, 3.907, 3.893, 3.911, 3.877, 3.787, 3.692, 3.677, 3.66,
  3.657, 3.637, 3.594, 3.545, 3.486, 3.471, 3.5
)
i <- 1
data$gasprice <- 0
for (J in 1:731)
{
  if (data$weekday[J] == 1) {i <- i + 1}
  
  data$gasprice[J] <- gasprice.per.week[i]
}

# add rallies and protest marches
rallies.and.protests <- c(
  274, 288, 289, 313, 314, 315, 316, 317, 318, 319, 320, 321,
  322, 323, 324, 325, 326, 327, 376, 416, 449, 575, 673, 687
)
data$rallyprotest <- data$instant %in% rallies.and.protests

# remove all variables not present in model
data <- subset(data, select=-c(yr, dteday, weekday, season, mnth, cnt))
scaled.data <- data.frame(scale(subset(data, select=-weathersit)))
scaled.data$atemp[595] <- -scaled.data$atemp[595]

# make weather situation ordinal in the data
hour.data = read.csv("hour.csv", header=TRUE)
prev.day <- "2011-01-01"
day.weathersit <- NULL
weathersit <- NULL
for (I in 1:nrow(hour.data))
{
  if (hour.data$dteday[I] != prev.day | hour.data$instant[I] == nrow(hour.data))
  {
    if (sum(day.weathersit == 3 | day.weathersit == 4) > 2)
    {
      weathersit <- c(weathersit, 3)
    } else if (sum(day.weathersit == 2) > 2)
    {
       weathersit <- c(weathersit, 2)
    }
    else
    {
      weathersit <- c(weathersit, 1)
    }
    # weathersit <- c(weathersit, min(max(day.weathersit), 3))
    # weathersit <- c(weathersit, mean(day.weathersit))
    day.weathersit <- NULL
  }
  day.weathersit <- c(day.weathersit, hour.data$weathersit[I])
  prev.day <- hour.data$dteday[I]
}
# scaled.data$weathersit <- ordered(data$weathersit, levels=1:3)
scaled.data$weathersit <- ordered(weathersit, levels=1:3)

# perform conditional independence tests
# - a small p-value is bad, as that indicates a dependence
# - first column are partial correlation coefficients used as effect sizes
M = lavCor(scaled.data)
results <- localTests(g, type="cis", sample.cov=M, sample.nobs=731)
top.results <- results[results$p.value < 1e-5,]
top.results
plotLocalTestResults(results)



# fit data to SEM using lavaan package
# create covariance matrix for multivariate Gaussian model
net <- toString(g, "lavaan")
fit <- sem(
    net, sample.cov=M, sample.nobs=731
)
summary(fit)

# show SEM with coefficients
lavaanPlot(
    model=fit,
    node_options=list(shape="box", fontname="Helvetica"),
    edge_options=list(color="grey"),
    coefs=TRUE
)

I = "weathersit"
J = "windspeed"
ggplot() +
    geom_point(aes(x=scaled.data[[I]], y=scaled.data[[J]])) +
    xlab(I) +
    ylab(J)
pcor(subset(scaled.data, select=c(warm, weathersit)))

ggplot(scaled.data) + geom_point(aes(x=warm, y=weathersit))
lm(atemp ~ hum, scaled.data)
