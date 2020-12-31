# @author Niels van Nistelrooij
# @author Victoria Bosch
library(dagitty)
library(bnlearn)
library(lavaan)
library(lavaanPlot)
library(pracma)
library(ggplot2)
library(ggpubr)

# full Bayesian network is specified below using dagitty package
g <- dagitty(paste('dag {',
    ####################################
    #            Variables             #
    ####################################
    # weather variables
    'weathersit [pos="-0.402,-0.329"]
    hum [pos="-0.412,-0.329"]
    windspeed [pos="-0.412,-0.337"]
    temp [pos="-0.421,-0.324"]
    atemp [pos="-0.421,-0.331"]',
    
    # day variables
    'season [pos="-0.428,-0.312"]
    day [pos="-0.422,-0.297"]
    workingday [pos="-0.402,-0.319"]
    holiday [pos="-0.403,-0.301"]',
    
    # extra variables
    'daylength [pos="-0.422,-0.316"]
    gasprice [pos="-0.422,-0.308"]
    rallyprotest [pos="-0.402,-0.308"]',
    
    # outcome variables
    'casual [pos="-0.412,-0.304"]
    registered [pos="-0.412,-0.321"]',
    
    ####################################
    #            Connections           #
    ####################################
    # weather relations
    'season -> daylength -> temp -> atemp
    weathersit -> {windspeed -> hum} -> atemp',
    
    # gas price relations
    '{season day} -> gasprice',
    
    # outcome relations
    '{day atemp workingday weathersit gasprice daylength holiday rallyprotest} -> casual
    {day atemp workingday weathersit gasprice daylength} -> registered',
'}'))
plot(g)  # plot Bayesian network



# use hourly data as source
hour.data = read.csv("hour.csv", header=TRUE)
# determine indices of day starts
day.starts <- hour.data$instant[!duplicated(hour.data$dteday)]

# function to compute day length in Washington D.C.
daylength <- function(days)
{
  result <- NULL
  L <- 38.8951  # latitude Washington D.C.
  for (day in days) 
  {
    P = asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(.00860 * (day - 186)))))
    
    D <- 24 - (24 / pi) * 
      acos(
        (sin(0.8333 * pi / 180) + sin(L * pi / 180) * sin(P))
        / 
          (cos(L * pi / 180) * cos(P))
      )
    
    result <- c(result, D)
  }
  return(result)
}

# initialize dataframe for daily data set
data <- data.frame(
    day=seq_along(day.starts),
    season=hour.data[day.starts, "season"] %% 4 > 1,
    holiday=hour.data[day.starts, "holiday"] == 1,
    workingday=hour.data[day.starts, "weekday"] %% 6 != 0,
    weathersit=ordered(1, levels=1:3),
    daylength=daylength(seq_along(day.starts)),
    rallyprotest=seq_along(day.starts) %in% c(
            274, 288, 289, 313, 314, 315, 316, 317, 318, 319, 320, 321,
            322, 323, 324, 325, 326, 327, 376, 416, 449, 575, 673, 687
        ),
    gasprice=-1,
    hum=-1,
    windspeed=-1,
    temp=-1,
    atemp=-1,
    casual=-1,
    registered=-1
)

# aggregate over hours to get daily variables
day.intervals <- c(day.starts, nrow(hour.data) + 1)
for (day in data$day)
{
    day.data <- hour.data[day.intervals[day]:(day.intervals[day + 1] - 1),]

    if (sum(day.data$weathersit == 3 | day.data$weathersit == 4) >= 3)
    {
        data$weathersit[day] <- 3
    } else if (sum(day.data$weathersit == 2 | day.data$weathersit == 3 | day.data$weathersit == 4) >= 3)
    {
        data$weathersit[day] <- 2
    } else
    {
        data$weathersit[day] <- 1
    }
    
    data[day, "hum"] <- mean(day.data$hum)
    data[day, "windspeed"] <- mean(day.data$windspeed)
    data[day, "temp"] <- mean(day.data$temp)
    data[day, "atemp"] <- mean(day.data$atemp)
    data[day, "casual"] <- sum(day.data$casual)
    data[day, "registered"] <- sum(day.data$registered)
}

# add weekly gasoline price
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
week.starts <- c(1, data$day[hour.data[day.starts, "weekday"] == 1], 732)
for (week in 1:(length(week.starts) - 1))
{
    data[week.starts[week]:(week.starts[week + 1] - 1), "gasprice"] <- gasprice.per.week[week]
}

# scale all variables except the ordinal weather situation to z-scores
numeric.data <- data.frame(lapply(subset(data, select=-weathersit), as.numeric))
scaled.data <- data.frame(scale(numeric.data))

# put ordinal weather situation back in dataframes
data <- cbind(numeric.data, weathersit=data$weathersit)
scaled.data <- cbind(scaled.data, weathersit=data$weathersit)
scaled.data$atemp[595] <- -scaled.data$atemp[595]  # reverse outlier



# perform conditional independence tests
# - a small p-value is bad, as that indicates a dependence
# - first column are partial correlation coefficients used as effect sizes
M = lavCor(scaled.data)
results <- localTests(g, type="cis", sample.cov=M, sample.nobs=nrow(data))
top.results <- results[sign(results$`2.5%`) == sign(results$`97.5%`),]
top.results
plotLocalTestResults(top.results, xlab="Partial correlation coeffcient (95% CI)")



# fit data to SEM using lavaan package
net <- toString(g, "lavaan")
fit <- lavaan(net, sample.cov=M, sample.nobs=nrow(data), auto.var=TRUE)
summary(fit, fit.measures=TRUE)

# show SEM with coefficients
lavaanPlot(
    model=fit,
    node_options=list(shape="box", fontname="Helvetica"),
    edge_options=list(color="grey"),
    coefs=TRUE
)



# predict with SEM using bnlearn package
net <- model2network(toString(g, "bnlearn"))
fit <- bn.fit(net, data)  # use non-scaled version of data for prediction

# predict users for different values of workingday, holiday, and weathersit
tbl <- expand.grid(workingday=c(1, 0), holiday=c(1, 0), weathersit=c(1, 2, 3))
tbl$weathersit <- ordered(tbl$weathersit, levels=1:3)
tbl$casual <- predict(fit, node="casual", data=tbl, method="bayes-lw")
tbl$registered <- predict(fit, node="registered", data=tbl, method="bayes-lw")
tbl

# predict users for different day lengths
tbl <- data.frame(daylength=linspace(min(data$daylength), max(data$daylength), n=100))

season <- replicate(100, predict(fit, node="season", data=tbl, method="bayes-lw"))
season <- data.frame(season=apply(season, 1, mean))
season.casual <- replicate(100, predict(fit, node="casual", data=season, method="bayes-lw"))
season.registered <- replicate(100, predict(fit, node="registered", data=season, method="bayes-lw"))
season.casual <- apply(season.casual, 1, mean)
season.registered <- apply(season.registered, 1, mean)

tbl$casual <- (
    predict(fit, node="casual", data=data.frame(season=mean(data$season)), method="bayes-lw") +
    predict(fit, node="casual", data=tbl, method="bayes-lw") -
    season.casual
)
tbl$registered <- (
    predict(fit, node="registered", data=data.frame(season=mean(data$season)), method="bayes-lw") +
    predict(fit, node="registered", data=tbl, method="bayes-lw") -
    season.registered
)
ggplot(tbl) +
    geom_point(aes(x=daylength, y=casual), colour="orange") +
    geom_point(aes(x=daylength, y=registered), colour="blue")

# predict registered users for different gas prices
tbl <- data.frame(gasprice=linspace(min(data$gasprice), max(data$gasprice), n=100))

season <- replicate(100, predict(fit, node="season", data=tbl, method="bayes-lw"))
season <- data.frame(season=apply(season, 1, mean))
season.casual <- replicate(100, predict(fit, node="casual", data=season, method="bayes-lw"))
season.registered <- replicate(100, predict(fit, node="registered", data=season, method="bayes-lw"))
season.casual <- apply(season.casual, 1, mean)
season.registered <- apply(season.registered, 1, mean)

day <- replicate(100, predict(fit, node="day", data=tbl, method="bayes-lw"))
day <- data.frame(day=apply(day, 1, mean))
day.casual <- replicate(100, predict(fit, node="casual", data=day, method="bayes-lw"))
day.registered <- replicate(100, predict(fit, node="registered", data=day, method="bayes-lw"))
day.casual <- apply(day.casual, 1, mean)
day.registered <- apply(day.registered, 1, mean)

tbl$casual <- (
    predict(fit, node="casual", data=data.frame(season=mean(data$season)), method="bayes-lw") +
    predict(fit, node="casual", data=data.frame(day=mean(data$day)), method="bayes-lw") +
    predict(fit, node="casual", data=tbl, method="bayes-lw") -
    season.casual -
    day.casual
)
tbl$registered <- (
    predict(fit, node="registered", data=data.frame(season=mean(data$season)), method="bayes-lw") +
    predict(fit, node="registered", data=data.frame(day=mean(data$day)), method="bayes-lw") +
    predict(fit, node="registered", data=tbl, method="bayes-lw") -
    season.registered -
    day.registered
)
ggplot(tbl) +
    geom_point(aes(x=gasprice, y=casual), colour="orange") +
    geom_point(aes(x=gasprice, y=registered), colour="blue")

# predict casual users for events
tbl <- expand.grid(rallyprotest=c(1, 0))
tbl$casual <- predict(fit, node="casual", data=tbl, method="bayes-lw")
tbl


# predict users given all other variables from 2011 to 2013
# first make correct dataframe for 2013, except the weather variables from 2011
year3 <- data.frame(
    day=732:1096,
    season=data[1:365, "season"] == 1,
    holiday=1:365 %in% c(1, 21, 49, 106, 147, 185, 245, 287, 315, 332, 359),
    rallyprotest=1:365 %in% c(26, 48, 250, 286),
    workingday=data[4:368, "workingday"] == 1,
    daylength=daylength(732:1096),
    gasprice=-1,
    hum=data[1:365, "hum"],
    windspeed=data[1:365, "windspeed"],
    temp=data[1:365, "temp"],
    atemp=data[1:365, "atemp"]
)

# add weekly gas prices to the 2013 dataframe
gasprice.per.week <- c(
    3.5, 3.519, 3.513, 3.489, 3.525, 3.665, 3.73, 3.776, 3.806, 3.774, 3.746,
    3.711, 3.694, 3.67, 3.629, 3.58, 3.52, 3.498, 3.49, 3.522, 3.538, 3.557,
    3.543, 3.538,  3.533, 3.547, 3.519, 3.52, 3.652, 3.716, 3.738, 3.708, 3.648,
    3.626, 3.618, 3.667, 3.66, 3.623, 3.545, 3.483, 3.429, 3.398, 3.382, 3.362,
    3.336, 3.295, 3.341, 3.423, 3.441, 3.447, 3.457, 3.466, 3.511
)
week.starts <- c(1, (1:365)[(hour.data[day.starts, "weekday"] == 5)[1:365]], 366)
for (week in 1:(length(week.starts) - 1))
{
  year3[week.starts[week]:(week.starts[week + 1] - 1), "gasprice"] <- gasprice.per.week[week]
}

# put ordinal weather situation in 2013 dataframe
numeric.year3 <- data.frame(lapply(year3, as.numeric))
year3 <- cbind(numeric.year3, weathersit=data[1:365, "weathersit"])

# combine data frames and predict casual and registered users for all data
years <- rbind(subset(data, select=-c(casual, registered)), year3)
years$casual <- predict(fit, node="casual", data=years, method="bayes-lw")
years$registered <- predict(fit, node="registered", data=years, method="bayes-lw")

# compare predictions to true numbers of users
ggarrange(
    ggplot(data) +
        geom_point(aes(x=day, y=casual)) +
        xlab("days") + ylab("True number of casual users"),
    ggplot(years) +
        geom_point(aes(x=day, y=casual)) +
        xlab("days") + ylab("Predicted number of casual users"),
    nrow=2
)
ggarrange(
    ggplot(data) +
        geom_point(aes(x=day, y=registered)) +
        xlab("days") + ylab("True number of registered users"),
    ggplot(years) +
        geom_point(aes(x=day, y=registered)) +
        xlab("days") + ylab("Predicted number of registered users"),
    nrow=2
)
ggarrange(
  ggplot(data) +
    geom_point(aes(x=day, y=casual + registered)) +
    xlab("days") + ylab("True total number of users"),
  ggplot(years) +
    geom_point(aes(x=day, y=casual + registered)) +
    xlab("days") + ylab("Predicted total number of users"),
  nrow=2
)
