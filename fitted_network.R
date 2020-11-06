library(dagitty)
library(bnlearn)
library(lavaan)
library(lavaanPlot)
library(pracma)

# full Bayesian network is specified below
g <- dagitty(paste('dag {',
    ####################################
    #            Variables             #
    ####################################                   
    # weather variables
    'atemp [pos="-0.401,-0.333"]
    casual [pos="-0.412,-0.304"]
    daylength [pos="-0.422,-0.334"]
    gasprice [pos="-0.421,-0.312"]
    holiday [pos="-0.409,-0.291"]
    hum [pos="-0.395,-0.319"]
    instant [pos="-0.421,-0.291"]
    rallyprotest [pos="-0.415,-0.291"]
    registered [pos="-0.412,-0.321"]
    temp [pos="-0.412,-0.334"]
    season [pos="-0.431,-0.303"]
    weathersit [pos="-0.387,-0.302"]
    windspeed [pos="-0.393,-0.333"]
    workingday [pos="-0.400,-0.294"]',
  
    ####################################
    #            Relations             #
    ####################################
    'season -> daylength -> temp -> atemp
    weathersit -> {windspeed -> hum} -> atemp
    
    {season instant} -> gasprice
    
    {instant atemp workingday weathersit gasprice daylength rallyprotest holiday} -> casual
    {instant atemp workingday weathersit gasprice daylength} -> registered',
'}'))
plot(g)



# use hourly data as source
hour.data = read.csv("hour.csv", header=TRUE)
day.starts <- hour.data$instant[!duplicated(hour.data$dteday)]


data <- data.frame(
    instant=seq_along(day.starts),
    season=hour.data[day.starts, "season"] %% 4 > 1,
    holiday=hour.data[day.starts, "holiday"] == 1,
    workingday=hour.data[day.starts, "weekday"] %% 6 != 0,
    weathersit=ordered(1, levels=1:3),
    hum=-1,
    windspeed=-1,
    temp=-1,
    atemp=-1,
    casual=-1,
    registered=-1
)

day.intervals <- c(day.starts, nrow(hour.data) + 1)
for (I in data$instant)
{
    day.data <- hour.data[day.intervals[I]:(day.intervals[I + 1] - 1),]

    if (sum(day.data$weathersit == 3 | day.data$weathersit == 4) >= 3)
    {
        data$weathersit[I] <- 3
    } else if (sum(day.data$weathersit == 2 | day.data$weathersit == 3 | day.data$weathersit == 4) >= 3)
    {
        data$weathersit[I] <- 2
    } else
    {
        data$weathersit[I] <- 1
    }
    
    data[I, "hum"] <- mean(day.data$hum)
    data[I, "windspeed"] <- mean(day.data$windspeed)
    data[I, "temp"] <- mean(day.data$temp)
    data[I, "atemp"] <- mean(day.data$atemp)
    data[I, "casual"] <- sum(day.data$casual)
    data[I, "registered"] <- sum(day.data$registered)
}

# add day length
data$daylength <- -1
L <- 38.8951  # latitude Washington D.C.
for (I in data$instant) 
{
    P = asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(.00860 * (I - 186)))))
  
    D <- 24 - (24 / pi) * 
      acos(
        (sin(0.8333 * pi / 180) + sin(L * pi / 180) * sin(P))
          / 
        (cos(L * pi / 180) * cos(P))
      )
    data[I, "daylength"] <- D
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
week.starts <- c(1, data$instant[hour.data[day.starts, "weekday"] == 1], 732)
data$gasprice <- -1
for (I in 1:(length(week.starts) - 1))
{
    data[week.starts[I]:(week.starts[I + 1] - 1), "gasprice"] <- gasprice.per.week[I]
}

# add rallies and protest marches
rallies.and.protests <- c(
    274, 288, 289, 313, 314, 315, 316, 317, 318, 319, 320, 321,
    322, 323, 324, 325, 326, 327, 376, 416, 449, 575, 673, 687
)
data$rallyprotest <- data$instant %in% rallies.and.protests

# scale all variables to z-scores
numeric.data <- data.frame(lapply(subset(data, select=-weathersit), as.numeric))
scaled.data <- data.frame(scale(numeric.data))

# put ordinal variable weathersit back in data.frame
data <- cbind(numeric.data, weathersit=data$weathersit)
scaled.data <- cbind(scaled.data, weathersit=data$weathersit)
scaled.data$atemp[595] <- -scaled.data$atemp[595]



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
fit <- sem(net, sample.cov=M, sample.nobs=nrow(data))
summary(fit)

# show SEM with coefficients
lavaanPlot(
    model=fit,
    node_options=list(shape="box", fontname="Helvetica"),
    edge_options=list(color="grey"),
    coefs=TRUE
)



# predict with SEM using bnlearn package
net <- model2network(toString(g, "bnlearn"))
fit <- bn.fit(net, data)

# predict users for different values of workingday, holiday, and weathersit
tbl <- expand.grid(workingday=c(1, 0), holiday=c(1, 0), weathersit=c(1, 2, 3))
tbl$weathersit <- ordered(tbl$weathersit, levels=1:3)
tbl$casual <- predict(fit, node="casual", data=tbl, method="bayes-lw")
tbl$registered <- predict(fit, node="registered", data=tbl, method="bayes-lw")
tbl

# predict users a year into the future
tbl <- data.frame(instant=as.numeric((nrow(data) + 1):(nrow(data) + 365)))
tbl$casual <- predict(fit, node="casual", data=tbl, method="bayes-lw")
tbl$registered <- predict(fit, node="registered", data=tbl, method="bayes-lw")
tbl

# predict users for different gasprices
tbl <- data.frame(gasprice=linspace(min(data$gasprice), max(data$gasprice), n=100))
tbl$casual <- predict(fit, node="casual", data=tbl, method="bayes-lw")
tbl$registered <- predict(fit, node="registered", data=tbl, method="bayes-lw")
tbl

tbl <- expand.grid(rallyprotest=c(1, 0))
tbl$casual <- predict(fit, node="casual", data=tbl, method="bayes-lw")
tbl$registered <- predict(fit, node="registered", data=tbl, method="bayes-lw")
tbl
