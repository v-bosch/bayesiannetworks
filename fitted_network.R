library(dagitty)
library(lavaan)
library(lavaanPlot)
library(ppcor)

# full Bayesian network is specified below
g <- dagitty(paste('dag {',
  ####################################
  #            Variables             #
  ####################################                   
  # weather variables
  'atemp [pos="-0.397,-0.269"]
  casual [pos="-0.378,-0.263"]
  daylength [pos="-0.397,-0.218"]
  gasprice [pos="-0.380,-0.360"]
  holiday [pos="-0.353,-0.284"]
  hum [pos="-0.397,-0.313"]
  instant [pos="-0.353,-0.326"]
  rallyprotest [pos="-0.378,-0.217"]
  registered [pos="-0.378,-0.304"]
  temp [pos="-0.408,-0.250"]
  warm [pos="-0.448,-0.271"]
  weatherlatent [latent,pos="-0.425,-0.263"]
  weathersit [pos="-0.425,-0.289"]
  windspeed [pos="-0.409,-0.274"]
  workingday [pos="-0.354,-0.237"]',
  
  ####################################
  #            Relations             #
  ####################################
  # weather relations
  '{warm} -> {weatherlatent daylength gasprice}
  instant -> gasprice
  weatherlatent -> {hum windspeed temp weathersit}
  daylength -> temp
  {hum temp windspeed} -> atemp',
  
  # outcome relations
  '{instant gasprice atemp hum holiday workingday daylength} -> registered
  {instant atemp workingday holiday rallyprotest daylength} -> casual
}'))
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
scaled.data <- data.frame(scale(data))



# perform conditional independence tests
# - a small p-value is bad, as that indicates a dependence
# - first column are partial correlation coefficients used as effect sizes
results <- localTests(g, scaled.data, type="cis")
top.results <- results[results$p.value < 1e-5,]
top.results
plotLocalTestResults(results)



# fit data to SEM using lavaan package
# create covariance matrix for multivariate Gaussian model
net <- toString(g,"lavaan")
fit <- sem(net, scaled.data, estimator="WLSMV")
summary(fit)

# show SEM with coefficients
lavaanPlot(
    model=fit,
    node_options=list(shape="box", fontname="Helvetica"),
    edge_options=list(color="grey"),
    coefs=TRUE
)
