library(dagitty)
library(lavaan)
library(ggplot2)
library(ppcor)

# full Bayesian network is specified below
g <- dagitty(paste('dag {',
  ####################################
  #            Variables             #
  ####################################                   
  # weather variables
  'atemp [pos="-0.396,-0.445"]
  casual [pos="-0.352,-0.207"]
  daylength [pos="-0.411,-0.159"]
  gasprice [pos="-0.416,-0.225"]
  holiday [pos="-0.314,-0.201"]
  hum [pos="-0.426,-0.447"]
  instant [pos="-0.315,-0.260"]
  rallyprotest [pos="-0.356,-0.129"]
  registered [pos="-0.352,-0.304"]
  spring [pos="-0.481,-0.199"]
  temp [pos="-0.406,-0.356"]
  weatherlatent [latent,pos="-0.464,-0.300"]
  weathersit [pos="-0.483,-0.295"]
  windspeed [pos="-0.420,-0.397"]
  winter [pos="-0.483,-0.251"]
  workingday [pos="-0.317,-0.332"]
  yearly [latent,pos="-0.462,-0.230"]',
  
  ####################################
  #            Relations             #
  ####################################
  # weather relations
  'yearly -> {winter spring}
  yearly -> {weatherlatent daylength gasprice}',
  'weatherlatent -> {hum windspeed temp weathersit}',
  '{hum temp windspeed} -> atemp',
  
  # outcome relations
  '{instant gasprice atemp workingday weatherlatent} -> registered
  {instant atemp workingday gasprice weatherlatent} -> casual
}'))
plot(g)


# read bikeshare data from storage
data <- read.csv("day.csv", header=TRUE)

# convert season variable to 4 binary variables
winter <- data$instant %% 365 > 122 & data$instant %% 365 < 243
spring <- data$instant %% 365 < 61 | data$instant %% 365 > 304 | 
  data$instant %% 365 > 152 & data$instant %% 365 < 213
data <- cbind(data, winter)
data <- cbind(data, spring)

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
gasprice.per.month <- c(
  0.82, 0.85, 0.94, 1, 1.03, 0.97, 0.96, 0.96, 0.95, 0.91, 0.89, 0.86,
  0.89, 0.94, 1.02, 1.03, 0.98, 0.93, 0.91, 0.98, 1.02, 0.99, 0.91, 0.87
)
data$mnth <- data$yr * 12 + data$mnth
data$gasprice <- 0
for (J in 1:731)
{
  G <- gasprice.per.month[data$mnth[J]]
  data$gasprice[J] <- G
}

# add rallies and protest marches
rallies.and.protests <- c(
  274, 288, 289, 313, 314, 315, 316, 317, 318, 319, 320, 321,
  322, 323, 324, 325, 326, 327, 376, 416, 449, 575, 673, 687
)
data$rallyprotest <- data$instant %in% rallies.and.protests

# remove all variables not present in model
data <- subset(data, select=-c(yr, dteday, season, weekday, mnth, cnt))
scaled.data <- data.frame(scale(data))



# perform conditional independence tests
# - a small p-value is bad, as that indicates a dependence
# - first column are partial correlation coefficients used as effect sizes
results <- localTests(g, scaled.data, type="cis")
top.results <- results[results$p.value < 1e-5,]
top.results
plotLocalTestResults(results)


# fit data to SEM using lavaan package
# create covariance matrix for multivariate Gaussian
net <- toString(g,"lavaan")
fit1 <- sem(net, scaled.data)
summary(fit1)

ggplot(data) + geom_point(aes(x=instant, y=casual))
pcor(subset(scaled.data, select=c(registered, holiday)))

lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = T)

