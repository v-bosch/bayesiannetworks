library(dagitty) 
library(lavaan)
library(bnlearn)
library(ggplot2)
library(zoo)

# full Bayesian network is specified below
g <- dagitty(paste('dag {',
  ####################################
  #            Variables             #
  ####################################                   
  # weather variables
  'atemp [pos="-0.368,-0.285"]
  autumn [pos="-0.497,-0.095"]
  casual [pos="-0.301,-0.085"]
  day [pos="-0.333,-0.445"]
  daylength [pos="-0.438,-0.082"]
  gasprice [pos="-0.441,-0.384"]
  holiday [pos="-0.311,-0.249"]
  hum [pos="-0.390,-0.354"]
  rallyprotest [pos="-0.334,-0.035"]
  registered [pos="-0.302,-0.381"]
  spring [pos="-0.499,-0.287"]
  summer [pos="-0.498,-0.188"]
  temp [pos="-0.389,-0.216"]
  weatherlatent [latent,pos="-0.438,-0.286"]
  weathersit [pos="-0.438,-0.185"]
  windspeed [pos="-0.389,-0.285"]
  winter [pos="-0.499,-0.384"]
  workingday [pos="-0.334,-0.249"]',
  
  ####################################
  #            Relations             #
  ####################################
  # weather relations
  'winter -> {spring -> {summer -> autumn}}
  {winter spring summer autumn} -> {weatherlatent daylength gasprice}
  daylength -> temp
  weatherlatent -> {weathersit temp windspeed hum}
  {temp windspeed hum} -> atemp',
  
  # outcome relations
  '{atemp workingday holiday rallyprotest} -> casual
  {atemp workingday holiday day gasprice} -> registered',
  # casual <-> registered
'}'))
plot(g)



# read bikeshare data from storage
data <- read.csv("day.csv", header=TRUE)

# convert season variable to 4 binary variables
winter <- data$season == 1
spring <- data$season == 2
summer <- data$season == 3
autumn <- data$season == 4
data <- cbind(data, winter)
data <- cbind(data, spring)
data <- cbind(data, summer)
data <- cbind(data, autumn)

# convert year and instant variables to continuous ratio variable
data$day <- data$instant + 112

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
data <- subset(data, select=-c(instant, yr, dteday, season, weekday, mnth, cnt))



# perform conditional independence tests
# - a small p-value is bad, as that indicates a dependence
# - still need to convert these p-values to effect sizes
results <- localTests(g, data, type="tetrads")
results[results$p.value < 1e-20,]

scaled.data <- data.frame(scale(data))
summary(lm(scaled.data$autumn ~ scaled.data$day))

t <- 35
ggplot() + geom_line(aes(x=1:(731-t+1), y=rollmean(data$weathersit, k=t)))
ggplot(data) + geom_point(aes(x=day, y=winter))
summary(lm(data$gasprice ~ data$day))


scaled.data <- data.frame(lapply(scaled.data, as.numeric))
net <- toString(g,"lavaan")
fit1 <- sem(net, scaled.data)

net <- model2network(toString(g,"bnlearn"))
fit <- bn.fit( net, scaled.data, sample.nobs=nrow(data))
fit
