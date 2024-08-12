# This is the script to fit the models and reproduce the figures in the paper, "Temperature 
# and time of season are the predominant drivers of cabbage stem flea beetle, Psylliodes 
# chrysocephala, arrival at oilseed rape crops". For GDPR reasons the data that is provided
# is the processed data where the data from the CSFB traps and the corresponding weather data
# from the Irriguide model have already been matched up. The code to produce the map in Fig 1
# is also not provided.

# Read in the data
library(readxl)
CSFB_dat <- read_xlsx("CSFB_data.xlsx", sheet = "Data",
                      col_types = c("text","date","numeric",rep("text",4),rep("numeric",16)),
                      na = "NA")

# Load packages required to run this script
library(ggplot2)
library(ggpubr)
library(brms)
library(tidybayes)
library(modelr)
library(dplyr)
library(lubridate)

############################################################################
############################################################################
###########                                                     ############
###########                 Figure 1 Code                       ############
###########                                                     ############
############################################################################
############################################################################

fig1a <- ggplot(CSFB_dat, aes(x=as.Date(DOY, origin="2020-01-01"), lty=Year)) +
  geom_line( aes(y=Cumulative.Count, col=Height), linewidth=1) +
  facet_wrap(~ Site.Location) +
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Cumulative Count")

############################################################################
############################################################################
###########                                                     ############
###########                 Figure 2 Code                       ############
###########                                                     ############
############################################################################
############################################################################

# Warnings relate to NA values in the data
p1 <- ggplot(CSFB_dat, aes(x = Avg.Temp.per.day, y = Count.per.day, color = Height)) +
  geom_point() +
  theme_bw(base_size=16) + 
  xlab("Mean temperature (celsius)") +
  ylab("Count (per day)")

p2 <- ggplot(CSFB_dat, aes(x = Wind.per.day, y = Count.per.day, color = Height)) +
  geom_point() +
  theme_bw(base_size=16) + 
  xlab("Wind speed (km/h)") +
  ylab("Count (per day)")

p3 <- ggplot(CSFB_dat, aes(x = Rainfall.per.day, y = Count.per.day, color = Height)) +
  geom_point() +
  theme_bw(base_size=16) + 
  xlab("Rainfall (mm)") +
  ylab("Count (per day)")

p4 <- ggplot(CSFB_dat, aes(x = Solar.Rad.per.day, y = Count.per.day, color = Height)) +
  geom_point() +
  theme_bw(base_size=16) + 
  xlab(expression ("Solar radiation (MJ/"~m^2~")")) +
  ylab("Count (per day)")

fig2 <- ggarrange(p1,p2,p3,p4,
                   nrow=2, ncol=2, common.legend = TRUE, labels="AUTO", legend="bottom")

############################################################################
############################################################################
###########                                                     ############
###########                 Model fitting                       ############
###########                                                     ############
############################################################################
############################################################################

# First create some transformations of variables that will be needed
CSFB_dat$DOY.pd.Sc <- CSFB_dat$DOY.per.day/365
CSFB_dat$DOY.pd.Sq.Sc <- CSFB_dat$DOY.pd.Sc^2
CSFB_dat$Avg.Temp.pd.Sq <- CSFB_dat$Avg.Temp.per.day^2
CSFB_dat$DOY.Sc <- CSFB_dat$DOY/365

# Now fit the models
m.full <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Rainfall.per.day + Wind.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.full <- add_criterion(m.full, criterion = c("loo", "waic"))

m.full.dropre <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Rainfall.per.day + Wind.per.day + Height) + (1|Year) + (1|Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.full.dropre <- add_criterion(m.full.dropre, criterion = c("loo", "waic"))

m.minus.rain <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Wind.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.rain <- add_criterion(m.minus.rain, criterion = c("loo", "waic"))

m.minus.wind <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Rainfall.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.wind <- add_criterion(m.minus.wind, criterion = c("loo", "waic"))

m.minus.rainwind <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.rainwind <- add_criterion(m.minus.rainwind, criterion = c("loo", "waic"))

m.minus.avg.temp.sq <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + DOY.pd.Sc * (Avg.Temp.per.day + Rainfall.per.day + Wind.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.avg.temp.sq <- add_criterion(m.minus.avg.temp.sq, criterion = c("loo", "waic"))

m.minus.avg.temp <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + DOY.pd.Sc * (Rainfall.per.day + Wind.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.avg.temp <- add_criterion(m.minus.avg.temp, criterion = c("loo", "waic"))

m.minus.doy <- brm(bf(Count.per.day ~ Avg.Temp.pd.Sq + Avg.Temp.per.day + Rainfall.per.day + Wind.per.day + Height + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.doy <- add_criterion(m.minus.doy, criterion = c("loo", "waic"))

m.full.plussun <- brm(bf(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Rainfall.per.day + Wind.per.day + Solar.Rad.per.day + Height) + (1 + Year||Site.Location), shape~Height), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
# Not adding loo and waic here because this model can't be included in comparisons due to the reduced sample size stemming from missing solar radiation values

m.minus.height <- brm(Count.per.day ~ DOY.pd.Sq.Sc + Avg.Temp.pd.Sq + DOY.pd.Sc * (Avg.Temp.per.day + Rainfall.per.day + Wind.per.day) + (1 + Year||Site.Location), family=negbinomial, data=CSFB_dat, chains=4, cores=4)
m.minus.height <- add_criterion(m.minus.height, criterion = c("loo", "waic"))

# Model comparisons
loo_compare(m.full, m.minus.rain, m.minus.wind, m.minus.rainwind, m.minus.avg.temp.sq, m.minus.avg.temp, m.minus.height, m.minus.doy, m.full.dropre, criterion = "loo")

# Model checking
check_brms <- function(model,             # brms model
                       integer = FALSE,   # integer response? (TRUE/FALSE)
                       plot = TRUE,       # make plot?
                       ...                # further arguments for DHARMa::plotResiduals 
) {
  
  mdata <- brms::standata(model)
  if (!"Y" %in% names(mdata))
    stop("Cannot extract the required information from this brms model")
  
  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(model, ndraws = 1000)),
    observedResponse = mdata$Y, 
    fittedPredictedResponse = apply(
      t(brms::posterior_epred(model, ndraws = 1000, re.form = NA)),
      1,
      mean),
    integerResponse = integer)
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  
  invisible(dharma.obj)
  
}

# Example check
mfull.check <- check_brms(m.full, integer = TRUE)
plot(mfull.check)

# Example summary
summary(m.full)

############################################################################
############################################################################
###########                                                     ############
###########                 Figure 3 Code                       ############
###########                                                     ############
############################################################################
############################################################################

# Extract only the relevant parts of the original dataset for prediction
preddata <- CSFB_dat[,c("Height","Rainfall.per.day","Wind.per.day","Solar.Rad.per.day",
                       "DOY.pd.Sc","DOY.pd.Sq.Sc","Avg.Temp.per.day","Avg.Temp.pd.Sq")]
# Calculate predictions for an average site in an average year
preddata$Site.Location <- NA
preddata$Year <- NA

facet_labels <- c("A","B")
names(facet_labels) <- c("1m", "Ground")
fig3 <- preddata %>%
  add_epred_draws(m.full) %>%
  ggplot(aes(x = as.Date(DOY.pd.Sc*365, origin = "2020-01-01"), y = Count.per.day)) +
  facet_wrap(~Height, scales="free") +
  stat_lineribbon(aes(y = .epred), alpha=1) +
  geom_point(data = CSFB_dat, alpha=0.5) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Set2") +
  xlab("Date") +
  theme_bw(base_size=16) +
  theme(legend.position = "bottom") +
  ylab("Count (per day)")

############################################################################
############################################################################
###########                                                     ############
###########                 Figure 4 Code                       ############
###########                                                     ############
############################################################################
############################################################################

# Fit the sinusoidal wave to the temperature data
# Calculate average temperature each day and convert DOY to years
temp_wave <- nls(Avg.Temp ~ a0 + a1*cos((a2-DOY.Sc)/(2*pi)), data=CSFB_dat, start = list(a0=10, a1=5, a2=0.5))
newdat <- expand.grid(DOY.Sc = seq_range(CSFB_dat$DOY.Sc, n=101)) 
temp_interval <- cbind(newdat,investr::predFit(temp_wave, newdata = newdat, interval="prediction", level=0.8))
temp_interval$fit-temp_interval$lwr
temp_interval$fit-temp_interval$upr

# Create data to predict from
preddata <- CSFB_dat %>%
  data_grid(DOY.pd.Sc = seq_range(DOY.Sc, n = 51),
            Avg.Temp.per.day = 15,
            Rainfall.per.day = 1.6,
            Wind.per.day = 8.2,
            Solar.Rad.per.day = 9.9,
            Height = c("Ground","1m"),
            Site.Location = NA,
            Year = NA)
# Overwrite Avg.Temp.per.day to get predictions over the period of interest
preddata$Avg.Temp.per.day <- summary(temp_wave)$coefficients[1] + summary(temp_wave)$coefficients[2]*cos((summary(temp_wave)$coefficients[3]-preddata$DOY.pd.Sc)/(2*pi))

# Create transformed variables in preddata
preddata$DOY.pd.Sq.Sc <- preddata$DOY.pd.Sc^2
preddata$Avg.Temp.pd.Sq <- preddata$Avg.Temp.per.day^2
preddata$Date <- as.Date(preddata$DOY.pd.Sc*365, origin = "2020-01-01")

# Make top panel
p.sine <- ggplot(CSFB_dat, aes(x=as.Date(yday(Date), origin = "2020-01-01"), y=Avg.Temp)) +
  geom_point() +
  theme_bw(base_size=16) +
  ylab("Temperature (\u00b0C)") +
  xlab("Date") +
  geom_line(data=preddata, aes(x=Date, y=Avg.Temp.per.day), col="blue", linewidth=1.3) +
  geom_line(data=preddata, aes(x=Date, y=Avg.Temp.per.day+3.28), col="red", linewidth=1.3) +
  geom_line(data=preddata, aes(x=Date, y=Avg.Temp.per.day-3.28), col="red", linewidth=1.3)

# Make next panel - follows a similar process to the above
# Set up conditions for the panel with different temperatures
preddata$Climate <- "Average"
preddata.cold <- preddata
preddata.hot <- preddata
preddata.cold$Avg.Temp.per.day <- preddata.cold$Avg.Temp.per.day-3.28
preddata.cold$Avg.Temp.pd.Sq <- preddata.cold$Avg.Temp.per.day^2
preddata.cold$Climate <- "Low"
preddata.hot$Avg.Temp.per.day <- preddata.hot$Avg.Temp.per.day+3.28
preddata.hot$Avg.Temp.pd.Sq <- preddata.hot$Avg.Temp.per.day^2
preddata.hot$Climate <- "High"

preddata <- rbind(preddata,
                  preddata.cold,
                  preddata.hot)

preddata$Level <- factor(preddata$Climate, levels=c("Low", "Average", "High"))

preddata$Date <- as.Date(preddata$DOY.pd.Sc*365, origin = "2020-01-01")

p.temps <- preddata[preddata$Height=="1m",] %>%
  add_epred_draws(m.full) %>%
  ggplot(aes(x = Date, y = Count.per.day)) +
  stat_lineribbon(aes(y = .epred, linetype=Level, col=Level), .width=0.001, fill=NA) +
  #geom_point(data = CSFB_dat[CSFB_dat$Height == "1m",]) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Set2") +
  ggtitle("Temperature (1m traps)") +
  xlab("Date") +
  ylab("Expectated number of beetles") +
  theme_bw(base_size = 16) +
  scale_x_date(breaks=waiver(), date_breaks = "1 month", date_labels="%b") +
  coord_cartesian(ylim=c(0, 3))

# And the next panel
preddata <- CSFB_dat %>%
  data_grid(DOY.pd.Sc = seq_range(DOY.Sc, n = 51),
            Avg.Temp.per.day = 15,
            Rainfall.per.day = 0,
            Wind.per.day = 8.2,
            Solar.Rad.per.day = 9.9,
            Height = c("Ground","1m"),
            Site.Location = NA,
            Year = NA)
preddata$Avg.Temp.per.day <- summary(temp_wave)$coefficients[1] + summary(temp_wave)$coefficients[2]*cos((summary(temp_wave)$coefficients[3]-preddata$DOY.pd.Sc)/(2*pi))

preddata$DOY.pd.Sq.Sc <- preddata$DOY.pd.Sc^2
preddata$Avg.Temp.pd.Sq <- preddata$Avg.Temp.per.day^2

preddata$Rain <- "Low"
preddata.midrain <- preddata
preddata.midrain$Rainfall.per.day <- 1.6
preddata.midrain$Rain <- "Average"
preddata.heavyrain <- preddata
preddata.heavyrain$Rainfall.per.day <- 6.25
preddata.heavyrain$Rain <- "High"

preddata <- rbind(preddata,preddata.midrain,preddata.heavyrain)
preddata$Level <- factor(preddata$Rain, levels=c("Low", "Average", "High"))

preddata$Date <- as.Date(preddata$DOY.pd.Sc*365, origin = "2020-01-01")

p.rain <- preddata[preddata$Height=="1m",] %>%
  add_epred_draws(m.full) %>%
  ggplot(aes(x = Date, y = Count.per.day)) +
  stat_lineribbon(aes(y = .epred, linetype=Level, col=Level), .width=0.001, fill=NA) +
  #geom_point(data = CSFB_dat[CSFB_dat$Height == "1m",]) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Set2") +
  ggtitle("Rainfall (1m traps)") +
  xlab("Date") +
  ylab("Expectated number of beetles") +
  theme_bw(base_size = 16) +
  scale_x_date(breaks=waiver(), date_breaks = "1 month", date_labels="%b") +
  coord_cartesian(ylim=c(0, 3))

# And the next panel
preddata <- CSFB_dat %>%
  data_grid(DOY.pd.Sc = seq_range(DOY.Sc, n = 51),
            Avg.Temp.per.day = 15,
            Rainfall.per.day = 1.6,
            Wind.per.day = 4.2,
            Solar.Rad.per.day = 9.9,
            Height = c("Ground","1m"),
            Site.Location = NA,
            Year = NA)
preddata$Avg.Temp.per.day <- summary(temp_wave)$coefficients[1] + summary(temp_wave)$coefficients[2]*cos((summary(temp_wave)$coefficients[3]-preddata$DOY.pd.Sc)/(2*pi))

preddata$DOY.pd.Sq.Sc <- preddata$DOY.pd.Sc^2
preddata$Avg.Temp.pd.Sq <- preddata$Avg.Temp.per.day^2

preddata$Windiness <- "Low"
preddata.midwind <- preddata
preddata.midwind$Wind.per.day <- 8.2
preddata.midwind$Windiness <- "Average"
preddata.heavywind <- preddata
preddata.heavywind$Wind.per.day <- 12.8
preddata.heavywind$Windiness <- "High"

preddata <- rbind(preddata,preddata.midwind,preddata.heavywind)
preddata$Level <- factor(preddata$Windiness, levels=c("Low", "Average", "High"))

preddata$Date <- as.Date(preddata$DOY.pd.Sc*365, origin = "2020-01-01")

p.wind <- preddata[preddata$Height=="1m",] %>%
  add_epred_draws(m.full) %>%
  ggplot(aes(x = Date, y = Count.per.day)) +
  stat_lineribbon(aes(y = .epred, linetype=Level, col=Level), .width=0.001, fill=NA) +
  #geom_point(data = CSFB_dat[CSFB_dat$Height == "1m",]) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Set2") +
  ggtitle("Wind (1m traps)") +
  xlab("Date") +
  ylab("Expected number of beetles") +
  theme_bw(base_size=16) +
  scale_x_date(breaks=waiver(), date_breaks = "1 month", date_labels="%b") +
  coord_cartesian(ylim=c(0, 3))

# And the next panel
preddata <- CSFB_dat %>%
  data_grid(DOY.pd.Sc = seq_range(DOY.Sc, n = 51),
            Avg.Temp.per.day = 15,
            Rainfall.per.day = 1.6,
            Wind.per.day = 8.2,
            Solar.Rad.per.day = 3.9,
            Height = c("Ground","1m"),
            Site.Location = NA,
            Year = NA)
preddata$Avg.Temp.per.day <- summary(temp_wave)$coefficients[1] + summary(temp_wave)$coefficients[2]*cos((summary(temp_wave)$coefficients[3]-preddata$DOY.pd.Sc)/(2*pi))

preddata$DOY.pd.Sq.Sc <- preddata$DOY.pd.Sc^2
preddata$Avg.Temp.pd.Sq <- preddata$Avg.Temp.per.day^2

preddata$Sunnyness <- "Low"
preddata.midsun <- preddata
preddata.midsun$Solar.Rad.per.day <- 9.9
preddata.midsun$Sunnyness <- "Average"
preddata.heavysun <- preddata
preddata.heavysun$Solar.Rad.per.day <- 18.0
preddata.heavysun$Sunnyness <- "High"

preddata <- rbind(preddata,preddata.midsun,preddata.heavysun)
preddata$Level <- factor(preddata$Sunnyness, levels=c("Low", "Average", "High"))

preddata$Date <- as.Date(preddata$DOY.pd.Sc*365, origin = "2020-01-01")

p.sun <- preddata[preddata$Height=="1m",] %>%
  add_epred_draws(m.full.plussun) %>%
  ggplot(aes(x = Date, y = Count.per.day)) +
  stat_lineribbon(aes(y = .epred, linetype=Level, col=Level), .width=0.001, fill=NA) +
  #geom_point(data = CSFB_dat[CSFB_dat$Height == "1m",]) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Set2") +
  ggtitle("Solar radiation (1m traps)") +
  xlab("Date") +
  ylab("Expected number of beetles") +
  theme_bw(base_size=16) +
  scale_x_date(breaks=waiver(), date_breaks = "1 month", date_labels="%b") +
  coord_cartesian(ylim=c(0, 3))

# Combine the bottom 4 together
p.predictions <- ggarrange(p.temps, p.rain, p.wind, p.sun, nrow=2, ncol=2, common.legend = T, legend = "bottom")

# Add the top one
ggarrange(p.sine, p.predictions, nrow=2, heights=c(1,2), labels=c("A", "B"))
