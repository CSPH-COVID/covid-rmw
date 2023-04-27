### ESTIMATING BETA IN THE INITIAL GROWTH PHASE OF THE PANDEMIC ###
# COLORADO #

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")

# source BigQuery credentials
source("./Admin Documents/Training and Access/bq_credentials.R")

# read in EMR hospitalization data from BigQuery
sql <- "SELECT * FROM `emresource.hospitalized`"
tb <- bq_project_query(x="co-covid-models", query=sql)
epi1 <- bq_table_download(tb) %>%
  arrange(measure_date) %>%
  rename(Iht = hospitalized) %>%
  filter(measure_date >= "2020-01-24",
         measure_date <= "2020-04-02")
  
dates <- as.data.frame(seq(as.Date("2020-01-24"), max(epi1$measure_date), "days")) %>%
  rename(measure_date = `seq(as.Date("2020-01-24"), max(epi1$measure_date), "days")`)

epi2 <- merge(epi1, dates, "measure_date", all = T) %>%
  replace(is.na(.), 0)

epi2$time <- 1:nrow(epi2)

# plot early hospitalizations
plot(epi2$Iht, lwd=6, type='h', xlab="Date", ylab="In-Hospital Patient Count",
     main="Colorado Hospital Census, 01/24/2020 to 04/30/2020", col='orange', axes=FALSE)
axis(1, seq(0, 140, 20))
axis(2, seq(0, 40, 10))

# identify historical parameters (either established from literature or estimated previously)
co_pop <- 5784308
n1 <- 1261007.6 # population age group 0-17
n2 <- 3673331.4 # population age group 18-64
n3 <- 849969 # population age group 65+
alpha <- 5.35 # latent period
gamma <- 0.2 # recovery rate (1/duration of infectiousness)
lambda <- 3.125 # ratio of infectiousness symptomatic/asymptomatic
pS1 <- 0.110023 # symptomatic fraction 0-17
pS2 <- 0.437025209 # symptomatic fraction 18-64
pS3 <- 0.774879 # symptomatic fraction 65+
hosp1 <- 0.0286 # hospitalization fraction among symptomatic 0-17
hosp2 <- 0.05521973 # hospitalization fraction among symptomatic 18-64
hosp3 <- 0.0995 # hospitalization fraction among symptomatic 65+
hlos1 <- 8.666667 # hospital length of stay 0-17
hlos2 <- 8.275081 # hospital length of stay 18-64
hlos3 <- 9.871245 # hospital length of stay 65+
dh1 <- 0.005504587 # death fraction among hospitalized 0-17
dh2 <- 0.044384033 # death fraction among hospitalized 18-64
dh3 <- 0.2979 # death fraction among hospitalized 65+
dnh1 <- 0.000147 # death fraction among non-hospitalized 0-17
dnh2 <- 0.001068919 # death fraction among non-hospitalized 18-64
dnh3 <- 0.02806 # death fraction among non-hospitalized 65+
tc <- 0

# list of parameters to be estimated (just beta)
params1 <- list(
  beta = 0.6)

N <- co_pop

# specify initial values
inits <- c(S1 = n1 - 1, E1 = 0, I1 = 1, A1 = 0, Ih1 = 0, R1 = 0, D1 = 0,
           S2 = n2,     E2 = 0, I2 = 0, A2 = 0, Ih2 = 0, R2 = 0, D2 = 0,
           S3 = n3,     E3 = 0, I3 = 0, A3 = 0, Ih3 = 0, R3 = 0, D3 = 0)
  
# set up the vector for daily hospitalizations, which we need for optimization
# we need to set up a one-column data frame for time in order to index our vector from 0 instead of 1
Hosp <- epi2[c("time", "Iht")]
time <- data.frame(time = 1:length(epi2$Iht))
DailyHosp <- merge(time, epi2, by = "time", all = TRUE)
DailyHosp[is.na(DailyHosp)] <- 0
hosp.vec <- as.vector(DailyHosp$Iht)

# set up the incrementing time vector for the SEIR model
Day <- 1:length(epi2$Iht)

# check to make sure the time and hospitalization vectors are the same length
length(hosp.vec)
length(Day)

# build the SEIR model as an R function
seir1 <- function(t, inits, parameters) {
  
  with(as.list(c(inits, parameters)), {
      
    dS1  <-                         - (I1+I2+I3)*(beta*lambda*S1*(1-tc))/N - (beta*S1*(A1+A2+A3)*(1-tc))/N
    dE1  <- - E1           /alpha   + (I1+I2+I3)*(beta*lambda*S1*(1-tc))/N + (beta*S1*(A1+A2+A3)*(1-tc))/N 
    dI1  <-  (E1*     pS1) /alpha        - I1*gamma
    dA1  <-  (E1*(1 - pS1))/alpha                   - A1*gamma
    dIh1 <-   I1* gamma*   hosp1                              - Ih1/hlos1
    dR1  <- - I1*(gamma*(1-hosp1-dnh1)) + I1*gamma + A1*gamma + Ih1/hlos1 - dh1*Ih1/hlos1 - dnh1*I1*gamma
    dD1  <-                                                                 dh1*Ih1/hlos1 + dnh1*I1*gamma
    
    dS2  <-                         - (I1+I2+I3)*(beta*lambda*S2*(1-tc))/N - (beta*S2*(A1+A2+A3)*(1-tc))/N
    dE2  <- - E2           /alpha   + (I1+I2+I3)*(beta*lambda*S2*(1-tc))/N + (beta*S2*(A1+A2+A3)*(1-tc))/N 
    dI2  <-  (E2*     pS2) /alpha       - I2*gamma
    dA2  <-  (E2*(1 - pS2))/alpha                  - A2*gamma
    dIh2 <-   I2* gamma*   hosp2                              - Ih2/hlos2
    dR2  <- - I2*(gamma*(1-hosp2-dnh2)) + I2*gamma + A2*gamma + Ih2/hlos2 - dh2*Ih2/hlos2 - dnh2*I2*gamma
    dD2  <-                                                                 dh2*Ih2/hlos2 + dnh2*I2*gamma
    
    dS3  <-                         - (I1+I2+I3)*(beta*lambda*S3*(1-tc))/N - (beta*S3*(A1+A2+A3)*(1-tc))/N
    dE3  <- - E3           /alpha   + (I1+I2+I3)*(beta*lambda*S3*(1-tc))/N + (beta*S3*(A1+A2+A3)*(1-tc))/N 
    dI3  <-  (E3*     pS3) /alpha       - I3*gamma
    dA3  <-  (E3*(1 - pS2))/alpha                  - A3*gamma
    dIh3 <-   I3* gamma*   hosp3                              - Ih3/hlos3
    dR3  <- - I3*(gamma*(1-hosp3-dnh3)) + I3*gamma + A3*gamma + Ih3/hlos3 - dh3*Ih3/hlos3 - dnh3*I3*gamma
    dD3  <-                                                                 dh3*Ih3/hlos3 + dnh3*I3*gamma      
      return(list(c(dS1, dE1, dI1, dA1, dIh1, dR1, dD1,
                    dS2, dE2, dI2, dA2, dIh2, dR2, dD2,
                    dS3, dE3, dI3, dA3, dIh3, dR3, dD3),
                  Iht = Ih1 + Ih2 + Ih3,
                  Itotal = I1 + I2 + I3 + A1 + A2 + A3,
                  Etotal = E1 + E2 + E3))
    })
  }
out1 <- as.data.frame(lsoda(y = inits, times = Day, func = seir1, parms = params1))

# test fitted curve
plot(epi2$Iht, lwd = 2,  type = 'h', col = 'black')
lines(out1$Iht, lwd = 2, type = 'l', col = 'green')

# define a function to calculate the residual sum of squares (RSS), passing in the parameter
# that is to be optimized for the best fit to the daily hospitalization data

RSS <- function(params1) {
  names(params1) <- c("beta")
  out1 <- lsoda(y = inits, times = Day, func = seir1, parms = params1)
  fit <- out1[, "Iht"]
  sum((hosp.vec - fit)^2)
}

# use the optim command and establish upper and lower bounds for parameter estimates
opt <- optim(par = c(0.4),
               fn = RSS,
               method = "L-BFGS-B",
               lower=c(0.2), 
               upper =c(0.7))

# check for convergence
opt$message
opt$value

# now examine the fitted values for the parameters of interest
opt_par <- setNames(opt$par, c("beta"))
opt_par

# if the parameters make sense, proceed to run the model a second time
params2 <- list(
  beta = opt_par[1])

seir2 <- function(t, inits, parameters) {
  
  with(as.list(c(inits, parameters)), {
    
    dS1  <-                         - (I1+I2+I3)*(beta*lambda*S1*(1-tc))/N - (beta*S1*(A1+A2+A3)*(1-tc))/N
    dE1  <- - E1           /alpha   + (I1+I2+I3)*(beta*lambda*S1*(1-tc))/N + (beta*S1*(A1+A2+A3)*(1-tc))/N 
    dI1  <-  (E1*     pS1) /alpha        - I1*gamma
    dA1  <-  (E1*(1 - pS1))/alpha                   - A1*gamma
    dIh1 <-   I1* gamma*   hosp1                              - Ih1/hlos1
    dR1  <- - I1*(gamma*(1-hosp1-dnh1)) + I1*gamma + A1*gamma + Ih1/hlos1 - dh1*Ih1/hlos1 - dnh1*I1*gamma
    dD1  <-                                                                 dh1*Ih1/hlos1 + dnh1*I1*gamma
    
    dS2  <-                         - (I1+I2+I3)*(beta*lambda*S2*(1-tc))/N - (beta*S2*(A1+A2+A3)*(1-tc))/N
    dE2  <- - E2           /alpha   + (I1+I2+I3)*(beta*lambda*S2*(1-tc))/N + (beta*S2*(A1+A2+A3)*(1-tc))/N 
    dI2  <-  (E2*     pS2) /alpha       - I2*gamma
    dA2  <-  (E2*(1 - pS2))/alpha                  - A2*gamma
    dIh2 <-   I2* gamma*   hosp2                              - Ih2/hlos2
    dR2  <- - I2*(gamma*(1-hosp2-dnh2)) + I2*gamma + A2*gamma + Ih2/hlos2 - dh2*Ih2/hlos2 - dnh2*I2*gamma
    dD2  <-                                                                 dh2*Ih2/hlos2 + dnh2*I2*gamma
    
    dS3  <-                         - (I1+I2+I3)*(beta*lambda*S3*(1-tc))/N - (beta*S3*(A1+A2+A3)*(1-tc))/N
    dE3  <- - E3           /alpha   + (I1+I2+I3)*(beta*lambda*S3*(1-tc))/N + (beta*S3*(A1+A2+A3)*(1-tc))/N 
    dI3  <-  (E3*     pS3) /alpha       - I3*gamma
    dA3  <-  (E3*(1 - pS2))/alpha                  - A3*gamma
    dIh3 <-   I3* gamma*   hosp3                              - Ih3/hlos3
    dR3  <- - I3*(gamma*(1-hosp3-dnh3)) + I3*gamma + A3*gamma + Ih3/hlos3 - dh3*Ih3/hlos3 - dnh3*I3*gamma
    dD3  <-                                                                 dh3*Ih3/hlos3 + dnh3*I3*gamma
    
    return(list(c(dS1, dE1, dI1, dA1, dIh1, dR1, dD1,
                  dS2, dE2, dI2, dA2, dIh2, dR2, dD2,
                  dS3, dE3, dI3, dA3, dIh3, dR3, dD3),
                Iht = Ih1 + Ih2 + Ih3,
                Itotal = I1 + I2 + I3 + A1 + A2 + A3,
                Etotal = E1 + E2 + E3))
  })
}
out2 <- as.data.frame(lsoda(y = inits, times = Day, func = seir2, parms = params2))
out2$date <- as.Date(out2$time, format="%m/%d/%Y", origin="01/24/2020")

# obtain final parameter estimates
round(opt_par, 4)

co_beta_fit <- ggplot() +
  geom_bar(data = epi2, aes(x = measure_date, y = Iht, fill = "color1"), stat = "identity") +
  geom_line(data = out2, aes(x = date, y = Iht, color = "color2"), linewidth = 1.5) +
  labs(x = NULL, y = "In-Hospital Patient Count") +
  ggtitle("Colorado EMResource Hospitalization Data Fit") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "2 weeks", limits = as.Date(c("2020-01-24", "2020-04-08")), date_labels = "%m/%d/%Y") +
  scale_fill_manual(values = c("color1" = "grey2"),
                    labels = c("color1" = "Observed Hospitalizations")) +
  scale_color_manual(values = c("color2" = "chartreuse2"),
                     labels = c("color2" = paste("Fitted Curve, beta =", round(opt_par, 4)))) +
  guides(color = guide_legend(nrow = 3)) +
  ggplot_theme; co_beta_fit
ggsave("./CSTE/Figures/co_beta_fit.png", height = 8, width = 13, plot = co_beta_fit)
