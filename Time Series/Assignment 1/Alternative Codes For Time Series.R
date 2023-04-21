

covidxts <- xts(covid[,2], order.by = as.Date(covid[,1], "%Y-%m-%d"))

attr(covidxts, 'frequency') <- 2
decompose(as.ts(covidxts))

decompose(as.ts(covidxts))

mo <- as.numeric(format(covid_clean$date[1], "%m"))
yr <- as.numeric(format(covid_clean$date[1], "%Y"))
day <- as.numeric(format(covid_clean$date[1], "%d"))
trial <- ts(covid_clean$contagion, start = c(yr, mo, day), frequency = 365)
decompose(trial)


inds <- seq(as.Date("2020-02-20"), as.Date("2021-02-11"), by = "day")
covidts6 <- ts(covid$contagion,
               start = c(2020, as.numeric(format(inds[1], "%j"))),
               frequency = 365)

inds <- seq(as.Date("2020-02-20"), as.Date("2021-02-11"), by = "day")
covidts6 <- ts(covid$contagion,
               start = c(2020, as.numeric(format(inds[1], "%j"))),
               frequency = 1)
covidts5 <- ts(covid$contagion,
               start = c(2020, as.numeric(format(inds[1], "%j"))),
               frequency = 30)

covidts7 <- ts(covid$contagion,
               start = c(02, as.numeric(format(inds[1], "%j"))),
               frequency = 1)

covidts7
plot(decompose(covidts6))



covidxts <- xts(covid, order.by = as.Date(covid[,1], "%Y-%m-%d"), frequency=1)
covidts <- ts(covidxts, frequency=1)

covidts2 <- ts(covid$contagion, start=as.Date(covid$date, "%Y-%m-%d"), frequency = 365)



#hint: invece di utilizzare mese anno dobbiamo utilizzare mese giorno


covid4 <- ts(covid, start = as.Date(covid$date[1]))


mo <- as.numeric(format(covid_clean$date[1], "%m"))
yr <- as.numeric(format(covid_clean$date[1], "%Y"))
day <- as.numeric(format(covid_clean$date[1], "%d"))
trial <- ts(covid_clean, start = c(yr, mo, day), frequency = 30)