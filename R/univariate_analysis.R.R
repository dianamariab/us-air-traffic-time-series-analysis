# Importarea datelor
setwd("C:/Users/diana/Desktop/[Proiect_SeriiDeTimp]BanceanuDiana_PredeselPatricia_BusoiTeodora")
df <- read.csv("USCarrier_Traffic.csv",header=T)



# Librarii
install.packages(c("tidyverse", "tseries", "forecast", "zoo"))
install.packages("gt")
install.packages("rlang")
install.packages("uroot")
install.packages("lmtest")
install.packages("FinTS")
install.packages("fpp2")
install.packages("changepoint")
install.packages("urca")
install.packages("tseries")

library(tseries)
library(urca)
library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(zoo)
library(ggplot2)
library(fpp2)
library(changepoint)
library(dplyr)
library(uroot)
library(moments)
library(gt)
library(lmtest)
library(FinTS)
library(fpp2)
library(vars)
library(tseries)
library(urca)
library(stats)
library(changepoint)
library(dplyr)
library(uroot)
library(TSA)
library(readxl)
library(FinTS)
library(moments)
library(lmtest)

df <- read.csv("USCarrier_Traffic.csv", header=T, stringsAsFactors=FALSE)

luni <- c("Jan"=1,"Feb"=2,"Mar"=3,"Apr"=4,"May"=5,"Jun"=6,
          "Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12)

df <- df %>%
  mutate(
    an    = as.integer(paste0("20", sub("-.*", "", Period))),
    luna  = luni[sub(".*-", "", Period)],
    Period = as.Date(paste(an, luna, "01", sep="-"))
  ) %>%
  dplyr::select(-an, -luna) %>%
  arrange(Period)

df$Period <- ceiling_date(df$Period, "month") - days(1)

# Transformam df in serie de timp
trafic <- ts(df$Total, start = c(year(min(df$Period)), month(min(df$Period))), frequency = 12)
trafic

# Imapartire in train si test
train <- window(trafic, end = c(2023, 12))  
test  <- window(trafic, start = c(2024, 1)) 


# Vizualizarea seriei- vedem daca avem trend, sezonalitate
ggplot(df, aes(x = Period, y = Total)) +
  geom_line(color = "darkorchid3") +
  labs(title = "Numarul lunar de pasageri al transportului aerian in US", x="Perioada", y = "Pasageri")

# Seria prezintă o sezonalitate puternică și un trend general de creștere de-a lungul întregii perioade analizate (2016–2026).

# Vârfurile apar constant în lunile de vară (iulie–august), ceea ce reflectă cererea ridicată de călătorii în sezonul vacanțelor, în timp ce valorile minime se înregistrează în lunile de iarnă (ianuarie–februarie).
# Trendul seriei este ușor ascendent în perioada 2016–2019, indicând o creștere treptată și constantă a numărului de pasageri.
# În anul 2020 apare o scădere bruscă și extrem de accentuată, cauzată de pandemia COVID-19, traficul aerian prăbușindu-se la valori minime istorice (sub 5.000 de pasageri), reflectând colapsul aproape total al industriei aviatice în acea perioadă.
# Începând cu a doua jumătate a anului 2020 și mai ales din 2021, se observă o revenire treptată, cu o reluare progresivă a sezonalității.
# În perioada 2022–2023 traficul revine aproape la nivelurile pre-pandemie, iar din 2024 îl depășește, seria atingând cele mai ridicate valori din întreaga perioadă analizată (peste 90.000 de pasageri în lunile de vară).
# În concluzie, seria prezintă toate caracteristicile unei serii de timp nestaționare: sezonalitate anuală clară, trend ascendent pe termen lung și un șoc extern major și temporar în 2020.


# Histogramă pentru distribuția traficului aerian lunar
ggplot(data.frame(trafic = as.numeric(trafic)), aes(x = trafic)) + 
  geom_histogram(bins = 25, fill = "maroon3", color = "purple4") +
  labs(title = "Distribuția traficului aerian lunar") +
  xlab("Număr pasageri") + 
  ylab("Frecvență") + 
  theme_bw()

# Distributia este asimetrica spre stanga, majoritatea valorilor fiind concentrate in zona valorilor mari
# Majoritatea lunilor din perioada analizata au avut intre 65.000 si 90.000 de pasageri
# Cele mai frecvente valori sunt intre 75.000 si 80.000, unde frecventa atinge un varf de aproape 19 luni
# Exista si luni cu valori mult mai mici, sub 30.000 de pasageri, dar acestea sunt rare, reprezentand lunile din perioada pandemiei COVID-19
# In concluzie, traficul aerian lunar in USA este in general ridicat, exceptie facand perioada pandemiei COVID-19 care a generat valori extreme, vizibile ca un grup separat in partea stanga a histogramei


# Statistici descriptive
summary(trafic)
sd(trafic)
skewness(trafic)
kurtosis(trafic)
jarque.bera.test(trafic)

# Tabel sumar 
Series <- c("Eșantion","Observații","Media","Mediana","Maximul","Minimul",
            "Deviație standard","Asimetrie","Aplatizare","Jarque-Bera","Probabilitate")
Trafic_aerian <- c("2016M01 - 2026M01", length(trafic), 
                   round(mean(trafic), 3),
                   round(median(trafic), 3),
                   round(max(trafic), 3),
                   round(min(trafic), 3),
                   round(sd(trafic), 3),
                   round(skewness(trafic), 3),
                   round(kurtosis(trafic), 3),
                   round(jarque.bera.test(trafic)$statistic, 3),
                   paste0(format.pval(jarque.bera.test(trafic)$p.value, digits = 3), ifelse(jarque.bera.test(trafic)$p.value < 0.01, "*", ""))
)
summary_statistics <- as.data.frame(cbind(Series, Trafic_aerian))

summary_statistics %>% 
  gt() %>% 
  tab_header(title = md("Statistici descriptive pentru traficul aerian lunar (2016M1–2026M1)"))


# Grafice de sezonalitate

ggsubseriesplot(trafic) +
  ylab("Number of Passengers") +
  ggtitle("Seasonal subseries plot: monthly US air traffic") +
  theme_bw()

# Din acest plot subserial sezonier se poate observa o sezonalitate clara cu o crestere semnificativa a nr de pasageri in lunile de vara (iulie-august) ceea ce sugereaza un varf al traficului aerian in perioada vacantelor.
# Lunile de inceput de an (ianuarie-februarie) au cele mai scazute medii, indicate de liniile albastre orizontale, confirmand o perioada de trafic redus in sezonul rece.
# In fiecare luna se observa o cadere brusca spre zero, reprezentand impactul pandemiei COVID-19 din 2020, care apare ca un outlier vizibil in toate subseriile lunare.
# Cu exceptia acestui soc pandemic, sezonalitatea este relativ stabila de-a lungul perioadei analizate, variatiile de la an la an in cadrul aceleiasi luni fiind mici si regulate.
# In concluzie, graficul reflecta un model sezonier recurent in traficul aerian din SUA cu varfuri constante vara si minime iarna.

ggseasonplot(trafic, polar = TRUE) +
  ylab("Number of Passengers") +
  ggtitle("Polar seasonal plot: monthly US air traffic") +
  theme_bw()

# Graficul arata evolutia lunara a traficului aerian din SUA in perioada 2016-2026, folosind un format polar pentru a evidentia sezonalitatea
# Se observa varfuri constante in lunile de vara (iulie-august) si scaderi in lunile de iarna (ianuarie-februarie), confirmate de dimensiunea mai mare a cercurilor in zona de jos a graficului
# Anii 2016-2019 au valori similare si apropiate, ceea ce indica o perioada de stabilitate si crestere lenta a traficului aerian
# Anul 2020 (linia verde inchis) are o scadere brusca si dramatica incepand din aprilie, cauzata de pandemia COVID-19, cercul sau fiind mult mai mic decat al celorlalti ani
# Anul 2021 (linia turcoaz) arata o revenire partiala, dar inca sub nivelurile normale, in special in prima jumatate a anului
# Incepand cu 2022-2023 traficul isi revine treptat, apropiindu-se de valorile pre-pandemice
# In 2024-2025, cercurile devin cele mai mari din tot graficul, sugerand ca traficul aerian a depasit nivelurile anterioare pandemiei si continua sa creasca
# In concluzie, graficul polar evidentiaza clar atat sezonalitatea recurenta a traficului aerian, cat si socul exceptional din 2020 si recuperarea graduala din anii urmatori


# Observarea trendului si sezonalitatii cu medie mobila si decompozitie
# Medie mobila pe 12 luni

df <- df %>%
  mutate(ma_12 = rollmean(Total, k = 12, fill = NA))

# Plot cu trend
ggplot(df, aes(x = Period)) +
  geom_line(aes(y = Total), color = "#7AC5CD") +
  geom_line(aes(y = ma_12), color = "deeppink4") +
  labs(title = "Trend cu medie mobilă (12 luni)", x="Perioada", y = "Pasageri")

# Acest grafic ilustrează evoluția lunară a numărului de pasageri din transportul aerian din SUA, evidențiind atât sezonalitatea, cât și trendul general al seriei pentru perioada 2016-2026.
# Se observă o sezonalitate puternică, cu variații regulate de-a lungul fiecărui an.
# Începând cu 2023-2024, media mobilă atinge cele mai ridicate valori din întreaga perioadă analizată, de peste 80.000 de pasageri, sugerând o creștere susținută a traficului aerian.
# Media mobilă pe 12 luni ajută la netezirea fluctuațiilor sezoniere și evidențiază clar următorul parcurs: creștere inițială stabilă, scădere brusca în 2020 și o revenire urmată de depășirea nivelurilor anterioare pandemiei.

# Descompunerea seriei

plot(decompose(trafic))

# Acest grafic prezintă o descompunere aditivă a seriei de timp pentru perioada 2016-2026, separând componentele principale: valoarea observată, trendul, sezonalitatea și componenta aleatoare.
# Se poate observa că seria are o sezonalitate clară. 
# Trendul evidențiază o creștere lentă și constantă în perioada 2016-2019, urmată de o scădere bruscă în 2020, coborând până în jurul valorii de 300.000, apoi o revenire treptată începând cu 2021.
# Deși a avut loc această scădere drastică, sezonalitatea a rămas aproape neschimbată pe toată perioada analizată chiar și în timpul șocului pandemic, ceea ce indică o regularitate puternică în comportamentul călătorilor, indiferent de context.
# Componenta aleatoare rămâne relativ stabilă în prima parte (2016-2019), dar înregistrează valori extreme negative în 2020, atingând aproape -30.000, reflectând imprevizibilitatea acestui moment. După 2021, variațiile aleatoare devin din nou mai temperate și se stabilizează.
# În ansamblu, seria este bine structurată cu o componentă sezonieră dominantă și un trend clar afectat de un eveniment.

# Descompunerea STL
d_stl <- stl(trafic,s.window = "periodic")
plot(d_stl)

# Componenta remainder evidențiază abateri semnificative în perioada 2020-2021, cu variații extreme negative ce ating aproape -30.000, confirmând șocul neașteptat al pandemiei. După 2021, fluctuațiile devin din nou foarte mici și regulate, aproape de zero, ceea ce indică faptul că modelul STL surprinde bine structura seriei în afara perioadei pandemice.

# Corelograma seriei de training inainte de diferentiere
ggtsdisplay(train)

#Corelograma confirmă că seria de training este posibil nestaționară — ACF descrește lent si prezinta valori mari la lag-uri mici. 

#Testarea radacinii unitare sezoniere
# HEGY test
hegy.test(train)
#toate F-urile sunt semnificative, nu este nevoie de diferntiere sezoniera. 
#t_1 nu e semnificativ, posibil avem nevoie doar de diferntiere de ordinul 1

# Canova-Hansen test
ch.test(train)
#nici un p-value nu e semnificativ => nu respingem H0 => sezonalitate este stabila


# Testarea stationaritatii in nivel


# ADF
adf_none <- ur.df(train, type = "none", selectlags = "AIC")
summary(adf_none)

adf_drift <- ur.df(train, type = "drift", selectlags = "AIC")
summary(adf_drift)

adf_trend <- ur.df(train, type = "trend", selectlags = "AIC")
summary(adf_trend)

#valorile calculate < valorile critice => nestationariate


# Testul KPSS (Kwiatkowski-Phillips-Schmidt-Shin)

train %>% ur.kpss() %>% summary()
#valoarea calculata < valorile critice => stationaritate

# Phillips-Perron
PP.test(train)
#p-value = 0.4526 > 0.05 - nu respingem H0 - seria este nestationara

#chiar daca testul kpss presupune ca seria ar fi stationara tot o sa aplicam diferentiere de ordin 1

#Diferentierea seriei 
# 1. Diferentiere de ordin 1
train_d1 <- diff(train)

ggtsdisplay(train_d1)
#corelograma presupune ca seria dupa diferentiere este stationara
#Verificam:
# Retestare stationaritate dupa diferentierea de ordin 1
summary(ur.df(train_d1, type = "none", selectlags = "AIC"))
summary(ur.df(train_d1, type = "drift", selectlags = "AIC"))
summary(ur.df(train_d1, type = "trend", selectlags = "AIC"))
#la ADF valorile calculate > valorile critice => stationaritate 

train_d1 %>% ur.kpss() %>% summary()
#valoarea calculata < valoarile critice => stationaritate

PP.test(train_d1)
#p value = 0.01 - respingem H0 - seria e stationara 

# 2. Verificam si diferentierea sezoniera
train_D1 <- diff(train, lag = 12)

ggtsdisplay(train_D1)

summary(ur.df(train_D1, type = "none", selectlags = "AIC"))
#stationara
summary(ur.df(train_D1, type = "drift", selectlags = "AIC"))
#stationara doar la 90%
summary(ur.df(train_D1, type = "trend", selectlags = "AIC"))
#nestationara
train_D1 %>% ur.kpss() %>% summary()
#nestationara
PP.test(train_D1)
#nestationara

# 3. Diferentiere de ordin 1 + diferentiere sezoniera
train_d1D1 <- diff(diff(train, lag = 12), differences = 1)

ggtsdisplay(train_d1D1)
#conform corelogramei seria e stationara
#în ACF apare un spike semnificativ la lag 1 → indică existența unei dependențe pe termen scurt,sugerând o posibilă componentă MA(1)
#în PACF apare un spike semnificativ la lag 1 → sugerează prezența unei componente AR(1)
#în ACF apare un spike negativ puternic la lag 12 → indică sezonalitate anuală,sugerând o posibilă componentă sezonieră de tip SMA(1)
#în PACF apare un spike semnificativ (negativ) la lag 12 → sugerează o componentă sezonieră autoregresivă de tip SAR(1)
#nu se observă spike-uri clare la lag 24 în PACF → componentele sezoniere de ordin mai mare (ex: SAR(2)) nu sunt susținute puternic de date

summary(ur.df(train_d1D1, type = "none", selectlags = "AIC"))
summary(ur.df(train_d1D1, type = "drift", selectlags = "AIC"))
summary(ur.df(train_d1D1, type = "trend", selectlags = "AIC"))
#valorile calculate > valorile critice => stationara
train_d1D1 %>% ur.kpss() %>% summary()
#valoarea calculata < valorile critice => stationara
PP.test(train_d1D1)
#p value = 0.01 => stationara
hegy.test(train_d1D1)
#hegy confirma ca seria este una stationara, toate p value sunt semnificative

#CONCLUZIE FINALĂ

# Toate testele sunt consistente:
# - ADF → staționară
# - KPSS → staționară
# - PP → staționară
# - HEGY → staționară

# Concluzie:
# → seria devine staționară după aplicarea:
#    d = 1 și D = 1



# Testul Zivot-Andrew
#H0: seria are o rădăcină unitară (este nestaționară), chiar și în prezența unei rupturi structurale
#H1: seria este staționară în jurul unei rupturi structurale

za_trend <- ur.za(train, model = "trend")

za_both <- ur.za(train, model = "both")

summary(za_trend)
# Statistica de test este -2.9691, mai mare decât toate valorile critice (1%: -4.93, 5%: -4.42, 10%: -4.11),
# deci nu putem respinge ipoteza nulă în varianta cu ruptură doar în trend.
# Seria originală nu este staționară nici în prezența unei rupturi structurale în trend.
# Punctul potențial de ruptură se află la poziția 54, corespunzând lunii iunie 2020,
# ceea ce coincide cu perioada de după debutul pandemiei COVID-19.

summary(za_both)
# Statistica de test este -5.8642, mai mică decât valoarea critică de 1% (-5.57),
# deci respingem ipoteza nulă — seria prezintă staționaritate în jurul unei rupturi
# atât în intercept cât și în trend.
# Coeficienții du și dt sunt puternic semnificativi (p < 0.001, ***),
# confirmând că ruptura structurală este reală și semnificativă statistic.
# Punctul potențial de ruptură se află la poziția 50, corespunzând lunii februarie 2020,
# ceea ce coincide cu debutul pandemiei COVID-19 și prăbușirea traficului aerian.

# În concluzie, testul Zivot-Andrews confirmă existența unei rupturi structurale majore în 2020,
# cauzată de pandemia COVID-19, iar seria prezintă staționaritate odată ce această ruptură este luată în calcul.




# Convertim pozițiile de ruptură identificate de testul Zivot-Andrews în date calendaristice.
start_date <- as.yearmon("2016-01")
break_trend_date <- start_date + (54 - 1) / 12  # poz 54 -> Iun 2020
break_both_date  <- start_date + (50 - 1) / 12  # poz 50 -> Feb 2020
break_trend_date
break_both_date



row1 <- c("Statistici", "Break în trend", "Break în intercept + trend")
row2 <- c("Min t-stat", "-2.9691", "-5.8642")
row3 <- c("1%", "-4.93", "-5.57")
row4 <- c("5%", "-4.42", "-5.08")
row5 <- c("10%", "-4.11", "-4.82")
row6 <- c("Punct potențial de ruptură", "2020M06", "2020M02")

zivot_andrew <- as.data.frame(rbind(row1, row2, row3, row4, row5, row6))

zivot_andrew %>% gt() %>% tab_header(
  title = md("Test Zivot-Andrews: rupturi structurale în traficul aerian lunar")
)

# Testul Zivot-Andrews indică posibile rupturi structurale în seria traficului aerian lunar. 
# Ambele variante ale testului indica stationaritatea seriei.

# Modelul final va avea forma:
# SARIMA(p,1,q)(P,1,Q)[12]

# Estimarea modelului SARIMA optim 

# Identificare automata a modelului ca referinta
fit_auto <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(fit_auto)
coeftest(fit_auto)


#ESTIMAREA MODELELOR CANDIDATE
fit1 <- Arima(train, order = c(0, 1, 0), seasonal = c(0, 1, 1))
coeftest(fit1) #sma1 semnificativ (***)

fit2 <- Arima(train, order = c(0, 1, 1), seasonal = c(0, 1, 1))
coeftest(fit2)# ma1 și sma1 ambii semnificativi (***)

fit3 <- Arima(train, order = c(1, 1, 0), seasonal = c(0, 1, 1))
coeftest(fit3)# ar1 și sma1 ambii semnificativi (***)

fit4 <- Arima(train, order = c(0, 1, 0), seasonal = c(1, 1, 1))
coeftest(fit4)#sar1 nesemnificativ (p = 0.4221)

fit5 <- Arima(train, order = c(0, 1, 0), seasonal = c(2, 1, 1))
coeftest(fit5) #sar1 și sar2 nesemnificativi

fit6 <- Arima(train, order = c(1, 1, 1), seasonal = c(0, 1, 1))
coeftest(fit6) #ar1 nesemnificativ (p = 0.493), ma1 la limită (p = 0.0508)

fit7 <- Arima(train, order = c(0, 1, 1), seasonal = c(1, 1, 1))
coeftest(fit7)#sar1 nesemnificativ (p = 0.4399)

fit8 <- Arima(train, order = c(1, 1, 0), seasonal = c(1, 1, 1))
coeftest(fit8)#sar1 nesemnificativ (p = 0.542)

# Rezumat modele - comparare
summary(fit1)  # AIC=1716.66  BIC=1721.5 RMSE=5860.708
summary(fit2)  # AIC=1699.37  BIC=1706.63 RMSE=5209.836 
summary(fit3)  # AIC=1702.1   BIC=1709.36 RMSE=5298.146

#fit2 — SARIMA(0,1,1)(0,1,1)[12] este modelul optim, având cele mai mici valori pentru toți cei trei indicatori. fit3 este apropiat, dar inferior lui fit2 pe toate criteriile.
#Concluzie:Se selectează fit2 ca model de prognoză

# Acuratete SARIMA 
# Aplicam testul Diebold-Mariano
# H0: prognozele au aceeasi acuratete
# H1: prognozele au acuratete diferita

dm.test(fit1$residuals, fit2$residuals, h=25) # p-value = 0.3246
dm.test(fit1$residuals, fit3$residuals, h=25) # p-value = 0.3229
dm.test(fit2$residuals, fit3$residuals, h=25) # p-value = 0.371

# În toate cele 3 comparații, p-value >> 0.05, deci acceptăm ipoteza nulă —
# cele trei modele SARIMA candidate au aceeași acuratețe din punct de vedere statistic.
# Prin urmare, selecția modelului optim se face pe baza criteriilor informaționale
# AIC și BIC, unde fit2 SARIMA(0,1,1)(0,1,1)[12] este cel mai bun,
# având cele mai mici valori (AIC=1699.37, BIC=1706.63).

# Verificare reziduuri
checkresiduals(fit1)  # p=0.027 < 0.05 -> reziduuri autocorelate, model inadecvat
checkresiduals(fit2)  # p=0.769 > 0.05 -> reziduuri aleatoare, model adecvat
checkresiduals(fit3)  # p=0.754 > 0.05 -> reziduuri aleatoare, model adecvat

# Modelul optim este SARIMA(0,1,1)(0,1,1)[12] - fit2
# Are cele mai mici criterii informaționale (AIC și BIC)
# și reziduurile nu prezintă autocorelație semnificativă.

# Verificarea stationaritatii modelului ales
autoplot(fit2)
# Toate punctele trebuie sa fie in interiorul cercului unitar
# => procesul este stationar

# Test Ljung-Box (autocorelare reziduuri)
Box.test(fit2$residuals, lag = 12, type = "Ljung-Box") # p-value = 0.6265 > 0.1
Box.test(fit2$residuals, lag = 24, type = "Ljung-Box") # p-value = 0.7667 > 0.1
Box.test(fit2$residuals, lag = 36, type = "Ljung-Box") # p-value = 0.6295 > 0.1
Box.test(fit2$residuals, lag = 48, type = "Ljung-Box") # p-value = 0.9412 > 0.1

# La toate cele 4 laguri testate (12, 24, 36, 48), p-value >> 0.05,
# deci nu putem respinge ipoteza nulă — reziduurile nu prezintă autocorelație semnificativă.


# Test Jarque-Bera (normalitate)

jarque.bera.test(fit2$residuals)

# Testul Jarque-Bera returnează o statistică de 5127.2 cu p-value < 2.2e-16,
# deci respingem ipoteza nulă — reziduurile nu sunt distribuite normal.
# Această non-normalitate este cauzată cel mai probabil de valorile extreme
# din perioada pandemiei COVID-19 (2020), care apar ca outlieri puternici
# și distorsionează distribuția reziduurilor.
#Totusi nu avem autocorelatie deci non-normalitatea nu influenteaza modelul SARIMA.


# Test ARCH LM (heteroscedasticitate)

ArchTest(fit2$residuals, lags = 12) # p-value = 1
ArchTest(fit2$residuals, lags = 24) # p-value = 1
ArchTest(fit2$residuals, lags = 36) # p-value = 1
ArchTest(fit2$residuals, lags = 48) # p-value = 0.4728

# Testul ARCH LM aplicat la toate cele 4 laguri returnează p-value >> 0.05 în toate cazurile.
# Nu putem respinge ipoteza nulă — deci nu există heteroscedasticitate în reziduurile modelului.

# Prognoza SARIMA

fit_sarima_accuracy <- fit2%>% forecast::forecast(h=25) 


# Precizia prognozei față de setul de test

forecast::accuracy(fit_sarima_accuracy, test)


# Plot prognoză SARIMA

fit_sarima_accuracy %>% autoplot() +
  ggtitle("Prognoza traficului aerian pe 25 luni – model SARIMA") +
  xlab("Timp") + ylab("Număr pasageri (milioane)") +
  theme_bw()

# Graficul prezintă prognoza traficului aerian pentru perioada 2024-2026
# folosind modelul SARIMA(0,1,1)(0,1,1)[12].
# Seria istorică evidențiază clar șocul pandemic din 2020, urmat de revenirea treptată
# și atingerea unor niveluri record în 2023-2024.
# Prognoza punctuală (linia albastră închis) indică o continuare relativ stabilă
# a traficului în jurul valorii de 75.000-80.000 de pasageri lunar,
# cu păstrarea sezonalității caracteristice.
# Benzile de încredere se lărgesc progresiv în timp, ceea ce este normal
# incertitudinea prognozei crește pe măsură ce orizontul de timp se extinde.
# Banda de 80% (albastru închis) și cea de 95% (albastru deschis) sugerează că
# valorile reale viitoare vor fi cel mai probabil între 50.000 și 110.000 de pasageri lunar.
# În concluzie, modelul oferă o prognoză corecta in concordanta cu tendințele istorice.


# Tehnici de netezire exponentiala 

# Holt-Winters aditiv

hw_a_model <- hw(train, seasonal="additive",h=25)  # 12 * 1(2024) + 1(ian 2025) = 13 luni din test
summary(hw_a_model)
forecast::accuracy(hw_a_model,test)


#Holt-Winters multiplicativ

hw_m_model <- hw(train, seasonal="multiplicative",h=25)  # 12 * 1(2024) + 1(ian 2025) = 13 luni din test
summary(hw_m_model)
forecast::accuracy(hw_m_model,test)


# Rezultatele empirice ale modelelor Holt-Winters
#HoltWinters_multiplicativ
Model1 <- c("Parametrii de netezire:",
            "Alpha (nivel) =  0.9904",
            "Beta (trend) =  0.01",
            "Gamma (sezonal) = 0.0091",
            "AIC = 2349.976",
            "AICc = 2357.822",
            "BIC = 2393.570")
#Holt-Winters_aditiv 
Model2 <- c("Parametrii de netezire:",
            "Alpha (nivel) =0.9999",
            "Beta (trend) = 1e-04",
            "Gamma (sezonal) = 1e-04", 
            "AIC = 2138.714",
            "AICc = 2146.561",
            "BIC = 2182.308")

rezultate_hw <- as.data.frame(cbind(Model1,
                                    Model2))

rezultate_hw %>% gt() %>% tab_header(
  title = md("Rezultatele empirice ale modelelor Holt-Winters pentru prognoza traficului aerian")
)


# Metrici de performanță--------------

Metrici <- c("ME","RMSE","MAE","MPE","MAPE","MASE")
HW_aditiv_antrenare <- c(-42.42351, 5883.341, 3031.414, -9.307404, 15.517522, 0.1792085)
HW_aditiv_test <- c(2173.98402, 4340.171, 3590.871, 2.363444, 4.345254, 0.2122820)
HW_multiplicativ_antrenare <- c(43.48601, 6444.886, 3532.551, -10.143241, 18.389924, 0.2088343)
HW_multiplicativ_test <- c(2597.25102, 4836.371, 3910.199, 2.945888, 4.729205, 0.2311597)

performanta_hw <- as.data.frame(cbind(Metrici,
                                      HW_multiplicativ_antrenare,
                                      HW_multiplicativ_test,
                                      HW_aditiv_antrenare,
                                      HW_aditiv_test))

performanta_hw %>% gt() %>% tab_header(
  title = md("Performanța prognozei cu modele Holt-Winters pentru traficul aerian")
)

autoplot(trafic) +
  autolayer(hw_a_model, series="HW aditiv", PI=FALSE) +
  autolayer(hw_m_model, series="HW multiplicativ", PI=FALSE) +
  xlab("Timp") +
  ylab("Număr pasageri (milioane)") +
  ggtitle("Prognoza traficului aerian pe baza modelelor Holt-Winters (2026)") +
  guides(colour=guide_legend(title="Model de prognoză")) +
  theme_bw()


# Modelul Holt-Winters aditiv are o potrivire mai bună decât cel multiplicativ, conform valorilor mai mici ale AIC (2138.714 vs 2349.976), AICc (2146.561 vs 2357.822) și BIC (2182.308 vs 2393.570).
# La modelul aditiv, parametrii de netezire sunt alpha = 0.9999, beta = 1e-04, gamma = 1e-04, ceea ce înseamnă că este foarte sensibil la ultimele date, în timp ce componenta de trend și cea sezonieră au influență redusă.
# La modelul multiplicativ, parametrii sunt mai echilibrați (alpha = 0.9904, beta = 0.01, gamma = 0.0091), dar criteriile informaționale sunt semnificativ mai mari, indicând o potrivire mai slabă.
# La antrenare, modelul aditiv are erori mai mici (RMSE = 5883 vs 6444, MAPE = 15.5% vs 18.4%), iar pe setul de test performanța acestuia rămâne superioară (RMSE = 4340 vs 4836, MAPE = 4.34% vs 4.73%).
# In concluzie, modelul Holt-Winters aditiv este mai potrivit pentru datele noastre, avand erori mai mici atat la antrenare cat si la testare si criterii informationale semnificativ mai mici.

# Testarea reziduurilor pentru modelul HW aditiv------------------------

res_hw_a <- residuals(hw_a_model)
autoplot(res_hw_a)+
  xlab("Month")+
  ylab("")+
  ggtitle("Reziduurile HW Aditiv")+
  theme_bw()

# Graficul arată reziduurile modelului Holt-Winters aditiv aplicat traficului aerian din SUA între 2016 și 2024.
# În perioada 2016-2019, reziduurile sunt mici și distribuite uniform în jurul valorii 0, semnalând o bună potrivire a modelului.
# În 2020 apare o abatere puternică și negativă, atingând aproape -40.000 cauzată de impactul pandemiei COVID-19 pe care modelul nu a putut să îl anticipeze.
# Așadar, modelul a funcționat bine în condiții normale, dar nu a putut anticipa șocurile neașteptate din perioada COVID-19.


# Histograma reziduurilor

gghistogram(res_hw_a)+
  ggtitle("Histograma reziduurilor HW Aditiv")

# Histograma reziduurilor este puternic asimetrică spre stânga cu marea majoritate a valorilor concentrate în jurul lui 0 unde frecvența atinge aproape 40 de observații.
# Totuși, există câteva valori extreme negative, în jurul valorilor -30.000 și -40.000, cauzate de șocul pandemic din 2020.
# Distribuția nu este normală, fiind dominată de aceste valori extreme care trag coada histogramei spre stânga.
# În general, cu excepția perioadei pandemice, modelul se potrivește bine datelor, reziduurile fiind mici și centrate în jurul valorii 0.


# Testul Jarque-Berra pentru normalitatea reziduurilor

# H0: seria este normal distribuita
# H1: seria nu este normal distribuita

jarque.bera.test(res_hw_a)

# Rezultatul testului arată X-squared = 2435.8 și un p-value mai mic decât 2.2e-16.
# Deoarece p-value-ul este mult sub pragul de semnificație de 0.05, respingem ipoteza nulă conform căreia reziduurile sunt distribuite normal.
# Acest rezultat confirmă ce se vedea deja în histogramă: distribuția reziduurilor diferă semnificativ de o distribuție normală, din cauza valorilor extreme generate de pandemia COVID-19 în 2020.


# Functia de autocorelatie a reziduurilor

ggAcf(res_hw_a)+
  ggtitle("Rezidurile ACF")

# Graficul ACF al reziduurilor arată că primul lag depășește semnificativ limita de încredere (aproximativ 0.42), indicând o autocorelare reziduală la lag 1.
# Reziduurile nu se comportă complet ca un zgomot alb, ceea ce indică faptul că modelul HW aditiv nu a reușit să surprindă în totalitate structura temporală a datelor.
# Per ansamblu, modelul are o performanță rezonabilă, dar prezența autocorelației reziduale sugerează că un model mai complex (ex. SARIMA) ar putea oferi rezultate mai bune.


#Testul Ljung-Box

# H0: model doesn't show lack of fit
# H1: model shows lack of fit

Box.test(res_hw_a, lag=20, type="Lj")

# Rezultatul testului indică X-squared = 32.656 și un p-value de 0.03679.
# Deoarece p-value-ul este mai mic decât pragul de semnificație de 0.05, respingem ipoteza nulă conform căreia reziduurile sunt necorelate.
# Așadar, reziduurile prezintă autocorelație semnificativă, ceea ce confirmă concluzia din graficul ACF și sugerează că modelul Holt-Winters aditiv nu a reușit să capteze în totalitate structura temporală a datelor.


#Testarea reziduurilor si pentru modelul HW Multiplicativ-------------

res_hw_m <- residuals(hw_m_model)
autoplot(res_hw_m)+
  xlab("Month")+
  ylab("")+
  ggtitle("Reziduurile HW Multiplicativ")+
  theme_bw()

# Graficul prezinta reziduurile modelului Holt-Winters multiplicativ aplicat traficului aerian din SUA între 2016 și 2024.
# Până în 2020, reziduurile sunt foarte mici și distribuite uniform în jurul valorii 0 semnalând o bună potrivire a modelului în condiții normale.
# În 2020 apare o abatere puternică, mai întâi negativă (aproape -1) urmată imediat de una puternic pozitivă (peste 2), cauzată de colapsul și revenirea bruscă a traficului aerian în perioada pandemiei.
# După 2021, reziduurile revin la un nivel foarte stabil și apropiat de 0, cu variații minime, ceea ce indică o bună potrivire a modelului în perioada post-pandemică.
# Așadar, modelul a funcționat bine în perioadele normale, dar nu a putut anticipa șocurile neașteptate din perioada COVID-19, acestea apărând ca valori extreme în grafic.


# Histograma reziduurilor

gghistogram(res_hw_m)+
  ggtitle("Histograma reziduurilor HW Multiplicativ")

# Histograma afișează distribuția reziduurilor pentru modelul Holt-Winters multiplicativ.
# Se observă că marea majoritate a reziduurilor sunt concentrate în jurul valorii 0, cu un vârf de frecvență de aproape 37 de observații, ceea ce sugerează că modelul face predicții foarte bune în condiții normale.
# Totuși, există câteva valori extreme atât în partea negativă (până la -1), cât și în partea pozitivă (până la 2.5), reprezentând abaterile cauzate de pandemia COVID-19 din 2020.
# Distribuția este puternic leptocurtică (ascuțită) și asimetrică spre dreapta din cauza outlierului pozitiv extrem, deci nu urmează o distribuție normală.


# Testul Jarque-Berra pentru normalitatea reziduurilor

# H0: seria este normal distribuita
# H1: seria nu este normal distribuita

jarque.bera.test(res_hw_m)

# Testul Jarque-Bera aplicat asupra reziduurilor modelului Holt-Winters multiplicativ are X-squared = 9253.3 și un p-value < 2.2e-16, ceea ce indică faptul că respingem ipoteza nulă a testului.
# Distribuția reziduurilor nu urmează o distribuție normală, lucru confirmat și de histogramă care arăta o distribuție puternic leptocurtică cu outlieri extremi.


# Functia de autocorelatie a reziduurilor

ggAcf(res_hw_m)+
  ggtitle("Rezidurile ACF")

# Autocorelările reziduurilor se află în interiorul limitelor de semnificație, ceea ce este un semn bun.
# Asta sugerează că modelul a capturat o mare parte din structura de autocorelare din date.


# Testul Ljung-Box

# H0: model doesn't show lack of fit
# H1: model shows lack of fit

Box.test(res_hw_m, lag=20,type="Lj")
# p-value > 0.1 => rezidurile nu sunt corelate 


#ETS

ets_model_train <- ets(train)
summary(ets_model_train)


# Tabel rezultate ETS

#ETS(A, N, A): Eroare Aditivă, Fără trend, Sezonalitate Aditivă

tabel_rezultate_ETS <-
  c("Parametri de netezire:",
    "Alpha (nivel) = 0.9999",
    "Gamma (sezonal) = 1e-04",
    "AIC = 2134.688",
    "AICc = 2140.688",
    "BIC = 2173.153")

rezultate_ets <- as.data.frame(tabel_rezultate_ETS)

rezultate_ets %>% gt() %>% tab_header(
  title = md("Rezultatele empirice ale modelului ETS pentru prognoza traficului aerian lunar")
)

# Modelul selectat automat este ETS(A,N,A): eroare aditivă, fără componentă de trend și sezonalitate aditivă.
# Alpha = 0.9999 acordă o importanță foarte mare valorilor recente, deci modelul reacționează rapid la schimbări, în timp ce gamma = 1e-04 indică o componentă sezonieră foarte stabilă.
# Valorile criteriilor de selecție (AIC = 2134.688, AICc = 2140.688, BIC = 2173.153) sunt comparabile cu cele ale modelului HW aditiv, confirmând o performanță bună.
# În concluzie, modelul este bine adaptat datelor și poate fi utilizat cu încredere pentru prognoză.



# Calculul perforanței prognozei

ets_model_train %>% forecast::forecast(h = 25) %>%
  forecast::accuracy() 

# Tabel cu metrici de performanță

Metrici <- c("ME","RMSE","MAE","MPE","MAPE","MASE")
ETS_antrenare <- c(80.10485, 5882.528, 3044.842, -9.033881, 15.48585, 0.1800023)

performanta_ets <- as.data.frame(cbind(Metrici, ETS_antrenare))

performanta_ets %>% gt() %>% tab_header(
  title = md("Performanța prognozei modelului ETS pentru traficul aerian")
)

# Modelul ETS(A,N,A) are o performanță bună în prognoza traficului aerian.
# Eroarea medie (ME = 80.10) sugereaza ca modelul subestimeaza in medie cu ~80 pasager per perioada, bias-ul fiind unul foarte mic raportat la scala seriei de trafic aerian => prognozele sunt ne-biasate
# RMSE = 5882 > MAE = 3044 indică erori moderate, performanța se deteriorează semnificativ în perioadele cu șocuri extreme.
# MAPE = 15.48% > 10% -> performanta de prognoza buna, iar MASE = 0.18 < 1 arată că modelul este semnificativ mai bun decât naivul.
# În general, prognoza este precisă pentru perioadele normale, performanța fiind afectată în principal de șocul pandemic din 2020.

autoplot(ets_model_train)

# Graficul prezintă cele trei componente ale modelului ETS(A,N,A): seria observată, nivelul și sezonalitatea.
# În componenta observată se vede clar scăderea bruscă din 2020 cauzată de pandemie, urmată de o revenire treptată și o depășire a nivelurilor pre-pandemice după 2022.
# Nivelul seriei urmează îndeaproape valorile observate, scăzând dramatic în 2020 până în jurul valorii de 15.000 apoi revenind și atingând cele mai mari valori din perioada analizată în 2023-2024, confirmând absența unui trend explicit în model.
# Sezonalitatea este clară și constantă de-a lungul întregii perioade analizate cu amplitudini între aproximativ -5.000 și +7.000, având un model repetitiv anual care justifică folosirea componentei sezoniere aditive.
# În ansamblu, modelul ETS(A,N,A) surprinde bine comportamentul datelor cu excepția șocului pandemic care a generat valori extreme greu de anticipat. 

checkresiduals(ets_model_train)

# Graficul reziduurilor arată că acestea sunt în mare parte aleatoare în jurul valorii 0, dar există o abatere extremă negativă în 2020 (aproape -40.000), cauzată de pandemia COVID-19, pe care modelul nu a putut să o anticipeze.
# ACF-ul reziduurilor arată un lag semnificativ (12), sugerând o structură sezonieră reziduală neexplicată complet de model.
# Histograma arată o distribuție puternic asimetrică spre stânga, cu majoritatea valorilor concentrate în jurul lui 0, dar cu o coadă lungă negativă cauzată de valorile extreme din 2020.
# Testul Ljung-Box are Q* = 32.29 și un p-value = 0.02899, sub pragul de 0.05, deci respingem ipoteza că reziduurile sunt complet aleatorii — modelul ETS(A,N,A) nu surprinde perfect toate tiparele din date.


train %>% ets() %>% forecast(h = 25) %>% autoplot() 

# Modelul urmează bine tendința istorică, surprinzând sezonalitatea și nivelul general al seriei în perioada 2016-2024.
# Prognoza pentru perioada 2024-2026 are o incertitudine foarte mare, banda de încredere lărgindu-se semnificativ.
# Această incertitudine mare este cauzată de variațiile extreme din trecut, în special căderea bruscă din 2020, care a mărit semnificativ sigma-ul modelului.
# Asta arată că, deși modelul oferă o estimare punctuală rezonabilă în jurul valorii de 75.000 de pasageri, există multă nesiguranță în prognoză din cauza șocului pandemic înregistrat în date.


# Compararea diferitelor metode univariate de prognoza -----------------

# Forecast pe perioada testului --------------------------
fc1 <- forecast(fit1, h = length(test))
fc2 <- forecast(fit2, h = length(test))
fc3 <- forecast(fit3, h = length(test))

# Erorile de forecast
e1 <- test - fc1$mean
e2 <- test - fc2$mean
e3 <- test - fc3$mean

# Calculăm erorile de prognoză ca diferență între valorile reale din setul de test
# și valorile prognozate de fiecare model, pentru a putea aplica testul Diebold-Mariano
# direct pe erorile din perioada de test.

# Aplicam testul Diebold-Mariano pe erorile din setul de test
# H0: prognozele au aceeasi acuratete
# H1: prognozele au acuratete diferita

dm.test(e1, e2, h = 1) # p-value = 0.004442
dm.test(e1, e3, h = 1) # p-value = 0.01519
dm.test(e2, e3, h = 1) # p-value = 0.01223

# Spre deosebire de testul DM aplicat pe reziduurile din antrenare,
# pe erorile din setul de test toate comparațiile sunt semnificative statistic.
# dm.test(e1, e2): p-value = 0.004 < 0.01 — fit2 este semnificativ mai bun decât fit1
#   (DM pozitiv înseamnă că e1 are erori mai mari decât e2).
# dm.test(e1, e3): p-value = 0.015 < 0.05 — fit3 este semnificativ mai bun decât fit1
#   (DM negativ înseamnă că e3 are erori mai mici decât e1).
# dm.test(e2, e3): p-value = 0.012 < 0.05 — fit3 este semnificativ mai bun decât fit2
#   pe setul de test (DM negativ indică erori mai mici pentru fit3).
# În concluzie, pe setul de test modelul SARIMA(1,1,0)(0,1,1)[12] - fit3
# pare să ofere prognoze mai precise, deși fit2 rămâne preferat
# pe baza criteriilor informaționale AIC și BIC.

# Generarea prognozelor
fc_sarima_test <- forecast(fit2, h = length(test))
fc_ets_test    <- forecast(ets_model_train, h = length(test))
hw_a_forecast  <- forecast(hw_a_model, h = length(test))
hw_m_forecast  <- forecast(hw_m_model, h = length(test))

# Grafic comparativ out-of-sample SARIMA vs ETS vs HW
autoplot(train, series = "Train observat") +
  autolayer(test, series = "Test realizat") +
  autolayer(fc_sarima_test$mean, series = "Prognoză SARIMA(0,1,1)(0,1,1)[12]") +
  autolayer(fc_ets_test$mean, series = "Prognoză ETS(A,N,A)") +
  autolayer(hw_a_forecast$mean, series = "Prognoză HW Aditiv") +
  autolayer(hw_m_forecast$mean, series = "Prognoză HW Multiplicativ") +
  xlab("Timp") +
  ylab("Număr pasageri") +
  ggtitle("Prognoze pe perioada de test: SARIMA vs ETS vs HWa vs HWm") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw()
#Toate cele 4 modele generează prognoze rezonabile și capturează sezonalitatea caracteristică traficului aerian. 
#SARIMA rămâne modelul optim, deși diferențele vizuale dintre modele sunt mici pe această perioadă de test post-pandemică.


# Acuratete out-of-sample comparativa SARIMA vs ETS vs HW
accuracy_sarima_ets_HW <- bind_rows(
  forecast::accuracy(fc_sarima_test, test) %>% as.data.frame() %>% mutate(Model = "SARIMA(0,1,1)(0,1,1)[12]"),
  forecast::accuracy(fc_ets_test, test) %>% as.data.frame() %>% mutate(Model = "ETS(A,N,A)"),
  forecast::accuracy(hw_a_forecast, test) %>% as.data.frame() %>% mutate(Model = "HW Aditiv"),
  forecast::accuracy(hw_m_forecast, test) %>% as.data.frame() %>% mutate(Model = "HW Multiplicativ")
)
print(accuracy_sarima_ets_HW)
#SARIMA este cel mai bun model pe toate metricile — are cel mai mic RMSE, MAPE, MASE și Theil's U pe setul de test.
#HW Aditiv este al doilea cel mai bun, foarte apropiat de SARIMA — diferența de MAPE este de doar 0.14 puncte procentuale, ceea ce explică rezultatul DM nesemnificativ între ele.
#HW Multiplicativ și ETS au performanțe mai slabe, cu MAPE peste 4.7% respectiv 5.1%.

# Testarea acuratetii modelelor folosind testul Diebold-Mariano-------------------
# Testul se aplica pe erorile de prognoza out-of-sample,
# nu pe reziduurile brute in-sample

err_sarima <- as.numeric(test - fc_sarima_test$mean)
err_ets    <- as.numeric(test - fc_ets_test$mean)
err_hw_a   <- as.numeric(test - hw_a_forecast$mean)
err_hw_m   <- as.numeric(test - hw_m_forecast$mean)

# Interpretare:
# H0: cele doua modele au aceeasi acuratete de prognoza
# H1: acuratetea de prognoza difera.

dm.test(e1 = err_sarima, e2 = err_ets,  h = 1, power = 2)
#SARIMA vs ETS → DM = −3.15, p = 0.004
#p < 0.05 → diferență semnificativă → SARIMA e mai bun decât ETS

dm.test(e1 = err_sarima, e2 = err_hw_a, h = 1, power = 2)
#SARIMA vs HW Aditiv → DM = −2.53, p = 0.018
#p < 0.05 → diferență semnificativă → SARIMA e mai bun decât HW Aditiv

dm.test(e1 = err_sarima, e2 = err_hw_m, h = 1, power = 2)
#SARIMA vs HW Multiplicativ → DM = −1.95, p = 0.063
#p > 0.05 → diferență nesemnificativă → SARIMA și HW Multiplicativ sunt similare ca acuratețe

dm.test(e1 = err_ets,    e2 = err_hw_a, h = 1, power = 2)
#ETS vs HW Aditiv → DM = 3.15, p = 0.004
#p < 0.05 → diferență semnificativă → HW Aditiv e mai bun decât ETS

dm.test(e1 = err_ets,    e2 = err_hw_m, h = 1, power = 2)
#ETS vs HW Multiplicativ → DM = 0.82, p = 0.419
#p > 0.05 → nesemnificativ → ETS și HW Multiplicativ sunt similare

dm.test(e1 = err_hw_a,   e2 = err_hw_m, h = 1, power = 2)
#HW Aditiv vs HW Multiplicativ → DM = −1.41, p = 0.171
#p > 0.05 → nesemnificativ → cele două HW sunt similare între ele

#SARIMA este singurul model semnificativ superior față de două modele alternative, confirmând că e cea mai bună alegere pentru prognoza traficului aerian.


# Prognoza in-sample (fitted values) pe perioada de training ---------------

fitted_sarima <- fitted(fit2)
fitted_ets    <- fitted(ets_model_train)
fitted_hw_a   <- fitted(hw_a_model)
fitted_hw_m   <- fitted(hw_m_model)

# Grafic comparativ in-sample SARIMA vs ETS vs HW
autoplot(train, series = "Training observat") +
  autolayer(fitted_sarima, series = "SARIMA fitted") +
  autolayer(fitted_ets,    series = "ETS fitted") +
  autolayer(fitted_hw_a,   series = "HW Aditiv fitted") +
  autolayer(fitted_hw_m,   series = "HW Multiplicativ fitted") +
  xlab("Timp") +
  ylab("Număr pasageri") +
  ggtitle("Ajustare in-sample: SARIMA vs ETS vs HW") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw()
#Ajustarea in-sample este bună pentru toate modelele în condiții normale. Diferența principală apare în 2020, unde SARIMA se adaptează mai rapid la șocul extrem. 
#Potrivirea foarte strânsă a tuturor modelelor pe training confirmă că niciunul nu este supraajustat, toate captează structura reală a datelor.

# Acuratete in-sample SARIMA vs ETS vs HW
forecast::accuracy(fitted_sarima, train)
forecast::accuracy(fitted_ets,    train)
forecast::accuracy(fitted_hw_a,   train)
forecast::accuracy(fitted_hw_m,   train)
#SARIMA este cel mai bun model in-sample — are cel mai mic RMSE (5209) și cel mai mic MAPE (9.01%), semnificativ mai bun decât celelalte modele.
#ETS și HW Aditiv sunt aproape identice pe training — RMSE ~5882 vs 5883 și MAPE ~15.49% vs 15.52%, confirmând rezultatul DM nesemnificativ dintre ele.
#HW Multiplicativ are cea mai slabă potrivire in-sample cu RMSE = 6444 și MAPE = 18.39%.




#CONCLUZIE GENERALA:
#Analiza a urmărit modelarea și prognoza traficului aerian lunar din SUA utilizând metodologia Box-Jenkins și tehnici de netezire exponențială, pe o serie de timp cu 121 observații lunare.
#Seria prezintă sezonalitate anuală puternică și stabilă (vârfuri iulie–august, minime ianuarie–februarie), confirmată vizual prin graficele subserial și polar, și un trend ușor ascendent în 2016–2019. Un șoc structural major apare în 2020 cauzat de pandemia COVID-19, urmat de revenire treptată și depășirea nivelurilor pre-pandemice după 2022. Testul Zivot-Andrews confirmă ruptura structurală în februarie–iunie 2020.
#Testele ADF (toate 3 specificații), PP și KPSS aplicate pe seria de training indică nestationaritate în nivel. Testul HEGY confirmă că nu este necesară diferențierea sezonieră (F-uri semnificative), iar Canova-Hansen confirmă că sezonalitatea este stabilă în timp. Seria devine staționară după aplicarea d=1 și D=1, confirmat de toate testele pe seria dublu diferențiată.
#Din corelograma seriei dublu diferențiate s-au identificat spike-uri la lag 1 (MA/AR) și lag 12 (SMA/SAR). Au fost estimate 8 modele candidate, validate prin auto.arima() ca referință. Modelul optim selectat este SARIMA(0,1,1)(0,1,1)[12] 
#Singura ipoteză neîndeplinită este normalitatea reziduurilor (Jarque-Bera p < 0.001), explicată prin șocul pandemic din 2020 — aceasta nu afectează validitatea prognozelor.
#Testul Diebold-Mariano confirmă că SARIMA este semnificativ superior față de ETS (p=0.004) și HW Aditiv (p=0.018), iar toate modelele sunt superioare unui model naiv (MASE < 1, Theil's U < 1).
#SARIMA(0,1,1)(0,1,1)[12] este modelul optim pentru prognoza traficului aerian lunar din SUA, oferind cele mai precise prognoze cu o eroare medie de doar ~4% față de valorile reale. Modelul captează bine sezonalitatea anuală și generalizează excelent pe date noi, confirmat de performanța superioară pe setul de test față de toate metodele alternative testate.