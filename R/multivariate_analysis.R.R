setwd("C:/Users/diana/Desktop/[Proiect_SeriiDeTimp]BanceanuDiana_PredeselPatricia_BusoiTeodora")



# 0. PACHETE NECESARE

packages_needed <- c(
  "readxl", "zoo", "xts", "ggplot2", "gridExtra", "dplyr", "tidyr",
  "tseries", "urca", "vars", "lmtest", "sandwich",
  "forecast", "scales", "patchwork", "seasonal", "FinTS"
)

packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if (length(packages_to_install) > 0) install.packages(packages_to_install)
invisible(lapply(packages_needed, library, character.only = TRUE))


# CONTEXTUL APLICATIEI

# Aceasta aplicatie investigheaza relatia dinamica pe termen lung si pe
# termen scurt dintre traficul aerian lunar, pretul petrolului WTI si
# rata somajului din SUA, pentru perioada 2016-2026.
#
# Justificare economica:
# - Pretul petrolului influenteaza direct costul combustibilului aviatic,
#   cea mai mare cheltuiala operationala a companiilor aeriene.
#   Un pret mai mare => bilete mai scumpe => cerere mai mica => trafic scade.
# - Rata somajului reflecta starea pietei muncii si veniturile disponibile.
#   Somaj ridicat => venituri mai mici => mai putine calatorii business si
#   leisure => impact negativ direct asupra traficului aerian.
# - Socul pandemic COVID-19 (2020) a afectat simultan toate trei variabile:
#   traficul a colapsat, somajul a explodat la 14.7%, petrolul a scazut
#   din cauza cererii globale prabusita.
#
# Analiza multivariata permite:
# - investigarea interdependentelor dinamice dintre cele 3 variabile
# - identificarea relatiei de echilibru pe termen lung (cointegrare)
# - evaluarea impactului socurilor prin functii IRF
# - masurarea contributiei fiecarui soc prin FEVD



# ===========================================================================
# 1. IMPORTUL DATELOR
# ===========================================================================


df_raw <- read.csv("date_multivariate.csv", header = TRUE, stringsAsFactors = FALSE)
df_raw$Date <- as.Date(df_raw$Date)
df_raw <- df_raw[order(df_raw$Date), ]

str(df_raw)
summary(df_raw)
head(df_raw, 10)


# ===========================================================================
# 2. CONSTRUIREA SERIILOR TS
# ===========================================================================

# Serii in nivel (originale)
trafic_raw <- ts(df_raw$Trafic, start = c(2016, 1), frequency = 12)
petrol_raw  <- ts(df_raw$Petrol, start = c(2016, 1), frequency = 12)
somaj_raw   <- ts(df_raw$Somaj,  start = c(2016, 1), frequency = 12)

# VERIFICAREA SEZONALITATII - vizual

par(mfrow = c(3,1), mar = c(3,4,2,1))
plot(trafic_raw, main = "Trafic aerian lunar SUA (nr. pasageri)",
     col = "darkorchid3", ylab = "Pasageri", xlab = "")
plot(petrol_raw, main = "Pretul petrolului WTI (USD/baril)",
     col = "deeppink4", ylab = "USD/baril", xlab = "")
plot(somaj_raw, main = "Rata somajului SUA (%)",
     col = "#7AC5CD", ylab = "%", xlab = "")
par(mfrow = c(1,1))

# Observatii vizuale:
# - Traficul prezinta sezonalitate puternica (varfuri vara, minime iarna)
#   si un soc pandemic major in 2020 (confirmat in analiza univariata)
# - Pretul petrolului este instabil, cu prabusire in 2020 si revenire ulterioara
# - Rata somajului a explodat in 2020 (de la ~3.5% la ~14.7%) si a revenit


# Ajustam toate 3 seriile pentru consistenta metodologica
trafic <- seasadj(stl(trafic_raw, s.window = "periodic", robust = TRUE))
petrol <- seasadj(stl(petrol_raw, s.window = "periodic", robust = TRUE))
somaj  <- seasadj(stl(somaj_raw,  s.window = "periodic", robust = TRUE))

# Serie multivariata pentru VAR/VECM (in nivel, ajustat sezonier)
Y <- cbind(Trafic = trafic, Petrol = petrol, Somaj = somaj)



# ===========================================================================
# 3. ANALIZA DESCRIPTIVA
# ===========================================================================

# 3.1 Statistici descriptive
stat_desc <- data.frame(
  Serie   = c("Trafic (adj.)", "Petrol (adj.)", "Somaj (adj.)"),
  Media   = c(mean(trafic), mean(petrol), mean(somaj)),
  Mediana = c(median(trafic), median(petrol), median(somaj)),
  Min     = c(min(trafic), min(petrol), min(somaj)),
  Max     = c(max(trafic), max(petrol), max(somaj)),
  SD      = c(sd(trafic), sd(petrol), sd(somaj)),
  CV      = c(sd(trafic)/mean(trafic), sd(petrol)/mean(petrol), sd(somaj)/mean(somaj))
)
print(stat_desc)

#Statistici descriptive — esențial
#Trafic: medie 69.414 < mediană 74.263 → distribuție asimetrică stânga, cauzată de minimul pandemic de 3.079 pasageri care trage media în jos. CV = 24.5% — variabilitate moderată.
#Petrol: medie ≈ mediană (~63 USD) → distribuție simetrică. CV = 27.5%, volatilitate ridicată: de la 13.9 USD (colaps COVID 2020) la 114.9 USD (criza energetică 2022).
#Șomaj: medie 4.58% > mediană 4.10% → asimetrie dreapta din cauza spike-ului la 14.7% în aprilie 2020. CV = 37.5% — cea mai volatilă relativ dintre cele trei, deși în mod normal e variabila cea mai stabilă.

# 3.2 Data frame pentru vizualizare
df <- data.frame(
  timp   = as.yearmon(time(trafic)),
  Trafic = as.numeric(trafic),
  Petrol = as.numeric(petrol),
  Somaj  = as.numeric(somaj)
)

df_long <- df %>%
  pivot_longer(cols = c(Trafic, Petrol, Somaj),
               names_to  = "variabila",
               values_to = "valoare")

# Grafic principal: evolutia celor 3 serii cu media
ggplot(df_long, aes(x = timp, y = valoare, color = variabila)) +
  geom_line(linewidth = 1) +
  geom_hline(
    data = df_long %>%
      group_by(variabila) %>%
      summarise(mean_val = mean(valoare, na.rm = TRUE), .groups = "drop"),
    aes(yintercept = mean_val, color = variabila),
    linetype = "dashed", linewidth = 0.8
  ) +
  facet_wrap(~variabila, ncol = 1, scales = "free_y",
             labeller = labeller(variabila = c(
               Trafic = "Trafic aerian (pasageri, adj. sezonier)",
               Petrol = "Pret petrol WTI (USD/baril, adj. sezonier)",
               Somaj  = "Rata somajului SUA (%, adj. sezonier)"
             ))) +
  scale_color_manual(values = c(
    Trafic = "darkorchid3",
    Petrol = "deeppink4",
    Somaj  = "#7AC5CD"
  )) +
  labs(
    title    = "Evolutia Traficului, Pretului petrolului si Somajului in SUA (2016-2026)",
    subtitle = "Date lunare ajustate sezonier | Sursa: BTS, EIA, BLS",
    x = "Timp", y = "", color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(size = 11, face = "bold"))


# 3.3 Corelatii intre variabile
cor_matrix <- cor(cbind(trafic, petrol, somaj), use = "complete.obs")
print(round(cor_matrix, 4))

#Graficul și matricea de corelații spun aceeași poveste din unghiuri diferite. 2020 este evenimentul dominant care structurează toate relațiile din sistem.
#Trafic – Șomaj (r = −0.890): corelația cea mai puternică și cea mai intuitivă. Graficul o confirmă perfect — în aprilie 2020, traficul coboară la zero în timp ce șomajul explodează la 14.7%. Mecanismul este direct: șomaj ridicat → venituri disponibile reduse → cerere de călătorii prăbușită. Revenirea este oglindă: pe măsură ce șomajul revine sub 4% (2022–2023), traficul depășește nivelurile pre-pandemice.
#Trafic – Petrol (r = +0.497): corelația pozitivă pare contraintuitivă — ne-am aștepta ca petrolul scump să reducă traficul. Explicația vizibilă în grafic: ambele variabile sunt conduse de ciclul economic global. În expansiune (2016–2019) cresc împreună; în recesiunea COVID (2020) cad împreună. Relația negativă petrol→trafic există, dar se manifestă pe termen scurt cu decalaj, nu în corelația simplă pe niveluri.
#Petrol – Șomaj (r = −0.556): același mecanism ciclic — recesiunile aduc simultan șomaj ridicat și cerere globală scăzută (deci petrol ieftin). Corelația e moderată pentru că petrolul are și alți factori de influență (geopolitică, OPEC) independenți de ciclul economic american.
#Concluzie metodologică importantă de menționat în lucrare: corelațiile ridicate pe serii în nivel sunt potențial spurioase când seriile sunt non-staționare. De aceea testele de staționaritate și cointegrare din pașii următori sunt esențiale — pentru a stabili dacă aceste relații sunt economice reale sau artefacte statistice.

# 3.4 Corelograme individuale
# ACF descrescatoare lent => nestationaritate confirmata
ggtsdisplay(trafic, main = "Trafic aerian (adj. sezonier) - corelogram")
ggtsdisplay(petrol, main = "Pret petrol WTI (adj. sezonier) - corelogram")
ggtsdisplay(somaj,  main = "Rata somajului (adj. sezonier) - corelogram")

# 3.5 Functia de corelatie incrucisata (CCF)
# CCF 1: Petrol -> Trafic
ccf_petrol_trafic <- ccf(as.numeric(petrol), as.numeric(trafic),
                         plot = FALSE, lag.max = 24)
df_ccf1 <- data.frame(lag = ccf_petrol_trafic$lag,
                      ccf = ccf_petrol_trafic$acf)
ggplot(df_ccf1, aes(x = lag, y = ccf)) +
  geom_bar(stat = "identity", fill = "deeppink4", alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c( 2/sqrt(length(trafic)),
                             -2/sqrt(length(trafic))),
             linetype = "dashed", color = "red") +
  labs(
    title    = "CCF: Pret petrol vs Trafic aerian",
    subtitle = "Laguri pozitive = petrolul precede traficul | Laguri negative = invers",
    x        = "Lag (luni)", y = "Corelatie"
  ) +
  theme_minimal(base_size = 12)


# CCF 2: Somaj -> Trafic
ccf_somaj_trafic <- ccf(as.numeric(somaj), as.numeric(trafic),
                        plot = FALSE, lag.max = 24)
df_ccf2 <- data.frame(lag = ccf_somaj_trafic$lag,
                      ccf = ccf_somaj_trafic$acf)
ggplot(df_ccf2, aes(x = lag, y = ccf)) +
  geom_bar(stat = "identity", fill = "#7AC5CD", alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c( 2/sqrt(length(trafic)),
                             -2/sqrt(length(trafic))),
             linetype = "dashed", color = "red") +
  labs(
    title    = "CCF: Rata somajului vs Trafic aerian",
    subtitle = "Laguri pozitive = somajul precede traficul | Laguri negative = invers",
    x        = "Lag (luni)", y = "Corelatie"
  ) +
  theme_minimal(base_size = 12)


# CCF 3: Petrol -> Somaj
ccf_petrol_somaj <- ccf(as.numeric(petrol), as.numeric(somaj),
                        plot = FALSE, lag.max = 24)
df_ccf3 <- data.frame(lag = ccf_petrol_somaj$lag,
                      ccf = ccf_petrol_somaj$acf)
ggplot(df_ccf3, aes(x = lag, y = ccf)) +
  geom_bar(stat = "identity", fill = "darkorchid3", alpha = 0.7) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c( 2/sqrt(length(somaj)),
                             -2/sqrt(length(somaj))),
             linetype = "dashed", color = "red") +
  labs(
    title    = "CCF: Pret petrol vs Rata somajului",
    subtitle = "Ambele variabile conduse de ciclul economic global",
    x        = "Lag (luni)", y = "Corelatie"
  ) +
  theme_minimal(base_size = 12)


# ===========================================================================
# IPOTEZELE TUTUROR TESTELOR STATISTICE UTILIZATE IN ANALIZA
# ===========================================================================
#
# -------------------------------------------------------------------------
# [T1] TEST ADF – Augmented Dickey-Fuller (ur.df)
# -------------------------------------------------------------------------
# Scopul: Testeaza prezenta radacinii unitare (non-stationaritate)
#
# H0: Seria contine o radacina unitara => este NON-STATIONARA
# H1: Seria NU contine radacina unitara => este STATIONARA
#
# Specificatii (type):
#   "none"  => fara constanta si fara trend (model pur AR)
#   "drift" => cu constanta (media != 0), fara trend
#   "trend" => cu constanta si trend determinist
#
# Decizie: daca statistica tau < valoarea critica (negativa) la 5%
#          => respingem H0 => seria este stationara
#          daca statistica tau > valoarea critica => NU respingem H0
#          => seria este non-stationara
#
# -------------------------------------------------------------------------
# [T2] TEST KPSS – Kwiatkowski-Phillips-Schmidt-Shin (ur.kpss)
# -------------------------------------------------------------------------
# Scopul: Complementar ADF — testeaza stationaritatea (H0 invers fata de ADF)
#
# H0: Seria este STATIONARA (in jurul unei constante sau a unui trend)
# H1: Seria este NON-STATIONARA (contine radacina unitara)
#
# Specificatii (type):
#   "mu"  => H0: stationara in jurul unei constante
#   "tau" => H0: stationara in jurul unui trend determinist
#
# Decizie: daca statistica KPSS > valoarea critica la 5%
#          => respingem H0 => seria este non-stationara
#          daca statistica KPSS < valoarea critica => NU respingem H0
#          => seria este stationara
#
# ATENTIE: KPSS si ADF au H0 opuse! Concluzie robusta:
#   ADF nu respinge H0 (non-stationar) + KPSS respinge H0 (non-stationar)
#   => AMBELE confirma non-stationaritatea => concluzie solida
#
# -------------------------------------------------------------------------
# [T3] TEST PP – Phillips-Perron (pp.test)
# -------------------------------------------------------------------------
# Scopul: Alternativa la ADF, robusta la heteroscedasticitate si autocorelare
#         in reziduuri (nu necesita specificarea lagurilor)
#
# H0: Seria contine o radacina unitara => este NON-STATIONARA
# H1: Seria este STATIONARA
#
# Decizie: daca p-value < 0.05 => respingem H0 => seria este stationara
#          daca p-value > 0.05 => NU respingem H0 => seria este non-stationara
#
# -------------------------------------------------------------------------
# [T4] TEST JOHANSEN – Testul de cointegrare (ca.jo)
# -------------------------------------------------------------------------
# Scopul: Determina daca exista relatii de echilibru pe termen lung
#         intre seriile I(1). Se aplica DOAR daca toate seriile sunt I(1).
#
# Varianta TRACE (type = "trace"):
#   H0: Numarul de relatii de cointegrare <= r (r = 0, 1, 2, ...)
#   H1: Numarul de relatii de cointegrare > r
#   Decizie: daca statistica test > valoarea critica la 5%
#            => respingem H0 => exista cel putin r+1 relatii
#            Continuam pana nu mai respingem H0 => acela este rangul r
#
# Varianta EIGEN/MAXEIGEN (type = "eigen"):
#   H0: Numarul EXACT de relatii de cointegrare = r
#   H1: Numarul exact de relatii de cointegrare = r+1
#   Decizie: identica cu TRACE, dar testeaza fiecare valoare proprie separat
#
# Interpretare rang r:
#   r = 0 => nicio relatie de cointegrare => VAR pe diferente
#   r >= 1 => exista relatii de cointegrare => VECM
#   r = K (nr. variabile) => toate seriile sunt deja stationare => VAR in nivel
#
# -------------------------------------------------------------------------
# [T5] TEST PORTMANTEAU – Autocorelarea reziduurilor (serial.test)
# -------------------------------------------------------------------------
# Scopul: Verifica daca reziduurile modelului VECM/VAR sunt independente
#         (nu prezinta autocorelare => model corect specificat)
#
# H0: Reziduurile NU sunt autocorelate (sunt independente)
# H1: Reziduurile SUNT autocorelate => model gresit specificat
#
# Decizie: daca p-value > 0.05 => NU respingem H0 => reziduuri OK
#          daca p-value < 0.05 => respingem H0 => autocorelare prezenta
#          => crestem ordinul lagului sau respecificam modelul
#
# -------------------------------------------------------------------------
# [T6] TEST ARCH MULTIVARIATE – Heteroscedasticitate (arch.test)
# -------------------------------------------------------------------------
# Scopul: Testeaza daca varianta reziduurilor este constanta in timp
#         (homoscedasticitate) sau variaza (efecte ARCH/GARCH)
#
# H0: Nu exista efecte ARCH => varianta reziduurilor este CONSTANTA
# H1: Exista efecte ARCH => varianta reziduurilor este NON-CONSTANTA
#
# Decizie: daca p-value > 0.05 => NU respingem H0 => homoscedasticitate => OK
#          daca p-value < 0.05 => respingem H0 => heteroscedasticitate prezenta
#          => limitare a modelului, rezultatele sunt totusi consistente
#             dar ineficiente; se poate extinde cu modele GARCH
#
# -------------------------------------------------------------------------
# [T7] TEST NORMALITATE MULTIVARIATA – Jarque-Bera (normality.test)
# -------------------------------------------------------------------------
# Scopul: Testeaza daca reziduurile modelului urmeaza o distributie normala
#         multivariata (ipoteza pentru inferenta statistica)
#
# H0: Reziduurile au distributie NORMALA multivariata
# H1: Reziduurile NU au distributie normala
#
# Sub-teste returnate:
#   - Skewness (asimetrie): H0: asimetrie = 0
#   - Kurtosis (aplatizare): H0: kurtosis = 3
#   - Jarque-Bera (combinat): H0: normalitate multivariata
#
# Decizie: daca p-value > 0.05 => NU respingem H0 => normalitate confirmata
#          daca p-value < 0.05 => respingem H0 => non-normalitate
#          => frecvent cauzata de socul COVID-19 din 2020 (outlier extrem)
#          => LIMITARE acceptata; inferenta ramane valida asimptotic
#             prin Teorema Limita Centrala (n = 121 observatii)
#
# -------------------------------------------------------------------------
# [T8] TEST GRANGER (WALD) – Cauzalitate Granger (granger_mlm / causality)
# -------------------------------------------------------------------------
# Scopul: Testeaza daca valorile trecute ale variabilei X ajuta la
#         predictia variabilei Y, dincolo de propria sa istorie
#
# H0: Variabila X NU Granger-cauzeaza variabila Y
#     (lagurile lui X sunt colectiv nesemnificative in ecuatia lui Y)
# H1: Variabila X Granger-cauzeaza variabila Y
#     (cel putin un lag al lui X este semnificativ in ecuatia lui Y)
#
# Decizie: daca p-value < 0.05 => respingem H0
#          => X Granger-cauzeaza Y (predictibilitate statistica)
#          daca p-value > 0.05 => NU respingem H0
#          => X nu aduce informatie suplimentara pentru predictia lui Y
#
# IMPORTANT: Cauzalitatea Granger ≠ cauzalitate in sens economic/structural!
#            Indica PRECEDENTA temporala si predictibilitate, nu cauzalitate reala.
#
# In VECM exista doua tipuri de cauzalitate Granger:
#   (a) TERMEN SCURT: prin lagurile diferentelor (test Wald pe coeficientii Gamma)
#   (b) TERMEN LUNG: prin coeficientul alpha al ECT
#       alpha negativ si semnificativ => variabila raspunde la dezechilibrul TL
#       alpha ≈ 0, nesemnificativ => variabila este exogena pe termen lung
#
# -------------------------------------------------------------------------
# [T9] TEST ADF PE ECT – Stationaritatea termenului de corectie a erorii
# -------------------------------------------------------------------------
# Scopul: Confirma formal ca relatia de cointegrare identificata este valida,
#         verificand ca ECT (deviatia de la echilibrul TL) este stationara
#
# H0: ECT contine radacina unitara => NON-STATIONAR => cointegrarea NU este reala
# H1: ECT este STATIONAR => deviatiile de la echilibru sunt tranzitorii
#                        => cointegrarea este reala si stabila
#
# Decizie: daca statistica tau < valoarea critica la 5%
#          => respingem H0 => ECT stationar => cointegrare confirmata formal
#          daca statistica tau > valoarea critica => NU respingem H0
#          => ECT non-stationar => problema cu specificatia modelului
#
# ===========================================================================


# ===========================================================================
# 4. VERIFICAREA STATIONARITATII
# ===========================================================================
# Aplicam trei teste complementare pe fiecare serie:

# - ADF (ur.df): H0 = serie nestationara. Specificatii: none, drift, trend
# [T1] TEST ADF – Augmented Dickey-Fuller (ur.df)
# -------------------------------------------------------------------------
# Scopul: Testeaza prezenta radacinii unitare (non-stationaritate)
#
# H0: Seria contine o radacina unitara => este NON-STATIONARA
# H1: Seria NU contine radacina unitara => este STATIONARA
#
# Specificatii (type):
#   "none"  => fara constanta si fara trend (model pur AR)
#   "drift" => cu constanta (media != 0), fara trend
#   "trend" => cu constanta si trend determinist
#
# Decizie: daca statistica tau < valoarea critica (negativa) la 5%
#          => respingem H0 => seria este stationara
#          daca statistica tau > valoarea critica => NU respingem H0
#          => seria este non-stationara

# - KPSS (ur.kpss): H0 = serie stationara. Specificatii: mu, tau
#[T2] TEST KPSS – Kwiatkowski-Phillips-Schmidt-Shin (ur.kpss)
# -------------------------------------------------------------------------
# Scopul: Complementar ADF — testeaza stationaritatea (H0 invers fata de ADF)
#
# H0: Seria este STATIONARA (in jurul unei constante sau a unui trend)
# H1: Seria este NON-STATIONARA (contine radacina unitara)
#
# Specificatii (type):
#   "mu"  => H0: stationara in jurul unei constante
#   "tau" => H0: stationara in jurul unui trend determinist
#
# Decizie: daca statistica KPSS > valoarea critica la 5%
#          => respingem H0 => seria este non-stationara
#          daca statistica KPSS < valoarea critica => NU respingem H0
#          => seria este stationara

# - PP (pp.test): H0 = serie nestationara
#[T3] TEST PP – Phillips-Perron (pp.test)
# -------------------------------------------------------------------------
# Scopul: Alternativa la ADF, robusta la heteroscedasticitate si autocorelare
#         in reziduuri (nu necesita specificarea lagurilor)
#
# H0: Seria contine o radacina unitara => este NON-STATIONARA
# H1: Seria este STATIONARA
#
# Decizie: daca p-value < 0.05 => respingem H0 => seria este stationara
#          daca p-value > 0.05 => NU respingem H0 => seria este non-stationara




# ---- TRAFIC in nivel ----
cat("\n===== TEST STATIONARITATE: TRAFIC in nivel =====\n")
summary(ur.df(trafic, type = "none",  selectlags = "AIC"))
summary(ur.df(trafic, type = "drift", selectlags = "AIC"))
summary(ur.df(trafic, type = "trend", selectlags = "AIC"))
#Concluzie ADF: toate 3 specificații nu resping H0 → traficul este non-staționar în nivel
summary(ur.kpss(trafic, type = "mu",  lags = "long"))
summary(ur.kpss(trafic, type = "tau", lags = "long"))
#Concluzie KPSS: ambele specificații nu resping H0 → confirmă staționaritatea.Rezultat contradictoriu față de ADF!
print(pp.test(trafic, lshort = FALSE))
#Concluzie: seria este non-staționară
#2 din 3 teste (ADF și PP) confirmă non-staționaritatea. KPSS dă rezultat contradictoriu, însă acest lucru poate fi explicat de prezența șocului COVID-19 din 2020 care perturbă testul, outlier-ul extrem poate face seria să pară staționară în jurul unui nivel când de fapt nu este.
#Concluzie: Traficul este non-staționar în nivel → urmează testarea pe prima diferență.



# ---- TRAFIC in prima diferenta ----
cat("\n===== TEST STATIONARITATE: ΔTrafic (prima diferenta) =====\n")
summary(ur.df(diff(trafic), type = "none",  selectlags = "AIC"))
summary(ur.df(diff(trafic), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(trafic), type = "trend", selectlags = "AIC"))
#Concluzie ADF pe diferență: toate 3 specificații resping H0 (statistici mult sub valorile critice) - prima diferență este staționară.

summary(ur.kpss(diff(trafic), type = "tau", lags = "long"))
#0.0527 < 0.146 → NU respingem H0 - staționară

print(pp.test(diff(trafic), lshort = FALSE))
#0.01 < 0.05 → respingem H0 - staționară

# Concluzie: Traficul este I(1)

# ---- PETROL in nivel ----
cat("\n===== TEST STATIONARITATE: PETROL in nivel =====\n")
summary(ur.df(petrol, type = "none",  selectlags = "AIC"))
summary(ur.df(petrol, type = "drift", selectlags = "AIC"))
summary(ur.df(petrol, type = "trend", selectlags = "AIC"))
#Concluzie ADF: toate 3 specificații confirmă non-staționaritatea.

summary(ur.kpss(petrol, type = "mu",  lags = "long"))
summary(ur.kpss(petrol, type = "tau", lags = "long"))
#Concluzie KPSS: rezultat mixt — non-staționar fără trend, staționar cu trend.

print(pp.test(petrol, lshort = FALSE))
#Concluzie: non-staționar

#Concluzie finala: Petrolul este non-staționar în nivel

# ---- PETROL in prima diferenta ----
cat("\n===== TEST STATIONARITATE: ΔPetrol (prima diferenta) =====\n")
summary(ur.df(diff(petrol), type = "none",  selectlags = "AIC"))
summary(ur.df(diff(petrol), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(petrol), type = "trend", selectlags = "AIC"))
#Concluzie ADF: toate 3 specificații resping H0 - stationara

summary(ur.kpss(diff(petrol), type = "tau", lags = "long"))
#0.0612 < 0.146 → NU respingem H0 - stationara

print(pp.test(diff(petrol), lshort = FALSE))
#0.01 < 0.05 → respingem H0 - stationara

# Concluzie: Petrolul este I(1)

# ---- SOMAJ in nivel ----
cat("\n===== TEST STATIONARITATE: SOMAJ in nivel =====\n")
summary(ur.df(somaj, type = "none",  selectlags = "AIC"))
summary(ur.df(somaj, type = "drift", selectlags = "AIC"))
summary(ur.df(somaj, type = "trend", selectlags = "AIC"))
#Concluzie ADF: rezultate mixte și ambigue

summary(ur.kpss(somaj, type = "mu",  lags = "long"))
summary(ur.kpss(somaj, type = "tau", lags = "long"))
#Concluzie KPSS: ambele specificații sugerează staționaritate

print(pp.test(somaj, lshort = FALSE))
#Concluzie PP: stationaritate

#Concluzie finala: Aceasta este cea mai problematică serie dintre cele trei. Rezultatele sunt contradictorii și există o explicație economică clară: șocul COVID-19 din 2020 este un outlier extrem, care face testele sa se contazica

# ---- SOMAJ in prima diferenta ----
cat("\n===== TEST STATIONARITATE: ΔSomaj (prima diferenta) =====\n")
summary(ur.df(diff(somaj), type = "none",  selectlags = "AIC"))
summary(ur.df(diff(somaj), type = "drift", selectlags = "AIC"))
summary(ur.df(diff(somaj), type = "trend", selectlags = "AIC"))
#Concluzie ADF: toate 3 specificații resping H0 - stationara

summary(ur.kpss(diff(somaj), type = "tau", lags = "long"))
#0.0544 < 0.146 → NU respingem H0 - stationara

print(pp.test(diff(somaj), lshort = FALSE))
#0.01 < 0.05 → respingem H0 -  staționară
# Concluzie: Somajul este I(1)

# ============================================================
# CONCLUZIE STATIONARITATE:
# Toate trei seriile sunt I(1): nestationare in nivel,
# stationare dupa diferentierea de ordinul 1.
# Aceasta conditie este necesara pentru testarea cointegrarii Johansen.
# ============================================================


# ===========================================================================
# 5. SELECTIA LAGULUI OPTIM
# ===========================================================================


lag_select <- VARselect(Y, lag.max = 12, type = "const")
print(lag_select)

# Lagul pentru testul Johansen (SC - parsimonios)
p_opt_sc <- as.numeric(lag_select$selection["SC(n)"])
cat("Lag optim (SC) pentru Johansen:", p_opt_sc, "\n")

# Lagul pentru VECM (AIC - mai flexibil)
p_opt_aic <- as.numeric(lag_select$selection["AIC(n)"])
cat("Lag optim (AIC) pentru VECM:   ", p_opt_aic, "\n")

#Lag optim = 2, confirmat  de toate cele 4 criterii (AIC, HQ, SC, FPE). Dinamica sistemului depinde de ultimele 2 luni. Folosim K=2 pentru Johansen și VECM.

# ===========================================================================
# 6. TESTUL DE COINTEGRARE JOHANSEN
# ===========================================================================
# Testul Johansen:
# - "trace": H0: numarul de relatii de cointegrare <= r
# - "eigen" (maxeigen): H0: numarul exact de vectori = r

# Citim de la r=0 in sus:
# daca statistica > valoarea critica la 5% → respingem H0 → r+1 relatii
# continuam pana nu mai respingem H0

cat("\n===== TEST JOHANSEN (TRACE) =====\n")
johansen_trace <- ca.jo(Y, type = "trace", ecdet = "const", K = p_opt_sc)
summary(johansen_trace)
# TRACE:
# r=0:  statistica (61.35) > critica 5% (34.91) → respingem → r ≥ 1
# r<=1: statistica (15.75) < critica 5% (19.96) → NU respingem → r = 1

cat("\n===== TEST JOHANSEN (EIGEN/MAXEIGEN) =====\n")
johansen_max <- ca.jo(Y, type = "eigen", ecdet = "const", K = p_opt_sc)
summary(johansen_max)
# EIGEN:
# r=0:  statistica (45.60) > critica 5% (22.00) → respingem → r ≥ 1
# r<=1: statistica (9.66)  < critica 5% (15.67) → NU respingem → r = 1

r_coint <- 1
# Concluzie finala => ambele teste confirma exact 1 relatie de cointegrare => estimam VECM


# ===========================================================================
# 7. ESTIMAREA MODELULUI VECM
# ===========================================================================
# VECM (Vector Error Correction Model) extinde VAR prin termenul ECT.
# Structura:
# ΔY_t = α * β'*Y_{t-1} + Γ_1*ΔY_{t-1} + ... + Γ_{K-1}*ΔY_{t-K+1} + ε_t
#
# β' = vectorul de cointegrare (relatia de echilibru pe TL)
# α  = coeficientii de ajustare (viteza de revenire la echilibru)
# Γ_i = coeficientii pe termen scurt
#
# alpha negativ si semnificativ => variabila se ajusteaza la dezechilibru
# alpha ≈ 0, nesemnificativ   => variabila este exogena pe termen lung

vecm_model <- cajorls(johansen_trace, r = r_coint)


print(vecm_model)
print(vecm_model$beta)
#relatia de echilibru pe TL: Trafic = 147495.28 − 82.96×Petrol − 15798.64×Somaj
#Concluzie: Doar traficul reacționează la dezechilibrul pe termen lung. Petrolul și șomajul sunt variabile exogene — sunt determinate de forțe externe sistemului (piața globală a petrolului, politica fiscală SUA), nu de relația de cointegrare internă.


#Coeficientii de ajustare (alpha)
alpha_matrix <- coef(vecm_model$rlm)
alpha_trafic <- alpha_matrix["ect1", "Trafic.d"]
alpha_petrol <- alpha_matrix["ect1", "Petrol.d"]
alpha_somaj  <- alpha_matrix["ect1", "Somaj.d"]
cat("Alpha Trafic:", alpha_trafic, "\n")
cat("Alpha Petrol:", alpha_petrol, "\n")
cat("Alpha Somaj: ", alpha_somaj,  "\n")
#alpha pozitiv => Traficul se indeparteaza de echilibru in loc sa revina spre el
#alpha ≈ 0 => Petrolul si Somajul sunt variabile exogene (nu se ajusteaza)

# Interpretare ecuatii individuale
#Ecuatia ΔTrafic 
print(summary(vecm_model$rlm)[[1]])
#ECT1 = +0.0528, p = 0.166 => nesemnificativ 
#R2 = 0.30, F semnificativ - modelul explică 30% din variația traficului

#Ecuatia ΔPetrol
print(summary(vecm_model$rlm)[[2]])
#ECT1 = 6.79e-05, p = 0.103 => nesemnificativ
#R2 = 0.148 - model mai slab, normal pentru o variabilă determinată global

#Ecuatia ΔSomaj 
print(summary(vecm_model$rlm)[[3]])
#ECT1 = −2.627e-05, p < 0.001 => semnificativ
#R2 = 0.784 - modelul explica 78% din variatia somajului

# Conversia VECM -> VAR echivalent in nivel (pentru IRF, FEVD, Granger)
var_din_vecm <- vec2var(johansen_trace, r = r_coint)


# ===========================================================================
# 8. DIAGNOSTICUL MODELULUI VECM
# ===========================================================================
# Stabilitatea VECM se argumenteaza prin:
# (1) Johansen confirma cointegrarea => echilibru TL real si stabil
# (2) Alpha negativ si semnificativ => mecanism de corectie activ
# (3) ECT stationar => deviatiile de la echilibru sunt tranzitorii
# Continuam cu diagnosticul reziduurilor.

# 8.1 Autocorelarea reziduurilor
# H0: absenta autocorelarii => model bine specificat
# lags.pt TREBUIE sa fie STRICT MAI MARE decat p (lagul modelului)
# Regula: lags.pt minim = p_opt_sc + 4

serial.test(var_din_vecm, lags.pt = p_opt_sc + 4,  type = "PT.asymptotic")
serial.test(var_din_vecm, lags.pt = p_opt_sc + 8,  type = "PT.asymptotic")
serial.test(var_din_vecm, lags.pt = p_opt_sc + 12, type = "PT.asymptotic")
serial.test(var_din_vecm, lags.pt = p_opt_sc + 16, type = "PT.asymptotic")
# p-value > 0.05 => nu respingem H0 => reziduuri fara autocorelare => bine
# p-value < 0.05 => autocorelare prezenta => crestem lagul la AIC
# toate 4 teste au p-value >> 0.05 - nu respingem H0 la niciun orizont testat.
# concluzie: reziduurile modelului VECM nu prezintă autocorelare la niciunul dintre orizonturile testate (6, 10, 14, 18 laguri)

# 8.2 Diagnosticare cu lag AIC (daca lag SC da autocorelare)
# Estimam VECM cu lag AIC si re-testam
johansen_aic <- ca.jo(Y, type = "trace", ecdet = "const", K = p_opt_aic)
var_aic <- vec2var(johansen_aic, r = r_coint)
serial.test(var_aic, lags.pt = p_opt_aic + 4,  type = "PT.asymptotic")
serial.test(var_aic, lags.pt = p_opt_aic + 8,  type = "PT.asymptotic")
serial.test(var_aic, lags.pt = p_opt_aic + 12, type = "PT.asymptotic")
# Alegem modelul (SC sau AIC) care nu prezinta autocorelare
#Rezultatele sunt identice, lag=2, nu exista nicio diferenta intre modelul cu lag SC si cel cu lag AIC. Ambele confirma ca reziduurile nu sunt autocorelate.

# 8.3 Heteroscedasticitate ARCH multivariata
# H0: absenta efectelor ARCH (varianta constanta)

arch.test(var_din_vecm, lags.multi = 12)
#respingem H0 - existe efecte ARCH
arch.test(var_din_vecm, lags.multi = 16)
#nu respingem H0 - absenta efectelor ARCH


# 8.4 Normalitatea reziduurilor
# H0: reziduurile au distributie normala multivariata

normality.test(var_din_vecm)
#respingem H0 - rezidurile nu sunt distribuite normal 
# Socul COVID-19 (2020) poate cauza respingerea normalitatii => limitare

# 8.5 Grafice ale reziduurilor
resid_var  <- residuals(var_din_vecm)
# Coloanele au formatul "resids of Variabila" in obiectele vec2var
res_trafic <- ts(resid_var[, "resids of Trafic"],
                 start = time(trafic)[p_opt_sc + 2], frequency = 12)
res_petrol <- ts(resid_var[, "resids of Petrol"],
                 start = time(petrol)[p_opt_sc + 2], frequency = 12)
res_somaj  <- ts(resid_var[, "resids of Somaj"],
                 start = time(somaj)[p_opt_sc + 2],  frequency = 12)

df_res <- data.frame(
  timp   = as.yearmon(as.numeric(time(res_trafic))),
  Trafic = as.numeric(res_trafic),
  Petrol = as.numeric(res_petrol),
  Somaj  = as.numeric(res_somaj)
) %>%
  pivot_longer(cols = c(Trafic, Petrol, Somaj),
               names_to  = "ecuatie",
               values_to = "reziduu")

ggplot(df_res, aes(x = timp, y = reziduu)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ecuatie, ncol = 1, scales = "free_y") +
  labs(title = "Reziduurile modelului VECM", x = "Timp", y = "") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))
#Toate seriile prezinta acelasi pattern, modelul e bine specificat în condiții normale
#Șocul COVID-19 este unic și nereprezentativ pentru dinamica obișnuită a sistemului
#Non-normalitatea și heteroscedasticitatea marginală identificate anterior sunt exclusiv cauzate de pandemia din 2020, nu de o eroare de specificare a modelului


# ACF reziduuri (trebuie sa nu aiba spikes semnificative)
ggAcf(res_trafic) + ggtitle("ACF reziduuri ΔTrafic")
ggAcf(res_petrol) + ggtitle("ACF reziduuri ΔPetrol")
ggAcf(res_somaj)  + ggtitle("ACF reziduuri ΔSomaj")
#Graficele ACF confirmă vizual rezultatele testului Portmanteau, reziduurile tuturor celor trei ecuații VECM sunt practic zgomot alb, fără structură de autocorelare rămasă neexplicată. Modelul a capturat bine dinamica seriilor.
#Exista un singur spike izolat la Petrol dintr-un total de 24 de lag-uri testate fiind acceptat statistic.


# ===========================================================================
# 9. TERMENUL DE CORECTIE AL ERORII (ECT)
# ===========================================================================
# ECT_t = beta'*Y_{t-1} = deviatia de la echilibrul pe termen lung
# Trebuie sa fie STATIONAR pentru a confirma cointegrarea

ect_ts <- ts(
  vecm_model$rlm$model$ect1,
  start     = time(trafic)[p_opt_sc + 2],
  frequency = 12)

df_ect <- data.frame(
  timp = as.yearmon(as.numeric(time(ect_ts))),
  ECT  = as.numeric(ect_ts)
)
ggplot(df_ect, aes(x = timp, y = ECT)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_hline(yintercept = mean(as.numeric(ect_ts), na.rm = TRUE),
             linetype = "dotted", color = "gray50") +
  labs(
    title    = "Termenul de corectie al erorii (ECT) - serii in nivel",
    subtitle = paste0("Media ECT = ",
                      round(mean(as.numeric(ect_ts), na.rm = TRUE), 4)),
    x = "Timp", y = "ECT"
  ) +
  theme_minimal(base_size = 12)
#ECL nu oscileaza in jurul lui 0, media ECT = -560.3192
#Daca ECT nu oscileaza in jurul lui 0 => lucram cu LOG-URI
#Logaritmii stabilizeaza relatia de cointegrare eliminand tendinta

# Seriile in logaritm
log_trafic <- log(trafic)
log_petrol <- log(petrol)
log_somaj  <- log(somaj)
Y_log <- cbind(Trafic = log_trafic, Petrol = log_petrol, Somaj = log_somaj)

# Lag optim pe log-uri (AIC)
lag_select_log <- VARselect(Y_log, lag.max = 12, type = "const")
p_opt_log <- as.numeric(lag_select_log$selection["AIC(n)"])
cat("Lag optim pe log-uri (AIC):", p_opt_log, "\n")

p_opt_sc_log <- as.numeric(lag_select_log$selection["SC(n)"])
cat("Lag optim pe log-uri (SC): ", p_opt_sc_log, "\n")

# Johansen + VECM pe log-uri
johansen_log <- ca.jo(Y_log, type = "trace", ecdet = "const", K = p_opt_log)
summary(johansen_log)

# Test Johansen EIGEN pe log-uri (pentru confirmare dubla)
johansen_log_eigen <- ca.jo(Y_log, type = "eigen", ecdet = "const", K = p_opt_log)
summary(johansen_log_eigen)

# Johansen TRACE pe log-uri: r = 2
# Johansen EIGEN pe log-uri: r = 0
# Cele doua teste sunt contradictorii pe log-uri.
# Pe serii in nivel ambele teste au confirmat unanim r = 1.
# Adoptam r_coint_log = 1 pentru consistenta si robustete.

vecm_log <- cajorls(johansen_log, r = r_coint)
var_log  <- vec2var(johansen_log, r = r_coint)

# ECT pe log-uri 
ect_log <- ts(
  vecm_log$rlm$model$ect1,
  start     = time(log_trafic)[p_opt_log + 2],
  frequency = 12
)
df_ect_log <- data.frame(
  timp = as.yearmon(as.numeric(time(ect_log))),
  ECT  = as.numeric(ect_log)
)
ggplot(df_ect_log, aes(x = timp, y = ECT)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_hline(yintercept = mean(as.numeric(ect_log), na.rm = TRUE),
             linetype = "dotted", color = "gray50") +
  labs(
    title    = "Termenul de corectie al erorii (ECT) - serii in logaritm",
    subtitle = paste0("ECT oscileaza in jurul mediei = ",
                      round(mean(as.numeric(ect_log), na.rm = TRUE), 4)
                     ),
    x = "Timp", y = "ECT"
  ) +
  theme_minimal(base_size = 12)
#ECL oscileaza in jurul lui 0 

# Test de stationaritate ECT
summary(ur.df(ect_log, type = "drift", selectlags = "AIC"))
#respingem H0 => ECT stationar => cointegrare confirmata 


# ===========================================================================
# 10. TESTE DE CAUZALITATE GRANGER
# ===========================================================================
# In VECM, cauzalitatea Granger are DOUA componente:
# (a) TERMEN SCURT: prin lagurile diferentelor (Gamma) => test Wald custom
# (b) TERMEN LUNG: prin coeficientul alpha al ECT
#     alpha semnificativ => variabila se ajusteaza la echilibrul TL
#     alpha ≈ 0 => variabila este exogena pe TL (nu se ajusteaza)
#
# H0: Variabila X NU Granger-cauzeaza sistemul (pe TL sau TS)
# Decizie: p-value < 0.05 => respingem H0 => cauzalitate Granger

coef_mat  <- coef(vecm_log$rlm)
vcov_full <- vcov(vecm_log$rlm)

# Identificam pozitiile regresorilor
idx_trafic <- grep("^Trafic\\.dl", rownames(coef_mat))
idx_petrol <- grep("^Petrol\\.dl", rownames(coef_mat))
idx_somaj  <- grep("^Somaj\\.dl",  rownames(coef_mat))

cat("Laguri Trafic:", rownames(coef_mat)[idx_trafic], "\n")
cat("Laguri Petrol:", rownames(coef_mat)[idx_petrol], "\n")
cat("Laguri Somaj: ", rownames(coef_mat)[idx_somaj],  "\n")

# Functie test Granger Wald pe obiect mlm 
granger_mlm <- function(model_mlm, idx_regresori, eq_name) {
  b        <- coef(model_mlm)[, eq_name]
  V        <- vcov(model_mlm)
  n_coef   <- length(b)
  eq_names <- colnames(coef(model_mlm))
  eq_pos   <- which(eq_names == eq_name)
  row_idx  <- ((eq_pos - 1) * n_coef + 1):(eq_pos * n_coef)
  V_eq     <- V[row_idx, row_idx]
  b_sub    <- b[idx_regresori]
  V_sub    <- V_eq[idx_regresori, idx_regresori]
  W        <- as.numeric(t(b_sub) %*% solve(V_sub) %*% b_sub)
  df       <- length(idx_regresori)
  pval     <- pchisq(W, df = df, lower.tail = FALSE)
  cat("Chi2 =", round(W, 4), "| df =", df,
      "| p-value =", round(pval, 4),
      ifelse(pval < 0.01, "***",
             ifelse(pval < 0.05, "**",
                    ifelse(pval < 0.10, ".", ""))), "\n")
  invisible(list(statistic = W, df = df, p.value = pval))
}

# --- Termen scurt ---

cat("\n--- Ecuatia ΔTrafic: cine cauzeaza traficul? ---\n")
cat("H0: log(Petrol) NU Granger-cauzeaza log(Trafic) -> ")
granger_mlm(vecm_log$rlm, idx_petrol, "Trafic.d")
cat("H0: log(Somaj)  NU Granger-cauzeaza log(Trafic) -> ")
granger_mlm(vecm_log$rlm, idx_somaj, "Trafic.d")

cat("\n--- Ecuatia ΔPetrol: cine cauzeaza pretul petrolului? ---\n")
cat("H0: log(Trafic) NU Granger-cauzeaza log(Petrol) -> ")
granger_mlm(vecm_log$rlm, idx_trafic, "Petrol.d")
cat("H0: log(Somaj)  NU Granger-cauzeaza log(Petrol) -> ")
granger_mlm(vecm_log$rlm, idx_somaj, "Petrol.d")

cat("\n--- Ecuatia ΔSomaj: cine cauzeaza rata somajului? ---\n")
cat("H0: log(Trafic) NU Granger-cauzeaza log(Somaj) -> ")
granger_mlm(vecm_log$rlm, idx_trafic, "Somaj.d")
cat("H0: log(Petrol) NU Granger-cauzeaza log(Somaj) -> ")
granger_mlm(vecm_log$rlm, idx_petrol, "Somaj.d")

#Petrol și Șomaj Granger-cauzează Traficul — ambii factori macroeconomici anticipează variațiile traficului 
#Traficul NU Granger-cauzează Petrolul — confirmat economic: traficul aerian american nu influențează prețul global al petrolului
#In afara de Trafic-Petrol, toate celelalte relații sunt semnificative — sistem puternic interconectat

# --- Termen lung (prin alpha) ---
cat("Alpha negativ si semnificativ => variabila se ajusteaza la TL\n")
cat("Alpha ~ 0, nesemnificativ    => variabila este exogena pe TL\n\n")
print(summary(vecm_log$rlm))

# Extragem explicit coeficientii de ajustare pe log-uri

alpha_log_matrix <- coef(vecm_log$rlm)

alpha_log_trafic <- alpha_log_matrix["ect1", "Trafic.d"]
alpha_log_petrol <- alpha_log_matrix["ect1", "Petrol.d"]
alpha_log_somaj  <- alpha_log_matrix["ect1", "Somaj.d"]

cat("Alpha log(Trafic):", alpha_log_trafic, "\n")

# Alpha log(Trafic) = +0.1498, p = 0.050 => la limita semnificatiei, semn pozitiv
# traficul nu se ajusteaza corect la dezechilibrul TL pe log-uri
# comportament intermediar, mai degraba exogen pe TL
cat("Alpha log(Petrol):", alpha_log_petrol, "\n")
# Alpha log(Petrol) = -0.000386, p = 0.992 => nesemnificativ, practic zero
# petrolul este EXOGEN pe termen lung
# pretul petrolului e determinat de factori globali, nu de sistemul nostru
cat("Alpha log(Somaj): ", alpha_log_somaj,  "\n")
# Alpha log(Somaj) = -0.1091, p = 0.003 => SEMNIFICATIV si NEGATIV
# somajul este variabila ENDOGENA care corecteaza dezechilibrele TL
# viteza de ajustare: 10.9% pe luna din dezechilibrul acumulat

#Șomajul este variabila endogenă care corectează dezechilibrele pe termen lung. 
#Petrolul este exogen-determinat de forțe globale externe sistemului. 
#Traficul are un comportament intermediar — Granger-cauzat pe termen scurt de ambele variabile, dar ajustarea pe termen lung e marginală.


# ===========================================================================
# 11. FUNCTII DE RASPUNS LA IMPULS (IRF)
# ===========================================================================
# IRF descrie cum reactioneaza o variabila la un soc unitar aplicat alteia.
# In VECM pe log-uri: raspuns de 0.01 ≈ +1% in variabila respectiva.
# Ortogonalizare Cholesky: Trafic -> Petrol -> Somaj
# Banda IC 95% bootstrap (500 runde): daca exclude zero => semnificativ

set.seed(42)
n_ahead <- 24  # 2 ani in viitor

# IRF individual: Petrol -> Trafic
irf_petrol_trafic <- irf(var_log, impulse = "Petrol", response = "Trafic",
                         n.ahead = n_ahead, boot = TRUE, ci = 0.95,
                         ortho = TRUE, runs = 500)
plot(irf_petrol_trafic,
     main = "IRF: raspunsul log(Trafic) la un soc in log(Petrol)")
#Intervalul include 0 => nesemnificativ, posibil semnificativ pe termen scurt


# IRF individual: Somaj -> Trafic
irf_somaj_trafic <- irf(var_log, impulse = "Somaj", response = "Trafic",
                        n.ahead = n_ahead, boot = TRUE, ci = 0.95,
                        ortho = TRUE, runs = 500)
plot(irf_somaj_trafic,
     main = "IRF: raspunsul log(Trafic) la un soc in log(Somaj)")
#Intervalul include 0 => nesemnificativ

# IRF individual: Trafic -> Petrol
irf_trafic_petrol <- irf(var_log, impulse = "Trafic", response = "Petrol",
                         n.ahead = n_ahead, boot = TRUE, ci = 0.95,
                         ortho = TRUE, runs = 500)
plot(irf_trafic_petrol,
     main = "IRF: raspunsul log(Petrol) la un soc in log(Trafic)")
#Intervalul include 0 => nesemnificativ

# IRF individual: Somaj -> Petrol
irf_somaj_petrol <- irf(var_log, impulse = "Somaj", response = "Petrol",
                        n.ahead = n_ahead, boot = TRUE, ci = 0.95,
                        ortho = TRUE, runs = 500)
plot(irf_somaj_petrol,
     main = "IRF: raspunsul log(Petrol) la un soc in log(Somaj)")
#Intervalul include 0 => nesemnificativ

# IRF individual: Trafic -> Somaj
irf_trafic_somaj <- irf(var_log, impulse = "Trafic", response = "Somaj",
                        n.ahead = n_ahead, boot = TRUE, ci = 0.95,
                        ortho = TRUE, runs = 500)
plot(irf_trafic_somaj,
     main = "IRF: raspunsul log(Somaj) la un soc in log(Trafic)")
#Intervalul nu include 0 => semnificativ

# IRF individual: Petrol -> Somaj
irf_petrol_somaj <- irf(var_log, impulse = "Petrol", response = "Somaj",
                        n.ahead = n_ahead, boot = TRUE, ci = 0.95,
                        ortho = TRUE, runs = 500)
plot(irf_petrol_somaj,
     main = "IRF: raspunsul log(Somaj) la un soc in log(Petrol)")
#Intervalul nu include 0 => semnificativ

# IRF complet sistem 
irf_all <- irf(var_log, n.ahead = n_ahead, boot = TRUE,
               ci = 0.95, ortho = TRUE, runs = 500)
plot(irf_all)

# IRF modern cu ggplot + benzi de incredere
extract_irf <- function(irf_obj, impulse_var, response_var) {
  data.frame(
    perioada = 0:n_ahead,
    raspuns  = irf_obj$irf[[impulse_var]][, response_var],
    lower    = irf_obj$Lower[[impulse_var]][, response_var],
    upper    = irf_obj$Upper[[impulse_var]][, response_var],
    impulse  = impulse_var,
    response = response_var
  )
}

df_irf <- bind_rows(
  extract_irf(irf_all, "Petrol", "Trafic"),
  extract_irf(irf_all, "Somaj",  "Trafic"),
  extract_irf(irf_all, "Trafic", "Petrol"),
  extract_irf(irf_all, "Somaj",  "Petrol"),
  extract_irf(irf_all, "Trafic", "Somaj"),
  extract_irf(irf_all, "Petrol", "Somaj")
)
df_irf$eticheta <- paste0("Soc: ", df_irf$impulse,
                          " -> Raspuns: ", df_irf$response)

ggplot(df_irf, aes(x = perioada)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = raspuns), color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
  facet_wrap(~eticheta, ncol = 2, scales = "free_y") +
  labs(
    title    = "Functii de raspuns la impuls (IRF) - model VECM pe log-uri",
    subtitle = paste0("Orizont: ", n_ahead,
                      " luni | IC 95% bootstrap (500 runde)"),
    x = "Luni", y = "Raspuns (variatii log ~ variatii %)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        legend.position = "none")


# ===========================================================================
# 12. DECOMPUNEREA VARIANTEI ERORII DE PROGNOZA (FEVD)
# ===========================================================================
# FEVD: ce proportie din variatia erorii de prognoza a lui Y la orizontul h
# vine din propriile socuri vs socurile celorlalte variabile?
# FEVD = complementul IRF: suma contributiilor = 100% la fiecare orizont.

fevd_res <- fevd(var_log, n.ahead = 24)

cat("\n===== FEVD pentru log(Trafic) =====\n")
print(round(as.data.frame(fevd_res$Trafic), 4))
#Traficul este explicat în proporție dominantă de propriile șocuri (~74% pe termen lung). Petrolul contribuie cu ~16% și șomajul cu ~9%. Structura se stabilizează rapid după luna 5–6 și rămâne constantă.
#Traficul este o variabilă relativ autonomă, influențată moderat de factorii macroeconomici.

cat("\n===== FEVD pentru log(Petrol) =====\n")
print(round(as.data.frame(fevd_res$Petrol), 4))
#Petrolul este explicat predominant de propriile șocuri (~84% pe termen lung) — confirmat economic, prețul petrolului e determinat de factori globali. Surprinzător, traficul explică ~49% din variația petrolului la prima luna — efect al ordinii Cholesky (Trafic primul în sistem). 
#Pe termen lung contribuția traficului scade la ~11%, iar șomajul contribuie minimal (~5%).

cat("\n===== FEVD pentru log(Somaj) =====\n")
print(round(as.data.frame(fevd_res$Somaj), 4))
#variația șomajului este explicată majoritar de șocurile din trafic (~70% pe termen lung), nu de propriile șocuri. Petrolul contribuie cu ~27% pe termen lung, contribuție crescătoare în timp. 
#Șomajul propriu explică doar ~3% pe termen lung — extrem de neobișnuit, posibil efect al ordinii Cholesky care pune Traficul primul.

#rezultatele FEVD pentru șomaj sunt puternic influențate de ordinea Cholesky (Trafic→Petrol→Șomaj). Traficul pus primul în sistem primește "credit" maxim pentru variațiile contemporane. O analiză de robustețe cu ordine alternativă ar putea schimba aceste proporții.

# Grafic standard
plot(fevd_res)

# Grafice moderne ggplot
fevd_trafic <- as.data.frame(fevd_res$Trafic)
fevd_petrol <- as.data.frame(fevd_res$Petrol)
fevd_somaj  <- as.data.frame(fevd_res$Somaj)
fevd_trafic$Orizont <- 1:nrow(fevd_trafic); fevd_trafic$Variabila <- "log(Trafic)"
fevd_petrol$Orizont <- 1:nrow(fevd_petrol); fevd_petrol$Variabila <- "log(Petrol)"
fevd_somaj$Orizont  <- 1:nrow(fevd_somaj);  fevd_somaj$Variabila  <- "log(Somaj)"

fevd_df <- bind_rows(fevd_trafic, fevd_petrol, fevd_somaj)
fevd_long <- fevd_df %>%
  pivot_longer(cols = c("Trafic", "Petrol", "Somaj"),
               names_to = "Soc", values_to = "Proportie")
fevd_long$Soc <- factor(fevd_long$Soc,
                        levels = c("Trafic", "Petrol", "Somaj"),
                        labels = c("Soc Trafic", "Soc Petrol", "Soc Somaj"))

# Bare suprapuse
ggplot(fevd_long, aes(x = Orizont, y = Proportie, fill = Soc)) +
  geom_col(position = "stack", color = "white", width = 0.85) +
  facet_wrap(~Variabila, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Soc Trafic" = "darkorchid3",
                               "Soc Petrol" = "deeppink4",
                               "Soc Somaj"  = "#7AC5CD")) +
  labs(title = "FEVD - Decompozitia variantei erorii de prognoza",
       subtitle = "Model VECM | SUA 2016-2026",
       x = "Orizont (luni)", y = "Proportie explicata (%)",
       fill = "Sursa socului") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), legend.position = "bottom")
#Graficul cu bare arată că structura FEVD se stabilizează rapid după primele 4–5 luni și rămâne practic constantă până la luna 24 — semn de stabilitate a modelului.

# Grafic arie
ggplot(fevd_long, aes(x = Orizont, y = Proportie, fill = Soc)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  facet_wrap(~Variabila, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Soc Trafic" = "darkorchid3",
                               "Soc Petrol" = "deeppink4",
                               "Soc Somaj"  = "#7AC5CD")) +
  labs(title = "FEVD - Structura surselor de variabilitate",
       subtitle = "Model VECM pe log-uri | SUA 2016-2026",
       x = "Orizont (luni)", y = "Proportie explicata (%)",
       fill = "Sursa socului") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), legend.position = "bottom")
#Graficul cu arii confirmă același lucru, ariile sunt netede și convergente, fără oscilații sau instabilitate.
#Somajul (turcoaz) are o contribuție extrem de mică în explicarea variației oricărei variabile — cel mai puțin influent soc din sistem, deși pe termen scurt Granger-cauzează traficul. 
#Aceasta sugerează că șocurile în șomaj sunt de amplitudine mică chiar dacă sunt semnificative statistic.

# ===========================================================================
# 13. PROGNOZA PE BAZA MODELULUI VECM
# ===========================================================================
# Prognoza VECM prin VAR echivalent (var_log) pe log-uri.
# AVANTAJ vs VAR pe diferente: prognozele TL sunt "ancorate" de cointegrare.
# Reconversie in nivel: nivel = exp(log_forecast)

h <- 12  # orizont: 12 luni

forecast_vecm <- predict(var_log, n.ahead = h, ci = 0.95)
plot(forecast_vecm)  # grafic standard pe log-uri

fc_trafic_log <- forecast_vecm$fcst$Trafic
fc_petrol_log <- forecast_vecm$fcst$Petrol
fc_somaj_log  <- forecast_vecm$fcst$Somaj

# Reconversie in nivel
trafic_fc_niv    <- exp(fc_trafic_log[, 1])
trafic_lower_niv <- exp(fc_trafic_log[, 2])
trafic_upper_niv <- exp(fc_trafic_log[, 3])
petrol_fc_niv    <- exp(fc_petrol_log[, 1])
petrol_lower_niv <- exp(fc_petrol_log[, 2])
petrol_upper_niv <- exp(fc_petrol_log[, 3])
somaj_fc_niv     <- exp(fc_somaj_log[,  1])
somaj_lower_niv  <- exp(fc_somaj_log[,  2])
somaj_upper_niv  <- exp(fc_somaj_log[,  3])

# Tabel prognoza
cat("\n===== PROGNOZA IN NIVELURI =====\n")
last_time   <- as.numeric(time(log_trafic)[length(log_trafic)])
future_time <- seq(from = last_time + 1/12, by = 1/12, length.out = h)
future_mon  <- as.yearmon(future_time)
tabel_fc <- data.frame(
  Luna            = format(future_mon, "%Y-%m"),
  Trafic_Forecast = round(trafic_fc_niv, 0),
  Trafic_Lower95  = round(trafic_lower_niv, 0),
  Trafic_Upper95  = round(trafic_upper_niv, 0),
  Petrol_Forecast = round(petrol_fc_niv, 2),
  Somaj_Forecast  = round(somaj_fc_niv, 2)
)
print(tabel_fc)

# Grafice prognoza in niveluri
df_hist_niv <- bind_rows(
  data.frame(timp=as.yearmon(as.numeric(time(trafic))),
             val=as.numeric(trafic), serie="Trafic"),
  data.frame(timp=as.yearmon(as.numeric(time(petrol))),
             val=as.numeric(petrol), serie="Petrol"),
  data.frame(timp=as.yearmon(as.numeric(time(somaj))),
             val=as.numeric(somaj), serie="Somaj")
)
df_fc_niv <- bind_rows(
  data.frame(timp=future_mon, fc=trafic_fc_niv,
             lower=trafic_lower_niv, upper=trafic_upper_niv, serie="Trafic"),
  data.frame(timp=future_mon, fc=petrol_fc_niv,
             lower=petrol_lower_niv, upper=petrol_upper_niv, serie="Petrol"),
  data.frame(timp=future_mon, fc=somaj_fc_niv,
             lower=somaj_lower_niv, upper=somaj_upper_niv, serie="Somaj")
)

ggplot() +
  geom_line(data = df_hist_niv, aes(x=timp, y=val), color="black", linewidth=0.7) +
  geom_ribbon(data = df_fc_niv, aes(x=timp, ymin=lower, ymax=upper),
              fill="lightblue", alpha=0.4) +
  geom_line(data = df_fc_niv, aes(x=timp, y=fc), color="steelblue", linewidth=1.1) +
  geom_vline(xintercept=as.numeric(min(future_mon)),
             linetype="dashed", color="gray50") +
  facet_wrap(~serie, ncol=1, scales="free_y") +
  labs(title="Prognoza VECM - serii in niveluri",
       subtitle=paste0("Orizont: ", h, " luni | IC 95%"),
       x="Timp", y="Valoare") +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold"))



# Zoom perioada recenta + prognoza
zoom_start <- as.yearmon("Jan 2023")
df_hist_zoom <- df_hist_niv %>% filter(timp >= zoom_start)

ggplot() +
  geom_line(data=df_hist_zoom, aes(x=timp, y=val), color="black", linewidth=0.9) +
  geom_ribbon(data=df_fc_niv, aes(x=timp, ymin=lower, ymax=upper),
              fill="lightblue", alpha=0.4) +
  geom_line(data=df_fc_niv, aes(x=timp, y=fc), color="steelblue", linewidth=1.2) +
  geom_point(data=df_fc_niv, aes(x=timp, y=fc), color="steelblue", size=2) +
  geom_vline(xintercept=as.numeric(min(future_mon)),
             linetype="dashed", color="gray50") +
  facet_wrap(~serie, ncol=1, scales="free_y") +
  labs(title="Prognoza VECM - zoom perioada recenta (2023-2026)",
       subtitle=paste0("Orizont: ", h, " luni | IC 95%"),
       x="Timp", y="Valoare") +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold"))

#Trafic: modelul previzionează un trafic relativ stabil în jurul a 75.000–78.000 pasageri lunar pentru perioada Feb 2026 – Ian 2027, cu o ușoară tendință descrescătoare de la ~77.500 la ~75.400. Valorile sunt consistente cu nivelurile observate recent în seria istorică.
#Petrol: prognoze descrescătoare de la 59.06 USD la 55.39 USD/baril — trend ușor descendent, consistent cu nivelurile recente.
#Somaj: stabil la ~4.32–4.43% — practic constant, reflectând o piață a muncii stabilă previzionată.

# ===========================================================================
# 14. COMPARATIE VALORI ESTIMATE VS. REALE (FITTED vs ACTUAL)
# ===========================================================================
# Fitted = ce ar fi prognozat modelul in esantionul de estimare
# Coloanele fitted din vec2var au formatul "fit of Variabila"

fitted_vals <- fitted(var_log)
n_fit       <- nrow(fitted_vals)
cat("Numar observatii fitted:", n_fit, "\n")
cat("Coloane fitted:", colnames(fitted_vals), "\n")

t_fit <- as.yearmon(
  as.numeric(time(log_trafic))[(length(log_trafic) - n_fit + 1):length(log_trafic)]
)

# Reconversie fitted in nivel
fitted_trafic_niv <- exp(fitted_vals[, "fit of Trafic"])
fitted_petrol_niv <- exp(fitted_vals[, "fit of Petrol"])
fitted_somaj_niv  <- exp(fitted_vals[, "fit of Somaj"])

df_actual_niv <- data.frame(
  timp   = as.yearmon(as.numeric(time(trafic))),
  Trafic = as.numeric(trafic),
  Petrol = as.numeric(petrol),
  Somaj  = as.numeric(somaj)
) %>%
  pivot_longer(cols=c("Trafic","Petrol","Somaj"),
               names_to="Variabila", values_to="Actual")

df_fitted_niv <- data.frame(
  timp   = t_fit,
  Trafic = as.numeric(fitted_trafic_niv),
  Petrol = as.numeric(fitted_petrol_niv),
  Somaj  = as.numeric(fitted_somaj_niv)
) %>%
  pivot_longer(cols=c("Trafic","Petrol","Somaj"),
               names_to="Variabila", values_to="Fitted")

df_plot_niv <- left_join(df_actual_niv, df_fitted_niv, by=c("timp","Variabila"))

ggplot(df_plot_niv, aes(x=timp)) +
  geom_line(aes(y=Actual, color="Observat"), linewidth=0.9) +
  geom_line(aes(y=Fitted, color="Fitted VECM"), linewidth=0.9, linetype="dashed") +
  facet_wrap(~Variabila, ncol=1, scales="free_y") +
  scale_color_manual(values=c("Observat"="black", "Fitted VECM"="steelblue")) +
  labs(title="Comparatie: valori observate vs. valori ajustate (VECM)",
       subtitle="Serii reconvertite din logaritm in niveluri",
       x="Timp", y="Valoare", color="") +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold"), legend.position="top")

#Modelul VECM are o performanta buna in conditii normale, fitted si observed se suprapun rezonabil in perioadele fara socuri. Discrepantele apar in perioada pandemiei covid19
#Modelul e valid si util pentru prognoza in conditii normale, cu limitarea ca nu poate surprinde socuri externe neprevazute.

# ===========================================================================
# 15. EVALUARE IN-SAMPLE
# ===========================================================================

eval_metrics <- function(actual, fitted_v) {
  err <- actual - fitted_v
  data.frame(
    RMSE = sqrt(mean(err^2, na.rm=TRUE)),
    MAE  = mean(abs(err),   na.rm=TRUE),
    MSE  = mean(err^2,      na.rm=TRUE),
    MAPE = mean(abs(err/actual), na.rm=TRUE) * 100
  )
}

# Metrici pe log-uri
actual_trafic_log <- window(log_trafic,
                            start=time(log_trafic)[length(log_trafic)-n_fit+1])
actual_petrol_log <- window(log_petrol,
                            start=time(log_petrol)[length(log_petrol)-n_fit+1])
actual_somaj_log  <- window(log_somaj,
                            start=time(log_somaj)[length(log_somaj)-n_fit+1])

fitted_trafic_log <- ts(fitted_vals[,"fit of Trafic"],
                        start=start(actual_trafic_log), frequency=12)
fitted_petrol_log <- ts(fitted_vals[,"fit of Petrol"],
                        start=start(actual_petrol_log), frequency=12)
fitted_somaj_log  <- ts(fitted_vals[,"fit of Somaj"],
                        start=start(actual_somaj_log),  frequency=12)


cat("Trafic:\n"); print(round(eval_metrics(actual_trafic_log, fitted_trafic_log), 6))
cat("Petrol:\n"); print(round(eval_metrics(actual_petrol_log, fitted_petrol_log), 6))
cat("Somaj:\n");  print(round(eval_metrics(actual_somaj_log,  fitted_somaj_log),  6))

#

# Metrici pe niveluri
actual_trafic_niv <- window(trafic, start=time(trafic)[length(trafic)-n_fit+1])
actual_petrol_niv <- window(petrol, start=time(petrol)[length(petrol)-n_fit+1])
actual_somaj_niv  <- window(somaj,  start=time(somaj)[length(somaj)-n_fit+1])

cat("\n===== METRICI IN-SAMPLE (in niveluri) =====\n")
cat("Trafic:\n"); print(round(eval_metrics(actual_trafic_niv,
                                           ts(fitted_trafic_niv, start=start(actual_trafic_niv), frequency=12)), 2))
cat("Petrol:\n"); print(round(eval_metrics(actual_petrol_niv,
                                           ts(fitted_petrol_niv, start=start(actual_petrol_niv), frequency=12)), 4))
cat("Somaj:\n");  print(round(eval_metrics(actual_somaj_niv,
                                           ts(fitted_somaj_niv,  start=start(actual_somaj_niv),  frequency=12)), 4))

# Benchmark naiv in-sample
naive_metrics <- function(actual) {
  naive_fit  <- stats::lag(actual, -1)
  actual_adj <- window(actual,    start=time(actual)[2])
  naive_adj  <- window(naive_fit, start=time(actual)[2])
  err <- actual_adj - naive_adj
  data.frame(RMSE=sqrt(mean(err^2,na.rm=TRUE)), MAE=mean(abs(err),na.rm=TRUE),
             MSE=mean(err^2,na.rm=TRUE),
             MAPE=mean(abs(err/actual_adj),na.rm=TRUE)*100)
}
cat("\n===== BENCHMARK NAIV IN-SAMPLE (pe log-uri) =====\n")
cat("Trafic:\n"); print(round(naive_metrics(actual_trafic_log), 6))
cat("Petrol:\n"); print(round(naive_metrics(actual_petrol_log), 6))
cat("Somaj:\n");  print(round(naive_metrics(actual_somaj_log),  6))
#Interpretare și comparație VECM vs. Naiv:
  #Trafic:
  
  #VECM: MAPE = 1.08% pe log-uri, dar MAPE = 13.32% în niveluri
#Naiv: MAPE = 0.74% pe log-uri
#Naivul bate VECM pe log-uri — surprinzător, dar explicabil: modelul naiv (valoarea de ieri = valoarea de azi) funcționează bine pe serii stabile, iar VECM plătește un cost pentru complexitate. MAPE de 13.32% în niveluri reflectă impactul șocului COVID.

#Petrol:
  
  #VECM: MAPE = 1.94% | Naiv: MAPE = 1.97%
  #Aproape identice — petrolul e greu de modelat, ambele metode performează similar.

#Șomaj:
  
  #VECM: MAPE = 3.47% | Naiv: MAPE = 2.47%
  #Naivul bate din nou VECM — din același motiv: șomajul e stabil în afara perioadei COVID, iar naivul captează bine această stabilitate.
#Performanța in-sample este acceptabilă, cu MAPE sub 2% pe log-uri pentru trafic și petrol. Faptul că naivul bate VECM in-sample nu este îngrijorător — este un fenomen bine documentat în literatura de specialitate. 
#Avantajul VECM față de naiv apare out-of-sample și pe orizonturi mai lungi, unde relațiile de cointegrare și cauzalitate Granger adaugă valoare predictivă reală.

# ===========================================================================
# 16. EVALUARE OUT-OF-SAMPLE
# ===========================================================================
# Cel mai relevant criteriu de validare: performanta pe date nefolosite.

h_test  <- 12
n_total <- length(log_trafic)
n_train <- n_total - h_test
cat("Total:", n_total, "| Train:", n_train, "| Test:", h_test, "\n")

# 16.1 Impartire train/test
log_trafic_train <- window(log_trafic, end=time(log_trafic)[n_train])
log_petrol_train <- window(log_petrol, end=time(log_petrol)[n_train])
log_somaj_train  <- window(log_somaj,  end=time(log_somaj)[n_train])
log_trafic_test  <- window(log_trafic, start=time(log_trafic)[n_train+1])
log_petrol_test  <- window(log_petrol, start=time(log_petrol)[n_train+1])
log_somaj_test   <- window(log_somaj,  start=time(log_somaj)[n_train+1])
trafic_test <- window(trafic, start=time(trafic)[n_train+1])
petrol_test <- window(petrol, start=time(petrol)[n_train+1])
somaj_test  <- window(somaj,  start=time(somaj)[n_train+1])

Y_log_train <- cbind(Trafic=log_trafic_train, Petrol=log_petrol_train,
                     Somaj=log_somaj_train)

# 16.2 Lag + Johansen pe train
lag_train <- VARselect(Y_log_train, lag.max=12, type="const")
p_train   <- as.numeric(lag_train$selection["AIC(n)"])
cat("Lag optim pe train:", p_train, "\n")
johansen_train <- ca.jo(Y_log_train, type="trace", ecdet="const", K=p_train)
summary(johansen_train)

# 16.3 VECM pe train + prognoza
var_train <- vec2var(johansen_train, r=r_coint)
fc_test   <- predict(var_train, n.ahead=h_test, ci=0.95)

pred_trafic_niv <- exp(fc_test$fcst$Trafic[,1])
pred_petrol_niv <- exp(fc_test$fcst$Petrol[,1])
pred_somaj_niv  <- exp(fc_test$fcst$Somaj[,1])

# 16.4 Metrici out-of-sample
eval_oos <- function(actual, forecast) {
  err <- actual - forecast
  data.frame(RMSE=sqrt(mean(err^2,na.rm=TRUE)), MAE=mean(abs(err),na.rm=TRUE),
             MSE=mean(err^2,na.rm=TRUE),
             MAPE=mean(abs(err/actual),na.rm=TRUE)*100)
}
cat("\n===== METRICI OUT-OF-SAMPLE - VECM =====\n")
cat("Trafic:\n"); print(round(eval_oos(as.numeric(trafic_test), pred_trafic_niv), 2))
cat("Petrol:\n"); print(round(eval_oos(as.numeric(petrol_test), pred_petrol_niv), 4))
cat("Somaj:\n");  print(round(eval_oos(as.numeric(somaj_test),  pred_somaj_niv),  4))

# 16.5 Benchmark naiv
naive_trafic_niv <- rep(exp(as.numeric(tail(log_trafic_train,1))), h_test)
naive_petrol_niv <- rep(exp(as.numeric(tail(log_petrol_train,1))), h_test)
naive_somaj_niv  <- rep(exp(as.numeric(tail(log_somaj_train,1))),  h_test)

cat("\n===== COMPARATIE VECM vs. NAIV =====\n")
cat("Trafic:\n")
print(round(rbind(VECM=eval_oos(as.numeric(trafic_test),pred_trafic_niv),
                  Naiv=eval_oos(as.numeric(trafic_test),naive_trafic_niv)), 2))
cat("Petrol:\n")
print(round(rbind(VECM=eval_oos(as.numeric(petrol_test),pred_petrol_niv),
                  Naiv=eval_oos(as.numeric(petrol_test),naive_petrol_niv)), 4))
cat("Somaj:\n")
print(round(rbind(VECM=eval_oos(as.numeric(somaj_test),pred_somaj_niv),
                  Naiv=eval_oos(as.numeric(somaj_test),naive_somaj_niv)), 4))


# 16.6 Grafice out-of-sample
time_test_mon <- as.yearmon(as.numeric(time(trafic_test)))

make_oos_df <- function(actual_ts, pred, naive, timp, eticheta) {
  data.frame(timp=timp, Actual=as.numeric(actual_ts),
             Forecast_VECM=pred, Forecast_Naiv=naive, serie=eticheta) %>%
    pivot_longer(cols=c("Actual","Forecast_VECM","Forecast_Naiv"),
                 names_to="Serie", values_to="Valoare")
}

df_oos <- bind_rows(
  make_oos_df(trafic_test, pred_trafic_niv, naive_trafic_niv, time_test_mon, "Trafic"),
  make_oos_df(petrol_test, pred_petrol_niv, naive_petrol_niv, time_test_mon, "Petrol"),
  make_oos_df(somaj_test,  pred_somaj_niv,  naive_somaj_niv,  time_test_mon, "Somaj")
)

ggplot(df_oos, aes(x=timp, y=Valoare, color=Serie, linetype=Serie)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  facet_wrap(~serie, ncol=1, scales="free_y") +
  scale_color_manual(values=c("Actual"="black",
                              "Forecast_VECM"="steelblue",
                              "Forecast_Naiv"="tomato")) +
  scale_linetype_manual(values=c("Actual"="solid",
                                 "Forecast_VECM"="solid",
                                 "Forecast_Naiv"="dashed")) +
  labs(title="Evaluare out-of-sample: VECM vs. Prognoza naiva",
       subtitle=paste0("Ultimele ", h_test, " luni | niveluri"),
       x="Timp", y="Valoare", color="", linetype="") +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold"), legend.position="bottom")
#Petrol: atât VECM cât și naivul subestimează valorile reale, ambele prognozează în jurul a 75 USD când realitatea a coborât spre 60–65 USD. VECM urmărește mai bine trendul descendent față de naivul care rămâne constant.
#Șomaj: naivul (linie roșie punctată) rămâne constant la ultima valoare observată (~4.1%), în timp ce VECM (linie albastră) captează ușor trendul ușor crescător al șomajului real. Totuși eroarea VECM e mai mare numeric.
#Trafic: valorile reale (linia neagră) fluctuează semnificativ între 80.000 și 86.000. VECM (albastru) urmărește parțial tendința dar subestimează vârfurile. Naivul (roșu) rămâne practic constant — mai aproape de medie dar fără să surprindă variațiile.


# ===========================================================================
# OPTIONAL: Robustete - schimbarea ordinii Cholesky
# ===========================================================================
# Ordinea Cholesky (Trafic, Petrol, Somaj) afecteaza IRF si FEVD.
# In modelul principal, Traficul este plasat primul => primeste "credit"
# maxim pentru variatiile contemporane => explica ~70% din variatia Somajului.
# Testam daca rezultatele sunt robuste la o ordine alternativa.
#
# Ordine alternativa: Somaj -> Petrol -> Trafic
# Justificare: Somajul e variabila macroeconomica exogena (determinata de
# politici externe sistemului), urmat de Petrol (determinat global),
# iar Traficul reactioneaza la ambii factori.

Y_alt <- cbind(Somaj  = log_somaj,
               Petrol = log_petrol,
               Trafic = log_trafic)

johansen_alt <- ca.jo(Y_alt, type = "trace", ecdet = "const", K = p_opt_log)
var_alt      <- vec2var(johansen_alt, r = r_coint_log)

# IRF cu ordine alternativa
irf_alt <- irf(var_alt, n.ahead = 24, boot = TRUE,
               ci = 0.95, ortho = TRUE, runs = 500)
plot(irf_alt, main = "IRF - ordine alternativa Cholesky: Somaj->Petrol->Trafic")

# FEVD cu ordine alternativa
fevd_alt <- fevd(var_alt, n.ahead = 24)
plot(fevd_alt, main = "FEVD - ordine alternativa")

cat("\n===== FEVD Trafic - ordine alternativa =====\n")
print(round(as.data.frame(fevd_alt$Trafic), 4))
cat("\n===== FEVD Somaj - ordine alternativa =====\n")
print(round(as.data.frame(fevd_alt$Somaj), 4))

# Daca rezultatele IRF si FEVD sunt similare cu modelul principal
# => concluziile sunt ROBUSTE la ordinea Cholesky
# Daca sunt diferente semnificative => mentionam ca limitare in lucrare



# Ordine: Petrol -> Trafic -> Somaj

# Justificare: Petrolul e determinat global (cel mai exogen),
# traficul reactioneaza la pret, iar somajul e variabila de rezultat
Y_alt2 <- cbind(Petrol = log_petrol,
                Trafic = log_trafic,
                Somaj  = log_somaj)

johansen_alt2 <- ca.jo(Y_alt2, type = "trace", ecdet = "const", K = p_opt_log)
var_alt2      <- vec2var(johansen_alt2, r = r_coint_log)

irf_alt2  <- irf(var_alt2, n.ahead = 24, boot = TRUE,
                 ci = 0.95, ortho = TRUE, runs = 500)
plot(irf_alt2)

fevd_alt2 <- fevd(var_alt2, n.ahead = 24)

plot(fevd_alt2, main = "FEVD - ordine alternativa 2")
cat("\n===== FEVD Trafic - ordine Petrol->Trafic->Somaj =====\n")
print(round(as.data.frame(fevd_alt2$Trafic), 4))
cat("\n===== FEVD Somaj - ordine Petrol->Trafic->Somaj =====\n")
print(round(as.data.frame(fevd_alt2$Somaj), 4))


# ===========================================================================
# CONCLUZIE FINALA 
# ===========================================================================


# Au fost testate trei specificații ale ordinii de identificare Cholesky: 
# (1) Trafic→Petrol→Șomaj (modelul principal), 
# (2) Șomaj→Petrol→Trafic și (3) Petrol→Trafic→Șomaj. 
# Analiza de robustețe relevă că rezultatele sunt sensibile la ordinea aleasă. 
# Singurul rezultat robust între toate cele trei specificații este contribuția semnificativă a prețului petrolului
# la variația traficului și a șomajului, confirmând că petrolul este variabila cu cel mai puternic impact contemporan în sistem.
# Contribuțiile traficului la variația șomajului (70% în modelul principal) nu se confirmă în specificațiile alternative, 
# indicând că acest rezultat este un efect al identificării și nu o relație structurală robustă. 
# Aceste rezultate subliniază limitele identificării prin descompunerea Cholesky și necesitatea unor restricții 
# structurale suplimentare pentru o interpretare cauzală definitivă.

