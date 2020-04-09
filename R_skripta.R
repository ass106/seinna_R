#' ---
#' title: "Seinna R Verkefni"
#' author: "Ástráður Stefánsson, ass106"
#' date: "3 apríl, 2020"
#' ---

options(scipen=999)
library(knitr)
library(kableExtra)
library(readr)
library(tidyverse)
require(scales)

#' # Fyrsti Hluti
#' **b)** Hér er gagnaskráin husnaedisverd\_2017.csv lesin inn og geymd í hlut sem ber upphafsstafina mína "as".

as <- read_delim("husnaedisverd_2017.csv",";", escape_double = FALSE, col_types = cols(byggar = col_integer(),efstah = col_integer(),
                                             fjbkar = col_integer(),fjeld = col_integer(),
                                             fjgeym = col_integer(),fjherb = col_integer(),
                                             fjibmhl = col_integer(),fjklos = col_integer(),
                                             fjmib = col_integer(),fjstof = col_integer(),
                                             fjsturt = col_integer(),haednr = col_integer(),
                                             kaupverd = col_double(),lyfta = col_integer(),
                                             matssvaedi = col_integer(),svfn = col_integer(),
                                             undirmatssvaedi = col_integer()),locale = locale(),
                                             na = "NA", trim_ws = TRUE)

#' **c)** Í skránni er dálkurinn kaupverð. Þar eru öll gildi í þúsundum króna en hér er þeim breytt í krónur

as$kaupverd <- as$kaupverd * 1000

#' **d)** Hér er ný breyta "Fermetraverð" sett inn í gagnarammann "as"

as$fermetraverd <- as$kaupverd / as$birtm2

#' **e)** Hér er búin til ný breyta sem byggir á teg_eign breytunni. Hún gerir þrengri skil á flokkum yfir tegund eigna.
#' Í stað þess að innihalda tegundirnar: Einbýlishús, Parhús, Raðhús og Íbúðareign þá eru Sérbýlin sett saman og Íbúðareign breytt í Íbúð.

teg_eign_groft <- as$teg_eign

teg_eign_groft <- forcats::fct_recode(teg_eign_groft, 
                                      Íbúð="Íbúðareign",
                                      Sérbýli="Einbýlishús",
                                      Sérbýli="Parhús",
                                      Sérbýli="Raðhús")

as$teg_eign_groft <- teg_eign_groft

#' **f)** Hér er útbúinn nýr gagnarammi, rvk, útfrá as en eingöngu utanum hverfi í Reykjavík. Breytan rvk inniheldur númer þessara hverfa en það eru Hlíðar, Skerjafjörður og Kringlan

hverfi<-c(80,100,150)
rvk <- filter(as, matssvaedi%in%hverfi)


#' **g)** Í stað númeranna fyrir hvert hverfi þá er hér matssvæðis breytunni yfir svæði breytt yfir í flokkabreytu og flokkarnir nefndir.

rvk$matssvaedi <- factor(rvk$matssvaedi)
levels(rvk$matssvaedi) <- c("Hlíðar", "Laugarneshverfi/Vogar", "Seljahverfi")

#' **h)** Hér tek ég úrtak úr þýðinu rvk.

set.seed(24) # þið setjið happatöluna ykkar hér inn
rvk %>% 
  group_by(matssvaedi) %>%
  sample_n(size = 100) -> urtak1

#' **i)** Hér að neðan má sjá töflu yfir hlutfall sérbýla á móti íbúða í hverfunum þremur.

serH <- c(nrow(filter(urtak1, teg_eign_groft == "Sérbýli", matssvaedi == "Hlíðar")),
            nrow(filter(urtak1, teg_eign_groft == "Íbúð", matssvaedi == "Hlíðar")))
serL <- c(nrow(filter(urtak1, teg_eign_groft == "Sérbýli", matssvaedi == "Laugarneshverfi/Vogar")),
            nrow(filter(urtak1, teg_eign_groft == "Íbúð", matssvaedi == "Laugarneshverfi/Vogar")))
serS <- c(nrow(filter(urtak1, teg_eign_groft == "Sérbýli", matssvaedi == "Seljahverfi")),
            nrow(filter(urtak1, teg_eign_groft == "Íbúð", matssvaedi == "Seljahverfi")))

hlidaHlutfoll <- c((serH[1]/(serH[1]+serH[2])), (serH[2]/(serH[1]+serH[2])))
laugarHlutfoll <- c((serL[1]/(serL[1]+serL[2])), (serL[2]/(serL[1]+serL[2])))
seljaHlutfoll <- c((serS[1]/(serS[1]+serS[2])), (serS[2]/(serS[1]+serS[2])))

taflaHl <- data.frame(Hlíðar = hlidarProsent,
                      Laugarneshverfi_Vogar = laugarProsent,
                      Seljahverfi = seljaProsent,
                      row.names = c("Hlutfall Sérbýla", "Hlutfall Íbúða"))

kable(taflaHl, "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)

#' **j)** Hér ætlum við að kanna hvort að marktækur munur sé á hlutfalli sérbýla í hverfunum þremur með kí-kvaðrat prófi. 
#' Við erum þeirrar trúar að stór munur sé til að mynda á hlutfalli sérbýla á milli þessara hverfa. Seljahverfi er með mjög hátt hlutfall sérbýla til að mynda.
#' Núlltilgátan okkar er sú að það sé enginn marktækur munur á hlutfalli sérbýla. Hæstu ásættanlegu villulíkur okkar eru 5%.

qchisq(0.95,300)
chisq.test(table(0.04, 0.12, 0.29))