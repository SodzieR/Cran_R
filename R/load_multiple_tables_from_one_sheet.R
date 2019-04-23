# Generall Script

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(readxl)

# Setup -------------------------------------------------------------------

data_path <- './data/clininet_sample.xlsx'
data_sheet_index <- 1L
meta_path <- './data/meta_clininet.xlsx'
meta_sheet_index <- 2L

# Load --------------------------------------------------------------------


# THIS WILL WORK ONLY FOR clininet.xlsx FILE

# I think it cant get more primitive than that, but well it works


# Had to skip those first six rows cause I couldnt get my element SplitRows
# appear with values
df <- read_xlsx(data_path, data_sheet_index, skip = 6L)

# Counts all rows in data frame
TotalRows <- nrow(df)

SplitRows <- which(is.na(df$`Klinka/Oddział: Klinika 1 / Klinika 2`))

# You want to get rid of the first empty row as it doesn't split tables
SplitRows <- SplitRows[-1]

# You want to keep theese 'splitting rows' as they are the only ones doing it
# This is a horrible solution to a problem, totally primitive
# It could be solved using a simple loop/function
SplitRows<- SplitRows[c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)]

# This now specifies correctly a start and an end of each table
SplitRows <- c(0, SplitRows, TotalRows + 1)



# You want to substract one int from the total numbers of splitting rows to prevent
# creating a single, empty data frame in the list
#--------------------------------------------------------------------------
# BARDZO PODOBNIE JEST Z P?OTEM OGRODZENIOWYM
# JEZELI P?OT SK?ADA SI? Z 10 SZTACHET TO DZIUR POMI?DZY NIMI B?DZIE O JEDN?
# MNIEJ CZYLI 9
# n = LICZBA SZTACHET | n - 1 = LICZBA DZIUR W P?OCIE
#--------------------------------------------------------------------------
NumSplits <- length(SplitRows)  - 1

# This statement creates an empty list with places for data frames that's
# equal to NumSplits intiger
DFs <- vector(mode = "list", length = NumSplits)


# Dzialanie petli opisze po polsku
# Zwykla petla for, posiadajaca elementy skladni podobne do petli foreach
# stala 'i' oznacza kazdy wiersz oddzielajacy tabele, oznaczenie for gwarantuje
# ze petla zostanie wykonana dla kazdego elementu i, czyli dla kazdego wiersza
# odzielajacego tabele od siebie
for (i in 1:NumSplits) {
  # ten warunek okresla wartosc stalej 'i' uzywana w petli
  if ( i == 1){
    # Element listy 'i' to tabela danych zaczynajaca sie od wiersza o indexie
    # pierwszym w ciagu NumSplits - na poczatku jest to pierwszy wyraz ciagu
    # czyli wiersz 0, nastepnie poprzez : wyrazony jest odcinek zapisywany jako data frame
    # w liscie DFS, zapisywanie elementu zaczynajacego sie od wiersza 1 zakonczy sie
    # na wierszu 96, czyli wierszu o jeden wyzszym niz wiersz o indexie 2 w ciagu
    # intow NumSplits co dokladnie przedstawia to cialo petli, a zarazem zapobiega to
    # zapisywaniu sie pustego i zarazem naszego oddzielajacego wiersza w tabeli w liscie DFs
    # trzeba zauwazyc ze ponizsza linijka kodu zadziala tylko dla pierwszego elementu listy
    # gdyz to dla niego 'i' rowne bedzie 1
    DFs[[i]] <- df[(SplitRows[i] + 1):(SplitRows[i+1] - 1), ]
  } else {
    # Po warunku 'else' odbywa sie kolejna czesc petli dla ktorej sa poddwane przypadki
    # inne niz dla 'i' rownego 1. Oznacza to ze dla ponizszej linijki kodu poddawane beda
    # wiersze o indexach, w ciagu NumSplits, innych niz index pierwszy. Jest tak, gdyz
    # praca odbywa sie tutaj na 'dziurach pomiedzy sztachetami w plocie' czyli na wierszach
    # oddzielajacych tabele od siebie, a nie tak jak w przypadku wyzej gdzie praca petli odbywala sie
    # na wirtualnym wierszu 0, ktory tak naprawde nie istnieje i oznacza poczatek zapisu tabeli
    DFs[[i]] <- df[(SplitRows[i] + 2):(SplitRows[i+1] - 1),]
    # ponizsza linijka petli zwraca nazwy kolumn dla kazdego z utworzonych tabel w liscie
    # niestety ta metoda jest w tym slaba i trzeba by tu bylo uporzadkowac kazda z tabel jednoczesnie,
    # lub po prostu poddac kazda tabele dzialanu funkcji czyszczacej i otrzymac w ten sposob czyste dane
    # dodatkowo nalezy pamietac, ze pierwsza tabela nie zostala w calosci zaimporotwana w calosci, pominieto
    # tam imie i naziwsko pacjenta, a takze LAB_ID - dla niej nalezaloby przypisac podczasz czyszczenia dodatkowe
    # dwie kolumny c('Surename_Name','LAB_ID'), o usunietych wartosciach w celu skompeltowania czystych danych
    colnames(DFs[[i]]) <- df[SplitRows[i] + 1, ]
  }
}


# First one isn't complete as it has missing LAB_ID and patient's surename_name
DFs[[1]]

#The rest is good
DFs[[2]]

DFs[[3]]

# etc...