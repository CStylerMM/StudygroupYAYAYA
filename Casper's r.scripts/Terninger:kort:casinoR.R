# Hands-On Programming with R - Kapitel 5
# Samlet kode i den rækkefølge bogen faktisk bruger

# --- 1. Udgangspunkt: en simpel atomic vector ---
die <- c(1, 2, 3, 4, 5, 6)
die
is.vector(die)
typeof(die)

# --- 2. Attributter: names og dim ---
names(die) <- c("one", "two", "three", "four", "five", "six")
die
names(die) <- NULL  # fjerner navnene igen

dim(die) <- c(2, 3) # giver die dimensioner -> bliver til en matrix
die

# --- 3. matrix() og array(): de "rigtige" funktioner til det samme ---
die <- c(1, 2, 3, 4, 5, 6)  # nulstil die til en almindelig vektor
m <- matrix(die, nrow = 2, byrow = TRUE)
m

ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
ar

# --- 4. class(): hvorfor die pludselig opfoerer sig som en matrix ---
dim(die) <- c(2, 3)
typeof(die)   # stadig "double"
class(die)    # men class er nu "matrix"

# --- 5. Coercion: problemet der motiverer lister ---
card <- c("ace", "hearts", 1)
card          # alt er blevet til character strings

# --- 6. list(): loesningen - et enkelt kort med blandede typer ---
card <- list("ace", "hearts", 1)
card

# --- 7. data.frame(): et lille eksempel med 3 raekker ---
df <- data.frame(
  face = c("ace", "two", "six"),
  suit = c("clubs", "clubs", "clubs"),
  value = c(1, 2, 3),
  stringsAsFactors = FALSE
)
df
str(df)

# --- 8. Den fulde 52-korts data.frame i haanden ---
# Bogens pointe: dette virker, men er en daarlig metode - for meget
# tastearbejde og for stor risiko for fejl.
deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace",
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five",
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine",
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),
  suit = c("spades", "spades", "spades", "spades", "spades", "spades",
           "spades", "spades", "spades", "spades", "spades", "spades", "spades",
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs",
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts"),
  value = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8,
            7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11,
            10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
  stringsAsFactors = FALSE
)
head(deck)

# --- 9. Den anbefalede vej: indlaes deck.csv i stedet for at skrive det ---
# (kraever at deck.csv er downloadet, se afsnit 5.9)
deck <- read.csv("/Users/vandmelonsguru/Dataanalyse/DATA/deck kap5.9/deck.csv", stringsAsFactors = FALSE)
head(deck)

# --- 10. Gem deck igen som CSV ---
write.csv(deck, file = "cards.csv", row.names = FALSE)

