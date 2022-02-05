# ------ zakomentuj lub usuń, jeżeli chcesz wywołać z CMD/PS
install.packages("ggplot2")
# -------

library(ggplot2)

tytanis = read.csv("TitanicMess.tsv", sep = "\t", header = TRUE, na.strings = c("", "NA"))

print(colSums(is.na(tytanis)))

print("Jak można zauważyć, w prawie 80% przypadków brakuje wartości w kolumnie 'Cabin'. Nie ma sensu brać tej kolumny pod uwagę. Kolumny PassengerId, Ticket i Name również zostaną usunięte, ponieważ nie będą one potrzebne w przyszłej analizie. W kolumnie ship wszystkie rekordy mają tę samą wartość - 'Tytanic'. Zostanie ona również usunięta, gdyż nie jest ona istotna. W kolumnie Embarked 2 rekordy mają pustą wartość - zostaną one zastąpione wartością, która najczęściej się pojawia. ")

tytanis = subset(tytanis, select = -c(Cabin, Name, Ticket, PassengerId, ship))

print(table(tytanis$Embarked))

print("Najczęściej pojawiającą się wartością Embarked jest S. 1 rekord posiada wartość Co zamiast wartości C, inny posiada Qe zamiat Q, natomiast jeszcze inne 2 rekordy posiadają wartość So zamiast S. Zostanie to poprawione.")

tytanis$Embarked[is.na(tytanis$Embarked)] = "S"
tytanis$Embarked[tytanis$Embarked == "Co"] = "C"
tytanis$Embarked[tytanis$Embarked == "Qe"] = "Q"
tytanis$Embarked[tytanis$Embarked == "So"] = "S"

print(table(tytanis$Embarked))
print(colSums(is.na(tytanis)))

print("Przyjrzyjmy się jeszcze, czy wszystkie dane są poprawnego typu")
print(sapply(tytanis, class))

print("Kolumny Survived, Pclass, Sex, SibSp i Embarked są poprawnego typu. Pozostałe dane (Age, Fare powinny zostać naprostowane. Zajmijmy się kolumną Age:")
print(table(tytanis$Age))
print(colSums(is.na(tytanis)))

print("Jak można zauważyć, problemem jest obecność ujemnych wartości oraz dwóch różnych separatorów dziesiętnych. Ujemne wartości zostaną zamienione na dodanie, a separatory zostaną ujednolicone do kropki.")
tytanis$Age = as.numeric(gsub(",", ".", tytanis$Age))
tytanis$Age = abs(tytanis$Age)
print(table(tytanis$Age))

print("20% przypadków Age posiada pustą wartość. Najłatwiejszą opcją byłoby zastąpienie pustych wartości medianą. Minusem tej opcji będzie to, że 20% rekordów będzie miało tą samą wartość, co mocno utrudni późniejszą analizę. Puste dane zostaną usunięte.")

print("W tabeli wartości dla kolumny Age, w oczy rzuca się wartość 4435. Załóżmy, że każdy wiek powyżej 100 lat jest podejrzany. Sprawdźmy, ile wpisów zawiera wiek > 100 lat. Jeżeli będą jakieś lata blisko tej granicy, skorzystamy z wzoru na wyznaczanie przedziałów dla wartości brzegowych lo = Q1 - 1.5*IQR oraz hi = Q3 + 1.5*IQR, gdzie IQR = Q3-Q1.")
tytanis = na.omit(tytanis)
for (row in 1:nrow(tytanis)) {
	if (tytanis[row, ]$Age > 100) {
		print(tytanis[row ,]$Age)
	}
}
print("Znalezione zostały tylko 2 wartości, do tego całkowicie pozbawione sensu - zostaną one usunięte.")
tytanis$Age[tytanis$Age > 100] = NA
tytanis = na.omit(tytanis)

print("Wiek zostanie jeszcze zaokrąglony do pelnych lat")
tytanis$Age = round(tytanis$Age)
print(table(tytanis$Age))

print("Kwestia wieku została załatwiona. Teraz pozostaje kwestia błędnego formatu danych kolumny Fare. Sprawdźmy, czy nie zawiera ona niedozwolonych znaków")
for (row in 1:nrow(tytanis)) {
	if (grepl('[A-Za-z]',tytanis[row ,]$Fare)) {
		print(tytanis[row ,]$Fare)
	}
}
print("Jedena z wartości zawiera literówkę. Literka zostanie usunięta. Dodatkowo, wartości zostaną przekonwertowane na typ numeric, z uśrednieniem separatora dziesiętnego.")
tytanis$Fare[tytanis$Fare == "15,9a"] = "15,9"
tytanis$Fare = as.numeric(gsub(",", ".", tytanis$Fare))

print("Spójrzmy na wartości dla kolumny Fare")
print(table(tytanis$Fare))
print("Jedna z wartości jest ujemna - zapewne jest to literówka. Zamieniona zostanie na wartość dodatnią. 15 rekordów posiada wartość Fare = 0. Część rekordów posiada wartość Fare > 100. Ciężko powiedzieć, które wartości należy traktować jako wartości odstające, a które nie - w końcu część osób mogła mieć jakieś specjalne vouchery na darmowy rejs, a z drugiej strony trzycyfrowe wartości mogą oznaczać jakąś klasę premium - możliwe, że na tytanisie był jakiś bogaty chłopiec, który chciał koniecznie popłynąć na górnym pokładzie, czy coś w ten deseń.")
tytanis$Fare = abs(tytanis$Fare)

print("Wartości Fare zostaną zaokrąglone do 2 miejsc po przecinku.")
tytanis$Fare = round(tytanis$Fare, digit = 2)
print("Spójrzmy na poprawnione wartości Fare")
print(table(tytanis$Fare))

print("Wygląda OK. Sprawdźmy, jakie wartości są jeszcze w kolumnie Sex")
print(table(tytanis$Sex))

print("Widać, że istnieją tutaj wartości z literówkami typu femmale, fem, malef, mal. Zostaną one poprawione.")
tytanis$Sex[tytanis$Sex == "mal"] = "male"
tytanis$Sex[tytanis$Sex == "malef"] = "male"
tytanis$Sex[tytanis$Sex == "fem"] = "female"
tytanis$Sex[tytanis$Sex == "femmale"] = "female"

print("Zobaczmy, jakie wartości są w kolumnie Survived")
print(table(tytanis$Survived))

print("Wygląda ok. Klasa?")
print(table(tytanis$Pclass))

print("Wygląda ok. SibSp?")
print(table(tytanis$SibSp))

print("Wartości od 1 - 8 wyglądają ok. Są to liczby całkowite. W tamtych czasach 8 rodzeństwa nie było niczym dziwnym. Parch?")
print(table(tytanis$Parch))

print("Wartości od 1 do 5 wyglądają ok. Są to liczby całkowite. Dziwi mnie trochę to, że informacja o dzieciach i rodzicach znajduje się w 1 kolumnie. 5 rodziców wygląda dziwnie, ale już 5 dzieci wygląda całkowicie ok. Dalsza obróbka danych z tej kolumny nie ma już chyba sensu. Przyjrzyjmy się raz jeszcze, czy wszystkie dane są poprawnego typu")
print(sapply(tytanis, class))

print("Czyszczenie danych zostało zakończone. Wygenerowany zostanie nowy plik tsv.")
write.table(tytanis, file='TitanicCleaned.tsv', quote = FALSE, sep='\t', col.names = TRUE, row.names = FALSE)

print("Podstawowa analiza - zobaczmy kto przeżył na podstawie samej płci")
females = subset(tytanis, Sex == "female")
males = subset(tytanis, Sex == "male")
print(paste("Przeżywalność kobiet [%]:", round(nrow(females[females$Survived == 1, ]) / nrow(females), digits = 2)))
print(paste("Przeżywalność mężczyzn [%]: ", round(nrow(males[males$Survived == 1, ]) / nrow(males), digits = 2)))

tytanis$Survived = as.factor(tytanis$Survived)

print(barplot(prop.table(table(tytanis$Survived, tytanis$Sex))))

print("Na podstawie płci, wieku i klasy przeżywalność prezentuje się następująco:")
print(ggplot(tytanis, aes(x = Age, fill=Survived, color=Survived)) +
  facet_wrap(Sex~Pclass) + 
  geom_histogram(binwidth = 5) +
  labs(x = 'Age', y = 'Survived'))

print("Zwróćmy uwagę na porównanie przeżywalności a portu, z którego wypłynęły dane osoby")
print(ggplot(tytanis, aes(x = Age, fill=Survived, color=Survived)) +
  facet_wrap(Sex~Embarked) + 
  geom_histogram(binwidth = 5) +
  labs(x = 'Age', y = 'Survived'))

print("Największą szansę na przeżycie miały kobiety poniżej 18 roku życia. Jak można zauważyć, najmniejszą szansę na przeżycie mieli mężczyźni w klasie 3. Kobiety z klas 1 i 2 przeżyły niemal w 100%, natomiast jeżeli chodzi o mężczyzn, to w klasach 1 i 2 przeżyli głównie chłopcy. W klasie 1 przeżyło około 50% dorosłych mężczyzn, natomiast w klasach 2 i 3 odsetek ten był znacznie niższy. Ciekawostką jest to, że kobiety z klasy najniższej (3), były ewakuowane w pierwszej kolejności przed mężczyznami z klasy najwyższej (1).")
