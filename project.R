#########################
#	Najprostsze uzycie	#
#########################

## ladujemy zbior danych do zmiennej np. a
#	library(mlbench)
#	library(rpart)
#	data(BostonHousing)
#	a <- BostonHousing
#
## uruchamiamy i zapisujemy wynik do zmiennej np. z
#	z <- alhe(a, regression=rpart)
## lub z uzyciem regresji liniowej, po prostu
##	z <- alhe(a)
#
## dla domyslnej wartosci TRUE parametru printStats, otrzymujemy informacje debugowe np.
##	> z <- alhe(a, regression=rpart)
##	[1] "Ilosc uzytych modeli:"
##	[1] 2
##	[1] "Uzyskany blad:"
##	[1] 12.23499
##	[1] "Blad dla calego zbioru danych:"
##	[1] 16.24467
##	[1] "Uzyskana poprawa (jak dodania):"
##	[1] 4.009681
#
## znaleziony osobnik zapisany w zmiennej z w postaci listy:
##	$genModels		- lista wszystkich wygenerowanych modeli
##	$models			- lista uzytych modeli regresji
##	$midPrediction	- usredniona predykcja uzytych modeli
##	$rank			- sredni blad sredniokwadratowy dla uzyskanego wyniku z przewidywana kolumna


#####################################
#	Ladowanie pakietow i danych		#
#####################################

## Ladujemy pakiet z przykladowymi zbiorami danych
#library(mlbench)
#
## Ladujemy pakiet do uzycia potem regresji na strukturze drzewiastej, a nie liniowej
#library(rpart)
#
## Ladujemy przykladowe dane
#data(BostonHousing)


#########################################
#	Przykladowe wzglednie liniowe dane	#
#########################################

#a <- data.frame(age=18:29, height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5))
#a1 <- data.frame(age=c(18,20,22,24), height=c(76.1,78.1,78.8,79.9))
#a2 <- data.frame(age=c(19,21,23,25,27), height=c(77,78.2,79.7,81.1,81.8))
#a3 <- data.frame(age=c(18,21,29), height=c(76.1,78.2,83.5))
#a4 <- data.frame(age=c(19,20,24,29), height=c(77,78.1,79.9,83.5))
#a5 <- data.frame(age=c(22,23,28), height=c(78.8,79.7,82.8))
#la <- list(a1,a2,a3,a4,a5)


#########################
#	Funkcje pomocnicze	#
#########################

# Oblicza srednia z obliczonych parami bledow sredniokwadratowych
#
# a[vector]	- pierwszy wektor danych
# b[vector]	- drugi wektor danych
#
# return[double]	- obliczony sredni blad sredniokwadratowy
mse = function(a, b) {
	return (mean((a - b)^2, na.rm = TRUE))
}

# Losuje wiersze ze zwracaniem rownoliczne z danym zbiorem
#
# dat[data.frame]	- dane do losowania z nich wierszy
#
# return[data.frame]	- wylosowane wiersze (co najmniej 2)
randRows = function(dat) {
	r <- dat[sample(nrow(dat), replace=T),]

	return (r)
}

# Generuje losowe ramki danych z wylosowanych wierszy
#
# dat[data.frame]	- dane do losowania z nich
# n[int]			- liczba losowanych modeli
#
# return[list]	- lista wygenerowanych ramek z losowanymi wierszami z danych
randDatas = function(dat, n=3) {
	datas <- list()

	for (i in 1:n) {
		datas[[i]] <- randRows(dat)
	}

	return (datas)
}

# Generuje liste modeli dla zadanej kolumny identyfikowanej podana etykieta
#
# datas[list]		- lista ramek z zaburzonymi zbiorami danych
# col[string]		- nazwa kolumny(etykiety) do przewidywania
# reg[function]		- funkcja uzywana do utworzenia modelu regresji {lm|rpart}
#
# return[list]	- lista utworzonych modeli dla zadanych zbiorow danych
genModels = function(datas, col, reg=lm) {
	res <- list()

	# przewidujemy podana kolumne na podstawie wszystkich innych
	frm <- paste(col, ".", sep=" ~ ")

	for (i in 1:length(datas)) {
		res[[i]] <- reg(formula(frm), datas[[i]])
	}
	
	return (res)
}

# Oblicza przewidywania dla wszystkich pojedynczych modeli,
# aby potem mozliwe bylo operowanie tylko na wartosciach tych przewidywan
#
# models[list]		- lista modeli
# dat[data.frame]	- pierwotne dane
#
# return[list]	- lista wektorow wartosci przewidywanych dla kolejnych modeli
calculateEachModel = function(models, dat) {
	result <- list()

	for (i in 1:length(models)) {
		result[[i]] <- predict(models[[i]], dat)
	}

	return (result)
}


#####################################################
#	Funkcje odpowiadajace genetycznosci algorytmu	#
#####################################################

# Liczy srednia z przewidywan oraz blad sredniokwadratowy dla niej
# na podstawie listy uzytych modeli w danym osobniku
#
# Funkcja tworzy osobniki z podanych danych i zwraca ich liste
#
# predictions[list]	- lista wektorow predykcji dla kolejnych modeli
# population[list]	- lista wektorow z wartosciami TURE i FALSE, gdzie TRUE na danej pozycji oznacza uzyty model w osobniku
# dat[data.frame]	- pierwotne dane
# column[string]	- kolumna(etykieta) ktora przewidujemy
#
# return[list]	- lista wygenerowanych osobnikow, gdzie osobnik reprezentuje nastepujaca ramka:
#			$predictionList[vector]	- lista wartosci boolowskich, oznaczajaca ktore modele zostaly uzyte do przewidywan np {true, false, true} to osobnik z predykcji 1 i 3
#			$midPrediction[vector]	- usrednione przewidywania
#			$rank[double]			- blad sredniokwadratowy predykcji (wartosc ktora minimalizujemy)
calculatePredictions = function(predictions, population, dat, column) {
	result <- list()

	for(i in 1:length(population)) {
		result[[i]] = list(predictionList=population[[i]], midPrediction=0, rank=0)

		usedPredictions = c()
		firstOne = TRUE

		# znajdz wszystkie modele uzyte w tym osobniku
		for(j in 1:length(population[[i]])) {
			if(population[[i]][[j]]) {
				# jezeli uzyty model, to dodaj jako kolumny w celu wyliczenia pozniej parami srednich predykcji 
				if (firstOne) {
					usedPredictions <- cbind(predictions[[j]])
					firstOne = FALSE
				} else {
					usedPredictions <- cbind(usedPredictions, predictions[[j]])
				}
			}
		}

		# policz srednia predykcje
		result[[i]]$midPrediction <- rowMeans(usedPredictions)
		
		# policz blad sredniokwadratowy
		result[[i]]$rank <- mse(result[[i]]$midPrediction, dat[,column])
	}

	return (result)
}

# Generuje pierwsza populacje jako wsystkie kombinacje o rozmiarze maksymalnym maxNmbTrue
# gdzie populacja to wektory wartosci TRUE i FALSE reprezentujace uzyte modele w osobniku
#
# maxNmbTrue[int]	- maksymalna ilosc modeli uzytych w osobniku
# modelSize[int]	- liczba modeli do utworzenia z nich kombinacji
#
# return[list]	- lista wektorow definiujacych osobniki w populacji
generateFirstPopulation = function(maxNmbTrue, modelSize) {
	result = list()

	pos = 1
	for (i in 1:maxNmbTrue) {
		# tworzymy kombinacje dla zadanej liczby modeli
		combinations <- combn(1:modelSize, i)

		for(j in 1:ncol(combinations)) {
			# tworzymy wektory opisujace osobniki
			result[[pos]] <- 1:modelSize %in% combinations[,j]
			pos <- pos + 1
		}
	}

	return(result)
}

# Wybiera z populacji losowe osobniki do reprodukcji
#
# population[list]	- (populacja) lista wektorow identyfikujacych osobniki
# amount[int]		- liczba osobnikow do wybrania
#
# return[list]	- podlista (z listy) populacji zawierajaca wylosowane osobniki
selection = function(population, amount) {
	result <- population[sample.int(length(population), amount, replace=T)]

	return (result)
}

# Rozszerza populacje o nowy element w kazdym zbiorze
# tj. dodaje kolejny model jako uzyty w danym osobniku i oblicza dla niego srednia predykcje i blad sredniokwadratowy
#
# toExtend[list]	- lista osobnikow do rozszerzenia
# predictions[list]	- lista wektorow predykcji dla kolejnych modeli
# dat[data.frame]	- pierwotne dane
# column[string]	- kolumna ktora przewidujemy
#
# return[list]		- lista nowych rozszerzeonych osobnikow
extendEntities = function(toExtend, predictions, dat, column) {
	newCombinations = list()

	for(i in 1:length(toExtend)) {
		newCombinations[[i]] <- toExtend[[i]]$predictionList

		# wezmy nieuzywane modele
		unusedModels <- which(toExtend[[i]]$predictionList == FALSE)
		
		# gdy wektor ma jedna pozycje, funkcja sample pobiera liczbe zamiast wektora o dlugosci 1
		# dlatego w celu unikniecia bledow rozwazamy te dwa przypadki
		# przypisujac losowo wybrane modele do osobnika
		if (length(unusedModels) == 1) {
			newCombinations[[i]][unusedModels[1]] <- TRUE
		} else {
			newCombinations[[i]][sample(unusedModels, 1)] <- TRUE
		}
	}

	# obliczamy wartosci identyfikujace osobnika z nowymi kombinacjami modeli
	result <- calculatePredictions(predictions, newCombinations, dat, column)

	return (result)
}

# Krzyzuje dwa osobniki reprezentowane przez uzyte modele i obliczony blad sredniokwadratowy
#
# set1[vector]	- pierwszy wektor uzytych modeli jako wartosci TRUE i FALSE na odpowiadajacych modelom pozycjach
# set2[vector]	- drugi wektor uzytych modeli
# maxTrue[int]	- maksymalna ilosc uzytych modeli w nowym osobinku
# rank1[double]	- blad sredniokwadratowy pierwszego osobnika
# rank2[double]	- blad sredniokwadratowy drugiego osobnika
#
# return[vector]	- wektor uzywanych modeli jako wartosci TRUE i FALSE odp. modelom
copulateEntity = function(set1, set2, maxTrue, rank1, rank2) {
	sm = set1 | set2
	w1 <- which(set1 == TRUE)
	w2 <- which(set2 == TRUE)

	un1 <- w1[-w2]
	un2 <- w2[-w1]
	
	ranks <- c(0,0)
	
	#oblicz stosunki rankingow (mniejszy == lepszy)
	if(rank1 > rank2) {
		#osobnik 2 jest lepszy
		ranks[1] <- min(1 - rank2/rank1, rank2/rank1)
		ranks[2] <- max(1 - rank2/rank1, rank2/rank1)
	} else {
		#osobnik 1 jest lepszy
		ranks[1] <- max(1 - rank1/rank2, rank1/rank2)
		ranks[2] <- min(1 - rank1/rank2, rank1/rank2)
	}
	
	while( length(un1) > 0 && length(un2) > 0) {
		if(sum(sm) > maxTrue) {
			if(sample(c(1,2), 1, prob=ranks) == 1) {
				toRm <- sample(un1, 1)
				sm[toRm] <- FALSE
				un1 = un1[-which(un1 == toRm)]
			} else {
				toRm <- sample(un2, 1)
				sm[toRm] <- FALSE
				un2 = un2[-which(un2 == toRm)]
			} 
		} else {
			break;
		}
	}
	
	#jezeli nam braklo to usowamy z tego co mamy
	if(sum(sm) > maxTrue) {
		toRm <- which(sm == TRUE)
		
		sm[sample(toRm, sum(sm) - maxTrue)] <- FALSE
	}
	
	return (sm)
}

# Krzyzuje osobniki ze soba, kolejno osobnika z osobnikiem po prawej stronie,
# a na koncu ostaniego z pierwszym
#
# toCopulate[list]	- lista osobnikow wybranych do krzyzowania
# predictions[list]	- lista wektorow predykcji dla kolejnych modeli
# dat[data.frame]	- pierwotne dane
# column[string]	- kolumna(etykieta) ktora przewidujemy
# maxTrue[int]		- maksymalna liczba uzytych modeli w osobniku
#
# return[list]		- lista osobnikow po krzyzowaniu
copulation = function(toCopulate, predictions, dat, column, maxTrue) {
	newSets = list()

	# krzyzuj kazdego z sasiadem po prawej stronie
	for(i in 1:(length(toCopulate) - 1)) {
		newSets[[i]] <- copulateEntity(toCopulate[[i]]$predictionList, toCopulate[[i+1]]$predictionList, maxTrue,
										toCopulate[[i]]$rank,			toCopulate[[i+1]]$rank)
	}

	# a ostatniego z pierwszym
	newSets[[length(toCopulate)]] <- copulateEntity(toCopulate[[length(toCopulate)]]$predictionList, toCopulate[[1]]$predictionList, maxTrue,
													toCopulate[[length(toCopulate)]]$rank,				toCopulate[[1]]$rank)

	# obliczamy wartosci identyfikujace osobnika po krzyzowaniu
	result <- calculatePredictions(predictions, newSets, dat, column)

	return (result)	
}

# Mutuje losowo wybrana ilosc osobnikow z populacji
#
# generation[list]	- lista osobnikow sposrod ktorych wybrane zostana do mutacji
# nMutate[int]		- liczba osobnikow do wykonania na nich mutacji
# predictions[list]	- lista wektorow predykcji dla kolejnych modeli
# dat[data.frame]	- pierwotne dane
# column[string]	- kolumna(etykieta) ktora przewidujemy
# maxTrue[int]		- maksymalna liczba uzytych modeli w osobniku
#
# return[list]	- lista danych osobnikow rozszerzona o zmutowane osobniki
mutation = function(generation, nMutate, predictions, dat, column, maxTrue) {
	result = generation
	newCombinations = list()
	pos = 1

	for(i in 1:nMutate) {
		# wylosuj osobnika do zmutowania
		toMutate <- generation[[sample.int(length(generation), 1)]]

		# wylosuj gen do zmiany i go zaneguj
		gen <- sample.int(length(toMutate$predictionList), 1)
		toMutate$predictionLis[[gen]] <- !toMutate$predictionLis[[gen]]

		# zlicz elementy w zbiorze
		trueFields <- which(toMutate$predictionList == TRUE) 

		# jak jest ich za duzo to usun losowo zeby byla odpowiednia ilosc
		if(length(trueFields) > maxTrue) {
			if (length(trueFields) == 1) {
				toMutate$predictionList[trueFields] <- FALSE
			} else {
				toMutate$predictionList[sample(trueFields, length(trueFields) - maxTrue)]
			}
		}

		# dodaj do zmutowanych kombinacji
		newCombinations[[pos]] <- toMutate$predictionList
		pos = pos + 1
	}

	# dodaj zmutowane osobniki do tych co byly
	result <- c(result, calculatePredictions(predictions, newCombinations, dat, column))

	return (result)
}


#########################
#	Funkcja pomocnicza	#
#########################

# Usuwa duplikaty z populacji na podstawie uzytych modeli
# Sortuje od najlepszych do najgorszych (rosnaco wg bledu sredniokwadratowego)
#
# population[list]	- lista osobnikow
#
# return[list]	- unikalna i posortowana lista osobnikow
uniqueOrder = function(population) {
	# zostaw tylko te unikalne osobniki
	population <- population[!duplicated(lapply(population, function(p) p$predictionList))]

	# posortuj od najlepszego do najgorszego
	population <- population[order(sapply(population, function(p) p$rank))]

	return (population)
}

#################################
#	Glowna funkcja - program	#
#################################

# Glowny program uruchamiajacy algorytm
# Generuje wstepna populacje, a nastepnie uruchamia petle zwiekszajac stopniowo wielkosc zbiorow
# Minimalizujemy blad sredniokwadratowy osobnikow
#
# dat[data.frame]	- ramka z pierwotnymi danymi dla ktorych uruchamiamy algorytm
# mAmount[int]		- liczba zaburzonych modeli do wygenerowania, minimum 2
# regression[function]	- funkcja uzywana do utworzenia modelu regresji {lm|rpart}
# bestInIter[int]	- liczba iteracji zwiekszania licznosci modeli przez ktora musi sie zachowac najlepszy osobnik zeby zakonczyc dzialanie algorytmu
# N[int]			- liczba uzytych modeli w osobniku dla pierwszej populacji (kombinacje N elementowe uzytych modeli)
# col[string]		- kolumna(etykieta) ktora chcemy przewidywac, gdy -1 wybierana ostatnia kolumna
# epsilon[double]	- roznica w ocenie najlepszych osobnikow kolejnych populacji po osiagnieciu ktorej nastepuje koniec dzialania algorytmu
# nIter[int]		- liczba iteracji przez ktora najlepszy osobnik sie nie zmienia i po ktorej zostaje rozszerzona liczba modeli w osobnikach
# popSize[int]		- maksymalny rozmiar populacji
# nSelect[int]		- liczba osobnikow wybieranych do krzyzowania
# nMutate[int]		- liczba osobnikow wybieranych do mutacji
# nExtend[int]		- liczba osobnikow, ktore beda rozszerzane o kolejny model w zbiorze
# nRemove[int]		- liczba osobnikow najgorszych usuwanych z populacji (zostaje maksymalnie popSize-nRemove) - przy dodawaniu kolejnego elementu zbiorow
# printStats[bool]	- czy drukowac proste statystyki na koncu
#
# return[data.frame]	- ramka z wynikami w formacie:
#			$genModels		- lista wszystkich wygenerowanych modeli
#			$models			- lista wybranych modeli uzytych w osobniku
#			$midPrediction	- srednia predykcja osobnika
#			$rank			- blad sredniokwadratowy
alhe = function(dat, mAmount=50, regression=lm, bestInIter=10, N=1, col=-1, epsilon=0.01, nIter=10, popSize=50, nSelect=10, nMutate=2, nExtend=25, nRemove=25, printStats=T) {
	# jak nie podano to wez ostatnia kolumne i przewiduj ja
	if (col == -1) {
		nms <- names(dat)
		col <- nms[length(nms)]
	}

	#########################
	#	Generacja danych	#
	#########################

	# generuj zaburzone ramki danych
	datas <- randDatas(dat, mAmount)

	# utworz modele zaburzonych danych
	models <- genModels(datas, col, regression)

	# policz dla nich przedykcje
	predictions <- calculateEachModel(models, dat)


	#####################################
	#	Generacja pierwszej populacji	#
	#####################################

	# generuj populacje o podanej liczbie uzytych modeli
	pop <- generateFirstPopulation(N, length(models))

	# utworz liste osobnikow na podstawie wygenerowanej populacji
	# i nadaj im wlasciwa strukture osobnikow
	population <- calculatePredictions(predictions, pop, dat, col)

	# zostaw unikalne i posortuj
	population <- uniqueOrder(population)


	#############################################
	#	Ukonkretnienie zmiennych decyzyjnych	#
	#############################################

	# ranking najlepszego i numer iteracji w ktorej powstal
	bestOneRank <- population[[1]]$rank
	bestOneIter <- 1

	# ranking poprzedniego najlepszego w danym rozmiarze	
	oldBestRank = population[[1]]$rank

	# warunki stopu i poczatkowe wartosci
	diff = 99999
	# licznosci modeli
	n <- N
	# licznik iteracji
	iterCount = 1
	# liczba rozszerzen modeli(iteracji) od ktorych utrzymuje sie najlepszy
	sameInIter = 0


	#########################
	#	Petla algorytmu		#
	#########################

	# iterujemy po maksymalnej licznosci podzbiorow
	# dopoki roznica pommiedzy pokoleniami > epsilon lub
	#	utrzymuje sie najlepszy od bestInIter zwiekszen modeli
	#	lub do uzytej maksymalnej liczby modeli

	while (diff > epsilon || sameInIter < bestInIter) {
		# wybieramy osobniki do krzyzowania
		toCopulate <- selection(population, nSelect)

		# krzyzujemy osobniki
		children <- copulation(toCopulate, predictions, dat, col, n)
		# mutujemy dzieci
		nextGeneration <- mutation(children, nMutate, predictions, dat, col, n)

		# rozszerzamy populacje
		population <- c(population, nextGeneration)

		# zostaw unikalne i posortuj
		population <- uniqueOrder(population)


		#####################
		#	Sprawdzenie		#
		#####################

		# sprawdz czy populacja nie urosla nam za bardzo
		if (length(population) > popSize) {
			# jesli jest za duza, odcinamy najslabsze osobniki
			population <- population[1:popSize]
		}

		# sprawdz czy jest lepszy od bierzacego
		if (bestOneRank > population[[1]]$rank) {
			bestOneRank <- population[[1]]$rank
			bestOneIter <- iterCount

			# ustaw ze osobnik utrzymuje sie od 0 zwiekszen modeli 
			sameInIter = 0
		}

		# sprawdz czy nie czas rozszerzyc populacje
		if (iterCount - bestOneIter > nIter) {

			#########################################################
			#	Rozszerzenie populacji - aktualizacja zmiennych		#
			#########################################################

			# resetujemy licznik iteracji
			iterCount = 1

			# sprawdz czy to juz nie koniec modeli (czy jest co rozszerzac)
			if(n == length(models)) {
				break
			} else {
				# zwieksz maksymalny rozmiar
				n = n + 1

				# oblicz roznice miedzy najlepszym w poprzednim osobnikiem a nowym
				diff = oldBestRank - bestOneRank

				# jezeli sie utrzymuje od poprzedniego rozszerzenia modeli to bedzie 0 -
				# tj. zwiekszamy licznik utrzymywania sie osobnika w kolejnych rozszerzeniach modeli
				if (diff == 0) {
					sameInIter = sameInIter + 1
				} else {
					# mamy nowego najelpszego osobinka wiec zapisujemy
					# aktualnego najlepszego jako poprzendiego
					oldBestRank = bestOneRank
				}
			}


			#####################################################
			#	Rozszerzenie populacji - zwiekszenie modeli		#
			#####################################################

			# a teraz dodaj do populacji osobiki o wieszej ilosci modeli
			# zeby dac wieksza szanse nowym osobnikom to pierwsza selekcja jest na rozszerzonej liscie (dlugosc max: popSize - nRemove + nExtend)

			# wybierz osobniki do rozszerzenia
			toExtend <- selection(population, nExtend)

			# ograniczenie rozmirau populacji
			population <- population[1:min(length(population), popSize - nRemove)]

			# generuj rozszerzone osobniki
			extended <- extendEntities(toExtend, predictions, dat, col)

			# dolacz rozszerzone osobniki do populacji
			population <- c(population, extended)

			# zostaw unikalne i posortuj
			population <- uniqueOrder(population)
		} else {
			# jak nie czas na zwiekszenie licznosci modeli to zwieksz licznik iteracji
			iterCount = iterCount + 1
		}
	}


	#################################################
	#	Koniec algorytmu - wyciagniecie wynikow		#
	#################################################

	result = list(genModels=models, models=list(), midPrediction=population[[1]]$midPrediction, rank=population[[1]]$rank)

	pos = 1
	for (i in 1:length(population[[1]]$predictionList)) {
		if (population[[1]]$predictionList[[i]]) {
			result$models[[pos]] <- models[[i]]
			pos = pos + 1
		}
	}


	#####################
	#	Debug stats		#
	#####################

	if (printStats) {
		print("Ilosc uzytych modeli:")
		print(length(result$models))

		print("Uzyskany blad:")
		print(result$rank)


		# tworzymy model, liczymy predykcje i blad sredniokwadratowy dla niezaburzonego zbioru danych
		modelAll <- genModels(list(dat), col, regression)[[1]]
		rankAll <- mse(predict(modelAll, dat), dat[,col])

		print("Blad dla calego zbioru danych:")
		print(rankAll)

		print("Uzyskana poprawa (jak dodania):")
		print(rankAll - result$rank)
	}

	return (result)
}

# funkcja testujaca algorytm
# dzieli zbior danych na dwa rozlaczne podzbiory
# powtarza zadana testCount ilosc razy algorytm na pierwszym zbiorze
# i oblicza blad srednio kwadratowy predykcji na zbiorze drugim
# oblicza takze blad srednio kwadratowy uzywajac modelu utworzonego na pelnym drugim zbiorze z nim samym
#
# testCount[int]	- ilosc powtorzen testu
# dat[data.frame]	- ramka z pierwotnymi danymi dla ktorych uruchamiamy algorytm
# mAmount[int]		- liczba zaburzonych modeli do wygenerowania, minimum 2
# regression[function]	- funkcja uzywana do utworzenia modelu regresji {lm|rpart}
# bestInIter[int]	- liczba iteracji zwiekszania licznosci modeli przez ktora musi sie zachowac najlepszy osobnik zeby zakonczyc dzialanie algorytmu
# N[int]			- liczba uzytych modeli w osobniku dla pierwszej populacji (kombinacje N elementowe uzytych modeli)
# col[string]		- kolumna(etykieta) ktora chcemy przewidywac, gdy -1 wybierana ostatnia kolumna
# epsilon[double]	- roznica w ocenie najlepszych osobnikow kolejnych populacji po osiagnieciu ktorej nastepuje koniec dzialania algorytmu
# nIter[int]		- liczba iteracji przez ktora najlepszy osobnik sie nie zmienia i po ktorej zostaje rozszerzona liczba modeli w osobnikach
# popSize[int]		- maksymalny rozmiar populacji
# nSelect[int]		- liczba osobnikow wybieranych do krzyzowania
# nMutate[int]		- liczba osobnikow wybieranych do mutacji
# nExtend[int]		- liczba osobnikow, ktore beda rozszerzane o kolejny model w zbiorze
# nRemove[int]		- liczba osobnikow najgorszych usuwanych z populacji (zostaje maksymalnie popSize-nRemove) - przy dodawaniu kolejnego elementu zbiorow
# return[list]	- lista list z wynikami dla poszczegolnych testow:
#			$selfResult[double]		- blad srednio kwadratowy z predykcji z modelu utworzonego z drugiego zbioru na nim samym
#			$meanResult[double]		- blad srednio kwadratowych ze sredniej z predykcji pojedynczych modeli na drugim zbiorze
#			$singleResults[vector]	- lista bledow srednio kwadratowych z predykcji pojedynczych modeli na drugim zbiorze
testAlhe = function(dat, testCount=10, mAmount=50, regression=lm, bestInIter=10, N=1, col=-1, epsilon=0.01, nIter=10, popSize=50, nSelect=10, nMutate=2, nExtend=25, nRemove=25) {
	# jak nie podano to wez ostatnia kolumne i przewiduj ja
	if (col == -1) {
		nms <- names(dat)
		col <- nms[length(nms)]
	}

	# zmienne wynikowe
	results <- list()

	# uruchamiamy testCount razy algorytm
	for (i in 1:testCount) {
		# podziel zbior danych na dwa wylosowane rozlaczne zbiory
		rws <- sample.int(nrow(dat), nrow(dat)/2)
		dat1 <- dat[rws,]
		dat2 <- dat[-rws,]

		z <- alhe(dat1, mAmount=mAmount, regression=regression, bestInIter=bestInIter, N=N, col=col, epsilon=epsilon, nIter=nIter, popSize=popSize, nSelect=nSelect, nMutate=nMutate, nExtend=nExtend, nRemove=nRemove, printStats=F)

		meanResult <- 0
		singleResults <- c()
		eachSingleResult <- 0

		# zeby obliczyc usredniona predykcje dla znalezionych modeli
		usedPredictions = c()
		firstOne = TRUE
		
		# liczymy blad dla kolejnych modeli i srednia z ich predykcji
		for (j in 1:length(z$models)) {
			prediction <- predict(z$models[[j]], dat2)

			if (firstOne) {
				usedPredictions <- cbind(prediction)
				firstOne = FALSE
			} else {
				usedPredictions <- cbind(usedPredictions, prediction)
			}
		}

		# policz srednia predykcje
		meanResult <- mse(rowMeans(usedPredictions), dat2[,col])
		nmbOfModels <- ncol(usedPredictions)

		usedPredictions = c()
		firstOne = TRUE

		# liczymy blad dla usrednienia wszystkich modeli
		for (j in 1:length(z$genModels)) {
			prediction <- predict(z$genModels[[j]], dat2)
			singleResults <- c(singleResults, mse(prediction, dat2[,col]))

			if (firstOne) {
				usedPredictions <- cbind(prediction)
				firstOne = FALSE
			} else {
				usedPredictions <- cbind(usedPredictions, prediction)
			}
		}

		# policz srednia predykcje
		eachSingleResult <- mse(rowMeans(usedPredictions), dat2[,col])


		results[[i]] <- list(meanResult=meanResult, nmbOfModels=nmbOfModels, minSingleResult=min(singleResults), eachMeanResult=eachSingleResult)
		
		if (i == 1) {
			print(c("Wynik naszego algorytmu", "Licznosc naszego modelu", "Minimum ze wszystkich modeli", "Nasz Lepszy od pojedynczego", "Usrednienie ze wszystkich modeli", "Nasz lepszy od wszystkich"))
		}
		
		betterThanOne <- 1
		betterThanAll <- 1
		if(meanResult - min(singleResults) >= 0) {
			betterThanOne <- 0
		}
		if(meanResult - eachSingleResult >= 0) {
			betterThanAll <- 0
		}
		print(c(meanResult, nmbOfModels, min(singleResults),betterThanOne, eachSingleResult, betterThanAll))

		# policz blad z modelu stworzeonego i wykonanego na czesci danych nie uzytych do algorytmu
		# modelAll <- genModels(list(dat1), col, regression)[[1]]
		# rankAll <- mse(predict(modelAll, dat1), dat1[,col])

		# results[[i]] <- list(selfResult=0, meanResult=0, singleResults=0)
		# blad srednio kwadratowy dla danych nie uzytych w algorytmie
		# results[[i]]$selfResult <- rankAll
		# blad srednio kwadratowy dla sredniej z zaburzonych modeli znalezionych w algorytmie
		# results[[i]]$meanResult <- meanResult
		# wektor bledow srednio kwadratowych dla pojedynczych modeli
		# results[[i]]$singleResults <- singleResults


		# print('----------------------------------------------')
		# print(z)
		# print('----------------------------------------------')
		# print(length(z$models))
		# print('----------------------------------------------')
		# print(rankAll)
		# print(meanResult)
		# print(rankAll - meanResult)
		# print('----------------------------------------------')
		# print(singleResults)
		# print('----------------------------------------------')
	}

print(c("Pojedynczy-nasz", "wszystkie - nasz", "Sredni rozmiar"))
	print(c(mean(sapply(results, function(r) r$minSingleResult - r$meanResult)),mean( sapply( results, function(r) r$eachMeanResult - r$meanResult)), mean(sapply(results, function(r) r$nmbOfModels))))
	return (results)
}


testAll = function()
{
	print(c("BestInIter zakres 1:20"))
	for(i in 1:20) {
		testAlhe(BostonHousing, 20, 50, regression=rpart, bestInIter=i)
	}
	
	print(c("nIter zakres 1:20"))
	for(i in 1:20) {
		testAlhe(BostonHousing, 20, 50, regression=rpart, nIter=i)
	}
	
	print(c("popSize zakres 10:150 co 10 nSelect= 0.2*popSize, nMutate=0.5*nSelect nExtend = popSize*0.5, nRemove=popSize*0.5"))
	for(i in 1:15) {
		testAlhe(BostonHousing, 20, 50, regression=rpart, popSize=i*10, nSelect=i*2, nMutate=i, nExtend=i*5, nRemove= i*5 )
	}
	
	print(c("selekcja zakres 10-100 co 5. popSize = 100, mutate 0.2, nExtend = popSize*0.5, nRemove=popSize*0.5"))
	for(i in 2:20) {
		testAlhe(BostonHousing, 20, 50, regression=rpart, popSize=100, nSelect=i*5, nMutate=i, nExtend=50, nRemove=50 )
	}
}

