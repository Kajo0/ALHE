#####################################
#	Ladowanie pakietow i danych		#
#####################################

# Ladujemy pakiet z przykladowymi zbiorami danych
library(mlbench)

# Ladujemy pakiet do uzycia potem regresji na strukturze drzewiastej, a nie liniowej
library(rpart)

# Ladujemy przykladowe dane
data(BostonHousing)


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

# Losuje wiersze ze zwracaniem
# Co najmniej 2 w celu wyeliminowania wartosci NA dla modelu regresji
#
# dat[data.frame]	- dane do losowania z nich wierszy
#
# return[data.frame]	- wylosowane wiersze (co najmniej 2)
randRows = function(dat) {
	while (TRUE) {
		r <- dat[sample(nrow(dat), sample.int(nrow(dat), 1), replace=T),]

		if (nrow(r) > 2) {
			break
		}
	}

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
# dat[data.frame]	- pierwotne dane z ktorych generowalismy zaburzone zbiory danych
# col[string]		- nazwa kolumny(etykiety) do przewidywania
# reg[function]		- funkcja uzywana do utworzenia modelu regresji {lm|rpart}
#
# return[list]	- lista utworzonych modeli dla zadanych zbiorow danych
genModels = function(dat, col, reg=lm) {
	res <- list()

	# przewidujemy podana kolumne na podstawie wszystkich innych
	frm <- paste(col, ".", sep=" ~ ")

	for (i in 1:length(dat)) {
		res[[i]] <- reg(formula(frm), dat[[i]])
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
	result <- population[sample.int(length(population), amount)]

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

	w <- unique(append(w1, w2))

	if (sum(sm) > maxTrue) {
		# usuwamy losowe wystepujace tylko w osobniku z mniejszym rankingiem (wiekszym bledem)
		if (rank1 > rank2) {
			r = w1
		} else {
			r = w2
		}

		toRemoveAmount = sum(sm) - maxTrue

		# jak wiecej do usuniecia niz jest w gorszym to usuwamy losowo z dwoch
		if (toRemoveAmount > length(r)) {
			r = w
		}

		if (length(r) == 1) {
			sm[r] <- FALSE
		} else {
			sm[sample(r, toRemoveAmount)] <- FALSE
		}
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

# Nasz program
# data		- data frame z danymi
# mAmount	- ile zaburzonych modeli generujemy
# N			- generuj kombinacje od N elementowych
# col		- kolumna do przewidywania
# epsilon	- jak roznica miedzy najlepszym z kolejnych rozmiarow jest mniejsza od tego to stop
# nIter		- liczba iteracji przez ktora musi zachowac sie najlepszy
# popSize	- rozmiar populacji
# nSelect	- liczba wybieranych do kopulacji
# nMutate	- liczba mutowanych
# nExtend	- liczba tych, ktore sa rozszerzane o kolejny element
# nRemove	- liczba najgorszych usuwanych z populacji (zostaje maksymalnie popSize-nRemove) przy dodawaniu kolejnego elementu zbiorow
# return	- lista w formacie:
#				models - wybrane modele
#				midPrediction - srednia predykcja
#				mse	- blad sredniokwadratowy
alhe = function(data, mAmount=5, N=1, bestInIter=10, col=-1, epsilon = 0.01, nIter=10, popSize=50, nSelect=10, nMutate=2, nExtend=25, nRemove=25) {
	if (col == -1) {
		nms <- names(data)
		col <- nms[length(nms)]
	}
	
	#podukuj dane
	datas <- randDatas(data, mAmount)
	
	#znajdz modele
	models <- genModels(datas, col)

	#policz dla nich predicty
	predictions <- calculateEachModel(models, data)
	
	#przygotuj pierwsza populacje
	#generuj osobnikow
	pop <- generateFirstPopulation(N, length(models))
	#a teraz wez je i policz dla nich srednie predykcje i bledy
	# i nadaj im wlasciwa strukture
	population <- calculatePredictions(predictions, pop, data, col )
	
	#zostaw tylko te unikalne osobniki
	population <- population[!duplicated(lapply(population, function(p) p$predictionList))]
	
	#po sortuj od najlepszego do najgorszego
	population <- population[order(sapply(population, function(p) p$rank))]
	
	#zapamietaj ranking najlepszego
	bestOneRank <- population[[1]]$rank
	bestOneIter <- 1
	
	#ranking poprzedniego najlepszego w danym rozmiarze
	diff = 99999
	
	#dane warunku stopu
	oldBestRank = population[[1]]$rank
	n <- N
	iterCount = 1
	
	sameInIter = 0
	
	#let's roll the ball
	#iterujemy po maksymalnej licznosci podzbiorow
	#do poki roznica pommiedzy pokoleniami > eps lub do liczby modeli
	while ( diff > epsilon || sameInIter < bestInIter) {
#		print(n)
#		print("------------------------------")
		toCopulate <- selection(population, min(nSelect, length(population)))
		
		children <- copulation(toCopulate, predictions, data, col, n)
		nextGeneration <- mutation(children, nMutate, predictions, data, col, n)
		
		#teraz trzeba dorzucic next gen do population
		population <- c(population, nextGeneration)
		
		#zostawic tylko unikalne osobniki
		population <- population[!duplicated(lapply(population, function(p) p$predictionList))]

		#posortowac wedlug rankingu (najlepszy na poczatku)
		population <- population[order(sapply(population, function(p) p$rank))]
		
		#sprawdz czy populacja nie urosla nam za bardzo
		if(length(population) > popSize) {
			#jesli jest za duza to trzeba odciac ostatnich zeby bylo ok
			population <- population[1:popSize]
		}
		
		#sprawdzamy besta
		if(bestOneRank > population[[1]]$rank) {
			bestOneRank <- population[[1]]$rank
			bestOneIter <- iterCount
			
			sameInIter = 0
		}
		
		#sprawdz czy nie czas rozszerzyc populacje
		if( iterCount - bestOneIter > nIter) {
			iterCount = 1
			#sprawdz czy to juz nie koniec modeli
			if( n == length(models)) {
				break
			} else {
				#zwieksz maksymalny rozmiar
				n <- n+1
				
				#oblicz roznice miedzy najlepszym w poprzednim rozmiarza a nowym
				diff = oldBestRank - bestOneRank
#TODO tu zazwyczaj 0 jest bo najlepszy jest zzawsze ten sam
				print("!!!!!!!!!!!!!")
				print(c(diff, oldBestRank, bestOneRank, sameInIter))
				
				if (diff == 0) {
					sameInIter = sameInIter + 1
				}
				
				#zapisz aktualnego najlepszego jako poprzendiego
				oldBestRank = bestOneRank
			}
			
			#a teraz dodaj do populacji osobiki o wieszej ilosci modeli
			toExtend <- selection(population, min(length(population), nExtend))
			population <- population[1:min(length(population), popSize - nRemove)]
			extended <- extendEntities(toExtend, predictions, data, col)
			
			#teraz dorzuc to do populacji
			population <- c(population, extended)
			
			#zostawic tylko unikalne osobniki
			population <- population[!duplicated(lapply(population, function(p) p$predictionList))]

			#posortowac wedlug rankingu (najlepszy na poczatku)
			population <- population[order(sapply(population, function(p) p$rank))]
			
			#zeby dac wieksza szanse nowym osobnikom to pierwsza selekcja jest na rozszerzonej liscie (dlugosc max: popSize - nRemove + nExtend)
		} else {
			#jak nei czas to zwieksz licnzik iteracji
			iterCount <- iterCount +1
		}

		print(c(n, diff, sameInIter))
	}
	
	#stworzmy wynik
#	result = list(models=list(), midPrediction=population[[1]]$midPrediction, mse=population[[1]]$rank)
#	position = 1
#	for(i in length(population[[1]]$predictionList)) {
#		if(population[[1]]$predictionList[[i]]) {
#			result[[1]]$models[[position]] <- models[[i]]
#			position <- position +1
#		} 
#	}
	
	#tylko dla debugu#############################################################
	print("Wynik:")
#	print(population[[1]]$predictionList)
	print(population[[1]]$rank)
	##############################################################################

#	return (result)
}