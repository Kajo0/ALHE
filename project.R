# Przykladowe wzglednie liniowe dane
a <- sampleData <- data.frame(age=18:29, height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5))
a1 <- data.frame(age=c(18,20,22,24), height=c(76.1,78.1,78.8,79.9))
a2 <- data.frame(age=c(19,21,23,25,27), height=c(77,78.2,79.7,81.1,81.8))
a3 <- data.frame(age=c(18,21,29), height=c(76.1,78.2,83.5))
a4 <- data.frame(age=c(19,20,24,29), height=c(77,78.1,79.9,83.5))
a5 <- data.frame(age=c(22,23,28), height=c(78.8,79.7,82.8))
la <- list(a1,a2,a3,a4,a5)
#sampleCombinations <- combn(1:1, 1)
#sampleModels <- list()
#sampleModels[[1]] <- lm(height~., sampleData)
##sampleModels[[1]] <- lm(height~., randRows(sampleData))
##sampleModels[[2]] <- lm(height~., randRows(sampleData))
##sampleModels[[3]] <- lm(height~., randRows(sampleData))
##sampleCombinations <- combn(1:3, 2)




# Oblicza blad sredniokwadratowy
# a			- pierwszy wektor danych
# b			- drugi wektor danych
# return	- obliczony blad
mse = function(a, b) {
	return (mean( (a - b)^2, na.rm = TRUE))
}

# Losujemy wiersze ze zwracaniem
# data		- data.frame z danymi
# return	- dafa.frame z losowymi wierszami
randRows = function(data) {
	return (data[sample(nrow(data), sample.int(nrow(data), 1), replace=T),])
}

# Losujemy rozne paczki wierszodanych
# data		- data.frame z danymi
# n			- ile modeli losujemy
# return	- lista wylosowanych data.frame
randDatas = function(data, n=3) {
	datas <- list()
	
	for (i in 1:n) {
		datas[[i]] <- randRows(data)
	}

	return (datas)
}

# Generujemy liste modeli dla zadanego parametru
# datas		- lista danych jako data.frame
# col		- nazwa kolumny do przewidywania
# return	- lista modeli
genModels = function(datas, col) {
	res <- list()

	frm <- paste(col, ".", sep=" ~ ")

	for (i in 1:length(datas)) {
		res[[i]] <- lm(formula(frm), datas[[i]])
	}
	
	return (res)
}

# Obliczamy przewidywania dla wszystkich pojedynczych modeli zeby potem operowac juz tylko na wartosciach
# models	- lista modeli
# data		- poczatkowe niezaburzone dane
# return	- lista predictow
calculateEachModel = function(models, data) {
	result <- list()
	
	for ( i in 1:length(models)) {
		result[[i]] <- predict(models[[i]], data)
	}
	
	return (result)
}

#zamienia format z tych wektorow na to na czym operujemy czyli:
#	phenotype	osobink
#		predictionList	lista wartosci boolowskich, ktore predykcje ten osobnik uwzglednia np {true, false, true} to osobnik z predykcji 1 i 3
#		midPrediction	usredniona predykcja
#		rank			blad srednio kwadratowy (minimalizujemy)
# Funkcja wypelnia do postaci gotowego osobnika i zwraca liste osobnikow
# liczy srednia predykcje i blad sredniokwadratowy (rank)
# predictions	predykcje dla kolejnych modeli
# population	wektory true, false z kotrych powstana osobiki
# data			poczatkowe, niezaburzone dane
# column		kolumna do przewidywania
calculatePredictions = function(predictions, population, data, column) {
	result <- list()
	
	for( i in 1:length(population)) {
		result[[i]] = list(predictionList=population[[i]], midPrediction=0, rank=0)
		
		usedPredictions = c()
		position = 1
		
		#znajdz wszystkie modele uzyte w tym osobniku
		for( j in 1:length(population[[i]])) {
			if(population[[i]][[j]]) {
				if(position ==1) {
					usedPredictions <- cbind(predictions[[j]])
					position <- position +1
				} else {
					usedPredictions <- cbind(usedPredictions, predictions[[j]])
				}
			}
		}
		
		#policz srednia predykcje i wrzuc ja w wektor kolumnowy
		result[[i]]$midPrediction <- rowMeans(usedPredictions)
		
		#policz blad sredniokwadratowy
		result[[i]]$rank <- mse(result[[i]]$midPrediction, data[,column])
	}
	
	return (result)
}

# Znajduje index listy w podanej liscie na ktorej blad srednio kw jest najmniejszy
# combined	- lista zbiorow modeli
# return	- indeks na liscie ktora kombinacja modeli ma najmniejszy blad srednio kw
findMinIndex = function(combined) {
	resIndex <- 1
	resMin <- combined[[1]]$mse

	for (i in 1:length(combined)) {
		if (resMin > combined[[i]]$mse) {
			resIndex <- i
			resMin <- combined[[i]]$mse
		}
	}

	return (resIndex)
}

# Generuje pierwsza populacje jako wsystkie kombinacje o rozmiarze maxymalnym maxNmbTrue
# maxNmbTrue	- maxymalnie tyle modeli zaliczonych do osobnika
# modelSize		- ogolna liczba modeli
generateFirstPopulation = function(maxNmbTrue, modelSize) {
	result = list()
	
	position = 1
	for (i in 1:maxNmbTrue) {
		combinations <- combn(1:modelSize, i)
		for(j in 1:ncol(combinations)) {
			result[[position]] <- 1:modelSize %in% combinations[,j]
			position <- position + 1
		}
	}
	
	return(result)
}

#Wybiera z populacji osobniki do kopulacji
# population	- populacja
# amount		- liczba osobnikow do wybrania
# return		- podlista listy population zawierajaca wybrane osobniki
selection = function(population, amount) {
	result <- population[sample.int(length(population), amount)]

	return (result)
}

#Rozszerza populacje o nowy element w kazdym zbiorze
# toExtend	- osobniki do rozszerzenia
# predictions - uzywane predykcje odpowiadajace modelom
# data			- nie zaburzone dane
# column			- przewidywana kolumna
#return			- nowe osobniki do populacji
extendEntities = function(toExtend, predictions, data, column) {
	
	newCombinations = list()
	for(i in 1:length(toExtend)) {
		newCombinations[[i]] <- toExtend[[i]]$predictionList
		
		unusedModels <- which(toExtend[[i]]$predictionList == F)
		if (length(unusedModels) == 1) {
			newCombinations[[i]][unusedModels[1]] <- T
		} else {
			newCombinations[[i]][sample(unusedModels, 1)] <- T
		}
		#unusedModels = c()
		#position =1
		##stworz liste modeli nie bedacych w tym zbiorze
		#for(j in 1:length(toExtend[[i]]$predictionList)) {
		#	if(! toExtend[[i]]$predictionList[[j]]) {
		#		unusedModel[[position]] <- j
		#		position <- position + 1
		#	}
		#}
		#
		##wylosuj element do dodania
		#newCombinations[[i]][[unusedModels[[sample.int(length(unusedModels))]]]] <- TRUE
	}
	
	result <- calculatePredictions(predictions, newCombinations, data, column)
	
	return (result)
}

# Kopuluje 2 zbiory
# set1		- pierwszy zbiór modeli
# set2		- drugi zbiór modeli
# maxTrue	- maksymalna licznosc nowego zbioru
copulateEntity = function(set1, set2, maxTrue, rank1, rank2) {
		##nie wiem jak to zrobic zeby wybieral z ustalonym prawdopodobienstwem
	sm = set1 | set2
	w1 <- which(set1 == T)
	w2 <- which(set2 == T)

	w <- unique(append(w1, w2))

	if (sum(sm) > maxTrue) {
		# usuwamy losowe wystepujace tylko w osobniku z mniejszym rankingiem
		if (rank1 < rank2) {
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
			sm[r] <- F
		} else {
			sm[sample(r, toRemoveAmount)] <- F
		}
	}

	return (sm)
}

#Krzyzowanie osobnikow
# toCopulate	- osobniki do krzyzowania
# predictions	- predykcje odpowiadajace modelom
# data			- nie zaburzone dane
# column		- kolumna
# maxTrue		- maksymalna liczba modeli w osobniku
#return			- osobniki po kopulacji
copulation = function(toCopulate, predictions, data, column, maxTrue) {
	newSets = list()
	#kopuluj kazdego z sasiadem po prawo
	
	for(i in 1:(length(toCopulate) - 1)) {
		newSets[[i]] <- copulateEntity(toCopulate[[i]]$predictionList, toCopulate[[i+1]]$predictionList, maxTrue,
										toCopulate[[i]]$rank, toCopulate[[i+1]]$rank)
	}
	#a ostatni z pierwszym
	newSets[[length(toCopulate)]] <- copulateEntity(toCopulate[[length(toCopulate)]]$predictionList, toCopulate[[1]]$predictionList, maxTrue,
													toCopulate[[length(toCopulate)]]$rank, toCopulate[[1]]$rank)
	
	result <- calculatePredictions(predictions, newSets, data, column)
	return (result)	
}

#Mutuje osobniki 
# generation	- osobniki sposrod ktorych trzeba wybrac te do mutacji
# nMutate		- liczba osobnikow do zmutowania
# predictions	- predykcje odpowiadajace elementom
# data			- poczatkowe nie zaburzone dane
# column		- przewidywana kolumna
# maxTrue		- maksymalna licznosc podzbioru
mutation = function(generation, nMutate, predictions, data, column, maxTrue) {
	#wszystkie ktore byly to beda
	result = generation
	newCombinations = list()
	position = 1

	for( i in 1:nMutate) {
		#wybierz osobnika do zmutowania
		toMutate <- generation[[sample.int(length(generation), 1)]]
		#wybierz gen do zmiany
		gen <- sample.int(length(toMutate$predictionList), 1)
		toMutate$predictionLis[[gen]] <- !toMutate$predictionLis[[gen]]
		
		#zlicz elementy w zbiorze
		trueFields <- c()
		for(j in 1:length(toMutate$predictionList)) {
			if(toMutate$predictionList[[j]]) {
				trueFields <- c(trueFields, j)
			}
		}
		#a jak jest ich za duzo to wywal losowo zeby bylo dobrze
		if(length(trueFields) > maxTrue) {
			toRemove = sample.int(length(trueFields), length(trueFields) - maxTrue)
			for(j in 1:length(toRemove)) {
				toMutate$predictionList[[j]] = !toMutate$predictionList[[j]]
			}
		}
		
		#dodaj do zmutowanych kombinacji
		newCombinations[[position]] <- toMutate$predictionList
		position = position + 1
	}

	#dodaj do tych co byly ich mutanty
	result <- c(result, calculatePredictions(predictions, newCombinations, data, column))
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
alhe = function(data, mAmount=5, N=1, col=-1, epsilon = 0.01, nIter=10, popSize=50, nSelect=10, nMutate=2, nExtend=25, nRemove=25) {
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
	#let's roll the ball
	#iterujemy po maksymalnej licznosci podzbiorow
	#do poki roznica pommiedzy pokoleniami > eps lub do liczby modeli
	while ( diff > epsilon ) {
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
		}
		
		#sprawdz czy nie czas rozszerzyc populacje
		if( iterCount - bestOneIter > nIter) {
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
				print(c(diff, oldBestRank, bestOneRank))
				
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

		print(c(n, diff))
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

	return (result)
}