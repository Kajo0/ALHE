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
###calculateModels(sampleModels, sampleCombinations, "height", sampleData)




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

# Obliczamy przewidywania i sredni blad kwadratowy dla danych kombinacji modeli
# 	i zapisujemy kombinacje numerow modeli uzyte do predykcji
# models		- lista modeli
# combinations	- tabela kombinacji bez takich 1:1, 2:2 lub 3:1, bo jest 1:3 -> format jak to co zwraca funckja ?combn
# col			- nazwa kolumny ktora przewidujemy uzywane do bledu srednio kw
# data			- poczatkowe niezaburzone dane
# return		- lista wynikow z:
#					models	- wektor numerow uzytych modeli z podanej listy
#					predict	- srednia z predykcji dla kombinacji modeli (w szczegolnosci dla kombinacji pojedynczej - predict modelu)
#					mse		- obliczony blad srednio kw
calculateModels = function(models, combinations, col, data) {
	res <- list()
	
	for (i in 1:ncol(combinations)) {
		res[[i]] <- list(models=c(), predict=0, mse=0)

		for (j in 1:nrow(combinations)) {
			modelNum <- combinations[j,i]
			# numery na liscie uzytych modeli
			res[[i]]$models <- append(res[[i]]$models, modelNum)
			
			# przewidujemy
			prediction <- predict(models[[modelNum]], data)
			
			if (j == 1) {
				res[[i]]$predict <- prediction
			} else {
				res[[i]]$predict <- cbind(res[[i]]$predict, prediction)
			}
		}

		# srednia z predykcji
		res[[i]]$predict <- rowMeans(cbind(res[[i]]$predict, prediction))
		
		# obliczamy blad srednio kwadratowy
		res[[i]]$mse <- mse(res[[i]]$predict, data[,col])
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

# Sortuje liste wg wartosci z podanej etykietki
# data		- lista danych
# col		- etykietka
# return	- posortowana lista
bubleListSortViaCol <- function(data, col) {
	itemCount <- length(data)

	repeat {
		hasChanged <- FALSE
		itemCount <- itemCount - 1
		if (itemCount == 0)
			break
		for(i in 1:itemCount) {
			if ( data[[i]][[col]] > data[[i+1]][[col]] ) {
				t <- data[[i]]
				data[[i]] <- data[[i+1]]
				data[[i+1]] <- t
				hasChanged <- TRUE
			}
		}
		if ( !hasChanged ) break;
	}

	return (data)
}

# Nasz program
# data		- data frame z danymi
# mAmount	- ile zaburzonych modeli generujemy
# N			- generuj kombinacje od N elementowych
# col		- kolumna do przewidywania
# epsilon	- jak roznica miedzy najlepszym z kolejnych rozmiarow jest mniejsza od tego to stop
# nIter		- liczba iteracji przez ktora musi zachowac sie najlepszy
# popSize	- rozmiar populacji
# return	- ??? to co bedize na koncu

alhe = function(data, mAmount=5, N=1, col=-1, epsilon = 0.01, nIter=10, popSize=50) {
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
	
	#po sortuj od najlepszego do najgorszego
	population <- bubleListSortViaCol(population, "rank")#population[order(population$rank),]
	
	#zapamietaj ranking najlepszego
	bestOneRank <- population[[1]]$rank
	bestOneIter <- 1
	
	#dane warunku stopu
	diff = 999
	n <- N
	iterCount = 1
	#let's roll the ball
	#iterujemy po maksymalnej licznosci podzbiorow
	#do poki roznica pommiedzy pokoleniami > eps lub do liczby modeli
	while ( diff > epsilon && n <= length(models) ) {
		print(n)
		print("------------------------------")
		toCopulate <- selection(population)
		children <- copulation(toCopulate)
		nextGeneration <- mutation(children)
		
		#teraz trzeba dorzucic next gen do population i zeby bylo posortowane
	
		#sprawdzamy besta
		if(bestOneRank > population[[1]]$rank) {
			bestOneRank <- population[[1]]$rank
			bestOneIter <- iterCount
		}
		
		#sprawdz czy nie czas rozszerzyc populacje
		if( iterCount - bestOneIter > nIter) {
			
		} else {
			#jak nei czas to zwieksz licnzik iteracji
			iterCount <- iterCount +1
		}
		
	}
	
	#stworzmy wynik
	result = list(models=list(), midPrediction=population[[1]]$midPrediction, mse=population[[1]]$rank)
	position = 1
	for(i in length(population[[1]]$predictionList)) {
		if(population[[1]]$predictionList[[i]]) {
			reslt[[1]]$models[[position]] <- models[[i]]
			position <- position +1
		} 
	}
	
	#tylko dla debugu#############################################################
	print("Wynik:")
	print(population[[1]]$predictionList)
	print(population[[1]]$rank)
	##############################################################################

	return (result)
}