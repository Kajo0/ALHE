# Przykladowe wzglednie liniowe dane
#sampleData <- data.frame(age=18:29, height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5))
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


# Nasz program
# data		- data frame z danymi
# col		- kolumna do przewidywania
# return	- ??? to co bedize na koncu
alhe = function(data, N=1, col=-1) {
	if (col == -1) {
		nms <- names(data)
		col <- nms[length(nms)]
	}
	
	datas <- randDatas(data, 5)
	models <- genModels(datas, col)

	er = 999
	epsilon = 1
	while (er > epsilon && N <= length(models)) {
	print(N)
	print("------------------------------")
		combinations <- combn(1:length(models), N)
		combined <- calculateModels(models, combinations, col, data)
		
		print(combined)
		##
		#er = 1
		##
		N = N + 1
	}
	
	res <- 0
	#res <- meanPreds(models, data, col)

	return (res)
}