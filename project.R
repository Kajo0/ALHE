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
randDatas = function(data, n=5) {
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

# Obliczamy srednia z predykcji i zwracamy liste sredniej predykcji i modelu
# models	- lista modeli
# data		- dane do ktorych zaburzonych podalismy modele
# return	- lista list ze srednia predykcji i modelem // ((mean=.., model=..), (...) ...)
meanPreds = function(models, data) {
	res <- list()
	
	for (i in 1:length(models)) {
		res[[i]] <- list(mean=mean(predict(models[[i]], data)), model=models[[i]])
	}

	return (res)
}

# Nasz program
# data		- data frame z danymi
# col		- kolumna do przewidywania
# return	- ??? to co bedize na koncu
alhe = function(data, col=-1) {
	if (col == -1) {
		nms <- names(data)
		col <- nms[length(nms)]
	}
	
	datas <- randDatas(data)
	models <- genModels(datas, col)
	res <- meanPreds(models, data)

	return (res)
}

# Oblicza blad sredniokwadratowy
# a			- pierwszy wektor danych
# b			- drugi wektor danych
# return	- obliczony blad
mse = function(a, b) {
	return (mean( (a - b)^2, na.rm = TRUE))
}