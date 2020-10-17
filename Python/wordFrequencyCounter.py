##	Count frequency of different words in strings
##	
##	DESCRIPTION:
##	Count number of occurences of each word in a
##	list of strings.
##	
##	EXAMPLE:
##	>>> wordFreq(['this is on sentence', 'that is another different sentence', 'is this a sentence ?'])
##	{'this': 2, 'is': 3, 'on': 1, 'sentence': 3, 'that': 1, 'another': 1, 'different': 1, 'a': 1, '?': 1}
def wordFreq(wordList):
	wordSepList = ' '.join(wordList).lower().split()
	wordSepFreq = [wordSepList.count(word) for word in wordSepList]
	return dict(zip(wordSepList, wordSepFreq))

##	De façon alternative, on peut définir la fonction avec un for loop et la fonction
##	get(). On spécifie que si la clé (word) n'existe pas alors la valeur est de 0 à 
##	laquelle on ajoute 1. 
##
##	C'est plus efficace étonnament.
def wordFreqLoop(wordList):
	wordsDict = {}
	for sentence in wordList:
		wordsList = sentence.lower().split()
		for word in wordsList:
			wordsDict[word] = wordsDict.get(word, 0) + 1
	return wordsDict
