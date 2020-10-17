##	Dictionnary of morse code
morseCodeDict = {
    'A': '.-',    'B': '-...',  'C': '-.-.',  'D': '-..',
    'E': '.',     'F': '..-.',  'G': '--.',   'H': '....',
    'I': '..',    'J': '.---',  'K': '-.-',   'L': '.-..',
    'M': '--',    'N': '-.',    'O': '---',   'P': '.--.',
    'Q': '--.-',  'R': '.-.',   'S': '...',   'T': '-',
    'U': '..-',   'V': '...-',  'W': '.--',   'X': '-..-',
    'Y': '-.--',  'Z': '--..',  '1': '.----', '2': '..---',
    '3': '...--', '4': '....-', '5': '.....', '6': '-....',
    '7': '--...', '8': '---..', '9': '----.', '0': '-----',
}

##	Convert text to morse code
##	
##	DESCRIPTION:
##	Converts given string of text into morse code. Code is
##	seperated by | and each letter by a space.
##	
##	EXAMPLE:
##	>>> textMorseConv("AleC was here")
##	'.- .-.. . -.-. | .-- .- ... | .... . .-. .'
##	
def textMorseConv(text):
	morseWords = []
	for word in text.upper().split():
		morseLetters = []
		for letter in word:
			morseLetters.append(morseCodeDict[letter])
		morseWords.append(' '.join(morseLetters))
	return ' | '.join(morseWords)
