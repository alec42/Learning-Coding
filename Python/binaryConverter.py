##	Binary to decimal converter
##
##	DESCRIPTION:
##	Convert binary number given as a string into its 
##	decimal equivalent.
##	
##	EXAMPLE:
##	>>> bin2dec('11')
##	3
def bin2dec(binNumber):
	decNumber = 0
	for b in range(len(binNumber)):
		decNumber += int(binNumber[::-1][b]) * 2**b
	return decNumber
