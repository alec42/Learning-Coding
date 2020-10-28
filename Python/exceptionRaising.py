def boundVerification(number, bounds):
	'''
	Bound verification
		
	DESCRIPTION:
		Checks that a number is within a given bounds
	'''
	lower, upper = bounds
	if (number < lower or number > upper):
		raise ValueError(f"The given number {number} is not within the range ({lower}, {upper})")
	return number