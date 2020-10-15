##	Create grid
##	
##	DESCRIPTION:
##	Creates empty grid of dimensions specified by the tuple
##	dim (rows, columns) and with each column being of width
##	colWidth = 3 by default.
##	
##	EXAMPLE:
##	>>> print(grid((2, 3), 3))
##	 -----------
##	|   |   |   |
##	 -----------
##	|   |   |   |
##	 -----------
def grid(dim, colWidth = 3):
	nRows, nCols = dim
	if ((nRows <= 0) or (nCols <= 0) or (colWidth <= 0)):
		return None

	gridOutput = ''
	for row in range(nRows):
		if row == 0:
			gridOutput += " " + "-" * ((colWidth * nCols) + (nCols - 1)) + "\n"
		gridOutput += "|" + (" " * colWidth + "|") * nCols + "\n"
		gridOutput += " " + "-" * ((colWidth * nCols) + (nCols - 1)) + "\n"
	return gridOutput

