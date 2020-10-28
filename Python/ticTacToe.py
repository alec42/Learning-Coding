#	Python Tic Tac Toe game

##	Table line seperators
##	
##	DESCRIPTION: 
##	Returns pattern of -+-+ for seperating line of a table. 
##	It is not very useful directly and is used conjunction 
##	with the Table class for the Tic Tac Toe game.
##		
##	EXAMPLE:
##	>>> print(TableLineSep(4))
##	-+-+-+-
def TableLineSep(n):
	assert n > 0
	return ("+-" * n)[1:]

##	Table line converter
##	
##	DESCRIPTION: 
##	Converts list of strings into a text string of the 
##	values seperated by "|". It is not very useful 
##	directly and is used in conjunction with the Table 
##	class for the Tic Tac Toe game.
##		
##	EXAMPLE:
##	>>> print(makeDataLines(["a", "b", "c"]))
##	a|b|c
def makeDataLines(values):
	return '|'.join(values) if values else ''


##	Table class
##	
##	DESCRIPTION:
##	Creates instances which are tables of size m (rows) 
##	by n (columns).
##	
##	NOTES:
##	+	Given python operates in lists and not matrices, the 
##		created object is in fact a list of length m * n. It 
##		can be visualised as a long list of all the columns 
##		pasted together end to end.
##	+	To access the item in row i of column j, we first get
##		to the point where row j starts : i * n then add j
##		to get the jth column. Thus, [i, j] => [i * n + j].
##	
##	EXAMPLE USAGE:
##	>>> testObject = Table(3, 2)
##	>>> testObject.setValues(['20', 'P', '20', 'FM', '21', "IFM"])
##	>>> print(testObject.getLine(2))
##	>>> print(testObject.getSize())
##	>>> print(testObject[1, 1])
class Table:
	def __init__(self, m, n):
		self.m, self.n = m, n
		self.data = [None for _ in range(self.m * self.n)]
	def __getitem__(self, index):
		i, j = index
		return self.data[i * self.n + j]
	def __setitem__(self, index, value):
		i, j = index
		self.data[i * self.n + j] = value
	def setValues(self, valuesIter):
		data = list(valuesIter)
		self.data = data
		return self
	def getLine(self, i):
		return self.data[(i*self.n):((i+1)*self.n)]
	def getSize(self):
		return self.m, self.n


##	Grid generator
##	
##	DESCRIPTION: 
##	Accepts an instance of the Table class as argument and
##	returns it as a table.
##	
##	EXAMPLE USAGE:
##	>>> testObject = Table(4, 3)
##	>>> testObject.setValues("X" for _ in range(12))
##	>>> print(gridGenerator(testObject))
##	X|X|X
##	-+-+-
##	X|X|X
##	-+-+-
##	X|X|X
##	-+-+-
##	X|X|X
##	
def gridGenerator(tableInst):
	m, n = tableInst.getSize()
	tableText = ""
	for row in range(m):
		if row != 0:
			tableText += TableLineSep(n) + "\n"
		tableText += makeDataLines(tableInst.getLine(row)) + "\n"
	return tableText



##	Tic Tac Toe class
##	
##	DESCRIPTION:
##	Creates game of Tic Tac Toe using predefined Table class.
##	
##	NOTES:
##	+	Based code for the win verifications on this post: https://stackoverflow.com/questions/39922967/python-determine-tic-tac-toe-winner
##	+	and this explanation oof generators: https://stackoverflow.com/questions/231767/what-does-the-yield-keyword-do
##	
##	EXAMPLE USAGE:
##	>>> 
##	>>> 
##	>>> 
##	>>> 
##	>>> 
class TicTacToe(Table):
	def __init__(self, m = 3, n = 3):
		#	By default, Tic Tac Toe is a 3x3 board. But, I'm leaving it 
		#	open in case I wanna get funky.
		self.m, self.n = m, n
		super().__init__(self.m, self.n)
		self.reset()

	def __str__(self):
		return gridGenerator(self)

	def reset(self):
		self.data = [' ' for _ in range(self.m * self.n)]
		return self

	def choose(self, symbol, i, j):
		if ((symbol not in {'X', 'O'}) or (self.data[i * self.n + j] in {'X', 'O'})):
			raise ValueError
		self.data[i * self.n + j] = symbol
		print(self)

	##	------------------------------------------------------------	##
	##	For a simple 3x3 board, we could hardcore ifelse statements to 
	##	check if a player has won. However, I prefer to generalize the
	##	verification procedure for a board of any size.
	##	
	##	The approach is thus to get the indexes of all possible win 
	##	combinations and return these lists as a generator with yield.
	##	Then, with another function, we check if a player has won.
	##	------------------------------------------------------------	##
	def winIndexes(self):
		##	get indexes of rows and columns:
		for row in range(self.m):
			yield [(row, col) for col in range(self.m)]
		for col in range(self.m):
			yield [(row, col) for row in range(self.m)]
		##	get indexes of diagonals (both directions)
		yield [(i, i) for i in range(self.m)]
		yield [(i, self.m - i - 1) for i in range(self.m)]
	def verifyWin(self, symbol):
		for indexes in self.winIndexes():
			if all(self.data[row * self.n + col] == symbol for row, col in indexes):
				return f'Player with symbol {symbol} has won'
		return False

testObject = TicTacToe()
testObject.choose("X", 0, 0)
# testObject.choose("X", 0, 1)
# testObject.choose("X", 0, 2)
# testObject.choose("O", 1, 0)
# testObject.choose("O", 1, 1)
# testObject.choose("O", 1, 2)
# print(testObject.getLine(0))
# print(testObject.verifyWin("O"))
# print(testObject.verifyWin("X"))


