class Table:
    def __init__(self, m, n):
        if ((m <= 0) or (n <= 0)):
            raise AssertionError
        self.m, self.n = m, n
        self.data = [None for _ in range(self.m * self.n)]
    
    def __getitem__(self, index):
        i, j = index
        if ((i < 0) or (i >= self.m) or (j < 0) or (j >= self.n)):
            raise IndexError
        return self.data[i * self.n + j]
    
    def __setitem__(self, index, value):
        i, j = index
        if ((i < 0) or (i >= self.m) or (j < 0) or (j >= self.n)):
            raise IndexError
        self.data[i * self.n + j] = value
    
    def set_values(self, valuesIter):
        data = list(valuesIter)
        if (len(data) != self.m * self.n):
            raise ValueError
        self.data = data
        return self
    
    def get_line(self, i):
        if ((i < 0) or (i >= self.m)):
            raise IndexError
        return self.data[(i*self.n):((i+1)*self.n)]
    
    def get_size(self):
        return self.m, self.n
    
    def grid(self, dim):
        m, n = dim
        if ((m <= 0) or (n <= 0)):
            raise AssertionError
        
        gridOutput = ''
        for row in range(n):
            if row == 0:
                gridOutput += " " + "-" * ((3 * n) + (m - 1)) + "\n"
            gridOutput += "|" + (" " * 3 + "|") * n + "\n"
            gridOutput += " " + "-" * ((3 * n) + (m - 1)) + "\n"
        return gridOutput