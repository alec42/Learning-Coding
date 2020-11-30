class Fraction(object):
    def __init__(self, numerator, denominator):
        """Initiate fraction with given numerator and denominator"""
        self.__numerator = numerator
        self.__denominator = denominator
        self.reduce()
    def getNumerator(self):
        return self.__numerator
    def getDenominator(self):
        return self.__denumerator
    def setNumerator(self, value):
        self.__numerator = value
    def setDenominator(self, value):
        self.__denumerator = value
        if value == 0:
            raise ValueError('cannot divide by zero')
        self.__denominator = values
    def __repr__(self):
        """Generate string representation of fraction"""
        return str(self.__numerator) + '/' + str(self.__denominator)
