##	Leap year identifier
##
##	DESCRIPTION:
##	Identifies if a given year is a leap year.
##	
##	EXAMPLE:
##	>>> leapYearCheck(2000)
##	True
def leapYearCheck(year):
	assert year%1 == 0
	return (year%4 == 0 and (year%100 != 0 or year%400 == 0))

##	Time calculator between years
##	
##	DESCRIPTION:
##	Calculates days, hours, minutes and seconds between two
##	given years returned as a tuple. Takes into account leap
##	years.
##	
##	EXAMPLE:
##	>>> leapYearTime((2000, 2002))
##	(1096, 26304, 1578240, 94694400)
def leapYearTime(years):
	year1, year2 = years
	assert year1%1 == 0 and year2%1 == 0
	days = 0
	for year in range(year1, year2+1):
		days += 365
		if leapYearCheck(year):
			days += 1
			
	return days, days*24, days*24*60, days*24*60*60
