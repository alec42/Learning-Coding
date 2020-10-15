##	List basics
emptyList = []
filledList = [1, 2, 3, 4]
print(emptyList, filledList)
print(list())

##	Accessing elements and properties of lists
print(filledList[2])
print(len(filledList))
print(filledList[2] + filledList[1])

##	Adding elements
emptyList.append("one")
print(emptyList)

##	Finding the position of elements
print(filledList.index(2))	#	finds position of the number 2 within the filled list

##	Inserting elements by position
print(filledList.insert(1, "two"))
print(filledList)

##	Removing elements
print(filledList.remove("two"))

print(emptyList)
del emptyList[0]
print(emptyList)

##	Extend the list with another list
print(emptyList)
emptyList.extend([1, "6"])
print(emptyList)

filledCharacterList = str(filledList)
print('OH'.join(filledCharacterList))