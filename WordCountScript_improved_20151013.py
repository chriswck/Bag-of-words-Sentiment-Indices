import csv
import re



"""
@INTPUT:
	positive: the path of given word list 
	
@OUTPUT:
	a dictionary, with @key = word, @value = number of appearances
"""
def readPositiveOrNegativeWord(wordfile):
	wordfiledict = {}
	f = open(wordfile, "r")
	for line in f:
		word = line.strip()
		wordfiledict[word] = 0
	return wordfiledict
      

"""
@INTPUT:
	filename: the path of given word list 
	positive: a dictionary with @key = positive_word, @value = ocurrences
	negative: a dictionary with @key = negative_word, @value = ocurrences
	
@OUTPUT:
	two list of tuples
	e.g:
		l_positive should be something like:
		[(keyword1, keyword2, keyword3), (1, 0, 0), (0, 1, 0), (0, 0, 1)],
		meaning the first file has keyword1 appeared one time
				the second file has keyword2 appeared one time
				the third file has keyword3 appeared one time
"""
def process(filename, positive, negative):

	l_positive = []
	l_negative = []
	sum_positive = ['Total Positive Words']
	sum_negative = ['Total Negative Words']

	namelist = []
	for key in sorted(positive):
		namelist.append(key)
	l_positive.append(tuple(namelist))

	namelist = []
	for key in sorted(negative):
		namelist.append(key)
	l_negative.append(tuple(namelist))


	f = open(filename, 'r')

	count = 1
	give_way = 0
	
	for line in f:
		# start = re.match("CLM Abreast of the Market", line)
		#end = re.match("AN", line)
		# if start:
		# 	print "starting new passage"
		
		if re.match('Document (awsj|AWSJ)0', line):
			count = count+1
			print "starting new passage",count, 'of', filename
			# convert two dictionaries' values as two sorted list
			# then append respectively to the list above
			l_positive.append(tuple(getValuesAsOrderedList(positive)))
			l_negative.append(tuple(getValuesAsOrderedList(negative)))
			sum_positive.append(sumPositiveNegative(positive))
			sum_negative.append(sumPositiveNegative(negative))

			#clear the two dictionaries
			resetDict(positive)
			resetDict(negative)
			give_way = 0
		
		if re.match("LP ", line): give_way = 1
		
		if give_way == 1:
			words = re.split(',|:|"|\.|!|-|\?| |\(|\)', line)
			
			for word in words:
				word = word.upper()
				if word in positive.keys():
					positive[word] += 1
				elif word in negative.keys():
					negative[word] += 1
				else:
					continue			

	return l_positive, l_negative, sum_positive, sum_negative 
			
def getDate(filename):
        with open(filename, 'r') as f:
                datelist = ["Date"]
                for line in f:
                        found = re.match('PD ', line)
                        if found:
                                datestr = line[len('PD ')+1:]
                                datelist.append(datestr.strip())
        return datelist

def total(filename):
        with open(filename, 'r') as f:
                total = ['Total Words/Article']
                for line in f:
                        found = re.match('WC ', line)
                        if found:
                                myanswer = line[len('WC ')+1:line.find('word')]
                                total.append(myanswer.strip())                                
        return total

def RatioOfWords(filename, sumpn):
    tots = total(filename)
    r = [float(x)/float(y) for x,y in zip(sumpn[1:],tots[1:])]
    ratio = ['Ratio'] + r
    return ratio

"""
This is a helper function to be used in process()

@INTPUT:
	dictionary_name: a dictionary with @key = key words, @value = ocurrences
	
@OUTPUT:
	a list of key values, ordered.
"""

def getValuesAsOrderedList(dictionary_name):
	mylist = []
	for key in sorted(dictionary_name.keys()):
		mylist.append(dictionary_name[key])
	return mylist

def sumPositiveNegative(dictionary_name):
        sumpn = sum(dictionary_name.values())
        return sumpn
        


"""
This is a helper function used in process()
Basically reset all values (meaning number of ocurrences) to zero
"""
def resetDict(dictionary_name):
	for key in dictionary_name:
		dictionary_name[key] = 0
    

def outputAsCSV(outputfilename, listname, sumpn):
        rotated_list = zip(*listname)
        with open(outputfilename, "wb") as f:
                writer = csv. writer(f,delimiter = ',')
                writer.writerow(getDate(filename))
                for element in rotated_list:
                        writer.writerow(element)
                writer.writerow(sumpn)
                writer.writerow(total(filename))
                writer.writerow(RatioOfWords(filename,sumpn))
                     

def outputAsCSVnorotation(outputfilename, listname, sumpn):
        with open(outputfilename, "wb") as f:
                writer = csv.writer(f,delimiter = ',')
                writer.writerow(getDate(filename))
                writer.writerow(sumpn)
                writer.writerow(total(filename))
                writer.writerow(RatioOfWords(filename,sumpn))

#%%
positivewordfile = 'Positive.txt'
negatiewordfile = 'Negative.txt'
positive = readPositiveOrNegativeWord(positivewordfile)
negative = readPositiveOrNegativeWord(negatiewordfile)

for i in range(2004,2005):
    filename = str(i) + '.txt'
    l_positive, l_negative, sum_positive, sum_negative = process(filename, positive, negative)
    outputAsCSVnorotation(('p'+str(i)+'.csv'),l_positive, sum_positive)
    outputAsCSVnorotation(('n'+str(i)+'.csv'),l_negative, sum_negative)

#%%
fruit_list = ['raspberry', 'apple', 'strawberry']
[ i for i, word in enumerate(fruit_list) if word.endswith('berry') ]
#outputAsCSV('positive1999.csv', l_positive, sum_positive)
#outputAsCSV('negative1999.csv', l_negative, sum_negative)
