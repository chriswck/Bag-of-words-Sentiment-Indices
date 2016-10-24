import csv
import re

"""
@INTPUT:
	positive: the path of given word list 
	
@OUTPUT:
	a dictionary, with @key = word, @value = number of appearances
"""
def readPositiveOrNegativeWord(keywordfile):
	countries = {}
	f = open(keywordfile, "r")
	for line in f:
		word = line.strip()
		countries[word] = 0
	return countries


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
def process(filename, countries):

	l_key = []
	sum_key = ['Total Key Words']
	

	namelist = []
	for word in sorted(countries):
		namelist.append(word)
	l_key.append(tuple(namelist))

	f = open(filename, "r")

	count = 0

	for line in f:
		# start = re.match("Document \d+ of \d+", line)
		#end = re.match("^AN ", line)
		# if start:
		# 	print "starting new passage"
                
		if re.match('Document (awsj|AWSJ)0', line):
			count += 1
			print 'starting new passage', count, 'of', filename
			# convert two dictionaries' values as two sorted list
			# then append respectively to the list above
			l_key.append(tuple(getValuesAsOrderedList(countries)))
			sum_key.append(sumPositiveNegative(countries))
			#clear the two dictionaries
			resetDict(countries)
			
		else:
			for each in countries.keys():
				if each in line.upper():
					countries[each] += 1

	return l_key, sum_key
			
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

"""
This is a helper function to be used in process()

@INTPUT:
	dictionary_name: a dictionary with @key = key words, @value = ocurrences
	
@OUTPUT:
	a list of key values, ordered.
"""

def getValuesAsOrderedList(dictionary_name):
	mylist = []
	for word in sorted(dictionary_name.keys()):
		mylist.append(dictionary_name[word])
	return mylist

def sumPositiveNegative(dictionary_name):
        sumpn = sum(dictionary_name.values())
        return sumpn
        
"""
This is a helper function used in process()
Basically reset all values (meaning number of ocurrences) to zero
"""

def resetDict(dictionary_name):
	for word in dictionary_name:
		dictionary_name[word] = 0
  

def outputAsCSV(outputfilename, listname, sumpn):
        rotated_list = zip(*listname)
        with open(outputfilename, "wb") as f:
                writer = csv. writer(f,delimiter = ',')
                writer.writerow(getDate(filename))
                for element in rotated_list:
                        writer.writerow(element)
                writer.writerow(sumpn)
                writer.writerow(total(filename))
                

def outputAsCSVnorotation(outputfilename, listname, sumpn):
        with open(outputfilename, "wb") as f:
                writer = csv.writer(f,delimiter = ',')
                writer.writerow(getDate(filename))
                writer.writerow(sumpn)
                writer.writerow(total(filename))


#%%
keywordfile = "CountryListUpdated_With Edits.txt"
key = readPositiveOrNegativeWord(keywordfile)

for i in range(2004,2005):
        try:
                filename = str(i) + '.txt'
        except:
                continue
        l_key, sum_key = process(filename, key)
        outputAsCSV(('CountryCheck'+str(i)+'.csv'), l_key, sum_key)
	
