import os, re
from BeautifulSoup import BeautifulSoup
# very helpful: http://fredericiana.com/2010/10/08/decoding-html-entities-to-text-in-python/
import HTMLParser

abstracts = ""
file("abstracts.txt","w").write("")
for i in range(1,995): 
	folder_name = "cogsci2013.htmls/" + str(i) + ".html"	
	print folder_name
	file_contents = file(folder_name,'r').read() # read the index.html file
	soup = BeautifulSoup(file_contents) # parse the HTML
	if (len(soup.find('p',attrs={'id':'abstract'}))>0):
		abstract = soup.find('p',attrs={'id':'abstract'}).contents[0].lower()
		abstract = re.sub("\n"," ",abstract)
		# beyond HTML entities using HTMLPareser; the encoding is readable by R
		file("abstracts.txt","a").write(HTMLParser.HTMLParser().unescape(abstract).encode('utf8')+"\n")
	else:
		file("abstracts.txt","a").write("the\n")

