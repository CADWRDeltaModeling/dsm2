import urllib, urlparse, string
# get info about cdec stations

if __name__ == "__main__":
    file1 = open("./cdec_stas.txt")
    for line in file1:
        if line:
            sta = line.split("\t")[0]
	    url = 'http://cdec4gov.water.ca.gov/cgi-progs/staMeta?station_id=' + sta
	    file = sta + ".html"
	    print file
	    urllib.urlretrieve(url,file)
    file1.close()
