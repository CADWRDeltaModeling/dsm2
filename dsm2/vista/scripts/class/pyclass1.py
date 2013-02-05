# Contributed by Edward Chang
def maxmm(n):
    """
    a function to return the maximum of a list of numbers
    """
    i=0
    temp=0
    temp2=0
    while i<len(n):
        if temp<n[i]: temp2 = n[i]
        else: temp2=temp
        temp=temp2
        print 'i equals: ',i, ', n[i] equals: ', n[i], ', current max is: ', temp
        i=i+1
    print 'total max is: ', temp
#
def readsum(n):
    """
    a function to sum up a given list of numbers from a list
    """
    i=0
    temp=0
    while i<len(n):
        temp2=temp+n[i]
        temp=temp2
        print 'element: ', i, ', value of element is: ',n[i], ', current sum:', temp2
        i=i+1
    print 'final sum: ', temp2
#
def readfilesum(n):
    """
    a function to read a column of numbers from a file and write the total
    sum to the console
    """
    import string
    f=open(n,'r')
    lines = f.readlines()
    i=0
    temp=0
    temp2=0
    while i<len(lines):
        temp2=temp+string.atof(lines[i])
        temp=temp2
        print 'element: ', i, ', value of element is: ', lines[i], ', current sum:', temp2
        i=i+1
    print 'final sum: ', temp2
    f.close()
#
def readfile2sum(n):
    """
    a function to read a two column of numbers from a file and write the sum
    sum to another file
    """
    import string
    f=open(n,'r')
    g=open('sumcol.txt', 'w')
    lines = f.readlines()
    i=0
    while i<len(lines):
        columns = string.split(lines[i])
        print i, columns
        if len(columns) < 2: break
        sumcol = string.atof(columns[0]) + string.atof(columns[1])
        print sumcol
        i=i+1
        g.write(repr(sumcol)+'\n')
    print 'done'
    f.close()
    g.close()
#
