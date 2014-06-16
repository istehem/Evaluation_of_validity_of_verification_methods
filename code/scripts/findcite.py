import os
import re

def genDict():
    d = dict()
    for root, dirs, files in os.walk("./LexGen/Report"):
    #for root, dirs, files in os.walk("./Evaluation_of_validity_of_verification_methods/report/final_report"):
        for file in files:
            if file.endswith(".tex"):
                d = findCite(d,os.path.join(root, file))
    return d

def findCite(d,fp):
    f = open(fp)
    string = f.read()
    f.close()
    p = re.compile('\\cite{([^}]*)}')
    xs = p.findall(string)
    for i in xs:
        if i in d:
            d[i] = d[i] + 1
        else:
            d[i] = 1
    return d

def printRes(d):
    refs = 0
    xs = []
    for k in d.keys():
        xs.append((d[k],k))
        refs = refs + d[k]
    n = len(xs)
    xs = reversed(sorted(xs))
    for i,s in xs:
        print ("%-030s: %02i" % (s, i))
    print ('#'*50)
    print ('totel number of references:  %i' % (refs))
    print ('number of unique references: %i' % (n))

if __name__ == '__main__':
    d = genDict()
    printRes(d)
