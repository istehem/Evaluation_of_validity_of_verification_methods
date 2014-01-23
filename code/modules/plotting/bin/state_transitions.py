def parse_data():
    f = open(raw_input(''),'r')
    xs = f.readlines()
    d = dict()
    tot = 0
    for row in xs:
        ys = ''.join(filter(lambda x: "[] \n\'".find(x) == -1,row)).split(',')
        ys = [] + filter(lambda x: x != '' and x != '\n',ys)
        transes = zip(ys,ys[1:])
        for trans in transes:
            if not trans in d:
                d[trans] = 1
            else:
                d[trans] = d[trans] + 1
            tot = tot + 1
    return (d,tot)

def gen_tex_file():
    print "nothing yet"

if __name__ == "__main__":
    gen_tex_file()   
