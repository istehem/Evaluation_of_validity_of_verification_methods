from pychart import *
from enums import *

colors = [line_style.black,
            line_style.blue,
            line_style.red,
            line_style.green,
            line_style.T(color = color.gold),
            line_style.T(color = color.darkgreen),
            line_style.T(color = color.khaki),
            line_style.T(color = color.rosybrown),
            line_style.T(color = color.tan),
            line_style.T(color = color.hotpink)
            ]

def get_data(item_names):
    f = open(raw_input(''),'r')
    xs = f.readlines()
    ds = dict()
    gr_index = 0
    for i in item_names:
      ds[item_names[i]] = dict()
    for row in xs:
      (ds,index) = parse_item(ds,row)
      if gr_index < index:
          gr_index = index
    return (ds,gr_index)

def get_data_for_item(d,name):
  ret = [(0,0)]
  if name in d:
    item_data = d[name]
    for n in sorted(item_data.keys()):
      tot = 0
      for item in d.keys():
        if n in d[item]:
          tot = tot + d[item][n]
      if tot == 0:
        tot = 1
      ret = ret + [(n,(item_data[n]/float(tot))*100)] #ret.append((k,status_data[k]))
  return ret

def parse_item(d,row):
  xs = ''.join(filter(lambda x: "[] \n\'".find(x) == -1,row)).split(',')
  xs = [] + filter(lambda x: x != '' and x != '\n',xs)
  index = len(xs)
  for s in xs:
    if index in d[s]:
      d[s][index] = d[s][index] + 1
    else:
      d[s][index] = 1
  return (d,index)

