from enums import *
from pychart import *

theme.get_options()
#theme.scale_factor = 1
#theme.reinitialize()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
#chart_object.set_defaults(line_plot.T, line_style=None)


tickmarks = [tick_mark.square,tick_mark.star,tick_mark.blacksquare,tick_mark.blackdia,tick_mark.tri]

colors = [line_style.black,
            line_style.blue,
            line_style.red,
            line_style.green,
            line_style.T(color = color.gold)]

def get_data_for_status(d,name):
  ret = [(0,0)]
  if name in d:
    status_data = d[name]
    for n in sorted(status_data.keys()):
      tot = 0
      for status in d.keys():
        if n in d[status]:
          tot = tot + d[status][n]
      ret = ret + [(n,(status_data[n]/float(tot))*100)] #ret.append((k,status_data[k]))
  return ret

def get_data():
    f = open(raw_input(''),'r')
    xs = f.readlines()
    ds = dict()
    gr_index = 0 
    for i in status_names():
      ds[status_names()[i]] = dict()
    for row in xs:
      (ds,index) = parse_status(ds,row)
      if gr_index < index:
          gr_index = index
    return (ds,gr_index)

def parse_status(d,row):
  xs = ''.join(filter(lambda x: x != ']' and x != '[' and x != '\n' and x != '\'' and x != ' ',row)).split(',')
  xs = [] + filter(lambda x: x != '' and x != '\n',xs)
  index = len(xs)
  for s in xs:
    if index in d[s]:
      d[s][index] = d[s][index] + 1
    else:
      d[s][index] = 1
  return (d,index)

def draw_plot():
    (data,gr_index) = get_data()
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "length of command sequences",format="/a-30{}%d"),
            x_range = (0,gr_index),
            y_axis = axis.Y(label = "Percentage in state",format="%s%%")#,
            #legend = legend.T(loc = (0,150))
            )


    for i in status_names():
        ar.add_plot(line_plot.T(data = get_data_for_status(data,status_names()[i]),line_style=colors[i-1],
             label=status_names()[i][19:].lower()))

    ar.draw()
    tb = text_box.T(loc=(40,130),text="Global Statuses")
    tb.draw()
    #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
