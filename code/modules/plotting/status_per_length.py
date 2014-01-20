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
            line_style.gray30]

def get_data_for_status(d,name):
  ret = [(0,0)]
  if name in d:
    status_data = d[name]
    for k in status_data.keys():
      tot = 0
      for name in d.keys():
        if k in d[name]:
          tot = tot + d[name][k]
      ret = ret + [(k,(status_data[k]/float(tot))*100)] #ret.append((k,status_data[k]))
  return ret

def get_data():
    f = open("history.txt",'r')
    xs = f.readlines()
    ds = dict()
    for i in status_names():
      ds[status_names()[i]] = dict()
    for row in xs:
      ds = parse_status(ds,row)
    return ds

def parse_status(d,row):
  xs = ''.join(filter(lambda x: x != ']' and x != '[' and x != '\n' and x != '\'' and x != ' ',row)).split(',')
  xs = [] + filter(lambda x: x != '' and x != '\n',xs)
  index = len(xs)
  for s in xs:
    if index in d[s]:
      d[s][index] = d[s][index] + 1
    else:
      d[s][index] = 1
  return d


#for elem in xs:
#  print elem
#    return [(Status.Ok,Functions.GetMode),
#            (Status.Deactivated,Functions.SetMode),
#            (Status.Stopped,Functions.MainFunction),
#            (Status.Ok,Functions.MainFunction)]

def draw_plot():
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "length of command sequences",format="/a-30{}%d"),
            y_axis = axis.Y(label = "Percentage in state",format="%s%%")#,
            #legend = legend.T(loc = (0,150))
            )


    data = get_data()
    for i in status_names():
        ar.add_plot(line_plot.T(data = get_data_for_status(data,status_names()[i]),line_style=colors[i-1],
             label=status_names()[i][19:].lower()))


#    ar.add_plot(line_plot.T(data = get_data_ok(),line_style=line_style.red,
#             label="OK"))
#
#    ar.add_plot(line_plot.T(data = get_data_failed(),line_style=line_style.black,
#             label="FAILED"))
#
#    ar.add_plot(line_plot.T(data = get_data_failed(),line_style=line_style.black,
#             label="DEACTIVATED"))
#
#    ar.add_plot(line_plot.T(data = get_data_failed(),line_style=line_style.black,
#             label="EXPIRED"))
#
#    ar.add_plot(line_plot.T(data = get_data_failed(),line_style=line_style.black,
#             label="EXPIRED"))
    ar.draw()
    #tb = text_box.T(loc=(0,0),text="shows something")
    #tb.draw()
    #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
