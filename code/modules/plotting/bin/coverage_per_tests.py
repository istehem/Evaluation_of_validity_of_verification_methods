import sys
from pychart import *
from helper import colors
from itertools import takewhile
theme.get_options()

def draw_plot():
    data, cov, tot = get_data()
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "Tests",format="/a-30{}%d",
              tic_interval=100),
            x_range = (0,len(data)),
            y_range = (0,100),
            y_axis = axis.Y(label = "Cover percentage",format="%s%%", tic_interval=10),
            legend = None
            )


    ar.add_plot(line_plot.T(data = data,line_style=colors[0]))
    ar.draw()
    tb = text_box.T(loc=(20,110),
        text="Coverage Per Tests\nnumber of lines: %i"%(int(tot)))
    tb.draw()

def get_data():
  data = []
  f = open(raw_input(''),'r')
  xs = f.readlines()
  cov = 0
  tot = 0
  for i, elem in enumerate(xs):
    cov, tot = map(lambda x: float(x),elem.split('*')[0].split('/'))
    data = data + [(i,cov/tot*100)]
  return (data,cov,tot)

if __name__ == "__main__":
    draw_plot()

