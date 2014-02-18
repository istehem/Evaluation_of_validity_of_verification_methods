from pychart import *
from enums import *
from helper import *
import sys

theme.get_options()
#theme.scale_factor = 1
#theme.reinitialize()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
#chart_object.set_defaults(line_plot.T, line_style=None)



def format_statuses(i):
    if i == 0:
        ret = ''
    else:
        ret = status_names()[i].split('_')[-1]
    return ret 


def draw_plot():
    #(data,gr_index,num_of_tests) = get_data(function_names())
    data = []
    statuses = get_greatest_seq()
    for i, s in enumerate(statuses):
      data = data + [(i,from_status_names()[s])]
    sys.stderr.write(str(data) + "\n")
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "commands",format="/a-30{}%d",
              tic_interval=20),
            x_range = (0,len(data)),
            y_range = (0,len(status_names())),
            y_axis = axis.Y(label = "statuses",format=format_statuses, tic_interval=1)
            #legend = legend.T(loc = (0,150))
            )


    ar.add_plot(line_plot.T(data = data,line_style=colors[0],label='s per c'))
    ar.draw()
    tb = text_box.T(loc=(40,145),text="For one test")
    tb.draw()

if __name__ == "__main__":
    draw_plot()
