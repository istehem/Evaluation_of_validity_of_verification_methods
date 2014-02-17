from pychart import *
from enums import *
from helper import *
import sys

theme.get_options()
#theme.scale_factor = 1
#theme.reinitialize()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
#chart_object.set_defaults(line_plot.T, line_style=None)

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
            y_axis = axis.Y(label = "status",format="%s", tic_interval=1)#,
            #legend = legend.T(loc = (0,150))
            )


#    for i in function_names():
#        ar.add_plot(line_plot.T(data = get_data_for_item(data,function_names()[i]),line_style=colors[i-1],
#             label=function_names()[i].lower()))
    ar.add_plot(line_plot.T(data = data,line_style=colors[0]))
    ar.draw()
    tb = text_box.T(loc=(40,130),text="For one test")
    tb.draw()
#    tb_nt = text_box.T(loc=(0,0),
#        text="Total number of tests: " + str(num_of_tests),
#        line_style=None)
#    tb_nt.draw()
    #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
