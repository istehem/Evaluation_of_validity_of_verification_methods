from enums import *
from pychart import *
from helper import *

theme.get_options()
#theme.scale_factor = 1
#theme.reinitialize()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
#chart_object.set_defaults(line_plot.T, line_style=None)

def draw_plot():
    (data,gr_index,num_of_tests) = get_data(status_names())
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "length of command sequences",format="/a-30{}%d",
              tic_interval=20),
            x_range = (0,gr_index),
            y_range = (0,100),
            y_axis = axis.Y(label = "Percentage in state",format="%s%%", tic_interval=10)#,
            #legend = legend.T(loc = (0,150))
            )


    for i in status_names():
        if i in [1,2]: #[3,4,5]
            ar.add_plot(line_plot.T(data = get_data_for_item(data,status_names()[i]),
                                    line_style=colors[i-1],
                                    label=status_names()[i][19:].lower()))

    ar.draw()
    tb_s = text_box.T(loc=(40,130),text="Global Statuses")
    tb_s.draw()
    tb_nt = text_box.T(loc=(0,0),
        text="Total number of tests: " + str(num_of_tests),
        line_style=None)
    tb_nt.draw()
    #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
