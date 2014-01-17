from enums import * 

from pychart import *

theme.get_options()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
#chart_object.set_defaults(line_plot.T, line_style=None)


tickmarks = [tick_mark.square,tick_mark.star,tick_mark.blacksquare,tick_mark.blackdia,tick_mark.tri]

colors = [line_style.black,
            line_style.blue,
            line_style.red,
            line_style.green,
            line_style.gray30]


def get_data(i):
    if i == 1:
        return get_data_ok()
    else:
        return get_data_failed()


def get_data_ok():
    return [(1,100),(2,50),(3,10)]

def get_data_failed():
    return [(1,0),(2,50),(3,90)]



#    return [(Status.Ok,Functions.GetMode),
#            (Status.Deactivated,Functions.SetMode),
#            (Status.Stopped,Functions.MainFunction),
#            (Status.Ok,Functions.MainFunction)]

def draw_plot():
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "length of command sequences"),
            y_axis = axis.Y(label = "Percentage in state"))

    
    
    for i in range(1,len(status_names())+1):
        ar.add_plot(line_plot.T(data = get_data(i),line_style=colors[i-1], 
             label=status_names()[i]))
        
    
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
    tb = text_box.T(loc=(0,0),text="shows something")
    tb.draw()
    #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
