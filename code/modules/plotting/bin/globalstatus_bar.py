#!/usr/bin/python

from pychart import *

theme.get_options()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
chart_object.set_defaults(line_plot.T, line_style=None)


class Status:
     Ok = 1
     Deactivated = 2
     Failed = 3 
     Expired = 4
     Stopped = 5

class Functions:
     GetMode = 1
     SetMode = 2
     MainFunction = 3
     CheckpointReached = 4
     PerformReset = 5
     GetFirstExpiredSeid = 6
     Init = 7
     GetLocalStatus = 8
     GetGlobalStatus = 9
     DeInit = 10

def status_names():
    return {   
        Status.Ok: "Ok",
        Status.Deactivated: "Deactivated",
        Status.Failed: "Failed",
        Status.Expired: "Expired",
        Status.Stopped: "Stopped"
    }

def function_names(): 
    return {
        Functions.GetMode: "GM",
        Functions.SetMode: "SM",
        Functions.MainFunction: "MF",
        Functions.CheckpointReached: "CR",
        Functions.PerformReset: "PR",
        Functions.GetFirstExpiredSeid: "GFES",
        Functions.Init: "I",
        Functions.GetLocalStatus: "GLS",
        Functions.GetGlobalStatus: "GGS",
        Functions.DeInit: "DI"
    }

def get_data():
    return [(Status.Ok,34,2),
            (Status.Deactivated,23,2),
            (Status.Stopped,12,2),
            (Status.Ok,13,2)]


def format_function(i):
     return function_names()[i] 

def format_status(i):
     return status_names()[i]


def number_of_calls():
     n = 0 
     for i in get_data():
        (_,c,_) = i
        n = n + c 
     return n

tickmarks = [tick_mark.square,tick_mark.star,tick_mark.blacksquare,tick_mark.blackdia,tick_mark.tri]
fillstyles = [fill_style.gray50]

def draw_plot():
    data = get_data()
    ar = area.T(
            loc = (0,0),
            x_axis = axis.X(label = "Called Function",format = format_function),
            y_axis = axis.Y(label = "number of times hit"))
    
#    for i, p in enumerate(data):
    ar.add_plot(bar_plot.T(data = data,fill_style = fillstyles[0],label=(str(45.0/number_of_calls())))
                    )
#             label=str(i) + " times" ))
    ar.draw()
    #tb.draw()
    #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
