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
    return [(Status.Ok,Functions.GetMode),
            (Status.Deactivated,Functions.SetMode),
            (Status.Stopped,Functions.MainFunction),
            (Status.Ok,Functions.MainFunction)]


def format_function(i):
     return function_names()[i] 

def format_status(i):
     return status_names()[i]

tickmarks = [tick_mark.square,tick_mark.star,tick_mark.blacksquare,tick_mark.blackdia,tick_mark.tri]


def draw_plot():
    data = get_data()
    ar = area.T(
            loc = (0,50),
            x_axis = axis.X(label = "Called Function",format = format_function),
            y_axis = axis.Y(label = "Global Status", format = format_status))

    for i, p in enumerate(data):
     ar.add_plot(line_plot.T(data = [p],tick_mark = tickmarks[i],
             label=str(i) + " times" ))
    ar.draw()
    tb = text_box.T(loc=(0,0),text="shows which functions that resulted\n in a specific global state")
    tb.draw()
   #draw_text(data)
    #a = arrow.T(head_style=2)
    #a.draw([(1,2),(3,4)])

if __name__ == "__main__":
    draw_plot()
