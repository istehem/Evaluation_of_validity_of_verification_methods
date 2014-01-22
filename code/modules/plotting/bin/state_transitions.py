from enums import *
from pychart import *

theme.get_options()
#theme.scale_factor = 1
#theme.reinitialize()
#chart_object.set_defaults(line_plot.T, line_style=line_style.T(cap_style=2,color=color.red))
#chart_object.set_defaults(line_plot.T, line_style=None)

locations = []

def draw_plot():
    ok = text_box.T(loc=(50,150),text="Ok")
    ok_info = text_box.T(loc=(50,125),text="100%",line_style=None)
    deactivated = text_box.T(loc=(50,100),text="Deactivated")
    ok_info.draw()
    deactivated.draw()
    ok.add_arrow((50,110),arrow=arrow.a0)
    ok.draw()

if __name__ == "__main__":
    draw_plot()
