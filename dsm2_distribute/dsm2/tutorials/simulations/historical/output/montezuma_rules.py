""" Draws several line plots """
from string import *

from vtools.graph.api import *
from vtools.data.api import *
from vtools.datastore.dss.api import *
from enthought.chaco2.api import VPlotContainer
from enthought.chaco2.tools.api import RangeSelection,LineInspector,LegendTool
from zoom_overlay import ZoomOverlay
""" edit following lines if the source is changed """ 


source = "hist.dss"
time_window=(datetime(2001,2,1),datetime(2001,10,1))

source_name = "MSCS op" 
zoom_name = source_name



def format_plot(plot,title=None,date_label=True,legend=False,yaxis_title=""):
    if title:
        plot.title=title
        plot.padding_top=40
    else:
        plot.padding_bottom=0
    if legend:
        plot.legend.visible=True
        plot.legend.line_width=2

        
    plot.y_axis.title=yaxis_title
    plot.line_width=2
    plot.bgcolor="whitesmoke"
    plot.fill_padding=True

    
    if date_label:
        plot.padding_bottom=40
    else:
        #Change the date format on the x axis to eliminate time
        plot.x_axis.tick_label_formatter.date_format=""
        plot.padding_bottom=0
        
selector_stage_up = "B=mtz_sl_us_scg,C=stage"
selector_stage_down = "B=mtz_sl_ds_scg,C=stage"
selector_velocity = "B=mtz_sl_ds_scg,C=vel"
selector_mscs_op = "B=mscs_radial,C=op-to-node"

stage_up=dss_retrieve_ts(source,selector_stage_up,time_window)
stage_down=dss_retrieve_ts(source,selector_stage_down,time_window)
dh = stage_up-stage_down
velocity=dss_retrieve_ts(source,selector_velocity,time_window)
mscs_op=dss_retrieve_ts(source,selector_mscs_op,time_window)
palette=get_palette("default")


# Simple time series plot. Note you don't really have to put
# plot_area=None. This is the default, and it means that the output
# will be a new plot area.
p0 = ts_plot(mscs_op,
                        name=source_name, \
                        color=palette[0])
format_plot(p0,title=source_name,date_label=True,legend=False,yaxis_title="feet")
p0.legend.tools.append(LegendTool(component=p0.legend))

p1 = ts_plot(mscs_op,name=zoom_name,color=palette[0 ])
format_plot(p1,title=None,date_label=True,legend=True,yaxis_title="feet")

zoomconnect=zoom_connect(p0,p1,source_name,zoom_name)
# Now we are reusing the plot area and plotting another time series on
# top of the first.
p2 = ts_plot(dh,name="dh (ft)",color=palette[1])
format_plot(p2,title=None,date_label=True,legend=True,yaxis_title="feet")
p2.index_range = p1.index_range

p3 = ts_plot(velocity,name="velocity",color=palette[2])
format_plot(p3,title=None,date_label=True,legend=True,yaxis_title="ft/s")
p3.overlays.append(LineInspector(p3, axis='index',
                                                write_metadata=False,
                                                is_listener=True))
p3.overlays.append(LegendTool(component=p3.legend))
                                                 
                                                
#p3 = ts_plot(stagediff,name="Up-down stage difference",color=palette[2])
#format_plot(p3,title=None,date_label=True,legend=True,yaxis_title="feet")
p3.index_range = p1.index_range

#p4 = ts_plot(down_stage,name="Downstream Stage",color=palette[2])
#format_plot(p4,title=None,date_label=True,legend=True,yaxis_title="feet")
#p4.index_range = p1.index_range

# The zoom container just contains the zoomer and zoomee 
zoom_container=VPlotContainer(stack_order="top_to_bottom",padding=0)
zoom_container.add(p0)
zoom_container.add(p1)
zoom_container.overlays.append(zoomconnect)
vpc = VPlotContainer(stack_order="top_to_bottom")
vpc.add(zoom_container)
#vpc.add(p1)
vpc.add(p2)
vpc.add(p3)
#vpc.add(p4)
#vpc.overlays.append(zoomconnect)

p0.padding_top=20
p0.padding_bottom=40
p1.padding_top=10
p1.padding_bottom=40
p2.padding_top=0
p2.padding_bottom=40
p3.padding_top=0
p3.padding_bottom=40
#p4.padding_top=0

# Now get a frame to display it live
fig=default_frame(vpc,size=(800,600))
display_frame(fig)
# Things you can do:
# z: enter/exit zoom mode
# esc: reset to original scale
# cntrl-c: copy to clipboard
# cntrl-s: save to image file



# EOF
