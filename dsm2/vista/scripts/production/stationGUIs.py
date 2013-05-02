#define a combo box to run selection
tw_all=None
def makeScriptComboBox(list) :
  from javax.swing import JComboBox, JButton, JPanel
  from java.awt.event import ActionListener
  jcb = JComboBox(list)
  jcb.setSelectedIndex(0)
  plot_btn = JButton("Plot station")
  # create a Python class
  class PlotButtonAction(ActionListener):
    # defines a constructor
    def __init__(self,combo_box):
      self.cb = combo_box
    def actionPerformed(self, event):
      plotStation(self.cb.getSelectedIndex(),None)
  # registers an instance of the action object with the button
  plot_btn.addActionListener(PlotButtonAction(jcb))
  #
  alltw_btn = JButton("All Data")
  class AllButtonAction(ActionListener):
    # defines a constructor
    def __init__(self,combo_box):
      self.cb = combo_box
    def actionPerformed(self, event):
      plotStation(self.cb.getSelectedIndex(),1)
  alltw_btn.addActionListener(AllButtonAction(jcb))
  #
  quit_btn = JButton("Quit")
  class QuitButtonAction(ActionListener):
    # defines a constructor
    def __init__(self,combo_box):
      self.cb = combo_box
    def actionPerformed(self, event):
      fr.dispose()
  quit_btn.addActionListener(QuitButtonAction(jcb))
  # add both button and combo box to a panel 
  panel = JPanel(); panel.setLayout(GridLayout(2,2))
  panel.add(alltw_btn); panel.add(jcb)
  panel.add(plot_btn); panel.add(quit_btn)
  return panel
#
# now make a new frame
fr = JFrame()
pane=fr.getContentPane()
from java.awt import GridLayout
# set layout by rows and columns
l= GridLayout(2,1)
pane.setLayout(l)
from javax.swing import JTextField
twf = JTextField(repr(tw))
pane.add(twf)
pane.add(makeScriptComboBox(labels))
# pack and show the frame
fr.pack(); fr.setVisible(1); fr.setTitle(type)
