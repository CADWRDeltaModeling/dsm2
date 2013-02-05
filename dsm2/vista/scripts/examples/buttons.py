#
from java.awt.event import ActionListener
class action(ActionListener):
	#defines a constructor
	def __init__(self,func):
		self.f = func
	def actionPerformed(self,evt):
		self.f.__call__()
#
def makeAction(myfunc):
	return action(myfunc)
#
# define a function to invoke a given script
def makeScriptButton(button_name,script_name):
	from javax.swing import JButton
	from java.awt.event import ActionListener
	btn = JButton(button_name)
        #defin a class to invoke a script
	# creating a class called action which does not exist
	# this creates a python class deriving from ActionListener( a java class)
	class action(ActionListener):
		# defines a constructor
		def __init__(self,sname):
			self.sname = sname
		def actionPerformed(self, event):
			execfile(self.sname)
	# registers an instance of the action object with the button
	btn.addActionListener(action(script_name))
	return btn
#
from javax.swing import *
# now make a new frame
fr = JFrame();
pane=fr.getContentPane()
from java.awt import GridLayout
# set layout to 2 rows by 2 columns
l= GridLayout(2,2)
pane.setLayout(l)
# add script invoking buttons
pane.add(makeScriptButton('Run ex1.py','ex1.py'))
pane.add(makeScriptButton('Run ex2.py','ex2.py'))
pane.add(makeScriptButton('Run ex3.py','ex3.py'))
# pack and show the frame
fr.pack(); fr.setVisible(1)
#
