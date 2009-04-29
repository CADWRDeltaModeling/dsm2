# Generates a GUI to gather information to allow for text parsing of files into DSS
#
import sys
from string import *

from widgets import *

from java.awt                   import *
from java.awt.event             import *
from java.lang import Math
from javax.swing                import *
from javax.swing.border         import *
from javax.swing.event          import *
from javax.swing.filechooser    import *

class TextConverterPanel(JPanel,ActionListener):
    fileList = JList(DefaultListModel())
    txtHeader = JTextField('')
    txtFooter = JTextField('')
    grpHeader = ButtonGroup()
    grpFooter = ButtonGroup()
    grpTimeSeries = ButtonGroup()
    def __init__(self):
        JPanel.__init__(self)
        self.initialize()
    def initialize(self):
        self.setBorder(TitledBorder('Test Border'))
        self.setLayout(GridBagLayout())
        gbc = GridBagConstraints()
        gbc.fill = GridBagConstraints.HORIZONTAL
        gbc.anchor = GridBagConstraints.NORTH
        gbc.gridx = 0
        gbc.gridy = 0
        gbc.gridheight = 1
        gbc.gridwidth = 1
        gbc.weightx = 1.0
        gbc.weighty = 1.0        
        self.add(self.createFilePanel(),gbc)
        gbc.gridy = 1
        self.add(self.createHeaderFooterPanel(),gbc)
        gbc.gridy = 2
        self.add(self.createSeriesNamePanel(),gbc)
        gbc.gridy = 3
        self.add(self.createRTSPanel(),gbc)
    def createFilePanel(self):
        pnl = JPanel()
        pnl.setBorder(TitledBorder('Choose Files'))
        pnl.setLayout(BorderLayout())
        pnl.add(self.fileList,BorderLayout.CENTER)
        fbtn = JButton('Browse')
        fbtn.setActionCommand('Load Files')
        fbtn.addActionListener(self)
        btnPanel = JPanel()
        btnPanel.add(fbtn)
        pnl.add(btnPanel,BorderLayout.EAST)
        return pnl
    def createHeaderFooterPanel(self):
        pnl = JPanel()
        pnl.setBorder(TitledBorder("Header / Footer"))
        pnl.setLayout(GridBagLayout())
        chcHeaderNOL = JRadioButton("Number of Lines",1)
        chcHeaderKeyword = JRadioButton("Keyword",0)
        self.grpHeader.add(chcHeaderNOL)
        self.grpHeader.add(chcHeaderKeyword)
        chcFooterNOL = JRadioButton("Number of Lines",1)
        chcFooterKeyword = JRadioButton("Keyword",0)
        self.grpFooter.add(chcFooterNOL)
        self.grpFooter.add(chcFooterKeyword)
        gbc = GridBagConstraints()
        gbc.fill = GridBagConstraints.HORIZONTAL
        gbc.gridx = 0
        gbc.gridy = 0
        gbc.gridheight = 2
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.weighty = 0.5
        pnl.add(JLabel("Header",JLabel.LEFT),gbc)
        gbc.gridx = 0
        gbc.gridy = 2
        gbc.gridheight = 2
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.weighty = 0.5
        pnl.add(JLabel("Footer",JLabel.LEFT),gbc)
        gbc.gridx = 1
        gbc.gridy = 0
        gbc.gridheight = 2
        gbc.gridwidth = 1
        gbc.weightx = 0.5
        gbc.weighty = 0.5
        pnl.add(self.txtHeader,gbc)
        gbc.gridx = 1
        gbc.gridy = 2
        gbc.gridheight = 2
        gbc.gridwidth = 1
        gbc.weightx = 0.5
        gbc.weighty = 0.5
        pnl.add(self.txtFooter,gbc)
        gbc.gridx = 2
        gbc.gridy = 0
        gbc.gridheight = 1
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.weighty = 0.25
        pnl.add(chcHeaderNOL,gbc)
        gbc.gridx = 2
        gbc.gridy = 1
        gbc.gridheight = 1
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.weighty = 0.25
        pnl.add(chcHeaderKeyword,gbc)
        gbc.gridx = 2
        gbc.gridy = 2
        gbc.gridheight = 1
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.weighty = 0.25
        pnl.add(chcFooterNOL,gbc)
        gbc.gridx = 2
        gbc.gridy = 3
        gbc.gridheight = 1
        gbc.gridwidth = 1
        gbc.weightx = 0.0
        gbc.weighty = 0.25
        pnl.add(chcFooterKeyword,gbc)
        return pnl
    def createSeriesNamePanel(self):
        pnl = JPanel()
        return pnl
    def createRTSPanel(self):
        chcRTS = JRadioButton('Regular Time Series',1)
        self.grpTimeSeries.add(chcRTS)
        pnl = ComponentTitledPanel(chcRTS)
        pnl.setTransmittingAllowed(1)
        p1 = RTSPanel()
        p1.add(JLabel('TEST'))
        pnl.setTransmitter(p1)
        return pnl
    def getFile(self):
        jfc = JFileChooser()
        ff = TextFileFilter()
        jfc.addChoosableFileFilter(ff);
        jfc.setFileFilter(ff);
        jfc.setMultiSelectionEnabled(1);
        resp = jfc.showOpenDialog(self);
        if resp == JFileChooser.CANCEL_OPTION:
            return
        if resp == JFileChooser.ERROR_OPTION:
            print 'ERROR Opening file'
            return
        files = jfc.getSelectedFiles()
        for file in files:
            self.fileList.getModel().addElement(file)
    def actionPerformed(self,evt):
        a = evt.getActionCommand()
        if a == 'Load Files':
            self.getFile()    

class TextFileFilter(FileFilter):
    def accept(self,file):
      if file.isDirectory():
        return 1
      extension = self.getExtension(file);
      if extension != None :
        if extension == 'txt':
          return 1        
        else:
          return 0
      return 0
    def getDescription(self):
      return 'Text Files'
    def getExtension(self,file):
      ext = None
      s = file.getName()
      i = rfind(s,'.')
      if i > 0 and  i < len(s) - 1:
        ext = lower(s[i+1:])
      return ext

class ExitListener(WindowAdapter):
    def windowClosing(self,evt):
        sys.exit(0)

class RTSPanel(JPanel,StateTransmitter):
    def __init__(self):
        JPanel.__init__(self)            
    def setChildrenEnabled(enable):
        #button.setEnabled(enable)
        #textField.setEnabled(enable)
        return

 
if __name__ == '__main__':
    frame = JFrame('Text Converter')
    frame.getContentPane().setLayout(BorderLayout())
    frame.getContentPane().add(TextConverterPanel(),BorderLayout.CENTER)
    frame.addWindowListener(ExitListener())
    frame.setSize(800,600)
    frame.show()

