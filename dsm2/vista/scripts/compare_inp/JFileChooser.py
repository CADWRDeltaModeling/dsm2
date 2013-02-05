from java.lang import *
from java.io   import *
from java.awt  import *
from java.awt.event import *
from javax.swing    import *
from javax.swing.filechooser import *

class JFileChooserTest(JPanel):
    def __init__(self):
        super(JFileChooserTest, self).__init__(BorderLayout())
        
        self.log = JTextArea(5, 20)
        self.log.margin = Insets(5,5,5,5)
        self.log.editable = False
        logScrollPane = JScrollPane(self.log)
        
        self.fc = JFileChooser()
        self.openButton = JButton("Open a File...")
        self.saveButton =  JButton("Save a File...")
        lsnr = ButtonListener(self)
        self.openButton.addActionListener(lsnr)
        self.saveButton.addActionListener(lsnr)
        
        buttonPanel = JPanel()
        buttonPanel.add(self.openButton)
        buttonPanel.add(self.saveButton)
        self.add(buttonPanel, BorderLayout.PAGE_START)
        self.add(logScrollPane, BorderLayout.CENTER)
        
    def createUI(self):
        frame = JFrame("FileChooserDemo")
        frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        frame.add(self)
        frame.pack()
        frame.visible = True
        
class ButtonListener(ActionListener):
    def __init__(self, target):
        self.target = target
        
    def actionPerformed(self, e):
        if e.source is self.target.openButton:
            returnVal = self.target.fc.showOpenDialog(self.target)
            
            if returnVal == JFileChooser.APPROVE_OPTION:
                file = self.target.fc.selectedFile
                self.target.log.append("Opening: %s.\n" % file.name)
            else:
                self.target.log.append("Open command cancelled by user.\n")
                self.target.log.caretPosition = self.target.log.document.length
                
        elif e.source is self.target.saveButton:
            returnVal = self.target.fc.showSaveDialog(self.target);
            if returnVal == JFileChooser.APPROVE_OPTION:
                file = self.target.fc.selectedFile
                self.target.log.append("Saving: %s.\n" % file.getName())
            else:
                self.target.log.append("Save command cancelled by user.\n")
            self.target.log.caretPosition = self.target.log.document.length
            
class Invoker(Runnable):
    def run(self):
        UIManager.put("swing.boldMetal", Boolean.FALSE)
        sample = JFileChooserTest()
        sample.createUI()
        
SwingUtilities.invokeLater(Invoker())