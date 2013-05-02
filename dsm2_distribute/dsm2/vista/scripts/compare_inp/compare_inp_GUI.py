from java.lang import *
from java.io   import *
from java.awt  import *
from java.awt.event import *
from javax.swing    import *
from javax.swing.filechooser import *
import os, sys

class OpenFile(JPanel):
    def __init__(self,create_report):
        super(OpenFile, self).__init__(GridLayout(6, 1))
        self.create_report = create_report
        self.log1 = JTextField("",80)
        self.log2 = JTextField("",80)
        #self.log3 = JTextField(str(os.environ.get("VISTA_HOME")).replace("/","\\")+"\scripts\compare_inp\Compare_echo_inp.html",80)
        #self.log3 = JTextField(str(os.getcwd()).replace("\\bin","\\scripts\\compare_inp")+"\Compare_echo_inp.html",80)
        if str(sys.getBaseProperties().getProperty('user.home'))!='':
            self.log3 = JTextField(str(sys.getBaseProperties().getProperty('user.home'))+"\\compare_inp\\",75)
        else:
            self.log3 = JTextField("c:\\compare_inp",75)
        self.log4 = JTextField("compare_echo_inp.html",35)
        self.log5 = JTextField("Status.........",80)
        self.log1.editable = False
        self.log2.editable = False
        self.log5.editable = False
        
        #logScrollPane = JScrollPane(self.log1)
        self.fc1 = JFileChooser()
        self.fc2 = JFileChooser()
        self.fc3 = JFileChooser()
        self.fc3.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
        self.openButton1 = JButton("Open a File...")
        self.openButton2 = JButton("Open a File...")
        self.openButton3 = JButton("Select Directory...")
        lsnr1 = ButtonListener(self,1)
        lsnr2 = ButtonListener(self,2)
        lsnr3 = ButtonListener(self,3)
        self.openButton1.addActionListener(lsnr1)
        self.openButton2.addActionListener(lsnr2)
        self.openButton3.addActionListener(lsnr3)
        buttonPanel1 = JPanel()
        buttonPanel1.add(JLabel("Echo File 1: "))
        buttonPanel1.add(self.log1)
        buttonPanel1.add(self.openButton1)
        buttonPanel2 = JPanel()
        buttonPanel2.add(JLabel("Echo File 2: "))
        buttonPanel2.add(self.log2)
        buttonPanel2.add(self.openButton2)
        buttonPanel3 = JPanel()
        buttonPanel3.add(JLabel("Output Directory: "))
        buttonPanel3.add(self.log3)
        buttonPanel3.add(self.openButton3)               
        textPanel = JPanel()
        textPanel.add(JLabel("Output File Name: "))
        textPanel.add(self.log4)
        textPanel.add(JLabel(" (Please change filename if you have multiple output HTML files under the same directory.)"))
        self.doButton = JButton("Create HTML Report",actionPerformed=self.call_create_report)
        self.viewButton = JButton("View the Report",actionPerformed=self.view_report)
        self.exitButton = JButton("Close this Program",actionPerformed=self.exit_app)
        self.viewButton.setEnabled(0)
        doPanel = JPanel()
        doPanel.add(self.doButton)
        doPanel.add(self.viewButton)
        doPanel.add(self.exitButton)        
        self.add(buttonPanel1)
        self.add(buttonPanel2)
        self.add(buttonPanel3)
        self.add(textPanel)
        self.add(doPanel)
        self.add(self.log5)
           
    def createUI(self):
        frame = JFrame("DSM2 Input Comparison Report Creator")
        frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        frame.add(self)
        frame.pack()
        frame.show()
        
    def call_create_report(self,event):
        val1 = self.log1.getText()
        val2 = self.log2.getText()
        val3 = self.log3.getText()
        val4 = self.log4.getText()
        self.create_report(val1,val2,val3, val4)
        self.log5.setText("Report has been successfully generated!")
        self.viewButton.setEnabled(1)
        if os.path.exists("C:/Program Files/Google/Chrome/Application/chrome.exe"):
            os.system('cmd /c start chrome "'+val3+'\\'+val4+'"')        
        elif os.path.exists("C:/Program Files/Mozilla Firefox/firefox.exe"):
            os.system('cmd /c start firefox "'+val3+'\\'+val4+'"')
        else:
            print "Please install Chrome and Firefox for best performance."
            os.system('cmd /c start "'+val3+'\\'+val4+'"')
        #java.lang.Runtime.getRuntime().exec(["C:\Program Files\Mozilla Firefox\Firefox.exe","www.cnn.com"])
    
    def view_report(self,event):
        val3 = self.log3.getText()
        val4 = self.log4.getText()
        if os.path.exists("C:/Program Files/Google/Chrome/Application/chrome.exe"):
            os.system('cmd /c start chrome "'+val3+'\\'+val4+'"')
        elif os.path.exists("C:/Program Files/Mozilla Firefox/firefox.exe"):
            os.system('cmd /c start firefox "'+val3+'\\'+val4+'"')            
        else:
            print "Please install Chrome and Firefox for best performance."
            os.system('cmd /c start "'+val3+'\\'+val4+'"')
        
    def exit_app(self,event):
        System.exit(0)
        
    def open_file(self,event):
        returnVal = self.fc.showOpenDialog(self)
        if returnVal == JFileChooser.APPROVE_OPTION:
            file = self.fc.selectedFile
            tmp_filename = file
        else:
            pass
    
class ButtonListener(ActionListener):
    def __init__(self, target,n):
        self.target = target
        self.n = n 
        
    def actionPerformed(self, e):
        if self.n == 1:
            returnVal = self.target.fc1.showOpenDialog(self.target)            
            if returnVal == JFileChooser.APPROVE_OPTION:
                file1 = self.target.fc1.selectedFile
                self.target.log1.setText(str(file1))
            else:
                self.target.log1.setText("Open command cancelled by user.\n")
                self.target.log1.caretPosition = self.target.log1.document.length
        if self.n == 2:
            returnVal = self.target.fc2.showOpenDialog(self.target)            
            if returnVal == JFileChooser.APPROVE_OPTION:
                file2 = self.target.fc2.selectedFile
                self.target.log2.setText(str(file2))
            else:
                self.target.log2.setText("Open command cancelled by user.\n")
                self.target.log2.caretPosition = self.target.log2.document.length
        if self.n == 3:
            returnVal = self.target.fc3.showOpenDialog(self.target)      
            if returnVal == JFileChooser.APPROVE_OPTION:
                file3 = self.target.fc3.selectedFile
                self.target.log3.setText(str(file3))
            else:
                self.target.log3.setText("Open command cancelled by user.\n")
                self.target.log3.caretPosition = self.target.log3.document.length                
            
class Invoker(Runnable):
    def run(self):
        UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel")
        sample = OpenFile()
        sample.createUI()
