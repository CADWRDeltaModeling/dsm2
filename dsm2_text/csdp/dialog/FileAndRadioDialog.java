/*
    Copyright (C) 1998 State of California, Department of Water
    Resources.

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/
package DWR.CSDP.dialog;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.*;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.BorderFactory; 


/**
 * a dialog box with a specified number of file choosers
 * and a specified number of radio buttons with OK and 
 * cancel buttons at the bottom.
 */
public class FileAndRadioDialog extends JDialog {
    JFrame _f;
    JButton _okButton = new JButton("Ok");
    JButton _cancelButton = new JButton("Cancel");
    
    JTextField _fileTextFields[];
    JButton _fileBrowseButtons[];
    JRadioButton _radioButtons[];
    String _filenames[];

    String[] _fileMessages;
    JFileChooser[] _fileDialogs;

    /**
     * constructor
     */
    public FileAndRadioDialog(JFrame parent, String title, boolean modal, 
		       String[] fileMessages, String[] radioLabels,
		       boolean[] radioStates, ItemListener[] radioListeners, 
		       JFileChooser[] fileDialogs) {
	super(parent, title, modal);
	_f = parent;
	_fileMessages = fileMessages;
	_fileDialogs = fileDialogs;

	_fileTextFields = new JTextField[_fileMessages.length];
	_fileBrowseButtons = new JButton[_fileMessages.length];
	_radioButtons = new JRadioButton[radioLabels.length];
	_filenames = new String[_fileMessages.length];

	configure(title, radioLabels, radioStates, radioListeners);
    }//constructor

    public void show(){
	_cancel = false;
	super.show();
    }

    public void configure(String title, String[] radioLabels,
			  boolean[] radioStates, 
			  ItemListener[] radioListeners){
	Container contentPane = getContentPane();
	//	contentPane.setLayout(new GridLayout(4,1));
	contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
	
	/*
	 * add 1st panel: file selectors
	 */
	JPanel fileDialogPanel = new JPanel();
	int nFiles = _fileMessages.length;
	fileDialogPanel.setLayout(new GridLayout(nFiles,3));
	for(int i=0; i<=nFiles-1; i++){
	    String fileMessage = _fileMessages[i];
	    fileDialogPanel.add(new JLabel(fileMessage));
	    _fileTextFields[i] = new JTextField();
	    JTextField jtf = _fileTextFields[i];
	    fileDialogPanel.add(jtf);
	    jtf.setEditable(false);
	    _fileBrowseButtons[i] = new JButton("browse...");
	    JButton jb = _fileBrowseButtons[i];
	    jb.setBorder(_raisedBevel);
	    fileDialogPanel.add(jb);
	    //add listener
	    jb.addActionListener(new GetFilename(this, i, _fileDialogs[i]));
	}
	contentPane.add(fileDialogPanel);
	
	/*
	 * add 2nd panel: options label
	 */
	int nRadioButtons = radioLabels.length;
	JLabel optionsLabel = new JLabel("Options");
	if(nRadioButtons > 0) contentPane.add(optionsLabel);
	Font font = new Font("Arial", Font.BOLD, 20);
	optionsLabel.setFont(font);
	optionsLabel.setForeground(Color.black);
	
	/*
	 * add 3rd panel: JRadioButtons
	 */
	JPanel optionsPanel = new JPanel();
	optionsPanel.setLayout(new GridLayout(nRadioButtons,1));
	for(int j=0; j<=nRadioButtons-1; j++){
	    String radioLabel = radioLabels[j];
	    boolean state = radioStates[j];
	    _radioButtons[j] = new JRadioButton(radioLabel, state);
	    JRadioButton jrb = _radioButtons[j];
	    ItemListener radioListener = radioListeners[j];
	    jrb.addItemListener(radioListener);
	    optionsPanel.add(jrb);
	}
	contentPane.add(optionsPanel);
	
	/*
	 * add 4th panel: ok and cancel buttons
	 */
	JPanel btnPanel = new JPanel();
	btnPanel.setLayout(new GridLayout(1,2));
	_okButton.setBorder(_raisedBevel);
	_cancelButton.setBorder(_raisedBevel);
	btnPanel.add(_okButton);
	btnPanel.add(_cancelButton);
	contentPane.add(btnPanel);
	ActionListener okListener = new SetOk(this);
	ActionListener cancelListener = new SetCancel(this);
	_okButton.addActionListener(okListener);
	_cancelButton.addActionListener(cancelListener);
	requestFocus();
	pack();
    }//configure
    
    public Insets getInsets() {
	return new Insets(30,10,10,10);
    }//getInsets
    
    public class SetOk implements ActionListener{
	FileAndRadioDialog _fard = null;
	public SetOk(FileAndRadioDialog fard){
	    _fard = fard;
	}
	public void actionPerformed(ActionEvent e){
	    _fard._ok = true;
	    //setVisible(false);
	    dispose();
	}
    }//class SetOk

    public class SetCancel implements ActionListener{
	FileAndRadioDialog _fard = null;
	public SetCancel(FileAndRadioDialog fard){
	    _fard = fard;
	}
	public void actionPerformed(ActionEvent e){
	    _fard._cancel = true;
	    //      setVisible(false);
	    dispose();
	}
    }//class SetCancel

    /*
     * Listener for browse button.  Sets directory.
     */
    public class GetFilename implements ActionListener{
	FileAndRadioDialog _fard = null;
	int _fileNumber = -Integer.MAX_VALUE;
	JFileChooser _jfc = null;
	public GetFilename(FileAndRadioDialog fard, int fileNumber,
			   JFileChooser jfc){
	    _fard = fard;
	    _fileNumber = fileNumber;
	    _jfc = jfc;
	}//constructor
	public void actionPerformed(ActionEvent e){
	    int fileState = -Integer.MAX_VALUE;
	    File directory = null;
	    String directoryString = null;
	    String filename = null;
	    fileState = _jfc.showOpenDialog(_fard);
	    if(fileState == JFileChooser.APPROVE_OPTION){
		filename = _jfc.getName(_jfc.getSelectedFile());
		directory = _jfc.getCurrentDirectory();
		directoryString = directory.getAbsolutePath()+File.separator;
		_fard.setFilename(_fileNumber, directoryString+filename);
		_fard.setCurrentDirectory(directory);
	    }else{
		//set cancel
	    }
	}//actionPerformed
    }//class getFilename

    public String getFilename(int index){
	JTextField tf = _fileTextFields[index];
	return tf.getText();
    }

    private void setFilename(int fileNumber, String filename){
	JTextField tf = _fileTextFields[fileNumber];
	tf.setText(filename);
    }

    /*
     * Sets current directory in all JFileChoosers
     */
    public void setCurrentDirectory(File directory){
	for(int i=0; i<=_fileMessages.length-1; i++){
	    JFileChooser jfc = _fileDialogs[i];
	    jfc.setCurrentDirectory(directory);
	}
    }

    public boolean _ok = false;
    public boolean _cancel = false;
    Border _raisedBevel = BorderFactory.createRaisedBevelBorder();
}//class OkDialog
