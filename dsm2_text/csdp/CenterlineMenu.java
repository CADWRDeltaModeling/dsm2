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
package DWR.CSDP;
import DWR.CSDP.dialog.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.io.*;
/**
 * calls methods for creating and editing centerlines
 *
 * @author
 * @version
 */
public class CenterlineMenu{

    public CenterlineMenu(CsdpFrame gui){
	_gui = gui;
	_td = new TextDialog((Frame)_gui, "Enter new centerline name", true);
	_ynd = new YesNoDialog(_gui, "messagemessagemessagemessagemessagemessagemessage", true);
    }

  public void setLandmark(Landmark landmark){
    _landmark = landmark;
  }

  /**
   * return to cursor (turn modes off)
   */
  public class CCursor implements ActionListener{
      CsdpFrame _gui;
      public CCursor(CsdpFrame gui){
	  _gui=gui;
      }

    public void actionPerformed(ActionEvent e){
      _gui.setStopEditingMode();
      _gui.setCursor(CsdpFunctions._defaultCursor);
    }
  }// class CAddPoint
  
    /**
     * Remove a centerline from the network.
     *
     * @author
     * @version $Id:
     */
    public class CRemove implements ActionListener{
	TextDialog _td;
	YesNoDialog _yndRemove;
	OkDialog _okd;
	public CRemove(CsdpFrame gui){
	    _gui = gui;
	    _td = new TextDialog((Frame)_gui, "Enter name of centerline to remove", true);
	    _yndRemove = new YesNoDialog(_gui, "Are you sure?", true, 25);
	    _okd = new OkDialog(_gui, "centerline doesn't exist", true);
	}
	public void actionPerformed(ActionEvent e){
	    _net = _gui.getNetwork();
	    if(_net == null){
		System.out.println("ERROR in CenterlineMenu.CRemove.actionPerformed: network is null!");
	    }else{
		_td.show();
		String cname = _td.tf.getText();
		//does specified centerline exist?
		if(_net.centerlineExists(cname)){
		    _yndRemove.setTitle("Remove Centerline "+cname+"?");
		    _yndRemove.reset();
		    _yndRemove.show();
		    if(_yndRemove._yes){
			_net.removeCenterline(cname);
			_gui._canvas1.redoNextPaint();
			_gui._canvas1.repaint();
		    }
		}else{
		    //requested centerline doesn't exist
		    _okd.setMessage("requested centerline doesn't exist");
		    _okd.show();
		}
	    }
	}
    }

  /**
   * Draw new centerline--mouse clicks will add points
   *
   * @author
   * @version $Id: CenterlineMenu.java,v 1.2 2002/10/21 19:58:26 btom Exp $
   */
  public class CCreate implements ActionListener{
      TextDialog _td;
  /**
   * assign instances of application and gui classes to class variables
   */
    public CCreate(App app, CsdpFrame gui) {
      _app = app;
      _gui = gui;
      _td = new TextDialog((Frame)_gui, "Enter new centerline name", true);
    }
    public void actionPerformed(ActionEvent e) {
      _net = _gui.getNetwork();
      if(_net == null){
	_net = new Network("delta");
	_gui.setNetwork(_net);
	_app._net = _net;
	_nplot = _app.setNetworkPlotter();
	_gui._canvas1.setNetworkPlotter(_nplot);
	_gui._canvas1.setUpdateNetwork(true);
	//removed for conversion to swing
	_gui._canvas1.redoNextPaint();
	_gui._canvas1.repaint();

	//	_gui.enableAfterNetwork();
	_gui.enableWhenNetworkExists();
      }//if net is null
      
      String centerlineName = null;
      ////      TextDialog d = new TextDialog((Frame)_gui, "Enter new centerline name", true);
      _td.show();
      centerlineName = _td.tf.getText();

      if(centerlineName.length() > 0){
	  if(_net.getCenterline(centerlineName) != null){
	      _ynd.setTitle("Centerline "+centerlineName+" already exists.  Replace?");
	      _ynd.reset();
	      _ynd.show();
	      if(_ynd._yes == true){
		  addCenterline(centerlineName);
	      }
	  }
	  else{
	      addCenterline(centerlineName);
	  }
      }//if centerline name is not blank
    }//actionPerformed
      protected void addCenterline(String centerlineName){
	_net.addCenterline(centerlineName);
	_net.setSelectedCenterlineName(centerlineName);
	_net.setSelectedCenterline(_net.getCenterline(centerlineName));
	_gui.enableAfterCenterlineSelected();
	_gui.setAddPointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
    
  }// class CCreate
    
/**
 * Create new centerline with 2 points.  User will specify DSM channel number and
 * the new centerline will have its first point at the upstream node and the 
 * second point at the downstream node.
 *
 * @author
 * @version
 */
    public class CDSMCreate implements ActionListener{
	/**
	 * assign instances of application and gui classes to class variables
	 */
	public CDSMCreate(DSMChannels DSMChannels, App app, CsdpFrame gui) {
	    _DSMChannels = DSMChannels;
	    _app = app;
	    _gui = gui;
	    _jfcChannelsInp = new JFileChooser();
	    _channelsInpFilter = new CsdpFileFilter
		(_channelsInpExtensions, _numChannelsInpExtensions);
	    _errorDialog = new OkDialog(_gui, "messagemessagemessagemessagemessagemessagemessagemessagemessagemessagemessage", true);
	    _td.setTitle("Enter a new DSM channel number");
	}

	public void actionPerformed(ActionEvent e) {
	    _gui.setDefaultModesStates();
	    _gui.setAllButtonsDefaultColor();
	    _net = _gui.getNetwork();
	    if(_net == null){
		_net = new Network("delta");
		_gui.setNetwork(_net);
		_app._net = _net;
		_nplot = _app.setNetworkPlotter();
		_gui._canvas1.setNetworkPlotter(_nplot);
		_gui._canvas1.setUpdateNetwork(true);
	//removed for conversion to swing
		_gui._canvas1.redoNextPaint();
		_gui._canvas1.repaint();

		//	_gui.enableAfterNetwork();
		_gui.enableWhenNetworkExists();
	    }//if net is null

	    _td.show();
	    String centerlineName = _td.tf.getText();
	    boolean loadAnotherChannelsInpFile = true;

	    //if channels.inp file not loaded OR if channel # doesn't exist in current
	    //DSMChannels object.  ask user if another file should be loaded--don't
	    //assume there is another file with the channel.
	    while(loadAnotherChannelsInpFile){
		if(_DSMChannels != null &&
		   _DSMChannels.channelExists(centerlineName) == false){
		    _ynd.setTitle("Channel "+centerlineName+" not found in channel connectivity file.  Load another file?");
		    _ynd.reset();
		    _ynd.show();
		    if(_ynd._yes == true){
			loadAnotherChannelsInpFile = true;
		    }else{
			loadAnotherChannelsInpFile = false;
		    }
		}

		if(_DSMChannels == null || loadAnotherChannelsInpFile){
		    String channelsFilename = null;
		    //	FileDialog fd = new FileDialog(_gui, "Open DSM2 channel connectivity file");
		    //	fd.show();
		    _jfcChannelsInp.setDialogTitle("Open DSM2 channel connectivity file");
		    _jfcChannelsInp.setApproveButtonText("Open");
		    _jfcChannelsInp.addChoosableFileFilter(_channelsInpFilter);
		    _jfcChannelsInp.setFileFilter(_channelsInpFilter);
		    
		    if(CsdpFunctions.getOpenDirectory() != null){
			_jfcChannelsInp.setCurrentDirectory
			    (CsdpFunctions.getOpenDirectory());
		    }
		    _filechooserState = _jfcChannelsInp.showOpenDialog(_gui);
		    if(_filechooserState == JFileChooser.APPROVE_OPTION){
			channelsFilename = _jfcChannelsInp.getName
			    (_jfcChannelsInp.getSelectedFile());
			_directory = _jfcChannelsInp.getCurrentDirectory().
			    getAbsolutePath()+File.separator;
			
			//	channelsFilename = fd.getFile();
			//	_directory = fd.getDirectory();
			_gui.setCursor(_waitCursor);
			_DSMChannels = _app.chanReadStore(_directory, channelsFilename);
			_gui.setDSMChannels(_DSMChannels);
			_gui.setCursor(_defaultCursor);
		    }else{
			loadAnotherChannelsInpFile = false;
		    }
		}//if DSMChannels is null
		
		if(_filechooserState == JFileChooser.APPROVE_OPTION){
		    if(_net.getCenterline(centerlineName) != null){
			_ynd.setTitle("Centerline "+centerlineName+" already exists. Replace?");
			_ynd.reset();
			_ynd.show();
			if(_ynd._yes == true){
			    //			    addDSMChannel(centerlineName);
			    loadAnotherChannelsInpFile = addDSMChannel(centerlineName);
			}
		    }
		    else{
			//			addDSMChannel(centerlineName);
			loadAnotherChannelsInpFile = addDSMChannel(centerlineName);
		    }
		}//if the cancel button wasn't pressed
	    }//while
	}//actionPerformed

    /**
     * adds a centerline for the specified DSM channel number.  First point is
     * located at upstream node, last point is located at downstream node.
     */
    protected boolean addDSMChannel(String centerlineName){
      int upnode = 0;
      int downnode = 0;
      String upnodeString = null;
      String downnodeString = null;
      float upnodeX   = 0.0f;
      float upnodeY   = 0.0f;
      float downnodeX = 0.0f;
      float downnodeY = 0.0f;
      Centerline centerline = null;
      boolean landmarkError = false;
      boolean channelsInpError = false;

      _net.addCenterline(centerlineName);
      centerline = _net.getCenterline(centerlineName);
      upnode   = _DSMChannels.getUpnode(centerlineName);
      downnode = _DSMChannels.getDownnode(centerlineName);

      if(upnode < 0 || downnode < 0){
	  _errorDialog.setMessage
	      ("ERROR:  node not found for centerline "+centerlineName);
	  _errorDialog.show();
	  channelsInpError = true;
      }
      
//        Integer upnodeInteger = new Integer(upnode);
//        Integer downnodeInteger = new Integer(downnode);
//        upnodeString = upnodeInteger.toString(upnode);
//        downnodeString = downnodeInteger.toString(downnode);

      upnodeString = Integer.toString(upnode);
      downnodeString = Integer.toString(downnode);

      boolean giveUp = false;
      float upX = -CsdpFunctions.BIG_FLOAT;
      float upY = -CsdpFunctions.BIG_FLOAT;
      float downX = -CsdpFunctions.BIG_FLOAT;
      float downY = -CsdpFunctions.BIG_FLOAT;

      while(giveUp == false){
	  if(DEBUG)System.out.println("landmark="+_landmark);
	  if(_landmark == null) _landmark = _gui.getLandmark(); //load landmark file
	  upX   = CsdpFunctions.metersToFeet(_landmark.getX(upnodeString));
	  upY   = CsdpFunctions.metersToFeet(_landmark.getY(upnodeString));
	  downX = CsdpFunctions.metersToFeet(_landmark.getX(downnodeString));
	  downY = CsdpFunctions.metersToFeet(_landmark.getY(downnodeString));
	  
	  if(upX < 0.0f || upY < 0.0f){
	      _errorDialog.setMessage
		  ("ERROR:  insufficient information in landmark file for node "+
		   upnodeString+".");
	      landmarkError = true;
	  }
	  if(downX < 0.0f || downY < 0.0f){
	      _errorDialog.setMessage
		  ("ERROR:  insufficient information in landmark file for node "+
		   downnodeString+".");
	      landmarkError = true;
	  }
	  if(landmarkError){
	      _errorDialog.show();
	      _ynd.setTitle("Load another landmark file?");
	      _ynd.reset();
	      _ynd.show();

	      if(_ynd._yes == true){
		  _landmark = _gui.getLandmark(); //load landmark file
	      }else if(_ynd._no == true || _ynd._cancel == true){
		  giveUp = true;
	      }
	  }else{
	      giveUp=true;
	  }
      }//while

      if(channelsInpError == false && landmarkError == false){
	  //getX function returns -BIG_FLOAT if node not found in open landmark file
	  if(upX<0.0f || upY<0.0f || downX<0.0f || downY<0.0){
	      _landmark = _gui.getLandmark(); //load landmark file
	  }//could use a while loop, but user would never get out if no landmark file
	  upnodeX   = _landmark.getX(upnodeString);
	  upnodeY   = _landmark.getY(upnodeString);
	  downnodeX = _landmark.getX(downnodeString);
	  downnodeY = _landmark.getY(downnodeString);
	  centerline.addCenterlinePoint(upnodeX, upnodeY);
	  centerline.addCenterlinePoint(downnodeX, downnodeY);
	  if(DEBUG)System.out.println
		       ("landmark coordinates: upstream xy, downstream xy="+
			upnodeX+","+upnodeY+","+downnodeX+","+downnodeY);
	  
	  _net.setSelectedCenterlineName(centerlineName);
	  _net.setSelectedCenterline(_net.getCenterline(centerlineName));
	  _gui.enableAfterCenterlineSelected();
	  _gui._canvas1.setUpdateNetwork(true);
	//removed for conversion to swing
	  _gui._canvas1.redoNextPaint();
	  _gui._canvas1.repaint();
      }
      return channelsInpError;
    }//addDSMChannel

      JFileChooser _jfcChannelsInp;
      CsdpFileFilter _channelsInpFilter;
      int _filechooserState;
  }// class CDSMCreate
  
  /**
   * Rename centerline
   */
  public class CRename implements ActionListener{
    public void actionPerformed(ActionEvent e) {
      if(_net != null){
	Centerline centerline = _net.getSelectedCenterline();
	String oldCenterlineName = _net.getSelectedCenterlineName();
	if(centerline != null){
	    _td.setTitle("Enter a new centerline name");
	    _td.show();
	    String newCenterlineName = _td.tf.getText();
	    centerline.setCenterlineName(newCenterlineName);
	    _net.renameCenterline(oldCenterlineName, newCenterlineName);
	}//if centerline has been selected
      }//if there is a network
    }//actionPerformed    
  }// class CRename
  
    /**
     * move point in centerline
     */
    public class CMovePoint implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setMovePointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setMovePointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CMovePoint

    /**
     * insert point in centerline
     */
    public class CInsertPoint implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setInsertPointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setInsertPointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CInsertPoint
    /**
     * add point to centerline
     */
    public class CAddPoint implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setAddPointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setAddPointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CAddPoint

    /**
     * delete point from centerline
     */
    public class CDeletePoint implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setDeletePointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setDeletePointMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CDelPoint

    /**
     * add cross-section to centerline
     */
    public class CAddXsect implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setAddXsectMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setAddXsectMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CAddXsect

    /**
     * remove cross-section from centerline
     */
    public class CRemoveXsect implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setRemoveXsectMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setRemoveXsectMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CRemoveXsect

    /**
     * move cross-section along centerline
     */
    public class CMoveXsect implements ItemListener, ActionListener{
      public void itemStateChanged(ItemEvent e){
	  //        _gui.setMoveXsectMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
      public void actionPerformed(ActionEvent e){
	  //        _gui.setMoveXsectMode();
	_gui.setCursor(CsdpFunctions._handCursor);
      }
  
    }// class CMoveXsect

  /**
   * undo changes since last restore
   */
  public class CRestore implements ActionListener{
    public void actionPerformed(ActionEvent e) {
    }    
    
  }//class CRestore
  
  /**
   * Keeps changes for next restore command
   */
  public class CKeep implements ActionListener{
    public void actionPerformed(ActionEvent e) {
    }    
    
  }
  
  /**
   * view data along centerline
   */
  public class CView implements ActionListener{
    public void actionPerformed(ActionEvent e) {
    }    
    
  }// class CView
  
  /**
   * display centerline info (name and length) at bottom of frame or canvas
   */
  public class CInfo implements ActionListener{
    public void actionPerformed(ActionEvent e) {
    }    
    
  }// class CInfo
  
  /**
   * ?
   */
  public class CList implements ActionListener{
    public void actionPerformed(ActionEvent e) {
    }    
    
  }// class CList
  
  /**
   * display centerline prop (A,P,W,Zc,Xc,r)
   */
  public class CSummary implements ActionListener{
    public void actionPerformed(ActionEvent e) {
    }    
    
  }// class CSummary
  
  Network _net;
  App _app;
  CsdpFrame _gui;
  NetworkPlot _nplot;
  Landmark _landmark;
  DSMChannels _DSMChannels = null;
  String _directory = null;
  Cursor _waitCursor = new Cursor(Cursor.WAIT_CURSOR);
  Cursor _defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR);
    OkDialog _errorDialog;
  protected static final boolean DEBUG = false;
    TextDialog _td;
    YesNoDialog _ynd;
    String[] _channelsInpExtensions = {"inp"};
    int _numChannelsInpExtensions = 1;
}//CenterlineMenu
