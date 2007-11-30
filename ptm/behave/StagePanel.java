package DWR.DMS.PTM.behave;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author miller
 * @version 1.0
 */

public class StagePanel extends JPanel {
	private JPanel contentPane;
	private JPanel mainPanel = new JPanel();
	private JPanel horzPanel = new JPanel();
	private JPanel vertPanel = new JPanel();
	private JPanel velPanel = new JPanel();

	private BorderLayout borderLayout1 = new BorderLayout();
	private GridBagLayout gridBagLayout = new GridBagLayout();
	private Border border;
	private TitledBorder horzPosBorder;
	private TitledBorder vertPosBorder;
	private TitledBorder velBorder;

	private JLabel lblRise1 = new JLabel("Rising Stage");
	private JLabel lblFall1 = new JLabel("Falling Stage");
	private JLabel lblRise2 = new JLabel("Rising Stage");
	private JLabel lblFall2 = new JLabel("Falling Stage");
	private JLabel lblRise3 = new JLabel("Rising Stage");
	private JLabel lblFall3 = new JLabel("Falling Stage");

	private JComboBox cbbRiseUnits = new JComboBox(Units.velocity);

	private JTextField txtRiseLowerHorz = new JTextField();
	private JTextField txtRiseUpperHorz = new JTextField();
	private JTextField txtFallLowerHorz = new JTextField();
	private JTextField txtFallUpperHorz = new JTextField();
	private JTextField txtRiseUpperVert = new JTextField();
	private JTextField txtFallLowerVert = new JTextField();
	private JTextField txtFallUpperVert = new JTextField();
	private JTextField txtRiseLowerVert = new JTextField();
	private JTextField txtRiseVel = new JTextField();
	private JTextField txtFallVel = new JTextField();

	private JLabel lblLower1 = new JLabel("Lower Limit");
	private JLabel lblUpper1 = new JLabel("Upper Limit");
	private JLabel lblLower2 = new JLabel("Lower Limit");
	private JLabel lblUpper2 = new JLabel("Upper Limit");
	private JLabel lblVel = new JLabel("Velocity");
	private JLabel lblUnits = new JLabel("Units");

	StageElement thisElement;


	//Construct the frame
	public StagePanel(StageElement element) {
		thisElement = element;
		try{
		initComponents();
		}catch(Exception e){
			System.out.println(e);
		}
		getParams();
	}
	//Component initialization
	private void initComponents() throws Exception  {
		contentPane = this;
		border = BorderFactory.createEtchedBorder(Color.white,new Color(148, 145, 140));
		horzPosBorder = new TitledBorder(border,"Horizontal Position");
		vertPosBorder = new TitledBorder(border,"Vertical Postion");
		velBorder = new TitledBorder(border,"Additional Velocity");
		contentPane.setLayout(borderLayout1);

		mainPanel.setLayout(gridBagLayout);
		horzPanel.setLayout(gridBagLayout);
		horzPanel.setBorder(horzPosBorder);
		txtRiseLowerHorz.setColumns(10);
		txtRiseUpperHorz.setColumns(10);
		txtFallLowerHorz.setColumns(10);
		txtFallUpperHorz.setColumns(10);
		txtRiseUpperVert.setColumns(10);

		vertPanel.setLayout(gridBagLayout);
		vertPanel.setBorder(vertPosBorder);
		txtRiseLowerVert.setColumns(10);
		txtFallLowerVert.setColumns(10);
		txtFallUpperVert.setColumns(10);

		velPanel.setBorder(velBorder);
		velPanel.setLayout(gridBagLayout);
		txtRiseVel.setColumns(10);
		cbbRiseUnits.setMinimumSize(new Dimension(51, 21));
		cbbRiseUnits.setPreferredSize(new Dimension(51, 21));
		txtFallVel.setColumns(10);

		// GridBagConstraints(gridx, gridy, width, height, weightx, weighty, anchor, fill, insets, ipadx, ipady)
		mainPanel.add(horzPanel,         new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
																,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
		horzPanel.add(lblRise1,      new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
															//              ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 5), 0, 0));
															,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		horzPanel.add(txtRiseLowerHorz,     new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0
																   ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		horzPanel.add(txtRiseUpperHorz,     new GridBagConstraints(3, 1, 1, 1, 0.0, 0.0
																   ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		horzPanel.add(lblLower1, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
														,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		horzPanel.add(lblUpper1, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0
														,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		horzPanel.add(lblFall1,    new GridBagConstraints(0, 2, 1, 1, 1.0, 0.0
														  //              ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 5), 0, 0));
														  ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		horzPanel.add(txtFallLowerHorz,   new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0
																 ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		horzPanel.add(txtFallUpperHorz,   new GridBagConstraints(3, 2, 1, 1, 0.0, 0.0
																 ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));

		mainPanel.add(vertPanel,       new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0
															  ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
		vertPanel.add(lblRise2,  new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
														//              ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 5), 0, 0));
														,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		vertPanel.add(txtRiseLowerVert,  new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0
																,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		vertPanel.add(txtRiseUpperVert,  new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0
																,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		vertPanel.add(lblLower2, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
														,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		vertPanel.add(lblUpper2, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
														,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		vertPanel.add(lblFall2,     new GridBagConstraints(0, 2, 1, 1, 1.0, 0.0
														   ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		//              ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 5), 0, 0));
		vertPanel.add(txtFallLowerVert,   new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0
																 ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		vertPanel.add(txtFallUpperVert,   new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0
																 ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));

		mainPanel.add(velPanel,  new GridBagConstraints(0, 2, 1, 1, 1.0, 1.0
														,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
		velPanel.add(lblRise3,    new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
														 //              ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 5), 0, 0));
														 ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		velPanel.add(txtRiseVel,    new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0
														   ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		velPanel.add(cbbRiseUnits,     new GridBagConstraints(2, 1, 1, 2, 0.0, 0.0
															  ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		velPanel.add(lblFall3,   new GridBagConstraints(0, 2, 1, 1, 1.0, 0.0
														//              ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 5), 0, 0));
														,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 5), 0, 0));
		velPanel.add(txtFallVel,  new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0
														 ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		velPanel.add(lblVel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
													,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		velPanel.add(lblUnits,  new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
													   ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));

		contentPane.add(mainPanel, BorderLayout.CENTER);

	}

	/**
    *  Passes information from Behavior Fields to PhysicalElement
    */
	public void setParams() {

		thisElement.setLowerHorizontalRising(txtRiseLowerHorz.getText());
		thisElement.setUpperHorizontalRising(txtRiseUpperHorz.getText());
		thisElement.setLowerHorizontalFalling(txtFallLowerHorz.getText());
		thisElement.setUpperHorizontalFalling(txtFallUpperHorz.getText());
		thisElement.setLowerVerticalRising(txtRiseLowerVert.getText());
		thisElement.setUpperVerticalRising(txtRiseUpperVert.getText());
		thisElement.setLowerVerticalFalling(txtFallLowerVert.getText());
		thisElement.setUpperVerticalFalling(txtFallUpperVert.getText());
		thisElement.setVelocityRising(txtRiseVel.getText(),String.valueOf(cbbRiseUnits.getSelectedIndex()));
		thisElement.setVelocityFalling(txtFallVel.getText(),String.valueOf(cbbRiseUnits.getSelectedIndex()));
	}

	/**
    *  Gets information from StageElement and fills text Fields
    */
	public void getParams() {
		
		System.out.println("start dbg");
		txtRiseLowerHorz.setText(thisElement.getLowerHorizontalRising());
		System.out.println("next dbg");
		txtRiseUpperHorz.setText(thisElement.getUpperHorizontalRising());
		txtFallLowerHorz.setText(thisElement.getLowerHorizontalFalling());
		txtFallUpperHorz.setText(thisElement.getUpperHorizontalFalling());
		txtRiseLowerVert.setText(thisElement.getLowerVerticalRising());
		txtRiseUpperVert.setText(thisElement.getUpperVerticalRising());
		txtFallLowerVert.setText(thisElement.getLowerVerticalFalling());
		txtFallUpperVert.setText(thisElement.getUpperVerticalFalling());
		txtRiseVel.setText(thisElement.getVelocityRising());
		txtFallVel.setText(thisElement.getVelocityFalling());
		System.out.println("cbb dbg");
		cbbRiseUnits.setSelectedIndex(Integer.parseInt(thisElement.getVelocityUnits()));
	}

}




