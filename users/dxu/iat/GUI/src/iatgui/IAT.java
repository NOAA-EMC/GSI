package iatgui;

import java.io.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
import javax.swing.border.*;

import java.beans.*;
import java.util.Random;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;
import java.util.LinkedList;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.text.NumberFormat;
import java.net.*;

public class IAT extends JPanel implements ActionListener, ItemListener,
		PropertyChangeListener {

	// Constant
	private final int LBL_WIDTH = 200;
	private final int LBL_HEIGHT = 15;
	private final int TEXTAREA_WIDTH = 400;
	private final int TEXTAREA_HEIGHT = 15;
	private final int BUTTON_WIDTH = 100;
	private final int BUTTON_HEIGHT = 15;
	private final int SPACER = 5;

	// 1. Main panels
	private JPanel theTitlePanel = new JPanel();
	private JPanel theRunPanel = new JPanel();
	private JPanel theConfigPanel = new JPanel();

	SpringLayout theConfigPanelLayout = new SpringLayout();

	// 2. Components of theRunPanel
	private JPanel theIatCheckBoxPanel = new JPanel(new GridLayout(5, 1));
	private JButton theRunButton = new JButton("Run");
	private JButton theParButton = new JButton("Generate PAR");

	// 3. Components of theConfigPanel
	private JPanel theEmptyConfigPanel = new JPanel();
	private JPanel theRadmonConfigPanel = new JPanel();
	private JPanel theGeConfigPanel = new JPanel();
	private JPanel theHitConfigPanel = new JPanel();

	/*
	 * 4. Configuration for individual IAT config panels
	 */
	// 0) Default Empty config panel
	private JLabel emptyLbl = new JLabel("Default");
	private JTextArea emptyTextArea = new JTextArea("Default data");

	// 1) Components for RADMON config panel

	// 4) Components for GE config panel

	// 5) Components for HIT config panel

	private JButton theCancelButton = new JButton("Cancel");

	// 5. IAT choice and its components
	private Choice theIAT_Choice = new Choice();
	private Choice theVsdb_Choice = new Choice();

	private JCheckBox theRADMON_CheckBox = new JCheckBox("RADMON", false);
	private JCheckBox theVSDB_CheckBox = new JCheckBox("VSDB", false);
	private JCheckBox theFD_CheckBox = new JCheckBox("fcstDiff", false);
	private JCheckBox theGE_CheckBox = new JCheckBox("gribExtrm", false);
	private JCheckBox theHIT_CheckBox = new JCheckBox("HIT", false);

	// 6. Five package classes
	private Radmon theRadmon = new Radmon();
	private Vsdb theVsdb = new Vsdb();
	private Ge theGe = new Ge();
	private FcstDiff theFcstDiff = new FcstDiff();
	private Hit theHit = new Hit();

	// Constructor
	public IAT() {
		// Set up initial panel
		setInitialPanel();

		// Add listener
		theIAT_Choice.addItemListener(this);
		theVsdb_Choice.addItemListener(this);

	}

	/**
	 * Invoked when button clicked
	 */
	public void actionPerformed(ActionEvent evt) {

	}

	/**
	 * Action to take when item chosen
	 */
	public void itemStateChanged(ItemEvent e) {

		Object source = e.getItemSelectable();

		if (source == theIAT_Choice) {
			switch (theIAT_Choice.getSelectedItem()) {
			case "choose...":
				addEmptyConfigPanel();
				break;
			case "radmon":
				addRADMON_ConfigPanel();
				break;
			case "vsdb":
				addVSDB_ConfigPanel();
				break;
			case "fcstDiff":
				addFcstDiffConfigPanel();
				break;
			case "gribExtrm":
				addGE_ConfigPanel();
				break;
			case "hit":
				addHIT_ConfigPanel();
				break;
			default:
				addEmptyConfigPanel();
				break;
			}
		} else if (source == theVsdb_Choice) {
			switch (theVsdb_Choice.getSelectedItem()) {
			case "top-level config":
				addVsdbTopLevelConfigPanel();
				break;
			case "step 1":
				addVsdbstep_1_ConfigPanel();
				break;
			case "step 2":
				addVsdbstep_2_ConfigPanel();
				break;
			case "step 3":
				addVsdbstep_3_ConfigPanel();
				break;
			case "step 4":
				addVsdbstep_4_ConfigPanel();
				break;
			case "step 5":
				addVsdbstep_5_ConfigPanel();
				break;
			case "step 6":
				addVsdbstep_6_ConfigPanel();
				break;
			default:
				addVsdbTopLevelConfigPanel();
				break;
			}
		}

	}

	private void addHIT_ConfigPanel() {

	}

	private void addGE_ConfigPanel() {

	}

	private void addFcstDiffConfigPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theFcstDiffConfigPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theFcstDiff.theFcstDiffConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints fcstDiffConfigPanelCons = theConfigPanelLayout
				.getConstraints(theFcstDiff.theFcstDiffConfigPanel);
		fcstDiffConfigPanelCons.setX(Spring.constant(10));
		fcstDiffConfigPanelCons.setY(Spring.constant(35));
		fcstDiffConfigPanelCons.setWidth(Spring.constant(900));
		fcstDiffConfigPanelCons.setHeight(Spring.constant(700));
		
		theFcstDiff.showConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	private void addVSDB_ConfigPanel() {
		theConfigPanel.removeAll();

		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addRADMON_ConfigPanel() {
		theConfigPanel.removeAll();

		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theEmptyConfigPanel);

		SpringLayout.Constraints emptyConfigPanelCons = theConfigPanelLayout
				.getConstraints(theEmptyConfigPanel);
		emptyConfigPanelCons.setX(Spring.constant(10));
		emptyConfigPanelCons.setY(Spring.constant(10 + SPACER));
		emptyConfigPanelCons.setWidth(Spring.constant(TEXTAREA_WIDTH));
		emptyConfigPanelCons.setHeight(Spring.constant(TEXTAREA_HEIGHT));

		// 4.1 add components into theConfigPanel
		theEmptyConfigPanel.add(emptyLbl);
		theEmptyConfigPanel.add(emptyTextArea);

		// 4.2 Position components within theConfigPanel using SpringLayout and
		// Contraint.
		SpringLayout emptyConfigPanelLayout = new SpringLayout();
		theEmptyConfigPanel.setLayout(emptyConfigPanelLayout);

		SpringLayout.Constraints emptyLblCons = emptyConfigPanelLayout
				.getConstraints(emptyLbl);
		emptyLblCons.setX(Spring.constant(10));
		emptyLblCons.setY(Spring.constant(10));
		emptyLblCons.setWidth(Spring.constant(LBL_WIDTH));
		emptyLblCons.setHeight(Spring.constant(LBL_HEIGHT));

		SpringLayout.Constraints emptyTextAreaCons = emptyConfigPanelLayout
				.getConstraints(emptyTextArea);
		emptyTextAreaCons.setX(Spring.constant(10));
		emptyTextAreaCons.setY(Spring.constant(10 + LBL_HEIGHT + SPACER));
		emptyTextAreaCons.setWidth(Spring.constant(TEXTAREA_WIDTH));
		emptyTextAreaCons.setHeight(Spring.constant(TEXTAREA_HEIGHT));

	}

	private void addEmptyConfigPanel() {

		theConfigPanel.removeAll();

		int spacer = 5;
		int xOrig = 110;
		int xWidth = 150;
		int yHeight = 30;

		// 4.1 add components into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theEmptyConfigPanel);
		// theConfigPanel.add(theGeConfigPanel);

		// 4.2 Position components within theConfigPanel using SpringLayout and
		// Contraint.
		theConfigPanel.setLayout(theConfigPanelLayout);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(xWidth));
		iatChoiceCons.setHeight(Spring.constant(yHeight));

		// Position theEmptyConfigPanel within the config panel
		SpringLayout.Constraints emptyConfigPanelCons = theConfigPanelLayout
				.getConstraints(theEmptyConfigPanel);
		emptyConfigPanelCons.setX(Spring.constant(10));
		emptyConfigPanelCons.setY(Spring.constant(10 + yHeight + spacer));
		emptyConfigPanelCons.setWidth(Spring.constant(600));
		emptyConfigPanelCons.setHeight(Spring.constant(500));

		// --------------------------------------------------------------------

		SpringLayout emptyConfigPanelLayout = new SpringLayout();
		theEmptyConfigPanel.setLayout(emptyConfigPanelLayout);

		SpringLayout.Constraints emptyLblCons = emptyConfigPanelLayout
				.getConstraints(emptyLbl);
		emptyLblCons.setX(Spring.constant(10));
		emptyLblCons.setY(Spring.constant(10));
		emptyLblCons.setWidth(Spring.constant(xWidth));
		emptyLblCons.setHeight(Spring.constant(yHeight));

		SpringLayout.Constraints emptyTextAreaCons = emptyConfigPanelLayout
				.getConstraints(emptyTextArea);
		emptyTextAreaCons.setX(Spring.constant(10));
		emptyTextAreaCons.setY(Spring.constant(10 + yHeight + spacer));
		emptyTextAreaCons.setWidth(Spring.constant(100));
		emptyTextAreaCons.setHeight(Spring.constant(200));

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbTopLevelConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theTopLevelConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdb_Choice within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdb_Choice within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theTopLevelConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showTopLevelConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbstep_1_ConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theStep1ConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdbStep1ConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdbStep1ConfigPanel within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theStep1ConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep1ConfigPanel();
		
		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbstep_2_ConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theStep2ConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdbStep2ConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdbStep2ConfigPanel within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theStep2ConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep2ConfigPanel();
		
		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbstep_3_ConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theStep3ConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdbStep3ConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdbStep3ConfigPanel within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theStep3ConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep3ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbstep_4_ConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theStep4ConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdbStep4ConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdbStep4ConfigPanel within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theStep4ConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep4ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbstep_5_ConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theStep5ConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdbStep5ConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdbStep5ConfigPanel within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theStep5ConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep5ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbstep_6_ConfigPanel() {
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theVsdb_Choice into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theVsdb_Choice);
		theConfigPanel.add(theVsdb.theStep6ConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theVsdbStep6ConfigPanel within the config panel
		SpringLayout.Constraints vsdbChoicelCons = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		vsdbChoicelCons.setX(Spring.constant(180));
		vsdbChoicelCons.setY(Spring.constant(10));
		vsdbChoicelCons.setWidth(Spring.constant(150));
		vsdbChoicelCons.setHeight(Spring.constant(30));

		// Position theVsdbStep6ConfigPanel within the config panel
		SpringLayout.Constraints vsdbTopLevelConfigPanelCons = theConfigPanelLayout
				.getConstraints(theVsdb.theStep6ConfigPanel);
		vsdbTopLevelConfigPanelCons.setX(Spring.constant(10));
		vsdbTopLevelConfigPanelCons.setY(Spring.constant(35));
		vsdbTopLevelConfigPanelCons.setWidth(Spring.constant(900));
		vsdbTopLevelConfigPanelCons.setHeight(Spring.constant(700));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep6ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	/*
	 * Set up the beginning paGE_ of IAT
	 */
	private void setInitialPanel() {
		// ==================================================================
		// 1. main panel
		// ==================================================================
		add(theTitlePanel);
		add(theRunPanel);
		add(theConfigPanel);

		SpringLayout totalLayout = new SpringLayout();
		setLayout(totalLayout);

		int xPos = 30;
		int yPos = 10;
		int ySpacer = 10;

		int titlePanelWidth = 1000;
		int titlePanelHeight = 60;
		int runPanelWidth = 1000;
		int runPanelHeight = 120;
		int configPanelWidth = 1000;
		int configPanelHeight = 800;

		SpringLayout.Constraints theTitlePanelCons = totalLayout
				.getConstraints(theTitlePanel);
		theTitlePanelCons.setX(Spring.constant(xPos));
		theTitlePanelCons.setY(Spring.constant(yPos));
		theTitlePanelCons.setWidth(Spring.constant(titlePanelWidth));
		theTitlePanelCons.setHeight(Spring.constant(titlePanelHeight));

		SpringLayout.Constraints theRunPanelCons = totalLayout
				.getConstraints(theRunPanel);
		theRunPanelCons.setX(Spring.constant(xPos));
		theRunPanelCons
				.setY(Spring.constant(yPos + titlePanelHeight + ySpacer));
		theRunPanelCons.setWidth(Spring.constant(runPanelWidth));
		theRunPanelCons.setHeight(Spring.constant(runPanelHeight));

		SpringLayout.Constraints theConfigPanelCons = totalLayout
				.getConstraints(theConfigPanel);
		theConfigPanelCons.setX(Spring.constant(xPos));
		theConfigPanelCons.setY(Spring.constant(yPos + titlePanelHeight
				+ ySpacer + runPanelHeight + ySpacer));
		theConfigPanelCons.setWidth(Spring.constant(configPanelWidth));
		theConfigPanelCons.setHeight(Spring.constant(configPanelHeight));

		// ==================================================================
		// 2. theTitlePanel ( 800 x 60 )
		// ==================================================================
		JLabel titleLabel = new JLabel("Independent Assessment Tool");
		titleLabel.setFont(new Font("Nimbus Mono L", Font.BOLD, 30));

		SpringLayout theTitlePanelLayout = new SpringLayout();
		theTitlePanel.setLayout(theTitlePanelLayout);

		SpringLayout.Constraints theTitlePanelLayoutCons = theTitlePanelLayout
				.getConstraints(titleLabel);
		theTitlePanelLayoutCons.setX(Spring.constant(200));
		theTitlePanelLayoutCons.setY(Spring.constant(0));
		theTitlePanelLayoutCons.setWidth(Spring.constant(800));
		theTitlePanelLayoutCons.setHeight(Spring.constant(60));

		theTitlePanel.add(titleLabel);
		theTitlePanel.setBackground(Color.LIGHT_GRAY);

		// ==================================================================
		// 3. theRunPanel ( 800 x 120 )
		// ==================================================================
		//
		// 3.1 theIatCheckBoxPanel
		//
		// Add components into theIatCheckBoxPanel
		theIatCheckBoxPanel.add(theRADMON_CheckBox);
		theIatCheckBoxPanel.add(theVSDB_CheckBox);
		theIatCheckBoxPanel.add(theFD_CheckBox);
		theIatCheckBoxPanel.add(theGE_CheckBox);
		theIatCheckBoxPanel.add(theHIT_CheckBox);

		// Set border for theIatCheckBoxPanel
		Border lowerBorder = BorderFactory.createLoweredBevelBorder();
		TitledBorder theIatCheckBoxPanelBorder = BorderFactory
				.createTitledBorder(lowerBorder, "IAT Selection");
		theIatCheckBoxPanelBorder.setTitleJustification(TitledBorder.CENTER);
		theIatCheckBoxPanel.setBorder(theIatCheckBoxPanelBorder);

		// 3.2 Add 3 components into theRunPanel
		theRunPanel.add(theIatCheckBoxPanel);
		theRunPanel.add(theRunButton);
		theRunPanel.add(theParButton);

		// 3.3 Position 3 components within theRunPanel
		SpringLayout theRunPanelLayout = new SpringLayout();
		theRunPanel.setLayout(theRunPanelLayout);

		int spacer = 5;
		int xOrig = 110;
		int xWidth = 150;
		int yHeight = 30;
		SpringLayout.Constraints iatCheckBoxPanelCons = theRunPanelLayout
				.getConstraints(theIatCheckBoxPanel);
		iatCheckBoxPanelCons.setX(Spring.constant(0));
		iatCheckBoxPanelCons.setY(Spring.constant(0));
		iatCheckBoxPanelCons.setWidth(Spring.constant(100));
		iatCheckBoxPanelCons.setHeight(Spring.constant(100));

		SpringLayout.Constraints runButtonCons = theRunPanelLayout
				.getConstraints(theRunButton);
		runButtonCons.setX(Spring.constant(xOrig));
		runButtonCons.setY(Spring.constant(70));
		runButtonCons.setWidth(Spring.constant(xWidth));
		runButtonCons.setHeight(Spring.constant(yHeight));

		SpringLayout.Constraints parButtonCons = theRunPanelLayout
				.getConstraints(theParButton);
		parButtonCons.setX(Spring.constant(xOrig + xWidth + spacer));
		parButtonCons.setY(Spring.constant(70));
		parButtonCons.setWidth(Spring.constant(xWidth));
		parButtonCons.setHeight(Spring.constant(yHeight));

		// ==================================================================
		// 4. theConfigPanel ( 800 x 600 )
		// ==================================================================
		Border lineBorder = BorderFactory.createLineBorder(Color.black);
		LineBorder theConfigPanelBorder = (LineBorder) BorderFactory
				.createLineBorder(Color.black);
		theConfigPanel.setBorder(theConfigPanelBorder);

		// theIAT_Choice (pull-down options)
		theIAT_Choice.add("choose...");
		theIAT_Choice.add("radmon");
		theIAT_Choice.add("vsdb");
		theIAT_Choice.add("fcstDiff");
		theIAT_Choice.add("gribExtrm");
		theIAT_Choice.add("hit");

		// theIAT_Choice (pull-down options)
		theVsdb_Choice.add("choose...");
		theVsdb_Choice.add("top-level config");
		theVsdb_Choice.add("step 1");
		theVsdb_Choice.add("step 2");
		theVsdb_Choice.add("step 3");
		theVsdb_Choice.add("step 4");
		theVsdb_Choice.add("step 5");
		theVsdb_Choice.add("step 6");

		// 4.1 add components into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theEmptyConfigPanel);
		// theConfigPanel.add(theGeConfigPanel);

		// 4.2 Position components within theConfigPanel using SpringLayout and
		// Contraint.
		theConfigPanel.setLayout(theConfigPanelLayout);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(xWidth));
		iatChoiceCons.setHeight(Spring.constant(yHeight));

		// Position theEmptyConfigPanel within the config panel
		SpringLayout.Constraints emptyConfigPanelCons = theConfigPanelLayout
				.getConstraints(theEmptyConfigPanel);
		emptyConfigPanelCons.setX(Spring.constant(10));
		emptyConfigPanelCons.setY(Spring.constant(10 + yHeight + spacer));
		emptyConfigPanelCons.setWidth(Spring.constant(600));
		emptyConfigPanelCons.setHeight(Spring.constant(500));

		// 4.1 add components into theConfigPanel
		theEmptyConfigPanel.add(emptyLbl);
		theEmptyConfigPanel.add(emptyTextArea);

		// 4.2 Position components within theConfigPanel using SpringLayout and
		// Contraint.
		SpringLayout emptyConfigPanelLayout = new SpringLayout();
		theEmptyConfigPanel.setLayout(emptyConfigPanelLayout);

		SpringLayout.Constraints emptyLblCons = emptyConfigPanelLayout
				.getConstraints(emptyLbl);
		emptyLblCons.setX(Spring.constant(10));
		emptyLblCons.setY(Spring.constant(10));
		emptyLblCons.setWidth(Spring.constant(xWidth));
		emptyLblCons.setHeight(Spring.constant(yHeight));

		SpringLayout.Constraints emptyTextAreaCons = emptyConfigPanelLayout
				.getConstraints(emptyTextArea);
		emptyTextAreaCons.setX(Spring.constant(10));
		emptyTextAreaCons.setY(Spring.constant(10 + yHeight + spacer));
		emptyTextAreaCons.setWidth(Spring.constant(100));
		emptyTextAreaCons.setHeight(Spring.constant(200));

	}

	/**
	 * Action to take whe property chanGE_d.
	 */
	public void propertyChange(PropertyChangeEvent evt) {

	}

	private static void createAndShowGUI() {
		// Create and set up the window.
		JFrame frame = new JFrame("IAT Control Panel");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		// Create and set up the content pane.
		JComponent newContentPane = new IAT();
		newContentPane.setPreferredSize(new Dimension(1300, 1000));
		newContentPane.setOpaque(true); // content panes must be opaque
		frame.setContentPane(newContentPane);

		// Display the window.
		frame.pack();
		frame.setSize(920, 800);
		frame.setResizable(true);
		Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
		if (frame.getWidth() > d.width)
			frame.setSize(d.width, frame.getHeight());
		if (frame.getHeight() > d.height)
			frame.setSize(frame.getWidth(), d.height);

		frame.setVisible(true);
		frame.setLocationRelativeTo(null);
	}

	/**
	 * Main method to run
	 */
	public static void main(String[] args) {
		// Schedule a job for the event-dispatching thread:
		// creating and showing this application's GUI.
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});
	}

}
