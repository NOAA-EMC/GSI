/*
 * Purpose: This is the GUI to configure VSDB.     
 *       
 * Author: Deyong Xu / RTI @ JCSDA
 * Last update: 1/27/2015, Initial coding
 *  
 */

package iatgui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class Vsdb extends JPanel implements SizeDefinition, ActionListener {
	// Number of ENV vars in each step
	private final int SIZE_TOP_LEVEL = 12;
	private final int SIZE_STEP1 = 9;
	private final int SIZE_STEP2 = 8;
	private final int SIZE_STEP3 = 11;
	private final int SIZE_STEP4 = 5;
	private final int SIZE_STEP5 = 10;
	private final int SIZE_STEP6 = 12;

	// 7 panels: top-level + 6 steps )
	public JPanel theTopLevelConfigPanel = new JPanel();
	public JPanel theStep1ConfigPanel = new JPanel();
	public JPanel theStep2ConfigPanel = new JPanel();
	public JPanel theStep3ConfigPanel = new JPanel();
	public JPanel theStep4ConfigPanel = new JPanel();
	public JPanel theStep5ConfigPanel = new JPanel();
	public JPanel theStep6ConfigPanel = new JPanel();

	// Components for VSDB config panel
	// top-level config panel
	public JLabel[] theTopLevelConfigLblArr = new JLabel[SIZE_TOP_LEVEL];
	public JTextArea[] theTopLevelConfigTxtArr = new JTextArea[SIZE_TOP_LEVEL];
	public String[] theTopLevelConfigEnvArr = new String[SIZE_TOP_LEVEL];
	public String[] theTopLevelConfigLblValueArr = new String[SIZE_TOP_LEVEL];
	public String[] theTopLevelConfigTxtValueArr = new String[SIZE_TOP_LEVEL];
	public String[] theTopLevelConfigTxtInitValueArr = new String[SIZE_TOP_LEVEL];
	public JButton[] theTopLevelConfigBrowseBtnArr = new JButton[SIZE_TOP_LEVEL];
	public JButton theTopLevelConfigSaveBtn = new JButton("Save");
	public JButton theTopLevelConfigResetBtn = new JButton("Default");

	// Step 1 config panel
	public JLabel[] theStep1ConfigLblArr = new JLabel[SIZE_STEP1];
	public JTextArea[] theStep1ConfigTxtArr = new JTextArea[SIZE_STEP1];
	public String[] theStep1ConfigEnvArr = new String[SIZE_STEP1];
	public String[] theStep1ConfigLblValueArr = new String[SIZE_STEP1];
	public String[] theStep1ConfigTxtValueArr = new String[SIZE_STEP1];
	public String[] theStep1ConfigTxtInitValueArr = new String[SIZE_STEP1];
	public JButton[] theStep1ConfigBrowseBtnArr = new JButton[SIZE_STEP1];
	public JButton theStep1ConfigSaveBtn = new JButton("Save");
	public JButton theStep1ConfigResetBtn = new JButton("Default");

	// Step 2 config panel
	public JLabel[] theStep2ConfigLblArr = new JLabel[SIZE_STEP2];
	public JTextArea[] theStep2ConfigTxtArr = new JTextArea[SIZE_STEP2];
	public String[] theStep2ConfigEnvArr = new String[SIZE_STEP2];
	public String[] theStep2ConfigLblValueArr = new String[SIZE_STEP2];
	public String[] theStep2ConfigTxtValueArr = new String[SIZE_STEP2];
	public String[] theStep2ConfigTxtInitValueArr = new String[SIZE_STEP2];
	public JButton theStep2ConfigSaveBtn = new JButton("Save");
	public JButton theStep2ConfigResetBtn = new JButton("Default");

	// Step 3 config panel
	public JLabel[] theStep3ConfigLblArr = new JLabel[SIZE_STEP3];
	public JTextArea[] theStep3ConfigTxtArr = new JTextArea[SIZE_STEP3];
	public String[] theStep3ConfigEnvArr = new String[SIZE_STEP3];
	public String[] theStep3ConfigLblValueArr = new String[SIZE_STEP3];
	public String[] theStep3ConfigTxtValueArr = new String[SIZE_STEP3];
	public String[] theStep3ConfigTxtInitValueArr = new String[SIZE_STEP3];
	public JButton[] theStep3ConfigBrowseBtnArr = new JButton[SIZE_STEP3];
	public JButton theStep3ConfigSaveBtn = new JButton("Save");
	public JButton theStep3ConfigResetBtn = new JButton("Default");

	// Step 4 config panel
	public JLabel[] theStep4ConfigLblArr = new JLabel[SIZE_STEP4];
	public JTextArea[] theStep4ConfigTxtArr = new JTextArea[SIZE_STEP4];
	public String[] theStep4ConfigEnvArr = new String[SIZE_STEP4];
	public String[] theStep4ConfigLblValueArr = new String[SIZE_STEP4];
	public String[] theStep4ConfigTxtValueArr = new String[SIZE_STEP4];
	public String[] theStep4ConfigTxtInitValueArr = new String[SIZE_STEP4];
	public JButton theStep4ConfigSaveBtn = new JButton("Save");
	public JButton theStep4ConfigResetBtn = new JButton("Default");

	// Step 5 config panel
	public JLabel[] theStep5ConfigLblArr = new JLabel[SIZE_STEP5];
	public JTextArea[] theStep5ConfigTxtArr = new JTextArea[SIZE_STEP5];
	public String[] theStep5ConfigEnvArr = new String[SIZE_STEP5];
	public String[] theStep5ConfigLblValueArr = new String[SIZE_STEP5];
	public String[] theStep5ConfigTxtValueArr = new String[SIZE_STEP5];
	public String[] theStep5ConfigTxtInitValueArr = new String[SIZE_STEP5];
	public JButton[] theStep5ConfigBrowseBtnArr = new JButton[SIZE_STEP5];
	public JButton theStep5ConfigSaveBtn = new JButton("Save");
	public JButton theStep5ConfigResetBtn = new JButton("Default");

	// Step 6 config panel
	public JLabel[] theStep6ConfigLblArr = new JLabel[SIZE_STEP6];
	public JTextArea[] theStep6ConfigTxtArr = new JTextArea[SIZE_STEP6];
	public String[] theStep6ConfigEnvArr = new String[SIZE_STEP6];
	public String[] theStep6ConfigLblValueArr = new String[SIZE_STEP6];
	public String[] theStep6ConfigTxtValueArr = new String[SIZE_STEP6];
	public String[] theStep6ConfigTxtInitValueArr = new String[SIZE_STEP6];
	public JButton[] theStep6ConfigBrowseBtnArr = new JButton[SIZE_STEP6];
	public JButton theStep6ConfigSaveBtn = new JButton("Save");
	public JButton theStep6ConfigResetBtn = new JButton("Default");

	// Constructor
	Vsdb() {
		initializeLblTxt();
		addListeners();
	}

	// Events this class will handle
	public void addListeners() {
		// Save buttons
		theTopLevelConfigSaveBtn.setActionCommand("toplevelSave");
		theStep1ConfigSaveBtn.setActionCommand("step1Save");
		theStep2ConfigSaveBtn.setActionCommand("step2Save");
		theStep3ConfigSaveBtn.setActionCommand("step3Save");
		theStep4ConfigSaveBtn.setActionCommand("step4Save");
		theStep5ConfigSaveBtn.setActionCommand("step5Save");
		theStep6ConfigSaveBtn.setActionCommand("step6Save");

		theTopLevelConfigSaveBtn.addActionListener(this);
		theStep1ConfigSaveBtn.addActionListener(this);
		theStep2ConfigSaveBtn.addActionListener(this);
		theStep3ConfigSaveBtn.addActionListener(this);
		theStep4ConfigSaveBtn.addActionListener(this);
		theStep5ConfigSaveBtn.addActionListener(this);
		theStep6ConfigSaveBtn.addActionListener(this);

		// Reset buttons
		theTopLevelConfigResetBtn.setActionCommand("toplevelReset");
		theStep1ConfigResetBtn.setActionCommand("step1Reset");
		theStep2ConfigResetBtn.setActionCommand("step2Reset");
		theStep3ConfigResetBtn.setActionCommand("step3Reset");
		theStep4ConfigResetBtn.setActionCommand("step4Reset");
		theStep5ConfigResetBtn.setActionCommand("step5Reset");
		theStep6ConfigResetBtn.setActionCommand("step6Reset");

		theTopLevelConfigResetBtn.addActionListener(this);
		theStep1ConfigResetBtn.addActionListener(this);
		theStep2ConfigResetBtn.addActionListener(this);
		theStep3ConfigResetBtn.addActionListener(this);
		theStep4ConfigResetBtn.addActionListener(this);
		theStep5ConfigResetBtn.addActionListener(this);
		theStep6ConfigResetBtn.addActionListener(this);

		// Top Level Browse buttons
		for (int index = 0; index < SIZE_TOP_LEVEL; index++) {
			theTopLevelConfigBrowseBtnArr[index]
					.setActionCommand(theTopLevelConfigLblValueArr[index]);
			theTopLevelConfigBrowseBtnArr[index].addActionListener(this);
		}

		// Step 1 browse button
		for (int index = 0; index < SIZE_STEP1; index++) {
			theStep1ConfigBrowseBtnArr[index]
					.setActionCommand(theStep1ConfigLblValueArr[index]);
			theStep1ConfigBrowseBtnArr[index].addActionListener(this);
		}

		// Step 3 browse button
		for (int index = 0; index < SIZE_STEP3; index++) {
			theStep3ConfigBrowseBtnArr[index]
					.setActionCommand(theStep3ConfigLblValueArr[index]);
			theStep3ConfigBrowseBtnArr[index].addActionListener(this);
		}

		// Step 5 browse button
		for (int index = 0; index < SIZE_STEP5; index++) {
			theStep5ConfigBrowseBtnArr[index]
					.setActionCommand(theStep5ConfigLblValueArr[index]);
			theStep5ConfigBrowseBtnArr[index].addActionListener(this);
		}

		// Step 6 browse button
		for (int index = 0; index < SIZE_STEP6; index++) {
			theStep6ConfigBrowseBtnArr[index]
					.setActionCommand(theStep6ConfigLblValueArr[index]);
			theStep6ConfigBrowseBtnArr[index].addActionListener(this);
		}

	}

	// Initialize labels and textareas
	public void initializeLblTxt() {
		initializeTopLevel();
		initializeStep1();
		initializeStep2();
		initializeStep3();
		initializeStep4();
		initializeStep5();
		initializeStep6();
	}

	// Initial top-level config panel's label and textareas
	public void initializeTopLevel() {
		String[] initialEnvValueArr = { "ENV_VSDBHOME", "ENV_WORKSPACE",
				"ENV_ACCOUNT", "ENV_CUE2RUN", "ENV_CUE2FTP", "ENV_GROUP",
				"ENV_GSTAT", "ENV_CANLDIR", "ENV_ECMANLDIR", "ENV_OBSPCP",
				"ENV_GFSFITDIR", "ENV_OBDATA" };
		String[] initialLblValueArr = { "VSDBHOME", "WORKSPACE", "ACCOUNT",
				"CUE2RUN", "CUE2FTP", "GROUP", "gstat", "canldir", "ecmanldir",
				"OBSPCP", "gfsfitdir", "obdata" };
		String[] initialTxtValueArr = { DirSetter.getVsdbRoot(),
				DirSetter.getVsdbWorkspace(), "glbss", "batch", "batch", "g01",
				"${WORKSPACE}/data/input/gstat", "$gstat/canl", "$gstat/ecm",
				"${WORKSPACE}/data/input/qpf/OBSPRCP",
				"${WORKSPACE}/data/input/f2o",
				"${WORKSPACE}/data/input/plot2d/obdata" };

		for (int index = 0; index < SIZE_TOP_LEVEL; index++) {
			// These values will be updated once "save" button is clicked.
			theTopLevelConfigLblValueArr[index] = initialLblValueArr[index];
			theTopLevelConfigTxtValueArr[index] = initialTxtValueArr[index];
			theTopLevelConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theTopLevelConfigEnvArr[index] = initialEnvValueArr[index];
			theTopLevelConfigLblArr[index] = new JLabel(
					initialLblValueArr[index]);
			theTopLevelConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
			theTopLevelConfigBrowseBtnArr[index] = new JButton("Browse");
		}
	}

	// Initial step 1 config panel's label and textareas
	public void initializeStep1() {
		String[] initialEnvValueArr = { "ENV_MAKEVSDBDATA", "ENV_1_MYARCH",
				"ENV_1_EXPNLIST", "ENV_1_FCYCLIST", "ENV_1_DUMPLIST",
				"ENV_1_VHRLIST", "ENV_1_DATEST", "ENV_1_DATEND",
				"ENV_1_VLENGTH" };
		String[] initialLblValueArr = { "MAKEVSDBDATA", "myarch", "expnlist",
				"fcyclist", "dumplist", "vhrlist", "DATEST", "DATEND",
				"vlength" };
		String[] initialTxtValueArr = { "YES",
				"${WORKSPACE}/data/input/fcst_data", "gfs ecm", "00",
				".gfs. .ecm.", "00", "20140201", "20140228", "120" };

		for (int index = 0; index < SIZE_STEP1; index++) {
			// These values will be updated once "save" button is clicked.
			theStep1ConfigLblValueArr[index] = initialLblValueArr[index];
			theStep1ConfigTxtValueArr[index] = initialTxtValueArr[index];
			theStep1ConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theStep1ConfigEnvArr[index] = initialEnvValueArr[index];
			theStep1ConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theStep1ConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
			theStep1ConfigBrowseBtnArr[index] = new JButton("Browse");
		}
	}

	// Initial step 2 config panel's label and textareas
	public void initializeStep2() {
		String[] initialEnvValueArr = { "ENV_MAKEMAPS", "ENV_2_FCYCLE",
				"ENV_2_MDLIST", "ENV_2_VHRLIST", "ENV_2_DATEST",
				"ENV_2_DATEND", "ENV_2_VLENGTH", "ENV_2_MAPTOP" };
		String[] initialLblValueArr = { "MAKEMAPS", "fcycle", "mdlist",
				"vhrlist", "DATEST", "DATEND", "vlength", "maptop" };
		String[] initialTxtValueArr = { "NO", "00 ", "gfs ecm", "00",
				"20140201", "20140228", "120", "10" };

		for (int index = 0; index < SIZE_STEP2; index++) {
			// These values will be updated once "save" button is clicked.
			theStep2ConfigLblValueArr[index] = initialLblValueArr[index];
			theStep2ConfigTxtValueArr[index] = initialTxtValueArr[index];
			theStep2ConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theStep2ConfigEnvArr[index] = initialEnvValueArr[index];
			theStep2ConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theStep2ConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
		}
	}

	// Initial step 3 config panel's label and textareas
	public void initializeStep3() {
		String[] initialEnvValueArr = { "ENV_CONUSDATA", "ENV_3_COMROT",
				"ENV_3_EXPNLIST", "ENV_3_FTYPLIST", "ENV_3_DUMPLIST",
				"ENV_3_PTYPLIST", "ENV_3_BUCKET", "ENV_3_FHOUT", "ENV_3_CYCLE",
				"ENV_3_DATEST", "ENV_3_DATEND" };
		String[] initialLblValueArr = { "CONUSDATA", "COMROT", "expnlist",
				"ftyplist", "dumplist", "ptyplist", "bucket", "fhout", "cycle",
				"DATEST", "DATEND" };
		String[] initialTxtValueArr = { "NO",
				"${WORKSPACE}/data/input/fcst_data", "gfs gfs2", "pgb pgb",
				".gfs. .gfs.", "PRATE PRATE", "6", "6", "00", "20140201",
				"20140228" };

		for (int index = 0; index < SIZE_STEP3; index++) {
			// These values will be updated once "save" button is clicked.
			theStep3ConfigLblValueArr[index] = initialLblValueArr[index];
			theStep3ConfigTxtValueArr[index] = initialTxtValueArr[index];
			theStep3ConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theStep3ConfigEnvArr[index] = initialEnvValueArr[index];
			theStep3ConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theStep3ConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
			theStep3ConfigBrowseBtnArr[index] = new JButton("Browse");

		}
	}

	// Initial step 4 config panel's label and textareas
	public void initializeStep4() {
		String[] initialEnvValueArr = { "ENV_CONUSPLOTS", "ENV_4_EXPNLIST",
				"ENV_4_CYCLIST", "ENV_4_DATEST", "ENV_4_DATEND" };
		String[] initialLblValueArr = { "CONUSPLOTS", "expnlist", "cyclist",
				"DATEST", "DATEND" };
		String[] initialTxtValueArr = { "NO", "gfs gfs2", "00", "20140201",
				"20140228" };

		for (int index = 0; index < SIZE_STEP4; index++) {
			// These values will be updated once "save" button is clicked.
			theStep4ConfigLblValueArr[index] = initialLblValueArr[index];
			theStep4ConfigTxtValueArr[index] = initialTxtValueArr[index];
			theStep4ConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theStep4ConfigEnvArr[index] = initialEnvValueArr[index];
			theStep4ConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theStep4ConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
		}
	}

	// Initial step 5 config panel's label and textareas
	public void initializeStep5() {
		String[] initialEnvValueArr = { "ENV_FIT2OBS", "ENV_5_FITDIR",
				"ENV_5_EXPNLIST", "ENV_5_ENDIANLIST", "ENV_5_CYCLE",
				"ENV_5_OINC_F2O", "ENV_5_FINC_F2O", "ENV_5_FMAX_F2O",
				"ENV_5_DATEST", "ENV_5_DATEND" };
		String[] initialLblValueArr = { "FIT2OBS", "fitdir", "expnlist",
				"endianlist", "cycle", "oinc_f2o", "finc_f2o", "fmax_f2o",
				"DATEST", "DATEND" };
		String[] initialTxtValueArr = { "YES", "${WORKSPACE}/data/input/f2o",
				"fit_model  fit_model2", "little little", "00", "24", "24",
				"120", "20130801", "20130814" };

		for (int index = 0; index < SIZE_STEP5; index++) {
			// These values will be updated once "save" button is clicked.
			theStep5ConfigLblValueArr[index] = initialLblValueArr[index];
			theStep5ConfigTxtValueArr[index] = initialTxtValueArr[index];
			theStep5ConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theStep5ConfigEnvArr[index] = initialEnvValueArr[index];
			theStep5ConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theStep5ConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
			theStep5ConfigBrowseBtnArr[index] = new JButton("Browse");
		}
	}

	// Initial step 6 config panel's label and textareas
	public void initializeStep6() {
		String[] initialEnvValueArr = { "ENV_MAPS2D", "ENV_6_MYARCH",
				"ENV_6_EXPNLIST", "ENV_6_DUMPLIST", "ENV_6_FDLIST",
				"ENV_6_CYCLE", "ENV_6_DATEST", "ENV_6_NDAYS", "ENV_6_NLEV",
				"ENV_6_GRID", "ENV_6_PBTM", "ENV_6_PTOP" };
		String[] initialLblValueArr = { "MAPS2D", "myarch", "expnlist",
				"dumplist", "fdlist", "cycle", "DATEST", "ndays", "nlev",
				"grid", "pbtm", "ptop" };
		String[] initialTxtValueArr = { "NO",
				"${WORKSPACE}/data/input/fcst_data", "gfs ecm", ".gfs. .ecm.",
				"anl 1 5 10", "00", "20140201", "28", "26", "G2", "1000", "1" };

		for (int index = 0; index < SIZE_STEP6; index++) {
			// These values will be updated once "save" button is clicked.
			theStep6ConfigLblValueArr[index] = initialLblValueArr[index];
			theStep6ConfigTxtValueArr[index] = initialTxtValueArr[index];
			theStep6ConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theStep6ConfigEnvArr[index] = initialEnvValueArr[index];
			theStep6ConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theStep6ConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
			theStep6ConfigBrowseBtnArr[index] = new JButton("Browse");
		}
	}

	// Display Top-level config panel
	public void showTopLevelConfigPanel() {
		theTopLevelConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout topLevelConfigPanelLayout = new SpringLayout();
		theTopLevelConfigPanel.setLayout(topLevelConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_TOP_LEVEL];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_TOP_LEVEL];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[SIZE_TOP_LEVEL];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_TOP_LEVEL; index++) {

			// Get values from stored JLabel and JTextArea arrays
			theTopLevelConfigLblArr[index] = new JLabel(
					theTopLevelConfigLblValueArr[index]);
			theTopLevelConfigTxtArr[index] = new JTextArea(
					theTopLevelConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theTopLevelConfigPanel.add(theTopLevelConfigLblArr[index]);
			theTopLevelConfigPanel.add(theTopLevelConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			switch (index) {
			case 0:
			case 1:
			case 6:
			case 7:
			case 8:
			case 9:
			case 10:
			case 11:
				theTopLevelConfigPanel
						.add(theTopLevelConfigBrowseBtnArr[index]);
				break;
			default:
				break;
			}

			// Position labels
			contraint_1_Arr[index] = topLevelConfigPanelLayout
					.getConstraints(theTopLevelConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = topLevelConfigPanelLayout
					.getConstraints(theTopLevelConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			contraint_3_Arr[index] = topLevelConfigPanelLayout
					.getConstraints(theTopLevelConfigBrowseBtnArr[index]);
			contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			contraint_3_Arr[index].setY(Spring.constant(yPos));
			contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			contraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theTopLevelConfigPanel.add(theTopLevelConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = topLevelConfigPanelLayout
				.getConstraints(theTopLevelConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theTopLevelConfigPanel.add(theTopLevelConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = topLevelConfigPanelLayout
				.getConstraints(theTopLevelConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

	}

	// Display step 1 config panel
	public void showStep1ConfigPanel() {
		theStep1ConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout step1ConfigPanelLayout = new SpringLayout();
		theStep1ConfigPanel.setLayout(step1ConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_STEP1];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_STEP1];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[SIZE_STEP1];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_STEP1; index++) {

			// Initialize each JLabel and JTextArea
			theStep1ConfigLblArr[index] = new JLabel(
					theStep1ConfigLblValueArr[index]);
			theStep1ConfigTxtArr[index] = new JTextArea(
					theStep1ConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theStep1ConfigPanel.add(theStep1ConfigLblArr[index]);
			theStep1ConfigPanel.add(theStep1ConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			if (index == 1) {
				theStep1ConfigPanel.add(theStep1ConfigBrowseBtnArr[index]);
			}

			// Position labels
			contraint_1_Arr[index] = step1ConfigPanelLayout
					.getConstraints(theStep1ConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = step1ConfigPanelLayout
					.getConstraints(theStep1ConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			contraint_3_Arr[index] = step1ConfigPanelLayout
					.getConstraints(theStep1ConfigBrowseBtnArr[index]);
			contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			contraint_3_Arr[index].setY(Spring.constant(yPos));
			contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			contraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theStep1ConfigPanel.add(theStep1ConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = step1ConfigPanelLayout
				.getConstraints(theStep1ConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theStep1ConfigPanel.add(theStep1ConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = step1ConfigPanelLayout
				.getConstraints(theStep1ConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));
	}

	// Display step 2 config panel
	public void showStep2ConfigPanel() {
		theStep2ConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout step2ConfigPanelLayout = new SpringLayout();
		theStep2ConfigPanel.setLayout(step2ConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_STEP2];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_STEP2];
		// SpringLayout.Constraints[] contraint_3_Arr = new
		// SpringLayout.Constraints[SIZE_STEP2];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_STEP2; index++) {

			// Initialize each JLabel and JTextArea
			theStep2ConfigLblArr[index] = new JLabel(
					theStep2ConfigLblValueArr[index]);
			theStep2ConfigTxtArr[index] = new JTextArea(
					theStep2ConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theStep2ConfigPanel.add(theStep2ConfigLblArr[index]);
			theStep2ConfigPanel.add(theStep2ConfigTxtArr[index]);

			// Position labels
			contraint_1_Arr[index] = step2ConfigPanelLayout
					.getConstraints(theStep2ConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = step2ConfigPanelLayout
					.getConstraints(theStep2ConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theStep2ConfigPanel.add(theStep2ConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = step2ConfigPanelLayout
				.getConstraints(theStep2ConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theStep2ConfigPanel.add(theStep2ConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = step2ConfigPanelLayout
				.getConstraints(theStep2ConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));
	}

	// Display step 3 config panel
	public void showStep3ConfigPanel() {
		theStep3ConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout step3ConfigPanelLayout = new SpringLayout();
		theStep3ConfigPanel.setLayout(step3ConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_STEP3];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_STEP3];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[SIZE_STEP3];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_STEP3; index++) {

			// Initialize each JLabel and JTextArea
			theStep3ConfigLblArr[index] = new JLabel(
					theStep3ConfigLblValueArr[index]);
			theStep3ConfigTxtArr[index] = new JTextArea(
					theStep3ConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theStep3ConfigPanel.add(theStep3ConfigLblArr[index]);
			theStep3ConfigPanel.add(theStep3ConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			if (index == 1) {
				theStep3ConfigPanel.add(theStep3ConfigBrowseBtnArr[index]);
			}

			// Position labels
			contraint_1_Arr[index] = step3ConfigPanelLayout
					.getConstraints(theStep3ConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = step3ConfigPanelLayout
					.getConstraints(theStep3ConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			contraint_3_Arr[index] = step3ConfigPanelLayout
					.getConstraints(theStep3ConfigBrowseBtnArr[index]);
			contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			contraint_3_Arr[index].setY(Spring.constant(yPos));
			contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			contraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theStep3ConfigPanel.add(theStep3ConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = step3ConfigPanelLayout
				.getConstraints(theStep3ConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theStep3ConfigPanel.add(theStep3ConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = step3ConfigPanelLayout
				.getConstraints(theStep3ConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

	}

	// Display step 4 config panel
	public void showStep4ConfigPanel() {
		theStep4ConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout step4ConfigPanelLayout = new SpringLayout();
		theStep4ConfigPanel.setLayout(step4ConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_STEP4];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_STEP4];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[SIZE_STEP4];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_STEP4; index++) {

			// Initialize each JLabel and JTextArea
			theStep4ConfigLblArr[index] = new JLabel(
					theStep4ConfigLblValueArr[index]);
			theStep4ConfigTxtArr[index] = new JTextArea(
					theStep4ConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theStep4ConfigPanel.add(theStep4ConfigLblArr[index]);
			theStep4ConfigPanel.add(theStep4ConfigTxtArr[index]);

			// Position labels
			contraint_1_Arr[index] = step4ConfigPanelLayout
					.getConstraints(theStep4ConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = step4ConfigPanelLayout
					.getConstraints(theStep4ConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theStep4ConfigPanel.add(theStep4ConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = step4ConfigPanelLayout
				.getConstraints(theStep4ConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theStep4ConfigPanel.add(theStep4ConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = step4ConfigPanelLayout
				.getConstraints(theStep4ConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

	}

	// Display step 5 config panel
	public void showStep5ConfigPanel() {
		theStep5ConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout step5ConfigPanelLayout = new SpringLayout();
		theStep5ConfigPanel.setLayout(step5ConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_STEP5];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_STEP5];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[SIZE_STEP5];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_STEP5; index++) {

			// Initialize each JLabel and JTextArea
			theStep5ConfigLblArr[index] = new JLabel(
					theStep5ConfigLblValueArr[index]);
			theStep5ConfigTxtArr[index] = new JTextArea(
					theStep5ConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theStep5ConfigPanel.add(theStep5ConfigLblArr[index]);
			theStep5ConfigPanel.add(theStep5ConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			if (index == 1) {
				theStep5ConfigPanel.add(theStep5ConfigBrowseBtnArr[index]);
			}

			// Position labels
			contraint_1_Arr[index] = step5ConfigPanelLayout
					.getConstraints(theStep5ConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = step5ConfigPanelLayout
					.getConstraints(theStep5ConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			contraint_3_Arr[index] = step5ConfigPanelLayout
					.getConstraints(theStep5ConfigBrowseBtnArr[index]);
			contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			contraint_3_Arr[index].setY(Spring.constant(yPos));
			contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			contraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theStep5ConfigPanel.add(theStep5ConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = step5ConfigPanelLayout
				.getConstraints(theStep5ConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theStep5ConfigPanel.add(theStep5ConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = step5ConfigPanelLayout
				.getConstraints(theStep5ConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

	}

	// Display step 6 config panel
	public void showStep6ConfigPanel() {
		theStep6ConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout step6ConfigPanelLayout = new SpringLayout();
		theStep6ConfigPanel.setLayout(step6ConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[SIZE_STEP6];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[SIZE_STEP6];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[SIZE_STEP6];

		// Add Labels and TextAreas
		for (int index = 0; index < SIZE_STEP6; index++) {

			// Initialize each JLabel and JTextArea
			theStep6ConfigLblArr[index] = new JLabel(
					theStep6ConfigLblValueArr[index]);
			theStep6ConfigTxtArr[index] = new JTextArea(
					theStep6ConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theStep6ConfigPanel.add(theStep6ConfigLblArr[index]);
			theStep6ConfigPanel.add(theStep6ConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			if (index == 1) {
				theStep6ConfigPanel.add(theStep6ConfigBrowseBtnArr[index]);
			}

			// Position labels
			contraint_1_Arr[index] = step6ConfigPanelLayout
					.getConstraints(theStep6ConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = step6ConfigPanelLayout
					.getConstraints(theStep6ConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			contraint_3_Arr[index] = step6ConfigPanelLayout
					.getConstraints(theStep6ConfigBrowseBtnArr[index]);
			contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			contraint_3_Arr[index].setY(Spring.constant(yPos));
			contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			contraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add "save" button
		theStep6ConfigPanel.add(theStep6ConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints saveBtnCons = step6ConfigPanelLayout
				.getConstraints(theStep6ConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH));
		saveBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theStep6ConfigPanel.add(theStep6ConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints resetBtnCons = step6ConfigPanelLayout
				.getConstraints(theStep6ConfigResetBtn);
		resetBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		resetBtnCons.setY(Spring.constant(yPos + 3 * SPACER));
		resetBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		resetBtnCons.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

	}

	public void actionPerformed(ActionEvent e) {
		// Get name of action component
		String actName = e.getActionCommand();

		// Check if browse button clicked.
		for (int index = 0; index < SIZE_TOP_LEVEL; index++) {
			if (actName.equals(theTopLevelConfigLblValueArr[index])) {
				String[] strArr = new String[1];
				int result = getDir(strArr);
				if (result == 0)
					theTopLevelConfigTxtValueArr[index] = strArr[0];
				theTopLevelConfigTxtArr[index].setText(strArr[0]);
				break;
			}
		}

		for (int index = 0; index < SIZE_STEP1; index++) {
			if (actName.equals(theStep1ConfigLblValueArr[index])) {
				String[] strArr = new String[1];
				int result = getDir(strArr);
				if (result == 0) {
					theStep1ConfigTxtValueArr[index] = strArr[0];
					theStep1ConfigTxtArr[index].setText(strArr[0]);
					break;
				}
			}
		}

		for (int index = 0; index < SIZE_STEP3; index++) {
			if (actName.equals(theStep3ConfigLblValueArr[index])) {
				String[] strArr = new String[1];
				int result = getDir(strArr);
				if (result == 0) {
					theStep3ConfigTxtValueArr[index] = strArr[0];
					theStep3ConfigTxtArr[index].setText(strArr[0]);
					break;
				}
			}
		}

		for (int index = 0; index < SIZE_STEP5; index++) {
			if (actName.equals(theStep5ConfigLblValueArr[index])) {
				String[] strArr = new String[1];
				int result = getDir(strArr);
				if (result == 0) {
					theStep5ConfigTxtValueArr[index] = strArr[0];
					theStep5ConfigTxtArr[index].setText(strArr[0]);
					break;
				}
			}
		}

		for (int index = 0; index < SIZE_STEP6; index++) {
			if (actName.equals(theStep6ConfigLblValueArr[index])) {
				String[] strArr = new String[1];
				int result = getDir(strArr);
				if (result == 0) {
					theStep6ConfigTxtValueArr[index] = strArr[0];
					theStep6ConfigTxtArr[index].setText(strArr[0]);
					break;
				}
			}
		}

		switch (actName) {
		case "toplevelSave":
			try {
				saveChangesTopLevel();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
				JOptionPane.showMessageDialog(null, "Error !");
			}
			break;
		case "toplevelReset":
			resetTopLevel();
			break;
		case "step1Save":
			try {
				saveChangesStep1();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step1Reset":
			resetStep1();
			break;
		case "step2Save":
			try {
				saveChangesStep2();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step2Reset":
			resetStep2();
			break;
		case "step3Save":
			try {
				saveChangesStep3();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step3Reset":
			resetStep3();
			break;
		case "step4Save":
			try {
				saveChangesStep4();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step4Reset":
			resetStep4();
			break;
		case "step5Save":
			try {
				saveChangesStep5();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step5Reset":
			resetStep5();
			break;
		case "step6Save":
			try {
				saveChangesStep6();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step6Reset":
			resetStep6();
			break;
		}

	}

	public int saveChangesMsg(String aTitleString) {
		int n = JOptionPane.showConfirmDialog(null, "Save changes?",
				aTitleString, JOptionPane.YES_NO_OPTION);
		if (n == 0) {
			JOptionPane.showMessageDialog(null, "Changes saved.");
		} else
			JOptionPane.showMessageDialog(null, "Changes not saved.");
		return n;
	}

	public int resetChanges(String aString) {
		int n = JOptionPane.showConfirmDialog(null, "Reset?", aString,
				JOptionPane.YES_NO_OPTION);
		return n;
	}

	// Save values in GUI to a file for top level config
	public void saveChangesTopLevel() throws IOException {
		int n = saveChangesMsg("top level config");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbTop_gui.config";
			else
				filename = filename + "/" + "vsdbTop_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_TOP_LEVEL; index++) {
				// Save values in GUI to value array
				theTopLevelConfigTxtValueArr[index] = theTopLevelConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theTopLevelConfigEnvArr[index]
						+ "=\"" + theTopLevelConfigTxtArr[index].getText()
						+ "\"";
				print_line.printf("%s%n", tmpString);

				// Need to export VSDBHOME, WORKSPACE and gstat explicitly
				// because they
				// are used to build some of ENV variables
				// export VSDBHOME
				if (index == 0) {
					tmpString = "export " + "VSDBHOME=\""
							+ theTopLevelConfigTxtArr[index].getText() + "\"";
					print_line.printf("%s%n", tmpString);
				}

				// export WORKSPACE
				if (index == 1) {
					tmpString = "export " + "WORKSPACE=\""
							+ theTopLevelConfigTxtArr[index].getText() + "\"";
					print_line.printf("%s%n", tmpString);
				}

				// export gstat
				if (index == 6) {
					tmpString = "export " + "gstat=\""
							+ theTopLevelConfigTxtArr[index].getText() + "\"";
					print_line.printf("%s%n", tmpString);
				}

			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Save values in GUI to a file for step 1 config
	public void saveChangesStep1() throws IOException {
		int n = saveChangesMsg("step1");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbStep1_gui.config";
			else
				filename = filename + "/" + "vsdbStep1_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_STEP1; index++) {
				// Save values in GUI to value array
				theStep1ConfigTxtValueArr[index] = theStep1ConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theStep1ConfigEnvArr[index]
						+ "=\"" + theStep1ConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Save values in GUI to a file for step 2 config
	public void saveChangesStep2() throws IOException {
		int n = saveChangesMsg("step2");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbStep2_gui.config";
			else
				filename = filename + "/" + "vsdbStep2_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_STEP2; index++) {
				// Save values in GUI to value array
				theStep2ConfigTxtValueArr[index] = theStep2ConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theStep2ConfigEnvArr[index]
						+ "=\"" + theStep2ConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Save values in GUI to a file for step 3 config
	public void saveChangesStep3() throws IOException {
		int n = saveChangesMsg("step3");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbStep3_gui.config";
			else
				filename = filename + "/" + "vsdbStep3_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_STEP3; index++) {
				// Save values in GUI to value array
				theStep3ConfigTxtValueArr[index] = theStep3ConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theStep3ConfigEnvArr[index]
						+ "=\"" + theStep3ConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Save values in GUI to a file for step 4 config
	public void saveChangesStep4() throws IOException {
		int n = saveChangesMsg("step4");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbStep4_gui.config";
			else
				filename = filename + "/" + "vsdbStep4_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_STEP4; index++) {
				// Save values in GUI to value array
				theStep4ConfigTxtValueArr[index] = theStep4ConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theStep4ConfigEnvArr[index]
						+ "=\"" + theStep4ConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Save values in GUI to a file for step 5 config
	public void saveChangesStep5() throws IOException {
		int n = saveChangesMsg("step5");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbStep5_gui.config";
			else
				filename = filename + "/" + "vsdbStep5_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_STEP5; index++) {
				// Save values in GUI to value array
				theStep5ConfigTxtValueArr[index] = theStep5ConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theStep5ConfigEnvArr[index]
						+ "=\"" + theStep5ConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Save values in GUI to a file for step 6 config
	public void saveChangesStep6() throws IOException {
		int n = saveChangesMsg("step6");
		if (n == 0) {
			String filename = DirSetter.getVsdbRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "vsdbStep6_gui.config";
			else
				filename = filename + "/" + "vsdbStep6_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < SIZE_STEP6; index++) {
				// Save values in GUI to value array
				theStep6ConfigTxtValueArr[index] = theStep6ConfigTxtArr[index]
						.getText();

				// Save values in GUI into file
				String tmpString = "export " + theStep6ConfigEnvArr[index]
						+ "=\"" + theStep6ConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	// Reset values in GUI for top level config
	public void resetTopLevel() {
		int n = resetChanges("top level config");
		if (n == 0) {
			for (int index = 0; index < SIZE_TOP_LEVEL; index++) {
				// Save values in GUI to value array
				theTopLevelConfigTxtArr[index]
						.setText(theTopLevelConfigTxtInitValueArr[index]);
			}
		}
	}

	// Reset values in GUI for step 1 config
	public void resetStep1() {
		int n = resetChanges("step1 config");
		if (n == 0) {
			for (int index = 0; index < SIZE_STEP1; index++) {
				// Save values in GUI to value array
				theStep1ConfigTxtArr[index]
						.setText(theStep1ConfigTxtInitValueArr[index]);
			}
		}
	}

	// Reset values in GUI for step 2 config
	public void resetStep2() {
		int n = resetChanges("step2 config");
		if (n == 0) {
			for (int index = 0; index < SIZE_STEP2; index++) {
				// Save values in GUI to value array
				theStep2ConfigTxtArr[index]
						.setText(theStep2ConfigTxtInitValueArr[index]);
			}
		}
	}

	// Reset values in GUI for step 3 config
	public void resetStep3() {
		int n = resetChanges("step3 config");
		if (n == 0) {
			for (int index = 0; index < SIZE_STEP3; index++) {
				// Save values in GUI to value array
				theStep3ConfigTxtArr[index]
						.setText(theStep3ConfigTxtInitValueArr[index]);
			}
		}
	}

	// Reset values in GUI for step 4 config
	public void resetStep4() {
		int n = resetChanges("step4 config");
		if (n == 0) {
			for (int index = 0; index < SIZE_STEP4; index++) {
				// Save values in GUI to value array
				theStep4ConfigTxtArr[index]
						.setText(theStep4ConfigTxtInitValueArr[index]);
			}
		}
	}

	// Reset values in GUI for step 5 config
	public void resetStep5() {
		int n = resetChanges("step5 config");
		if (n == 0) {
			for (int index = 0; index < SIZE_STEP5; index++) {
				// Save values in GUI to value array
				theStep5ConfigTxtArr[index]
						.setText(theStep5ConfigTxtInitValueArr[index]);
			}
		}
	}

	// Reset values in GUI for step 6 config
	public void resetStep6() {
		int n = resetChanges("step6 config");
		if (n == 0) {
			for (int index = 0; index < SIZE_STEP6; index++) {
				// Save values in GUI to value array
				theStep6ConfigTxtArr[index]
						.setText(theStep6ConfigTxtInitValueArr[index]);
			}
		}
	}

	// Get directory via "browse" button
	public int getDir(String[] strArr) {
		// Create the log first, because the action listeners
		// need to refer to it.
		JFileChooser fc = new JFileChooser();
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		// fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

		int returnVal = fc.showOpenDialog(Vsdb.this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			strArr[0] = file.toString();
		}

		return returnVal;
	}
}
