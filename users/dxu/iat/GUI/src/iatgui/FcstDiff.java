/*
 * Purpose: This is the GUI to configure Forecast Difference.     
 *       
 * Author: Deyong Xu / RTI @ JCSDA
 * Last update: 1/27/2015, Initial coding 
 * 
 */

package iatgui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextArea;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class FcstDiff extends JPanel implements SizeDefinition, ActionListener {
	// Number of ENV vars in each step
	private final int ENV_VAR_SIZE = 13;

	public JPanel theConfigPanel = new JPanel();

	// 3) Components for FcstDiff config panel
	private JLabel[] theConfigLblArr = new JLabel[ENV_VAR_SIZE];
	private JTextArea[] theConfigTxtArr = new JTextArea[ENV_VAR_SIZE];
	private String[] theConfigEnvArr = new String[ENV_VAR_SIZE];
	private String[] theConfigLblValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtInitValueArr = new String[ENV_VAR_SIZE];
	private JButton[] theConfigBrowseBtnArr = new JButton[ENV_VAR_SIZE];

	private JButton theConfigSaveBtn = new JButton("Save");
	private JButton theConfigResetBtn = new JButton("Default");
	private JButton theNotesBtn = new JButton("Notes");

	private String theNotes = "";

	private JRadioButton[] theAirRadioBtnArr = new JRadioButton[2];
	// Group the radio buttons.
	ButtonGroup theAirGroup = new ButtonGroup();

	private JRadioButton[] theSfcRadioBtnArr = new JRadioButton[2];
	// Group the radio buttons.
	ButtonGroup theSfcGroup = new ButtonGroup();

	private JRadioButton[] theUvRadioBtnArr = new JRadioButton[2];
	// Group the radio buttons.
	ButtonGroup theUvGroup = new ButtonGroup();

	// Constructor
	FcstDiff() {
		initialize();
		addListeners();
	}

	// Events this class will handle.
	public void addListeners() {
		// Save buttons
		theConfigSaveBtn.setActionCommand("Save");
		theConfigSaveBtn.addActionListener(this);

		// Reset buttons
		theConfigResetBtn.setActionCommand("Reset");
		theConfigResetBtn.addActionListener(this);

		// Param description buttons
		theNotesBtn.setActionCommand("Notes");
		theNotesBtn.addActionListener(this);

		// Step 1 browse button
		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			theConfigBrowseBtnArr[index]
					.setActionCommand(theConfigLblValueArr[index]);
			theConfigBrowseBtnArr[index].addActionListener(this);
		}

	}

	// Initialize labels and text areas
	public void initialize() {
		// Create some temporary values as example
		String[] initialENV_ValueArr = { "ENV_WORKSPACE_DIR",
				"ENV_FCSTDIFF_PKG_DIR", "ENV_CDATE", "ENV_EDATE", "ENV_CYC",
				"ENV_EXPLIST", "ENV_MODEL_1", "ENV_MODEL_2", "ENV_INPUT_1",
				"ENV_INPUT_2", "ENV_MAPAIR", "ENV_MAPSFC", "ENV_MAPUV" };

		String[] initialLblValueArr = { "Workspace Dir", "FcstDiff pkg dir",
				"CDATE (yyyymmdd)", "edate (ddMMMyyy)", "cycle",
				"expr names (2)", "model type of exp 1 (gfs/gdas/ecm)",
				"model type of exp 2 (gfs/gdas/ecm)", "input dir of exp 1",
				"input dir of exp 2", "map air option (yes/no)",
				"map sfc option (yes/no)", "map uv option (yes/no)" };

		String[] initialLblNoteArr = {
				"Data directories for input, output and temp data",
				"FcstDiff pkg location", "Date in format of yyyymmdd",
				"Date in format of ddMMMyyy", "Cycle information",
				"Two expr names ",
				"Model type of exp 1 : choice of gfs/gdas/ecm",
				"Model type of exp 1 : choice of gfs/gdas/ecm",
				"Input directory of exp 1", "Input directory of exp 2",
				"OPtion to make map air data", "Option to make map sfc data",
				"Option to make map uv data" };

		String[] initialTxtValueArr = {
				"/data/users/dxu/workspace/fcstDiff_workspace",
				"/data/users/dxu/iat/fcstDiff_pkg", "20140203", "03Feb2014",
				"00", "cntrl ecmwf", "gfs", "ecm",
				"${WORKSPACE}/data/input/cntrl_fcst",
				"${WORKSPACE}/data/input/ecmwf_fcst", "yes", "no", "no" };

		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			// These values will be updated once "save" button is clicked.
			theConfigLblValueArr[index] = initialLblValueArr[index];
			theConfigTxtValueArr[index] = initialTxtValueArr[index];
			theConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theConfigEnvArr[index] = initialENV_ValueArr[index];
			theConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theConfigTxtArr[index] = new JTextArea(initialTxtValueArr[index]);
			theConfigBrowseBtnArr[index] = new JButton("Browse");

			theNotes = theNotes + initialLblValueArr[index] + " :   "
					+ initialLblNoteArr[index] + "\n";
		}

		String airName = "";
		String sfcName = "";
		String uvName = "";
		for (int i = 0; i < 2; i++) {
			switch (i) {
			case 0:
				airName = "yes";
				sfcName = "yes";
				uvName = "yes";
				break;
			case 1:
				airName = "no";
				sfcName = "no";
				uvName = "no";
				break;
			}

			// Create radio button
			theAirRadioBtnArr[i] = new JRadioButton(airName);
			theAirRadioBtnArr[i].setMnemonic(KeyEvent.VK_B);
			theAirRadioBtnArr[i].setName(airName);
			// Group radio buttons together
			theAirGroup.add(theAirRadioBtnArr[i]);

			// Create radio button
			theSfcRadioBtnArr[i] = new JRadioButton(sfcName);
			theSfcRadioBtnArr[i].setMnemonic(KeyEvent.VK_B);
			theSfcRadioBtnArr[i].setName(sfcName);
			// Group radio buttons together
			theSfcGroup.add(theSfcRadioBtnArr[i]);

			// Create radio button
			theUvRadioBtnArr[i] = new JRadioButton(uvName);
			theUvRadioBtnArr[i].setMnemonic(KeyEvent.VK_B);
			theUvRadioBtnArr[i].setName(uvName);
			// Group radio buttons together
			theUvGroup.add(theUvRadioBtnArr[i]);

		}

		// First radio button is selected by default
		theAirRadioBtnArr[0].setSelected(true);
		theSfcRadioBtnArr[1].setSelected(true);
		theUvRadioBtnArr[1].setSelected(true);

	}

	public void showConfigPanel() {
		theConfigPanel.removeAll();

		// Create a SpringLayout for theConfigPanel
		SpringLayout configPanelLayout = new SpringLayout();
		theConfigPanel.setLayout(configPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control position
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[ENV_VAR_SIZE];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[ENV_VAR_SIZE];
		SpringLayout.Constraints[] contraint_3_Arr = new SpringLayout.Constraints[ENV_VAR_SIZE];

		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			// Initialize each JLabel and JTextArea
			theConfigLblArr[index] = new JLabel(theConfigLblValueArr[index]);
			theConfigTxtArr[index] = new JTextArea(theConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theConfigPanel.add(theConfigLblArr[index]);
			if (index < 10)
				theConfigPanel.add(theConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			switch (index) {
			case 0:
			case 1:
			case 8:
			case 9:
				theConfigPanel.add(theConfigBrowseBtnArr[index]);
				break;
			case 10:
				SpringLayout.Constraints[] contraintArr = new SpringLayout.Constraints[2];

				for (int index1 = 0; index1 < 2; index1++) {
					// Add Region radio button
					theConfigPanel.add(theAirRadioBtnArr[index1]);

					// Position Region radio buttons
					contraintArr[index1] = configPanelLayout
							.getConstraints(theAirRadioBtnArr[index1]);
					int tmpX_Pos = xPos + LBL_WIDTH + SPACER + index1
							* (BUTTON_WIDTH + SPACER);
					contraintArr[index1].setX(Spring.constant(tmpX_Pos));
					contraintArr[index1].setY(Spring.constant(yPos));
					contraintArr[index1]
							.setWidth(Spring.constant(BUTTON_WIDTH));
					contraintArr[index1].setHeight(Spring
							.constant(BUTTON_HEIGHT));
				}
				break;
			case 11:
				SpringLayout.Constraints[] contraintArr1 = new SpringLayout.Constraints[2];

				for (int index1 = 0; index1 < 2; index1++) {
					// Add Region radio button
					theConfigPanel.add(theSfcRadioBtnArr[index1]);

					// Position Region radio buttons
					contraintArr1[index1] = configPanelLayout
							.getConstraints(theSfcRadioBtnArr[index1]);
					int tmpX_Pos = xPos + LBL_WIDTH + SPACER + index1
							* (BUTTON_WIDTH + SPACER);
					contraintArr1[index1].setX(Spring.constant(tmpX_Pos));
					contraintArr1[index1].setY(Spring.constant(yPos));
					contraintArr1[index1].setWidth(Spring
							.constant(BUTTON_WIDTH));
					contraintArr1[index1].setHeight(Spring
							.constant(BUTTON_HEIGHT));
				}
				break;
			case 12:
				SpringLayout.Constraints[] contraintArr2 = new SpringLayout.Constraints[2];

				for (int index1 = 0; index1 < 2; index1++) {
					// Add Region radio button
					theConfigPanel.add(theUvRadioBtnArr[index1]);

					// Position Region radio buttons
					contraintArr2[index1] = configPanelLayout
							.getConstraints(theUvRadioBtnArr[index1]);
					int tmpX_Pos = xPos + LBL_WIDTH + SPACER + index1
							* (BUTTON_WIDTH + SPACER);
					contraintArr2[index1].setX(Spring.constant(tmpX_Pos));
					contraintArr2[index1].setY(Spring.constant(yPos));
					contraintArr2[index1].setWidth(Spring
							.constant(BUTTON_WIDTH));
					contraintArr2[index1].setHeight(Spring
							.constant(BUTTON_HEIGHT));
				}
				break;

			default:
				break;
			}

			// Position label
			contraint_1_Arr[index] = configPanelLayout
					.getConstraints(theConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextArea
			contraint_2_Arr[index] = configPanelLayout
					.getConstraints(theConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			contraint_3_Arr[index] = configPanelLayout
					.getConstraints(theConfigBrowseBtnArr[index]);
			contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			contraint_3_Arr[index].setY(Spring.constant(yPos));
			contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			contraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		yPos += LBL_HEIGHT;
		yPos += 4 * SPACER;

		// Add "save" button
		theConfigPanel.add(theConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints contraint_4 = configPanelLayout
				.getConstraints(theConfigSaveBtn);
		contraint_4.setX(Spring.constant(xPos + LBL_WIDTH));
		contraint_4.setY(Spring.constant(yPos + 3 * SPACER));
		contraint_4.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_4.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theConfigPanel.add(theConfigResetBtn);
		// Position "reset" button
		SpringLayout.Constraints contraint_5 = configPanelLayout
				.getConstraints(theConfigResetBtn);
		contraint_5.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		contraint_5.setY(Spring.constant(yPos + 3 * SPACER));
		contraint_5.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_5.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "Notes" button
		theConfigPanel.add(theNotesBtn);
		// Position "ParamDescr" button
		SpringLayout.Constraints contraint_6 = configPanelLayout
				.getConstraints(theNotesBtn);
		contraint_6.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER + BUTTON_WIDTH + SPACER));
		contraint_6.setY(Spring.constant(yPos + 3 * SPACER));
		contraint_6.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_6.setHeight(Spring.constant(2 * BUTTON_HEIGHT));
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// Get name of action component
		String actName = e.getActionCommand();

		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			if (actName.equals(theConfigLblValueArr[index])) {
				String[] strArr = new String[1];
				int result = getDir(strArr);
				if (result == 0) {
					theConfigTxtValueArr[index] = strArr[0];
					theConfigTxtArr[index].setText(strArr[0]);
					break;
				}
			}
		}

		switch (actName) {
		case "Save":
			try {
				saveChanges();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "Reset":
			resetConfig();
			break;
		case "Notes":
			displayNotes();
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

	// Save values in GUI to a file for step 1 config
	public void saveChanges() throws IOException {
		int n = saveChangesMsg("fcstDiff");
		if (n == 0) {
			String filename = DirSetter.getFcstDiffRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "fcstDiff_gui.config";
			else
				filename = filename + "/" + "fcstDiff_gui.config";

			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < ENV_VAR_SIZE; index++) {
				// Save values in GUI to value array
				theConfigTxtValueArr[index] = theConfigTxtArr[index].getText();

				// Save values in GUI into file
				String tmpString;
				if (index == 10) {
					for (int index2 = 0; index2 < 2; index2++) {
						if (theAirRadioBtnArr[index2].isSelected()) {
							tmpString = "export " + theConfigEnvArr[index]
									+ "=\""
									+ theAirRadioBtnArr[index2].getName()
									+ "\"";
							print_line.printf("%s%n", tmpString);
							break;
						}
					}
				} else if (index == 11) {
					for (int index2 = 0; index2 < 2; index2++) {
						if (theSfcRadioBtnArr[index2].isSelected()) {
							tmpString = "export " + theConfigEnvArr[index]
									+ "=\""
									+ theSfcRadioBtnArr[index2].getName()
									+ "\"";
							print_line.printf("%s%n", tmpString);
							break;
						}
					}
				} else if (index == 12) {
					for (int index2 = 0; index2 < 2; index2++) {
						if (theUvRadioBtnArr[index2].isSelected()) {
							tmpString = "export " + theConfigEnvArr[index]
									+ "=\""
									+ theUvRadioBtnArr[index2].getName() + "\"";
							print_line.printf("%s%n", tmpString);
							break;
						}
					}
				} else {
					tmpString = "export " + theConfigEnvArr[index] + "=\""
							+ theConfigTxtArr[index].getText() + "\"";
					print_line.printf("%s%n", tmpString);
				}

				// Copy ENV_WORKSPACE to WORKSPACE to keep the same naming
				// convention: ENV_VARNAME
				if (index == 1) {
					tmpString = "export WORKSPACE=${ENV_WORKSPACE_DIR}";
					print_line.printf("%s%n", tmpString);
				}
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	public int resetChanges(String aString) {
		int n = JOptionPane.showConfirmDialog(null, "Reset?", aString,
				JOptionPane.YES_NO_OPTION);
		return n;
	}

	// Reset values in GUI for step 1 config
	public void resetConfig() {
		int n = resetChanges("fcstDiff config");
		if (n == 0) {
			for (int index = 0; index < ENV_VAR_SIZE; index++) {
				// Save values in GUI to value array
				theConfigTxtArr[index].setText(theConfigTxtInitValueArr[index]);
			}
		}
	}

	// Display notes
	public void displayNotes() {
		JOptionPane.showMessageDialog(null, theNotes, "InfoBox:",
				JOptionPane.INFORMATION_MESSAGE);
	}

	// Get directory via "browse" button
	public int getDir(String[] strArr) {
		// Create the log first, because the action listeners
		// need to refer to it.
		JFileChooser fc = new JFileChooser();
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		// fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

		int returnVal = fc.showOpenDialog(FcstDiff.this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			strArr[0] = file.toString();
		}

		return returnVal;
	}

}
