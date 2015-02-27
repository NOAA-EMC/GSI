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

public class Ge extends JPanel implements SizeDefinition, ActionListener {
	// Number of ENV vars in each step
	private final int ENV_VAR_SIZE = 12;

	public JPanel theConfigPanel = new JPanel();

	// 3) Components for GE config panel
	private JLabel[] theConfigLblArr = new JLabel[ENV_VAR_SIZE];
	private JTextArea[] theConfigTxtArr = new JTextArea[ENV_VAR_SIZE];
	private String[] theConfigEnvArr = new String[ENV_VAR_SIZE];
	private String[] theConfigLblValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtInitValueArr = new String[ENV_VAR_SIZE];
	private JButton[] theConfigBrowseBtnArr = new JButton[ENV_VAR_SIZE];

	private JButton theConfigSaveBtn = new JButton("Save");
	private JButton theConfigResetBtn = new JButton("Default");

	// Constructor
	Ge() {
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

		String[] initialENV_ValueArr = { "ENV_GRIBEXTR_WORKSAPCE",
				"ENV_GRIBEXTR_DIR", "ENV_CYCLETIME", "ENV_GFS_GES_INPUT_DIR",
				"ENV_GFS_GES_INPUT_FILE", "ENV_TITLE1",
				"ENV_CRNT_ANL_INPUT_DIR", "ENV_CRNT_ANL_INPUT_FILE",
				"ENV_TITLE2", "ENV_REF_ANL_INPUT_DIR",
				"ENV_REF_ANL_INPUT_FILE", "ENV_TITLE3" };

		String[] initialLblValueArr = { "gribExtr_worksapce", "gribExtr_dir",
				"cycleTime", "gfs_ges_input_dir", "gfs_ges_input_file",
				"title1", "crnt_anl_input_dir", "crnt_anl_input_file",
				"title2", "ref_anl_input_dir", "ref_anl_input_file", "title3" };

		String[] initialTxtValueArr = {
				"/data/users/dxu/workspace/ge_workspace",
				"/data/users/dxu/iat/ge_pkg/ge", "2013073012",
				"${WORKSPACE}/data/input/gfs",
				"pgbf06.gfs.2013073006", "gfs_ges",
				"${WORKSPACE}/data/input/gfs",
				"pgbanl.gfs.2013073012", "gfs_anl",
				"${WORKSPACE}/data/input/ecm",
				"pgbanl.ecm.2013073012", "ecmwf_anl" };

		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			// These values will be updated once "save" button is clicked.
			theConfigLblValueArr[index] = initialLblValueArr[index];
			theConfigTxtValueArr[index] = initialTxtValueArr[index];
			theConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theConfigEnvArr[index] = initialENV_ValueArr[index];
			theConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theConfigTxtArr[index] = new JTextArea(initialTxtValueArr[index]);
			theConfigBrowseBtnArr[index] = new JButton("Browse");
		}

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
			theConfigPanel.add(theConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			switch (index) {
			case 0:
			case 1:
			case 3:
			case 4:
			case 6:
			case 7:
			case 9:
			case 10:
				theConfigPanel.add(theConfigBrowseBtnArr[index]);
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
		// Position "save" button
		SpringLayout.Constraints contraint_5 = configPanelLayout
				.getConstraints(theConfigResetBtn);
		contraint_5.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		contraint_5.setY(Spring.constant(yPos + 3 * SPACER));
		contraint_5.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_5.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// Get name of action component
		String actName = e.getActionCommand();

		System.out.println("actName is (ge) " + actName);

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
		int n = saveChangesMsg("ge");
		if (n == 0) {
			String filename = DirSetter.getGeRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\scripts\\" + "ge_gui.config";
			else
				filename = filename + "/scripts/" + "ge_gui.config";

			System.out.println(filename);
			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < ENV_VAR_SIZE; index++) {
				// Save values in GUI to value array
				theConfigTxtValueArr[index] = theConfigTxtArr[index].getText();

				// Save values in GUI into file
				String tmpString = "export " + theConfigEnvArr[index] + "=\""
						+ theConfigTxtArr[index].getText() + "\"";
				print_line.printf("%s%n", tmpString);

				if (index == 0) {
					// Save values in GUI into file
					tmpString = "export WORKSPACE" + "=\""
							+ theConfigTxtArr[index].getText() + "\"";
					print_line.printf("%s%n", tmpString);

				}
				System.out.println(tmpString);
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
		int n = resetChanges("ge config");
		if (n == 0) {
			for (int index = 0; index < ENV_VAR_SIZE; index++) {
				// Save values in GUI to value array
				theConfigTxtArr[index].setText(theConfigTxtInitValueArr[index]);
			}
		}
	}

	// Get directory via "browse" button
	public int getDir(String[] strArr) {
		// Create the log first, because the action listeners
		// need to refer to it.
		JFileChooser fc = new JFileChooser();
		// fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

		int returnVal = fc.showOpenDialog(Ge.this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			System.out.println("dir selected is: " + file.toString());
			strArr[0] = file.toString();
			System.out.println("dddd is " + strArr[0]);
			System.out.println("val " + returnVal);
		} else {
			System.out.println("val " + returnVal);
		}

		return returnVal;
	}

}
