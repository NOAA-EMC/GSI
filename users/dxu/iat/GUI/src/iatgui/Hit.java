/*
 * Purpose: This is the GUI to configure Hurricane Intensity and Track.     
 *       
 * Author: Deyong Xu / RTI @ JCSDA
 * Last update: 1/27/2015, Initial coding 
 * 
 */

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

public class Hit extends JPanel implements SizeDefinition, ActionListener {
	// Number of ENV vars in each step
	private final int ENV_VAR_SIZE = 10;

	public JPanel theConfigPanel = new JPanel();

	// 3) Components for Hit config panel
	private JLabel[] theConfigLblArr = new JLabel[ENV_VAR_SIZE];
	private JTextArea[] theConfigTxtArr = new JTextArea[ENV_VAR_SIZE];
	private String[] theConfigEnvArr = new String[ENV_VAR_SIZE];
	private String[] theConfigLblValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtInitValueArr = new String[ENV_VAR_SIZE];
	private JButton[] theConfigBrowseBtnArr = new JButton[ENV_VAR_SIZE];

	private JButton theConfigSaveBtn = new JButton("Save");
	private JButton theConfigResetBtn = new JButton("Default");

	private JRadioButton[] theRegionRadioBtnArr = new JRadioButton[2];
	// Group the radio buttons.
	ButtonGroup theRegionGroup = new ButtonGroup();

	private JRadioButton[] theYearRadioBtnArr = new JRadioButton[4];
	// Group the radio buttons.
	ButtonGroup theYearGroup = new ButtonGroup();

	private JRadioButton[] theMeanRadioBtnArr = new JRadioButton[2];
	// Group the radio buttons.
	ButtonGroup theMeanGroup = new ButtonGroup();

	// Constructor
	Hit() {
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

		String[] initialENV_ValueArr = { "ENV_SCRDIR", "ENV_EXPDIR",
				"ENV_MDLIST", "ENV_MDPLOT", "ENV_CYC", "ENV_STMP",
				"ENV_REGION", "ENV_YEAR", "ENV_STORMS", "ENV_MEAN" };

		String[] initialLblValueArr = { "scrdir", "expdir", "mdlist", "mdplot",
				"cyc", "STMP", "Region", "Year", "Storms", "Generate Mean?" };

		String[] initialTxtValueArr = { "/data/users/dxu/iat/hit_pkg/hit",
				"/data/users/dxu/workspace/hit_workspace/data/input",
				"prhw2014", "hw14", "00 06 12 18",
				"/data/users/dxu/workspace/hit_workspace/data/output", "", "",
				"Fausto", "" };

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

		String regionName = "";
		String regionCmdName = "";
		for (int i = 0; i < 2; i++) {
			switch (i) {
			case 0:
				regionName = "ep";
				regionCmdName = "East Pacific";
				break;
			case 1:
				regionName = "at";
				regionCmdName = "Atlantic";
				break;
			}

			// Create radio button
			theRegionRadioBtnArr[i] = new JRadioButton(regionName);
			theRegionRadioBtnArr[i].setMnemonic(KeyEvent.VK_B);
			theRegionRadioBtnArr[i].setName(regionName);
			theRegionRadioBtnArr[i].setActionCommand(regionCmdName);
			// Group radio buttons together
			theRegionGroup.add(theRegionRadioBtnArr[i]);
		}

		// First radio button is selected by default
		theRegionRadioBtnArr[0].setSelected(true);

		String yearName = "";
		for (int i1 = 0; i1 < 4; i1++) {
			switch (i1) {
			case 0:
				yearName = "2011";
				break;
			case 1:
				yearName = "2012";
				break;
			case 2:
				yearName = "2013";
				break;
			case 3:
				yearName = "2014";
				break;
			}

			// Create radio button
			theYearRadioBtnArr[i1] = new JRadioButton(yearName);
			theYearRadioBtnArr[i1].setMnemonic(KeyEvent.VK_B);
			theYearRadioBtnArr[i1].setName(yearName);
			theYearRadioBtnArr[i1].setActionCommand(yearName);
			// Group radio buttons together
			theYearGroup.add(theYearRadioBtnArr[i1]);
		}

		// First radio button is selected by default
		theYearRadioBtnArr[3].setSelected(true);

		String meanName = "";
		for (int i = 0; i < 2; i++) {
			switch (i) {
			case 0:
				meanName = "yes";
				break;
			case 1:
				meanName = "no";
				break;
			}

			// Create radio button
			theMeanRadioBtnArr[i] = new JRadioButton(meanName);
			theMeanRadioBtnArr[i].setMnemonic(KeyEvent.VK_B);
			theMeanRadioBtnArr[i].setName(meanName);
			theMeanRadioBtnArr[i].setActionCommand(meanName);
			// Group radio buttons together
			theMeanGroup.add(theMeanRadioBtnArr[i]);
		}

		// First radio button is selected by default
		theMeanRadioBtnArr[1].setSelected(true);

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

			// Position label
			contraint_1_Arr[index] = configPanelLayout
					.getConstraints(theConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextArea
			if (index <= 5 || index == 8) {
				contraint_2_Arr[index] = configPanelLayout
						.getConstraints(theConfigTxtArr[index]);
				contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
						+ SPACER));
				contraint_2_Arr[index].setY(Spring.constant(yPos));
				contraint_2_Arr[index]
						.setWidth(Spring.constant(TEXTAREA_WIDTH));
				contraint_2_Arr[index].setHeight(Spring
						.constant(TEXTAREA_HEIGHT));
			}

			switch (index) {
			case 0:
			case 1:
			case 5:
				// Add "Browse" buttons for items that are directories.
				theConfigPanel.add(theConfigBrowseBtnArr[index]);

				// Position Browse button
				contraint_3_Arr[index] = configPanelLayout
						.getConstraints(theConfigBrowseBtnArr[index]);
				contraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
						+ SPACER + TEXTAREA_WIDTH + SPACER));
				contraint_3_Arr[index].setY(Spring.constant(yPos));
				contraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
				contraint_3_Arr[index]
						.setHeight(Spring.constant(BUTTON_HEIGHT));
				break;
			case 6: // Add radio button for region
				SpringLayout.Constraints[] contraintArr = new SpringLayout.Constraints[2];

				for (int index1 = 0; index1 < 2; index1++) {
					// Add Region radio button
					theConfigPanel.add(theRegionRadioBtnArr[index1]);

					// Position Region radio buttons
					contraintArr[index1] = configPanelLayout
							.getConstraints(theRegionRadioBtnArr[index1]);
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
			case 7: // Add radio button for year
				SpringLayout.Constraints[] contraintArr1 = new SpringLayout.Constraints[4];

				for (int index1 = 0; index1 < 4; index1++) {
					// Add Region radio button
					theConfigPanel.add(theYearRadioBtnArr[index1]);

					// Position Region radio buttons
					contraintArr1[index1] = configPanelLayout
							.getConstraints(theYearRadioBtnArr[index1]);
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
			case 9: // Add radio button for generating mean track
				SpringLayout.Constraints[] contraintArr2 = new SpringLayout.Constraints[2];

				for (int index1 = 0; index1 < 2; index1++) {
					// Add Region radio button
					theConfigPanel.add(theMeanRadioBtnArr[index1]);

					// Position Region radio buttons
					contraintArr2[index1] = configPanelLayout
							.getConstraints(theMeanRadioBtnArr[index1]);
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
			String filename = DirSetter.getHitRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\" + "hit_gui.config";
			else
				filename = filename + "/" + "hit_gui.config";

			System.out.println(filename);
			FileWriter configFile = new FileWriter(filename, false);
			PrintWriter print_line = new PrintWriter(configFile);

			print_line.printf("%s%n", "#!/bin/bash");
			print_line.printf("%s%n", "set -ax");
			for (int index = 0; index < ENV_VAR_SIZE; index++) {
				// Save values in GUI to value array
				theConfigTxtValueArr[index] = theConfigTxtArr[index].getText();

				String tmpString;
				if (index == 6) {
					for (int index2 = 0; index2 < 2; index2++) {
						if (theRegionRadioBtnArr[index2].isSelected()) {
							tmpString = "export " + theConfigEnvArr[index]
									+ "=\""
									+ theRegionRadioBtnArr[index2].getName()
									+ "\"";
							print_line.printf("%s%n", tmpString);
							System.out.println(tmpString);
							break;
						}
					}
				} else if (index == 7) {
					for (int index2 = 0; index2 < 4; index2++) {
						if (theYearRadioBtnArr[index2].isSelected()) {
							tmpString = "export " + theConfigEnvArr[index]
									+ "=\""
									+ theYearRadioBtnArr[index2].getName()
									+ "\"";
							print_line.printf("%s%n", tmpString);
							System.out.println(tmpString);
							break;
						}
					}
				} else if (index == 9) {
					for (int index2 = 0; index2 < 2; index2++) {
						if (theMeanRadioBtnArr[index2].isSelected()) {
							tmpString = "export " + theConfigEnvArr[index]
									+ "=\""
									+ theMeanRadioBtnArr[index2].getName()
									+ "\"";
							print_line.printf("%s%n", tmpString);
							System.out.println(tmpString);
							break;
						}
					}
				} else {
					// Save values in GUI into file
					tmpString = "export " + theConfigEnvArr[index] + "=\""
							+ theConfigTxtArr[index].getText() + "\"";
					print_line.printf("%s%n", tmpString);
					System.out.println(tmpString);
				}
			}

			// Close PrintWriter
			print_line.close();
		}
	}

	public int resetChanges(String aString) {
		int n = JOptionPane.showConfirmDialog(null, "Reset?", aString,
				JOptionPane.YES_NO_OPTION);

		theRegionRadioBtnArr[0].setSelected(true);
		theYearRadioBtnArr[0].setSelected(true);

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
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		// fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

		int returnVal = fc.showOpenDialog(Hit.this);

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
