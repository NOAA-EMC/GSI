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
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JTextArea;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class Radmon extends JPanel implements SizeDefinition, ActionListener {
	// Redefine constants below for local use.
	private final int TEXTAREA_WIDTH = 400;
	private final int TEXTAREA_HEIGHT = 15;

	// Number of ENV vars in each step
	private final int ENV_VAR_SIZE = 9;

	// 2 panel
	public JPanel theConfigPanel = new JPanel();

	// Step 1 config panel
	private JLabel[] theConfigLblArr = new JLabel[ENV_VAR_SIZE];
	private JTextArea[] theConfigTxtArr = new JTextArea[ENV_VAR_SIZE];
	private String[] theConfigEnvArr = new String[ENV_VAR_SIZE];
	private String[] theConfigLblValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtValueArr = new String[ENV_VAR_SIZE];
	private String[] theConfigTxtInitValueArr = new String[ENV_VAR_SIZE];
	private JButton[] theConfigBrowseBtnArr = new JButton[ENV_VAR_SIZE];

	private JRadioButton theStep1SelectionBtn = new JRadioButton("data extract");
	private JRadioButton theStep2SelectionBtn = new JRadioButton(
			"img generation");
	// Group the radio buttons.
	ButtonGroup theBtnGroup = new ButtonGroup();

	private JButton theConfigSaveBtn = new JButton("Save");
	private JButton theConfigResetBtn = new JButton("Default");

	Radmon() {
		initializeLblTxt();
		addListeners();
	}

	public void addListeners() {
		// Save buttons
		theConfigSaveBtn.setActionCommand("step1Save");
		theConfigSaveBtn.addActionListener(this);

		// Reset buttons
		theConfigResetBtn.setActionCommand("step1Reset");
		theConfigResetBtn.addActionListener(this);

		// Step 1 browse button
		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			theConfigBrowseBtnArr[index]
					.setActionCommand(theConfigLblValueArr[index]);
			theConfigBrowseBtnArr[index].addActionListener(this);
		}

		theStep1SelectionBtn.addActionListener(this);
		theStep2SelectionBtn.addActionListener(this);

	}

	// Initialize labels and text areas
	public void initializeLblTxt() {
		initialize();

	}

	// Initial step 1 config panel's label and textareas
	public void initialize() {
		String[] initialEnvValueArr = { "ENV_MY_RADMON", "ENV_WORKSPACE",
				"ENV_MY_TANKDIR", "ENV_PTMP", "ENV_STMP",
				"ENV_RADSTAT_LOCATION", "ENV_LITTLE_ENDIAN", "ENV_ID",
				"ENV_CYCLES" };
		String[] initialLblValueArr = { "RADMON_HOME", "RADMON_WORKSPACE",
				"TANKDIR", "PTMP", "STMP", "RADSTAT_LOCATION", "LITTLE_ENDIAN",
				"ID", "CYCLES (seperated by space)" };
		String[] initialTxtValueArr = {
				"/data/users/dxu/iat/radmon_pkg/radmon/util/Radiance_Monitor",
				"/data/users/dxu/workspace/radmon_workspace",
				"${WORKSPACE}/data/output/radmon_tank", "${WORKSPACE}/log",
				"${WORKSPACE}/run", "${WORKSPACE}/data/input/input_for_test", "1", "mytest",
				"2013063000 2013063006" };

		for (int index = 0; index < ENV_VAR_SIZE; index++) {
			// These values will be updated once "save" button is clicked.
			theConfigLblValueArr[index] = initialLblValueArr[index];
			theConfigTxtValueArr[index] = initialTxtValueArr[index];
			theConfigTxtInitValueArr[index] = initialTxtValueArr[index];

			theConfigEnvArr[index] = initialEnvValueArr[index];
			theConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			theConfigTxtArr[index] = new JTextArea(initialTxtValueArr[index]);
			theConfigBrowseBtnArr[index] = new JButton("Browse");
		}

		theStep1SelectionBtn.setMnemonic(KeyEvent.VK_B);
		theStep1SelectionBtn.setActionCommand("data extract");
		theStep1SelectionBtn.setSelected(true);

		theStep2SelectionBtn.setMnemonic(KeyEvent.VK_C);
		theStep2SelectionBtn.setActionCommand("img generation");

		theBtnGroup.add(theStep1SelectionBtn);
		theBtnGroup.add(theStep2SelectionBtn);

	}

	// Display step 1 config panel
	public void showRadmonConfigPanel() {
		theConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout configPanelLayout = new SpringLayout();
		theConfigPanel.setLayout(configPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints[] contraint_1_Arr = new SpringLayout.Constraints[ENV_VAR_SIZE];
		SpringLayout.Constraints[] contraint_2_Arr = new SpringLayout.Constraints[ENV_VAR_SIZE];
		SpringLayout.Constraints[] constraint_3_Arr = new SpringLayout.Constraints[ENV_VAR_SIZE];

		// Add Labels and TextAreas
		for (int index = 0; index < ENV_VAR_SIZE; index++) {

			// Initialize each JLabel and JTextArea
			theConfigLblArr[index] = new JLabel(theConfigLblValueArr[index]);
			theConfigTxtArr[index] = new JTextArea(theConfigTxtValueArr[index]);

			// Add Labels and TextAreas
			theConfigPanel.add(theConfigLblArr[index]);
			theConfigPanel.add(theConfigTxtArr[index]);

			// Add "Browse" buttons for items that are directories.
			if (index >= 0 && index <= 5) {
				theConfigPanel.add(theConfigBrowseBtnArr[index]);
			}

			// Position labels
			contraint_1_Arr[index] = configPanelLayout
					.getConstraints(theConfigLblArr[index]);
			contraint_1_Arr[index].setX(Spring.constant(xPos));
			contraint_1_Arr[index].setY(Spring.constant(yPos));
			contraint_1_Arr[index].setWidth(Spring.constant(LBL_WIDTH));
			contraint_1_Arr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextAreas
			contraint_2_Arr[index] = configPanelLayout
					.getConstraints(theConfigTxtArr[index]);
			contraint_2_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER));
			contraint_2_Arr[index].setY(Spring.constant(yPos));
			contraint_2_Arr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			contraint_2_Arr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			constraint_3_Arr[index] = configPanelLayout
					.getConstraints(theConfigBrowseBtnArr[index]);
			constraint_3_Arr[index].setX(Spring.constant(xPos + LBL_WIDTH
					+ SPACER + TEXTAREA_WIDTH + SPACER));
			constraint_3_Arr[index].setY(Spring.constant(yPos));
			constraint_3_Arr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			constraint_3_Arr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		// Add radio button
		theConfigPanel.add(theStep1SelectionBtn);
		SpringLayout.Constraints contraint_1 = configPanelLayout
				.getConstraints(theStep1SelectionBtn);
		contraint_1.setX(Spring.constant(xPos + LBL_WIDTH));
		contraint_1.setY(Spring.constant(yPos + SPACER));
		contraint_1.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_1.setHeight(Spring.constant(BUTTON_HEIGHT));

		theConfigPanel.add(theStep2SelectionBtn);
		SpringLayout.Constraints contraint_2 = configPanelLayout
				.getConstraints(theStep2SelectionBtn);
		contraint_2.setX(Spring.constant(xPos + 2 * LBL_WIDTH + SPACER));
		contraint_2.setY(Spring.constant(yPos + SPACER));
		contraint_2.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_2.setHeight(Spring.constant(BUTTON_HEIGHT));

		yPos += LBL_HEIGHT;
		yPos += 4 * SPACER;

		// Add "save" button
		theConfigPanel.add(theConfigSaveBtn);

		// Position "save" button
		SpringLayout.Constraints contraint_3 = configPanelLayout
				.getConstraints(theConfigSaveBtn);
		contraint_3.setX(Spring.constant(xPos + LBL_WIDTH));
		contraint_3.setY(Spring.constant(yPos + 3 * SPACER));
		contraint_3.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_3.setHeight(Spring.constant(2 * BUTTON_HEIGHT));

		// Add "reset" button
		theConfigPanel.add(theConfigResetBtn);
		// Position "save" button
		SpringLayout.Constraints contraint_4 = configPanelLayout
				.getConstraints(theConfigResetBtn);
		contraint_4.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ BUTTON_WIDTH + SPACER));
		contraint_4.setY(Spring.constant(yPos + 3 * SPACER));
		contraint_4.setWidth(Spring.constant(BUTTON_WIDTH));
		contraint_4.setHeight(Spring.constant(2 * BUTTON_HEIGHT));
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// Get name of action component
		String actName = e.getActionCommand();

		System.out.println("actName is (radmon) " + actName);

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
		case "step1Save":
			try {
				saveChanges();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			break;
		case "step1Reset":
			resetStep1();
			break;
		}

		if (actName.equalsIgnoreCase("img generation")) {
			theConfigTxtArr[ENV_VAR_SIZE - 1].disable();
		}

		if (actName.equalsIgnoreCase("data extract")) {
			theConfigTxtArr[ENV_VAR_SIZE - 1].enable();
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
		int n = saveChangesMsg("step1");
		if (n == 0) {
			String filename = DirSetter.getRadmonRoot();

			if (DirSetter.isWindows())
				filename = filename + "\\parm\\" + "radmon_gui.config";
			else
				filename = filename + "/parm/" + "radmon_gui.config";

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
				System.out.println(tmpString);

				// Copy ENV_WORKSPAC	E to WORKSPACE to keep the same naming convention: ENV_VARNAME
				if (index == 1){
					tmpString = "export WORKSPACE=${ENV_WORKSPACE}";
					print_line.printf("%s%n", tmpString);
					System.out.println(tmpString);
				}							
			}
			
			if (theStep1SelectionBtn.isSelected()) {
				// Save values in GUI into file
				String tmpString = "export ENV_RUN_STEP=1";
				print_line.printf("%s%n", tmpString);
				System.out.println(tmpString);
			} else if (theStep2SelectionBtn.isSelected()) {
				String tmpString = "export ENV_RUN_STEP=2";
				print_line.printf("%s%n", tmpString);
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
	public void resetStep1() {
		int n = resetChanges("step1 config");
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

		int returnVal = fc.showOpenDialog(Radmon.this);

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
