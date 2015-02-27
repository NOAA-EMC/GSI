/*
 * Purpose: This is the main GUI to run IAT. Specific GUI for individual 
 *          IAT package is handled in separate classes.   
 *       
 * Author: Deyong Xu / RTI @ JCSDA
 * Last update: 1/27/2015, Initial coding
 *  
 */

package iatgui;

import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;

import java.beans.*;

public class IAT extends JPanel implements SizeDefinition, ActionListener,
		ItemListener, PropertyChangeListener {

	// 1. Main panels
	private JPanel theTitlePanel = new JPanel();
	private JPanel theRunPanel = new JPanel();
	private JPanel theConfigPanel = new JPanel();

	SpringLayout theConfigPanelLayout = new SpringLayout();

	// 2. Components of theRunPanel
	private JPanel theIatCboxPanel = new JPanel(new GridLayout(5, 1));
	private JButton theRunBtn = new JButton("Run");
	private JButton theStatBtn = new JButton("Check job status");
	private JButton theParBtn = new JButton("Generate PAR");
	private JButton theExitBtn = new JButton("Exit");

	// 3. Components of theConfigPanel

	// 4. IAT choice and its components
	private Choice theIAT_Choice = new Choice();
	private Choice theVsdb_Choice = new Choice();
	private Choice theRadmon_Choice = new Choice();

	private JCheckBox theFcstDiffCbox = new JCheckBox("Fcst Diff (FcstDiff)",
			false);
	private JCheckBox theGeCbox = new JCheckBox("Grib Extremes (Ge)", false);
	private JCheckBox theHitCbox = new JCheckBox(
			"Hurricane Intensity and Track (Hit)", false);
	private JCheckBox theRadmonCbox = new JCheckBox("Radmon", false);
	private JCheckBox theVsdbCbox = new JCheckBox("Vsdb", false);

	// 5. Five package classes
	private JobStat theJobStat = new JobStat();
	private EmptyConfig theEmptyConfig = new EmptyConfig();

	private FcstDiff theFcstDiff = new FcstDiff();
	private Ge theGe = new Ge();
	private Hit theHit = new Hit();
	private Radmon theRadmon = new Radmon();
	private Vsdb theVsdb = new Vsdb();

	// Constructor
	public IAT() {
		// Set up initial panel
		setInitialPanel();

		// Add listener
		theIAT_Choice.addItemListener(this);
		theVsdb_Choice.addItemListener(this);
		theRadmon_Choice.addItemListener(this);

		theRunBtn.addActionListener(this);
		theStatBtn.addActionListener(this);
		theParBtn.addActionListener(this);
		theExitBtn.addActionListener(this);

	}

	/**
	 * Invoked when button clicked
	 */
	public void actionPerformed(ActionEvent evt) {

		// Job stat button is clicked
		if (evt.getSource() == theStatBtn) {
			executeStatBtn();
		}

		// Run button is clicked
		if (evt.getSource() == theRunBtn) {
			executeRunBtn();
		}

		// PAR button is clicked
		if (evt.getSource() == theParBtn) {
			executeParBtn();
		}

		// Exit button is clicked
		if (evt.getSource() == theExitBtn) {
			int n = JOptionPane.showConfirmDialog(null, "Exit IAT?", "",
					JOptionPane.YES_NO_OPTION);
			if (n == 0)
				System.exit(0);
		}

	}

	// Check job status
	private void executeStatBtn() {
		// Run "showJobStat.sh"
		if (DirSetter.isLinux()) {
			String aStr = null;
			try {
				// run "showJobStat.sh" under the current directory, which can
				// not be changed.
				Process prcs = Runtime.getRuntime().exec(
						DirSetter.getGUI_Root() + "/showJobStat.sh");

				JTextArea aTxt = new JTextArea("");
				// stdout
				BufferedReader stdout = new BufferedReader(
						new InputStreamReader(prcs.getInputStream()));
				// stderr
				BufferedReader stderr = new BufferedReader(
						new InputStreamReader(prcs.getErrorStream()));

				// read the output from the command
				while ((aStr = stdout.readLine()) != null) {
					aTxt.append(aStr + "\n");
				}

				// read any errors from the attempted command
				while ((aStr = stderr.readLine()) != null) {
					aTxt.append(aStr + "\n");
				}

				theJobStat.setJobStat(aTxt.getText());

				showJobStatPanel();

			} catch (IOException e) {
				JOptionPane.showMessageDialog(null, "exception thrown");
				e.printStackTrace();
				System.exit(-1);
			}
		}

		showJobStatPanel();
	}

	// Generate PAR
	private void executeParBtn() {
		List<String> pkgList = new ArrayList<String>();

		if (theFcstDiffCbox.isSelected())
			pkgList.add("   - FcstDiff\n");

		if (theGeCbox.isSelected())
			pkgList.add("   - Ge\n");

		if (theHitCbox.isSelected())
			pkgList.add("   - Hit\n");

		if (theRadmonCbox.isSelected())
			pkgList.add("   - Radmon\n");

		if (theVsdbCbox.isSelected())
			pkgList.add("   - Vsdb\n");

		// Check how many packages are selected.
		if (pkgList.size() == 0)
			JOptionPane.showMessageDialog(null,
					"No package selected, please select package.");
		else {
			for (String str : pkgList) {
				System.out.println(str);
			}
		}

	}

	// Run IAT
	private void executeRunBtn() {
		List<String> pkgList = new ArrayList<String>();

		if (theFcstDiffCbox.isSelected())
			pkgList.add("   - FcstDiff\n");

		if (theGeCbox.isSelected())
			pkgList.add("   - Ge\n");

		if (theHitCbox.isSelected())
			pkgList.add("   - Hit\n");

		if (theRadmonCbox.isSelected())
			pkgList.add("   - Radmon\n");

		if (theVsdbCbox.isSelected())
			pkgList.add("   - Vsdb\n");

		if (pkgList.size() == 0) {
			JOptionPane.showMessageDialog(null,
					"No package selected, please select package.");
		} else {
			String pkgToRun = "";
			for (String str : pkgList) {
				pkgToRun = pkgToRun + str;
			}

			boolean confirmed = confirm("run following IAT packages: \n"
					+ pkgToRun);

			// Run fcstDiff
			if (confirmed == true) {
				// Run FcstDiff
				if (theFcstDiffCbox.isSelected()) {
					if (DirSetter.isLinux()) {
						try {
							// Run fcstDiff main script in VSDB HOME directory.
							Process prcs = Runtime.getRuntime().exec(
									"./runFcstDiffByGUI.sh",
									null,
									new File(DirSetter.getFcstDiffRoot())
											.getAbsoluteFile());
						} catch (IOException e) {
							// TODO Auto-generated catch block
							System.out.println("error in running fcstDiff!!!");
							e.printStackTrace();
						}
					}
				}

				// Run GE
				if (theGeCbox.isSelected()) {
					if (DirSetter.isLinux()) {
						try {
							// Run fcstDiff main script in VSDB HOME directory.
							Process prcs = Runtime
									.getRuntime()
									.exec("./runGeByGUI.sh",
											null,
											new File(
													(DirSetter.getGeRoot() + "/scripts"))
													.getAbsoluteFile());
						} catch (IOException e) {
							// TODO Auto-generated catch block
							System.out
									.println("error in running gribExtremes !!!");
							e.printStackTrace();
						}
					}
				}

				// RUN HIT
				if (theHitCbox.isSelected()) {
					if (DirSetter.isLinux()) {
						try {
							// Run fcstDiff main script in VSDB HOME directory.
							Process prcs = Runtime.getRuntime().exec(
									"./runHitByGUI.sh",
									null,
									new File(DirSetter.getHitRoot())
											.getAbsoluteFile());
						} catch (IOException e) {
							// TODO Auto-generated catch block
							System.out
									.println("error in running Hurricane Intensity and Track !!!");
							e.printStackTrace();
						}
					}
				}

				if (theRadmonCbox.isSelected()) {
					if (DirSetter.isLinux()) {
						try {
							// Run vsdb main script in VSDB HOME directory.
							Process prcs = Runtime.getRuntime().exec(
									"./runRadmonByGUI.sh",
									null,
									new File(DirSetter.getRadmonRoot()
											+ "/parm").getAbsoluteFile());
						} catch (IOException e) {
							// TODO Auto-generated catch block
							System.out.println("error in running radmon!!!");
							e.printStackTrace();
						}
					}
				}

				if (theVsdbCbox.isSelected()) {
					if (DirSetter.isLinux()) {
						try {
							// Run vsdb main script in VSDB HOME directory.
							Process prcs = Runtime.getRuntime().exec(
									"./runVsdbByGUI.sh",
									null,
									new File(DirSetter.getVsdbRoot())
											.getAbsoluteFile());
						} catch (IOException e) {
							// TODO Auto-generated catch block
							System.out.println("error in running vsdb!!!");
							e.printStackTrace();
						}
					}
				}
			}
		}
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
			case "FcstDiff":
				addFcstDiffConfigPanel();
				break;
			case "Ge":
				addGE_ConfigPanel();
				break;
			case "Hit":
				addHIT_ConfigPanel();
				break;
			case "Radmon":
				addRADMON_ConfigPanel();
				break;
			case "Vsdb":
				addVSDB_ConfigPanel();
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
			case "step 1 config":
				addVsdbStep_1_ConfigPanel();
				break;
			case "step 2 config":
				addVsdbStep_2_ConfigPanel();
				break;
			case "step 3 config":
				addVsdbStep_3_ConfigPanel();
				break;
			case "step 4 config":
				addVsdbStep_4_ConfigPanel();
				break;
			case "step 5 config":
				addVsdbStep_5_ConfigPanel();
				break;
			case "step 6 config":
				addVsdbStep_6_ConfigPanel();
				break;
			default:
				addVsdbTopLevelConfigPanel();
				break;
			}
		}

	}

	private void showJobStatPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and JobStatPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theJobStat.theJobStatPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theJobStat within the config panel
		SpringLayout.Constraints jobStatlCons = theConfigPanelLayout
				.getConstraints(theJobStat.theJobStatPanel);
		jobStatlCons.setX(Spring.constant(10));
		jobStatlCons.setY(Spring.constant(35));
		jobStatlCons.setWidth(Spring.constant(PANEL_WIDTH));
		jobStatlCons.setHeight(Spring.constant(PANEL_HEIGHT));

		theJobStat.showConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	private void addEmptyConfigPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theFcstDiffConfigPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theEmptyConfig.theEmptyConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints emptyConfigPanelCons = theConfigPanelLayout
				.getConstraints(theEmptyConfig.theEmptyConfigPanel);
		emptyConfigPanelCons.setX(Spring.constant(10));
		emptyConfigPanelCons.setY(Spring.constant(35));
		emptyConfigPanelCons.setWidth(Spring.constant(PANEL_WIDTH));
		emptyConfigPanelCons.setHeight(Spring.constant(PANEL_HEIGHT));

		theEmptyConfig.showConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	private void addFcstDiffConfigPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theFcstDiffConfigPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theFcstDiff.theConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints fcstDiffConfigPanelCons = theConfigPanelLayout
				.getConstraints(theFcstDiff.theConfigPanel);
		fcstDiffConfigPanelCons.setX(Spring.constant(10));
		fcstDiffConfigPanelCons.setY(Spring.constant(35));
		fcstDiffConfigPanelCons.setWidth(Spring.constant(PANEL_WIDTH));
		fcstDiffConfigPanelCons.setHeight(Spring.constant(PANEL_HEIGHT));

		theFcstDiff.showConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	private void addGE_ConfigPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theFcstDiffConfigPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theGe.theConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints geConfigPanelCons = theConfigPanelLayout
				.getConstraints(theGe.theConfigPanel);
		geConfigPanelCons.setX(Spring.constant(10));
		geConfigPanelCons.setY(Spring.constant(35));
		geConfigPanelCons.setWidth(Spring.constant(PANEL_WIDTH));
		geConfigPanelCons.setHeight(Spring.constant(PANEL_HEIGHT));

		theGe.showConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	private void addHIT_ConfigPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theFcstDiffConfigPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theHit.theConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(150));
		iatChoiceCons.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints geConfigPanelCons = theConfigPanelLayout
				.getConstraints(theHit.theConfigPanel);
		geConfigPanelCons.setX(Spring.constant(10));
		geConfigPanelCons.setY(Spring.constant(35));
		geConfigPanelCons.setWidth(Spring.constant(PANEL_WIDTH));
		geConfigPanelCons.setHeight(Spring.constant(PANEL_HEIGHT));

		theHit.showConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();
	}

	private void addRADMON_ConfigPanel() {
		// ======================================
		// Step 1: Set up theConfigPanel
		// ======================================
		// Wipe out stuff within theConfigPanel
		theConfigPanel.removeAll();

		// Add theIAT_Choice and theFcstDiffConfigPanel into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theRadmon.theConfigPanel);

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		contraint_1.setX(Spring.constant(10));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theFcstDiffConfigPanel within the config panel
		SpringLayout.Constraints contraint_2 = theConfigPanelLayout
				.getConstraints(theRadmon.theConfigPanel);
		contraint_2.setX(Spring.constant(10));
		contraint_2.setY(Spring.constant(35));
		contraint_2.setWidth(Spring.constant(PANEL_WIDTH));
		contraint_2.setHeight(Spring.constant(PANEL_HEIGHT));

		theRadmon.showRadmonConfigPanel();

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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdb_Choice within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theTopLevelConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showTopLevelConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbStep_1_ConfigPanel() {
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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdbStep1ConfigPanel within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theStep1ConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep1ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbStep_2_ConfigPanel() {
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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdbStep2ConfigPanel within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theStep2ConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep2ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbStep_3_ConfigPanel() {
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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdbStep3ConfigPanel within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theStep3ConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep3ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbStep_4_ConfigPanel() {
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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdbStep4ConfigPanel within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theStep4ConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep4ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbStep_5_ConfigPanel() {
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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdbStep5ConfigPanel within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theStep5ConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

		// Redirect vsdb panel display to class Vsdb.
		theVsdb.showStep5ConfigPanel();

		// Now refresh theConfigPanel
		theConfigPanel.revalidate();
		theConfigPanel.repaint();

	}

	private void addVsdbStep_6_ConfigPanel() {
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
		SpringLayout.Constraints contraint_1 = theConfigPanelLayout
				.getConstraints(theVsdb_Choice);
		contraint_1.setX(Spring.constant(180));
		contraint_1.setY(Spring.constant(10));
		contraint_1.setWidth(Spring.constant(150));
		contraint_1.setHeight(Spring.constant(30));

		// Position theVsdbStep6ConfigPanel within the config panel
		SpringLayout.Constraints aConstraint = theConfigPanelLayout
				.getConstraints(theVsdb.theStep6ConfigPanel);
		aConstraint.setX(Spring.constant(10));
		aConstraint.setY(Spring.constant(35));
		aConstraint.setWidth(Spring.constant(PANEL_WIDTH));
		aConstraint.setHeight(Spring.constant(PANEL_HEIGHT));

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
		// 3.1 theIatCboxPanel
		//
		// Add components into theIatCboxPanel
		theIatCboxPanel.add(theFcstDiffCbox);
		theIatCboxPanel.add(theGeCbox);
		theIatCboxPanel.add(theHitCbox);
		theIatCboxPanel.add(theRadmonCbox);
		theIatCboxPanel.add(theVsdbCbox);

		// Set border for theIatCboxPanel
		Border lowerBorder = BorderFactory.createLoweredBevelBorder();
		TitledBorder theIatCboxPanelBorder = BorderFactory.createTitledBorder(
				lowerBorder, "IAT Selection");
		theIatCboxPanelBorder.setTitleJustification(TitledBorder.CENTER);
		theIatCboxPanel.setBorder(theIatCboxPanelBorder);

		// 3.2 Add 3 components into theRunPanel
		theRunPanel.add(theIatCboxPanel);
		theRunPanel.add(theRunBtn);
		theRunPanel.add(theStatBtn);
		theRunPanel.add(theParBtn);
		theRunPanel.add(theExitBtn);

		// 3.3 Position 3 components within theRunPanel
		SpringLayout theRunPanelLayout = new SpringLayout();
		theRunPanel.setLayout(theRunPanelLayout);

		int spacer = 5;
		int choiceWidth = 300;
		int choiceHgt = 100;
		int x1 = 0;
		int y1 = 0;

		int btnWidth = 150;
		int btnHgt = 30; 
		
		// Position of Run btn
		int x2 = x1 + choiceWidth + spacer;
		int y2 = choiceHgt - btnHgt - y1;

		// Position of Status btn
		int x3 = x2 + btnWidth + spacer;
		int y3 = y2;

		// Position of PAR btn
		int x4 = x3 + btnWidth + spacer;
		int y4 = y3;

		// Position of Exit btn
		int x5 = x4 + btnWidth + spacer;
		int y5 = y4;

		// Set IAT Choice box panel
		SpringLayout.Constraints iatCheckBoxPanelCons = theRunPanelLayout
				.getConstraints(theIatCboxPanel);
		iatCheckBoxPanelCons.setX(Spring.constant(x1));
		iatCheckBoxPanelCons.setY(Spring.constant(y1));
		iatCheckBoxPanelCons.setWidth(Spring.constant(choiceWidth));
		iatCheckBoxPanelCons.setHeight(Spring.constant(choiceHgt));

		// Set Run btn
		SpringLayout.Constraints runButtonCons = theRunPanelLayout
				.getConstraints(theRunBtn);
		runButtonCons.setX(Spring.constant(x2));
		runButtonCons.setY(Spring.constant(y2));
		runButtonCons.setWidth(Spring.constant(btnWidth));
		runButtonCons.setHeight(Spring.constant(btnHgt));

		// Set Status btn
		SpringLayout.Constraints statButtonCons = theRunPanelLayout
				.getConstraints(theStatBtn);
		statButtonCons.setX(Spring.constant(x3));
		statButtonCons.setY(Spring.constant(y3));
		statButtonCons.setWidth(Spring.constant(btnWidth));
		statButtonCons.setHeight(Spring.constant(btnHgt));

		// Set PAR btn
		SpringLayout.Constraints parButtonCons = theRunPanelLayout
				.getConstraints(theParBtn);
		parButtonCons.setX(Spring.constant(x4));
		parButtonCons.setY(Spring.constant(y4));
		parButtonCons.setWidth(Spring.constant(btnWidth));
		parButtonCons.setHeight(Spring.constant(btnHgt));

		// Set Exit btn
		SpringLayout.Constraints exitButtonCons = theRunPanelLayout
				.getConstraints(theExitBtn);
		exitButtonCons.setX(Spring.constant(x5));
		exitButtonCons.setY(Spring.constant(y5));
		exitButtonCons.setWidth(Spring.constant(btnWidth));
		exitButtonCons.setHeight(Spring.constant(btnHgt));

		// ==================================================================
		// 4. theConfigPanel ( 800 x 600 )
		// ==================================================================
		Border lineBorder = BorderFactory.createLineBorder(Color.black);
		LineBorder theConfigPanelBorder = (LineBorder) BorderFactory
				.createLineBorder(Color.black);
		theConfigPanel.setBorder(theConfigPanelBorder);

		// theIAT_Choice (pull-down options)
		theIAT_Choice.add("choose...");
		theIAT_Choice.add("FcstDiff");
		theIAT_Choice.add("Ge");
		theIAT_Choice.add("Hit");
		theIAT_Choice.add("Radmon");
		theIAT_Choice.add("Vsdb");

		// theIAT_Choice (pull-down options)
		theVsdb_Choice.add("choose...");
		theVsdb_Choice.add("top-level config");
		theVsdb_Choice.add("step 1 config");
		theVsdb_Choice.add("step 2 config");
		theVsdb_Choice.add("step 3 config");
		theVsdb_Choice.add("step 4 config");
		theVsdb_Choice.add("step 5 config");
		theVsdb_Choice.add("step 6 config");

		// theIAT_Choice (pull-down options)
		theRadmon_Choice.add("choose...");
		theRadmon_Choice.add("step 1 config");
		theRadmon_Choice.add("step 2 config");

		// 4.1 add components into theConfigPanel
		theConfigPanel.add(theIAT_Choice);
		theConfigPanel.add(theEmptyConfig.theEmptyConfigPanel);

		// 4.2 Position components within theConfigPanel using SpringLayout and
		// Contraint.
		theConfigPanel.setLayout(theConfigPanelLayout);

		int xWidth = 150;
		int yHeight = 30;

		// Position theIAT_Choice within the config panel
		SpringLayout.Constraints iatChoiceCons = theConfigPanelLayout
				.getConstraints(theIAT_Choice);
		iatChoiceCons.setX(Spring.constant(10));
		iatChoiceCons.setY(Spring.constant(10));
		iatChoiceCons.setWidth(Spring.constant(xWidth));
		iatChoiceCons.setHeight(Spring.constant(yHeight));

		// Position theEmptyConfig within the config panel
		SpringLayout.Constraints emptyConfigCons = theConfigPanelLayout
				.getConstraints(theEmptyConfig.theEmptyConfigPanel);
		emptyConfigCons.setX(Spring.constant(10));
		emptyConfigCons.setY(Spring.constant(10));
		emptyConfigCons.setWidth(Spring.constant(xWidth));
		emptyConfigCons.setHeight(Spring.constant(yHeight));

		// Redirect to class EmptyConfig to show its own components.
		theEmptyConfig.showConfigPanel();
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

	public boolean confirm(String aString) {
		int n = JOptionPane.showConfirmDialog(null, aString, "",
				JOptionPane.YES_NO_OPTION);
		if (n == 0) {
			JOptionPane.showMessageDialog(null, aString);
		} else
			JOptionPane.showMessageDialog(null, "NOT to " + aString);
		return n == 0 ? true : false;
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
