package iatgui;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class FcstDiff {
	// Constant
	private final int LBL_WIDTH = 200;
	private final int LBL_HEIGHT = 15;
	private final int TEXTAREA_WIDTH = 400;
	private final int TEXTAREA_HEIGHT = 15;
	private final int BUTTON_WIDTH = 100;
	private final int BUTTON_HEIGHT = 15;
	private final int SPACER = 5;

	public JPanel theFcstDiffConfigPanel = new JPanel();

	// 3) Components for FcstDiff config panel
	public JLabel[] FcstDiffConfigLblArr = new JLabel[16];
	public JTextArea[] FcstDiffConfigTxtArr = new JTextArea[16];

	public String[] FcstDiffConfigLblValueArr = new String[16];
	public String[] FcstDiffConfigTxtValueArr = new String[16];;

	public JButton[] FcstDiffConfigBrowseBtnArr = new JButton[16];
	public JButton FcstDiffConfigSaveBtn = new JButton("save");


	// Constructor
	FcstDiff() {
		initializeLblTxt();
	}
	
	// Initialize labels and textareas
	public void initializeLblTxt() {
		// Create some temporary values as example
		String[] initialLblValueArr = { "Workspace Dir", "FcstDiff pkg dir",
				"CDATE (yyyymmdd)", "edate (ddMMMyyy)", "cycle",
				"model name 1", "model name 2", "model Type 1 (gfs/gdas/ecm)",
				"model type 2 (gfs/gdas/ecm)", "input dir 1", "input dir 2",
				"forecast hour list", "area list", "map air option (yes/no)",
				"map sfc option (yes/no)", "map uv option (yes/no)" };
		String[] initialTxtValueArr = { "/data/users/dxu/fcstDiff_workspace",
				"/data/users/dxu/fcstDiff_pkg", "20140515", "15May2014", "00",
				"cntrl", "ecmwf", "gfs", "ecm",
				"/data/users/dxu/fcstDiff_workspace/data/input/cntrl_fcst",
				"/data/users/dxu/fcstDiff_workspace/data/input/ecmwf_fcst",
				"f00 f24 f48 f72 f96 f120", "gb nh sh tr na ua", "yes", "no",
				"no" };

		for (int index = 0; index < 16; index++) {
			// These values will be updated once "save" button is clicked.
			FcstDiffConfigLblValueArr[index] = initialLblValueArr[index];
			FcstDiffConfigTxtValueArr[index] = initialTxtValueArr[index];

			FcstDiffConfigLblArr[index] = new JLabel(initialLblValueArr[index]);
			FcstDiffConfigTxtArr[index] = new JTextArea(
					initialTxtValueArr[index]);
		}
	}

	public void showConfigPanel() {
		theFcstDiffConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout fcstDiffConfigPanelLayout = new SpringLayout();
		theFcstDiffConfigPanel.setLayout(fcstDiffConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Constraint to control position
		SpringLayout.Constraints[] lblConsArr = new SpringLayout.Constraints[16];
		SpringLayout.Constraints[] txtConsArr = new SpringLayout.Constraints[16];
		SpringLayout.Constraints[] btnConsArr = new SpringLayout.Constraints[16];

		for (int index = 0; index < 16; index++) {

			// System.out.println(FcstDiffConfigLblValueArr[index]);
			// Initialize each JLabel and JTextArea
			FcstDiffConfigLblArr[index] = new JLabel(
					FcstDiffConfigLblValueArr[index]);
			FcstDiffConfigTxtArr[index] = new JTextArea(
					FcstDiffConfigTxtValueArr[index]);
			FcstDiffConfigBrowseBtnArr[index] = new JButton("Browse");

			// Add into FcstDiffConfigLblArr
			theFcstDiffConfigPanel.add(FcstDiffConfigLblArr[index]);
			theFcstDiffConfigPanel.add(FcstDiffConfigTxtArr[index]);

			switch (index) {
			case 0:
			case 1:
			case 9:
			case 10:
				theFcstDiffConfigPanel.add(FcstDiffConfigBrowseBtnArr[index]);
				break;
			default:
				break;
			}

			// Position label
			lblConsArr[index] = fcstDiffConfigPanelLayout
					.getConstraints(FcstDiffConfigLblArr[index]);
			lblConsArr[index].setX(Spring.constant(xPos));
			lblConsArr[index].setY(Spring.constant(yPos));
			lblConsArr[index].setWidth(Spring.constant(LBL_WIDTH));
			lblConsArr[index].setHeight(Spring.constant(LBL_HEIGHT));

			// Position TextArea
			txtConsArr[index] = fcstDiffConfigPanelLayout
					.getConstraints(FcstDiffConfigTxtArr[index]);
			txtConsArr[index].setX(Spring.constant(xPos + LBL_WIDTH + SPACER));
			txtConsArr[index].setY(Spring.constant(yPos));
			txtConsArr[index].setWidth(Spring.constant(TEXTAREA_WIDTH));
			txtConsArr[index].setHeight(Spring.constant(TEXTAREA_HEIGHT));

			// Position Browse button
			btnConsArr[index] = fcstDiffConfigPanelLayout
					.getConstraints(FcstDiffConfigBrowseBtnArr[index]);
			btnConsArr[index].setX(Spring.constant(xPos + LBL_WIDTH + SPACER
					+ TEXTAREA_WIDTH + SPACER));
			btnConsArr[index].setY(Spring.constant(yPos));
			btnConsArr[index].setWidth(Spring.constant(BUTTON_WIDTH));
			btnConsArr[index].setHeight(Spring.constant(BUTTON_HEIGHT));

			yPos += LBL_HEIGHT;
			yPos += SPACER;

		}

		theFcstDiffConfigPanel.add(FcstDiffConfigSaveBtn);
		SpringLayout.Constraints saveBtnCons = fcstDiffConfigPanelLayout
				.getConstraints(FcstDiffConfigSaveBtn);
		saveBtnCons.setX(Spring.constant(xPos + LBL_WIDTH + SPACER
				+ TEXTAREA_WIDTH + SPACER));
		saveBtnCons.setY(Spring.constant(yPos + SPACER));
		saveBtnCons.setWidth(Spring.constant(BUTTON_WIDTH));
		saveBtnCons.setHeight(Spring.constant(BUTTON_HEIGHT));

	}


}
