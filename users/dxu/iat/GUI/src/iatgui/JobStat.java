package iatgui;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class JobStat implements SizeDefinition {
	public JPanel theJobStatPanel = new JPanel();
	public JLabel theLbl = new JLabel("Job Stats:");
	public JTextArea theTxt = new JTextArea("");
	public JScrollPane theScroll;

	// Constructor
	JobStat() {
		theTxt.setEditable(false);
		theScroll = new JScrollPane(theTxt,
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	}

	public void setJobStat(String aStr) {
		theTxt.setText(aStr);
	}

	public void showConfigPanel() {
		theJobStatPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout aLayout = new SpringLayout();
		theJobStatPanel.setLayout(aLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Add an label and textarea
		theJobStatPanel.add(theLbl);
		theJobStatPanel.add(theScroll);

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints lblCons = new SpringLayout.Constraints();
		SpringLayout.Constraints txtCons = new SpringLayout.Constraints();

		// Position Label
		lblCons = aLayout.getConstraints(theLbl);
		lblCons.setX(Spring.constant(xPos));
		lblCons.setY(Spring.constant(yPos));
		lblCons.setWidth(Spring.constant(LBL_WIDTH));
		lblCons.setHeight(Spring.constant(LBL_HEIGHT));

		// Position textarea
		txtCons = aLayout.getConstraints(theScroll);
		txtCons.setX(Spring.constant(xPos));
		txtCons.setY(Spring.constant(yPos + LBL_HEIGHT + SPACER));
		txtCons.setWidth(Spring.constant(TEXTAREA_WIDTH));
		txtCons.setHeight(Spring.constant(TEXTAREA_HEIGHT));

	}

}
