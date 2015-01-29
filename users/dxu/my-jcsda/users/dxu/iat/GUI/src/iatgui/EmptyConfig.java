package iatgui;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class EmptyConfig implements SizeDefinition {
	public JPanel theEmptyConfigPanel = new JPanel();
	public JLabel theEmptyLbl = new JLabel("");

	// Constructor
	EmptyConfig() {
	}

	public void showConfigPanel() {
		theEmptyConfigPanel.removeAll();

		// Create a SpringLayout for theFcstDiffConfigPanel
		SpringLayout emptyConfigPanelLayout = new SpringLayout();
		theEmptyConfigPanel.setLayout(emptyConfigPanelLayout);

		// Start point
		int xPos = 5;
		int yPos = 5;

		// Add an empty label.
		theEmptyConfigPanel.add(theEmptyLbl);

		// Constraint to control positions of Label and TextArea
		SpringLayout.Constraints lblCons = new SpringLayout.Constraints();

		// Position TextArea
		lblCons = emptyConfigPanelLayout.getConstraints(theEmptyLbl);
		lblCons.setX(Spring.constant(xPos));
		lblCons.setY(Spring.constant(yPos));
		lblCons.setWidth(Spring.constant(600));
		lblCons.setHeight(Spring.constant(400));
	}

}
