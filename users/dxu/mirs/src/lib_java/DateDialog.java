/**
 * $Id: DateDialog.java
 * @Description: This class represent a Date selection
 *
 * @version 1.00 2007-10-29
 *
 * @author Wanchun Chen
 *
 */
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class DateDialog extends JDialog
                        implements ActionListener, FocusListener
{

  private String yyyy;
  private String mm;
  private String dd;
  
  private JLabel yearLabel	= new JLabel("Year (4 digits)") ;
  private JLabel monthLabel	= new JLabel("Month (2 digits)") ;
  private JLabel dayLabel	= new JLabel("Day (2 digits)") ;
  
  private JTextField yearField	= new JTextField(32);
  private JTextField monthField	= new JTextField(32);
  private JTextField dayField	= new JTextField(32);
  
  private JButton okButton = new JButton("OK");
  private JButton cancelButton = new JButton("Cancel");
  private JButton defaultButton = new JButton("Default");
  
  public DateDialog(JFrame frame, String title, boolean model)
  {
    super(frame, title, model);
    setSize(300, 160);
    
    JPanel panel = new JPanel();
    
    SpringLayout springLayout = new SpringLayout();
    panel.setLayout(springLayout);

     
	SpringLayout.Constraints yearLabelCons = springLayout.getConstraints(yearLabel);
        yearLabelCons.setX(Spring.constant(0));
        yearLabelCons.setY(Spring.constant(5));
        yearLabelCons.setWidth(Spring.constant(125));
        yearLabelCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints yearFieldCons = springLayout.getConstraints(yearField);
        yearFieldCons.setX(Spring.constant(125));
        yearFieldCons.setY(Spring.constant(5));
        yearFieldCons.setWidth(Spring.constant(125));
        yearFieldCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints monthLabelCons = springLayout.getConstraints(monthLabel);
        monthLabelCons.setX(Spring.constant(0));
        monthLabelCons.setY(Spring.constant(35));
        monthLabelCons.setWidth(Spring.constant(125));
        monthLabelCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints monthFieldCons = springLayout.getConstraints(monthField);
        monthFieldCons.setX(Spring.constant(125));
        monthFieldCons.setY(Spring.constant(35));
        monthFieldCons.setWidth(Spring.constant(125));
        monthFieldCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints dayLabelCons = springLayout.getConstraints(dayLabel);
        dayLabelCons.setX(Spring.constant(0));
        dayLabelCons.setY(Spring.constant(65));
        dayLabelCons.setWidth(Spring.constant(125));
        dayLabelCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints dayFieldCons = springLayout.getConstraints(dayField);
        dayFieldCons.setX(Spring.constant(125));
        dayFieldCons.setY(Spring.constant(65));
        dayFieldCons.setWidth(Spring.constant(125));
        dayFieldCons.setHeight(Spring.constant(25));
	
	
    	SpringLayout.Constraints okCons = springLayout.getConstraints(okButton);
    	okCons.setX(Spring.constant(50));
    	okCons.setY(Spring.constant(100));
    	okCons.setWidth(Spring.constant(75));
    	okCons.setHeight(Spring.constant(25));
    
    	SpringLayout.Constraints cancelCons = springLayout.getConstraints(cancelButton);
    	cancelCons.setX(Spring.constant(150));
    	cancelCons.setY(Spring.constant(100));
    	cancelCons.setWidth(Spring.constant(75));
    	cancelCons.setHeight(Spring.constant(25));

     
    
    panel.add(yearLabel);
    panel.add(yearField);
    panel.add(monthLabel);
    panel.add(monthField);
    panel.add(dayLabel);
    panel.add(dayField);
    panel.add(okButton);
    panel.add(cancelButton);
    
    
    okButton.addActionListener(this);
    cancelButton.addActionListener(this);
    
    yearField.addFocusListener(this);
    monthField.addFocusListener(this);
    dayField.addFocusListener(this);

    Container contentPane = getContentPane();
    contentPane.add(panel, "Center");
  }    
    
  public void focusGained(FocusEvent event)
  {
  }

  public void focusLost(FocusEvent event)
  {
    if ( event.getSource() == yearField ) {
        yyyy = yearField.getText();
    }
    else if ( event.getSource() == monthField ) {
        mm = monthField.getText();
    }
    else if ( event.getSource() == dayField ) {
        dd = dayField.getText();
    }
  }
  
  public void actionPerformed(ActionEvent event)
  {

    if ( event.getSource() == okButton ) {
        setVisible(false);
    }
    else if ( event.getSource() == cancelButton ) {
        setVisible(false);
    }
  }
  
  public String getDate() {
    return yyyy + "-" + mm + "-" + dd ;
  }
  
}
