import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.filechooser.*;


public class ViewDialog extends JDialog implements ActionListener 
{
	private JTextArea textArea = new JTextArea(40,86);
	private JButton editButton = new JButton("Edit");
	private JButton saveButton = new JButton("Save");
	private JButton saveAsButton = new JButton("Save As...");
	private JButton cancelButton = new JButton("Close");
	private boolean saveButtonPressed   = false;
	private boolean cancelButtonPressed = false;
	private static final long serialVersionUID = 1L;
	
	public ViewDialog(JFrame parent, String title, boolean modal) {
		super(parent, modal);

		setTitle(title);
		setSize(800,600);
    		
		JPanel southPanel = new JPanel();
		southPanel.add(editButton);
		southPanel.add(saveButton);
		southPanel.add(saveAsButton);
		southPanel.add(cancelButton);
		add(southPanel, "South");

		JPanel centerPanel = new JPanel();
		centerPanel.setLayout(new BorderLayout());
		JScrollPane scrollingArea = new JScrollPane(textArea);
		centerPanel.add(scrollingArea,BorderLayout.CENTER);
		add(centerPanel,   "Center");

		editButton.addActionListener(this);
		saveButton.addActionListener(this);
		saveAsButton.addActionListener(this);
		cancelButton.addActionListener(this);	

		SymWindow aSymWindow = new SymWindow();
		this.addWindowListener(aSymWindow);

		textArea.setEditable(false);

		pack();
		setLocation(50,50);
	}
	
	
	/**
	 * dump stringBuffer into textArea
	 */
	public void dumpContent(StringBuffer stringBuffer) 
	{		
		textArea.append(stringBuffer.toString());
		textArea.setCaretPosition(0);	
	}
	
	public String getContent() 
	{		
		return textArea.getText();
	}
	
	public boolean isSaveButtonPressed()
	{
		return saveButtonPressed;    
	}
	
	public boolean isCancelButtonPressed()
	{
	    	return cancelButtonPressed;    
	}

        /**
         * action to take when buttons are clicked
         */
	public void actionPerformed(ActionEvent event) {
	    
	    if ( event.getSource() == editButton ) {
	    	textArea.setEditable(true);
	    }
	    
	    else if ( event.getSource() == saveButton ) {
	    	saveButtonPressed = true;
	    	cancelButtonPressed = false;
	    	setVisible(false);
	    }
	    
	    else if ( event.getSource() == saveAsButton ) {
	    
	  	JFileChooser chooser = new JFileChooser(".");
	  	FileNameExtensionFilter filter = new FileNameExtensionFilter("Bash file", "bash");
    	  	chooser.setFileFilter(filter);
		int returnVal = chooser.showSaveDialog(this);
	  
	  	if(returnVal == JFileChooser.APPROVE_OPTION) {
 	  		String aFile = chooser.getSelectedFile().getName();
			String aPath = chooser.getCurrentDirectory().getPath() + "/" ;
			try {
				PrintWriter out = new PrintWriter(new FileWriter(aPath + aFile));
				out.println(textArea.getText());
				out.close();
			}
			catch ( IOException ioe )
			{		    
	    			System.out.println("" + ioe);	    
			} 
	  	}

	    }
	    
	    else if ( event.getSource() == cancelButton ) {
		saveButtonPressed =  false;
		cancelButtonPressed = true;
	     	setVisible(false);
	    }
	}
	
	
        /**
         * internal class to close the dialog window
         */
	class SymWindow extends java.awt.event.WindowAdapter
	{
		public void windowClosing(java.awt.event.WindowEvent event)
		{
			Object object = event.getSource();
			if (object == ViewDialog.this)
				dispose();
		}
	}
	
}
