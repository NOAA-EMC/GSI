/**
 * $Id: PathDialog.java
 * @Description: This class represent a Dialog to set paths in MIRS system. You can 
 * 	set and reset to default values from here. It's used by Monitor and from
 *	GUI of Monitor, "Edit" -> "Path Settings", then the popup window will come up.
 *	The usage is sort of straitfoward.
 * 
 * @version 1.00 2007-01-09
 *
 * @author Wanchun Chen
 *	
 */


import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Enumeration;
import java.awt.image.*; 
import java.awt.*; 
import java.awt.event.*; 

import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.border.*;
import java.util.Map;
import java.util.HashMap;

import java.io.*;

/**
 * Constructor of PathDialog
 */
public class PathDialog extends JDialog 
			implements ActionListener, TreeSelectionListener, FocusListener 
{
  
  private String bashPath = "#!/bin/bash\n\n";
  
  private Map<String, String> paths = new HashMap<String, String>();
  
  private String majorNodeString 		= new String("Major Root Path");
  private String researchNodeString	      	= new String("Research Data & Path");	
  private String externalNodeString	      	= new String("External Data & Path");	
  private String staticNodeString	      	= new String("Static Data & Path");	
  private String semiStaticNodeString 	     	= new String("Semi-Static Data & Path");
  private String testbedNodeString	      	= new String("Testbed Data & Path");	
  private String dynamicNodeString	      	= new String("Dynamic Data & Path");	
  private String controlFileNodeString 	        = new String("Control Files");	        
  private String fileListNodeString	      	= new String("File List");		
  private String applicationNodeString 	        = new String("Application");	        

  private String rootPathKey 			= new String("rootPath");
  private String dataPathKey 			= new String("dataPath");
  private String binPathKey  			= new String("binPath");
  private String logPathKey  			= new String("logPath");
  private String idlPathKey  			= new String("IDL");
  private String libPathKey  			= new String("LD_LIBRARY_PATH"); 

  private String researchDataPathKey 		= new String("researchDataPath");
  private String fwdPathKey 			= new String("fwdPath");	  
  private String out1dvarPathKey 		= new String("out1dvarPath");    
  private String monitorFileKey 		= new String("monitorFile");     
  private String modelNonErrPathKey 		= new String("modelNonErrPath");

  private String externalDataPathKey 		= new String("externalDataPath"); 
  private String rdrSensor1PathKey 		= new String("rdrSensor1Path");	
  private String rdrSensor2PathKey 		= new String("rdrSensor2Path");
  private String rdrF16PathKey 			= new String("rdrF16Path");
  private String rdrOrbitPathKey 		= new String("rdrOrbitPath");	
  private String nwpGdasGridPathKey 		= new String("nwpGdasGridPath");
  private String nwpEcmwfGridPathKey 		= new String("nwpEcmwfGridPath");
  private String nwpGfsGridPathKey 		= new String("nwpGfsGridPath");

  private String staticDataPathKey 		= new String("staticDataPath");	      
  private String instrumentPathKey 		= new String("instrumentPath");	      
  private String instrumentSensor1FileKey 	= new String("instrumentSensor1File");   
  private String instrumentSensor2FileKey 	= new String("instrumentSensor2File");     
  private String instrumentSensor1Sensor2FileKey= new String("instrumentSensor1Sensor2File");
  private String topographyFileKey 		= new String("topographyFile");	     
  private String antennaPathKey 		= new String("antennaPath");	     
  private String antennaSensor1FileKey 		= new String("antennaSensor1File");      
  private String antennaSensor2FileKey 		= new String("antennaSensor2File");	     
  private String tune1FileKey 			= new String("tune1File");	      
  private String tune2FileKey 			= new String("tune2File");
  private String nedtNominalFileKey 		= new String("nedtNominalFile");
  private String modelErrNominalFileKey 	= new String("modelErrNominalFile");
  //private String covBkgAtmFileKey  	    	= new String("covBkgAtmFile");
  //private String covBkgSfcFileKey  	    	= new String("covBkgSfcFile");
  private String CRTMcoeffPathKey   	    	= new String("CRTMcoeffPath");
  private String siceEmissCatalogFileKey   	= new String("siceEmissCatalogFile");
  private String snowEmissCatalogFileKey   	= new String("snowEmissCatalogFile");

  private String semiStaticDataPathKey 		= new String("semiStaticDataPath");
  private String biasPathKey 			= new String("biasPath");	  
  private String regressPathKey 		= new String("regressPath");	  

  private String testbedDataPathKey 		= new String("testbedDataPath");  
  private String nedtPathKey 			= new String("nedtPath");       
  private String nedtSensor1PathKey 		= new String("nedtSensor1Path");       
  private String nedtSensor2PathKey 		= new String("nedtSensor2Path");       
  private String nedtSensor1Sensor2PathKey 	= new String("nedtSensor1Sensor2Path");       
  private String edrPathKey 			= new String("edrPath");
  private String depPathKey 			= new String("depPath");
  private String figsPathKey 			= new String("figsPath");       
  private String perfsMonitorPathKey 		= new String("perfsMonitorPath"); 
  private String logFileKey 			= new String("logFile");	  

  private String dynamicDataPathKey 		= new String("dynamicDataPath");
  private String tdrPathKey 			= new String("tdrPath");	     
  private String tdrSensor1PathKey 		= new String("tdrSensor1Path");	
  private String tdrSensor2PathKey 		= new String("tdrSensor2Path");	  
  private String sdrPathKey 			= new String("sdrPath");	     
  private String sdrSensor1PathKey 		= new String("sdrSensor1Path");	
  private String sdrSensor2PathKey	 	= new String("sdrSensor2Path");	 
  private String fmsdrPathKey 			= new String("fmsdrPath");	  
  private String choppPathKey 			= new String("choppPath");	  
  private String nwpAnalysPathKey 		= new String("nwpAnalysPath");  
  private String fwdAnalysPathKey 		= new String("fwdAnalysPath");  
  private String regressRetrPathKey 		= new String("regressRetrPath"); 

  private String controlDataPathKey 		= new String("controlDataPath");	  
  private String rdr2tdrSensor1ControlFileKey 	= new String("rdr2tdrSensor1ControlFile"); 
  private String rdr2tdrSensor2ControlFileKey 	= new String("rdr2tdrSensor2ControlFile");   
  private String mergeNedtControlFileKey 	= new String("mergeNedtControlFile");   
  private String tdr2sdrSensor1ControlFileKey 	= new String("tdr2sdrSensor1ControlFile"); 
  private String tdr2sdrSensor2ControlFileKey 	= new String("tdr2sdrSensor2ControlFile");   
  private String fmControlFileKey 		= new String("fmControlFile");	       
  private String fmsdr2edrControlFileKey 	= new String("fmsdr2edrControlFile");  
  private String grid2nwpControlFileKey 	= new String("grid2nwpControlFile");     
  private String fwdControlFileKey 		= new String("fwdControlFile");	       
  private String regressControlFileKey 		= new String("regressControlFile");	   
  private String choppControlFileKey 		= new String("choppControlFile");	     
  private String mergeEdrControlFileKey 	= new String("mergeEdrControlFile");	  
  private String biasCompuControlFileKey 	= new String("biasCompuControlFile");	 
  private String biasVerifControlFileKey 	= new String("biasVerifControlFile");	 
  private String regressGenControlFileKey 	= new String("regressGenControlFile");
  private String modifyNedtControlFileKey 	= new String("modifyNedtControlFile");
  private String psFigsGenControlFileKey 	= new String("psFigsGenControlFile");    

  private String inputDataPathKey 		= new String("inputDataPath");  	    
  private String rdrSensor1ListKey 		= new String("rdrSensor1List");		   
  private String rdrSensor2ListKey 		= new String("rdrSensor2List");		   
  private String tdrSensor1ListKey 		= new String("tdrSensor1List");		   
  private String tdrSensor2ListKey 		= new String("tdrSensor2List");		   
  private String sdrSensor1ListKey 		= new String("sdrSensor1List");		   
  private String sdrSensor2ListKey 		= new String("sdrSensor2List");
  private String sdrSensor3ListKey 		= new String("sdrSensor3List");
  private String sdrSensor4ListKey 		= new String("sdrSensor4List");
  private String fmsdrListKey 			= new String("fmsdrList");
  private String fmsdr4BiasListKey 		= new String("fmsdr4BiasList");		 
  private String ecfmsdrListKey 		= new String("ecfmsdrList");		 
  private String fmsdr4ChoppListKey 		= new String("fmsdr4ChoppList");	  
  private String fmsdr4NwpListKey 		= new String("fmsdr4NwpList");
  private String fmsdr4RegressListKey 		= new String("fmsdr4RegressList");	
  private String fmsdr4ApplyRegressListKey 	= new String("fmsdr4ApplyRegressList");
  private String edrListKey 			= new String("edrList");
  private String edr4BiasListKey 		= new String("edr4BiasList");	      
  private String edr4MergeListKey 		= new String("edr4MergeList");	      
  private String nedtListKey 			= new String("nedtList");	      
  private String nedtSensor1ListKey 		= new String("nedtSensor1List");	      
  private String nedtSensor2ListKey 		= new String("nedtSensor2List");	      
  private String gridSfcNwpAnalysListKey 	= new String("gridSfcNwpAnalysList");	 
  private String gridAtmNwpAnalysListKey 	= new String("gridAtmNwpAnalysList");	 
  private String nwpAnalysListKey 		= new String("nwpAnalysList");		   
  private String nwpAnalysRetrListKey 		= new String("nwpAnalysRetrList");	    
  private String nwpAnalys4BiasListKey 		= new String("nwpAnalys4BiasList");	   
  private String nwpAnalys4RegressListKey 	= new String("nwpAnalys4RegressList");   
  private String fwdAnalys4BiasListKey 		= new String("fwdAnalys4BiasList");	 

  private String rdr2tdrSensor1ApplicationKey 	= new String("rdr2tdrSensor1Src");   
  private String rdr2tdrSensor2ApplicationKey 	= new String("rdr2tdrSensor2Src");	 
  private String mergeNedtApplicationKey 	= new String("mergeNedtSrc");	 
  private String tdr2sdrApplicationKey 		= new String("tdr2sdrSrc");	 
  private String fmApplicationKey 		= new String("fmSrc");		 
  private String choppApplicationKey 		= new String("choppSrc");	 
  private String fmsdr2edrApplicationKey 	= new String("fmsdr2edrSrc");	 
  private String mergeEdrApplicationKey 	= new String("mergeEdrSrc");
  private String vippApplicationKey 		= new String("vippSrc");
  private String nedtMonitorApplicationKey 	= new String("nedtMonitorSrc");	 
  private String nwpGenAnalysApplicationKey 	= new String("nwpGenAnalysSrc");   
  private String fwdApplicationKey 		= new String("fwdSrc");		 
  private String determineBiasApplicationKey 	= new String("determineBiasSrc");  
  private String regressAlgApplicationKey 	= new String("regressAlgSrc");	 
  private String applyRegressAlgApplicationKey 	= new String("applyRegressAlgSrc");

  
  private String satIdKey = new String("satId");
  private String satId  = null;

  private String processModeKey = new String("processMode");
  private String processMode  = null;
 
  /**
   * Constructor
   * @param frame 	the frame
   * @param title	the title of this dialog
   * @param model	the model
   */
  public PathDialog(JFrame frame, String title, boolean model) 
  { 
    super(frame, title, model);
    setSize(1000,850);
    
    // the root of the class tree is Object
    root = new DefaultMutableTreeNode("Path Category");
    //model = new DefaultTreeModel(root);

    
    majorNode       = new DefaultMutableTreeNode(majorNodeString);
    researchNode    = new DefaultMutableTreeNode(researchNodeString);
    externalNode    = new DefaultMutableTreeNode(externalNodeString);
    staticNode      = new DefaultMutableTreeNode(staticNodeString);
    semiStaticNode  = new DefaultMutableTreeNode(semiStaticNodeString);
    testbedNode     = new DefaultMutableTreeNode(testbedNodeString);
    dynamicNode     = new DefaultMutableTreeNode(dynamicNodeString);
    controlFileNode = new DefaultMutableTreeNode(controlFileNodeString);
    fileListNode    = new DefaultMutableTreeNode(fileListNodeString);
    applicationNode = new DefaultMutableTreeNode(applicationNodeString);


    root.add(majorNode);
    root.add(researchNode);
    root.add(externalNode);
    root.add(staticNode);
    root.add(semiStaticNode);
    root.add(testbedNode);
    root.add(dynamicNode);
    root.add(controlFileNode);
    root.add(fileListNode);
    root.add(applicationNode);

    
    //tree = new JTree(model);
    tree = new JTree(root);


    // set up selection mode
    tree.addTreeSelectionListener(this);


    //tree.setRootVisible(false);

    int mode = TreeSelectionModel.SINGLE_TREE_SELECTION;
    tree.getSelectionModel().setSelectionMode(mode);


    // add tree and text area to the content pane
    JPanel panel = new JPanel();
    
    //SpringLayout springLayout = new SpringLayout();
    panel.setLayout(springLayout);
    
    //JScrollPane treePanel = new JScrollPane(tree);
    treePanel = new JScrollPane(tree);
    
    SpringLayout.Constraints treeCons = springLayout.getConstraints(treePanel);
    treeCons.setX(Spring.constant(0));
    treeCons.setY(Spring.constant(0));
    treeCons.setWidth(Spring.constant(200));
    treeCons.setHeight(Spring.constant(800));
    
    SpringLayout.Constraints rightCons = springLayout.getConstraints(rightPanel);
    rightCons.setX(Spring.constant(205));
    rightCons.setY(Spring.constant(0));
    rightCons.setWidth(Spring.constant(795));
    rightCons.setHeight(Spring.constant(775));
    
    SpringLayout.Constraints okCons = springLayout.getConstraints(okButton);
    okCons.setX(Spring.constant(425));
    okCons.setY(Spring.constant(790));
    okCons.setWidth(Spring.constant(75));
    okCons.setHeight(Spring.constant(25));
    
    SpringLayout.Constraints cancelCons = springLayout.getConstraints(cancelButton);
    cancelCons.setX(Spring.constant(510));
    cancelCons.setY(Spring.constant(790));
    cancelCons.setWidth(Spring.constant(75));
    cancelCons.setHeight(Spring.constant(25));

    SpringLayout.Constraints defaultCons = springLayout.getConstraints(defaultButton);
    defaultCons.setX(Spring.constant(595));
    defaultCons.setY(Spring.constant(790));
    defaultCons.setWidth(Spring.constant(125));
    defaultCons.setHeight(Spring.constant(25));

    panel.add(treePanel);
    panel.add(rightPanel);

    panel.add(okButton);
    panel.add(cancelButton);
    panel.add(defaultButton);    

    Container contentPane = getContentPane();
    contentPane.add(panel, "Center");
    
    ////////////////////////////////////////////////////////////////////
    // load default major node stuff
    ////////////////////////////////////////////////////////////////////

    loadMajorNode();
   
   
    ////////////////////////////////////////////////////////////////////
    // Add various action listeners
    ////////////////////////////////////////////////////////////////////
    
    okButton.addActionListener(this);
    cancelButton.addActionListener(this);
    defaultButton.addActionListener(this);    

    rootPathField.addFocusListener(this);
    dataPathField.addFocusListener(this);
    binPathField.addFocusListener(this);
    logPathField.addFocusListener(this);
    idlPathField.addFocusListener(this);
    libPathField.addFocusListener(this);
    
    researchDataPathField.addFocusListener(this);
    fwdPathField.addFocusListener(this);	 
    out1dvarPathField.addFocusListener(this);	 
    monitorFileField.addFocusListener(this);	 
    modelNonErrPathField.addFocusListener(this);   

    externalDataPathField.addFocusListener(this); 
    rdrSensor1PathField.addFocusListener(this);  
    rdrSensor2PathField.addFocusListener(this);
    rdrOrbitPathField.addFocusListener(this);
    nwpGdasGridPathField.addFocusListener(this);
    nwpEcmwfGridPathField.addFocusListener(this);
    nwpGfsGridPathField.addFocusListener(this);

    staticDataPathField.addFocusListener(this);        
    instrumentPathField.addFocusListener(this);        
    instrumentSensor1FileField.addFocusListener(this);   
    instrumentSensor2FileField.addFocusListener(this);     
    instrumentSensor1Sensor2FileField.addFocusListener(this);
    topographyFileField.addFocusListener(this);        
    antennaPathField.addFocusListener(this);	       
    antennaSensor1FileField.addFocusListener(this);      
    antennaSensor2FileField.addFocusListener(this);        
    tune1FileField.addFocusListener(this);	       
    tune2FileField.addFocusListener(this);
    nedtNominalFileField.addFocusListener(this);
    modelErrNominalFileField.addFocusListener(this);       
    //covBkgAtmFileField.addFocusListener(this);         
    //covBkgSfcFileField.addFocusListener(this);         
    CRTMcoeffPathField.addFocusListener(this);	         
    siceEmissCatalogFileField.addFocusListener(this);        
    snowEmissCatalogFileField.addFocusListener(this);        

    semiStaticDataPathField.addFocusListener(this);
    biasPathField.addFocusListener(this);	   
    regressPathField.addFocusListener(this);	   

    testbedDataPathField.addFocusListener(this); 
    nedtPathField.addFocusListener(this);      
    nedtSensor1PathField.addFocusListener(this);  
    nedtSensor2PathField.addFocusListener(this);   
    nedtSensor1Sensor2PathField.addFocusListener(this);  
    edrPathField.addFocusListener(this);
    depPathField.addFocusListener(this);
    figsPathField.addFocusListener(this);      
    perfsMonitorPathField.addFocusListener(this);
    logFileField.addFocusListener(this);       

    dynamicDataPathField.addFocusListener(this);
    tdrPathField.addFocusListener(this);       
    tdrSensor1PathField.addFocusListener(this);  
    tdrSensor2PathField.addFocusListener(this);    
    sdrPathField.addFocusListener(this);       
    sdrSensor1PathField.addFocusListener(this);  
    sdrSensor2PathField.addFocusListener(this);    
    fmsdrPathField.addFocusListener(this);     
    choppPathField.addFocusListener(this);     
    nwpAnalysPathField.addFocusListener(this); 
    fwdAnalysPathField.addFocusListener(this); 
    regressRetrPathField.addFocusListener(this);   

    controlDataPathField.addFocusListener(this);       
    rdr2tdrSensor1ControlFileField.addFocusListener(this); 
    rdr2tdrSensor2ControlFileField.addFocusListener(this); 
    mergeNedtControlFileField.addFocusListener(this); 
    tdr2sdrSensor1ControlFileField.addFocusListener(this); 
    tdr2sdrSensor2ControlFileField.addFocusListener(this); 
    fmControlFileField.addFocusListener(this);         
    fmsdr2edrControlFileField.addFocusListener(this); 
    grid2nwpControlFileField.addFocusListener(this);   
    fwdControlFileField.addFocusListener(this);        
    regressControlFileField.addFocusListener(this);    
    choppControlFileField.addFocusListener(this);      
    mergeEdrControlFileField.addFocusListener(this);   
    biasCompuControlFileField.addFocusListener(this);  
    biasVerifControlFileField.addFocusListener(this);  
    regressGenControlFileField.addFocusListener(this);
    modifyNedtControlFileField.addFocusListener(this); 
    psFigsGenControlFileField.addFocusListener(this);  

    inputDataPathField.addFocusListener(this); 
    rdrSensor1ListField.addFocusListener(this);      
    rdrSensor2ListField.addFocusListener(this);        
    tdrSensor1ListField.addFocusListener(this);      
    tdrSensor2ListField.addFocusListener(this);        
    sdrSensor1ListField.addFocusListener(this);      
    sdrSensor2ListField.addFocusListener(this);
    sdrSensor3ListField.addFocusListener(this);      
    sdrSensor4ListField.addFocusListener(this);
    fmsdrListField.addFocusListener(this);
    fmsdr4BiasListField.addFocusListener(this);    
    fmsdr4ChoppListField.addFocusListener(this); 
    fmsdr4NwpListField.addFocusListener(this);
    fmsdr4BiasListField.addFocusListener(this);
    fmsdr4RegressListField.addFocusListener(this); 
    fmsdr4ApplyRegressListField.addFocusListener(this);
    edrListField.addFocusListener(this);
    edr4BiasListField.addFocusListener(this);      
    edr4MergeListField.addFocusListener(this);     
    nedtListField.addFocusListener(this);	       
    nedtSensor1ListField.addFocusListener(this);     
    nedtSensor2ListField.addFocusListener(this);       
    gridSfcNwpAnalysListField.addFocusListener(this); 
    gridAtmNwpAnalysListField.addFocusListener(this); 
    nwpAnalysListField.addFocusListener(this);     
    nwpAnalysRetrListField.addFocusListener(this); 
    nwpAnalys4BiasListField.addFocusListener(this); 
    nwpAnalys4RegressListField.addFocusListener(this); 
    fwdAnalys4BiasListField.addFocusListener(this); 

    rdr2tdrSensor1ApplicationField.addFocusListener(this); 
    rdr2tdrSensor2ApplicationField.addFocusListener(this); 
    mergeNedtApplicationField.addFocusListener(this); 
    tdr2sdrApplicationField.addFocusListener(this);    
    fmApplicationField.addFocusListener(this);         
    choppApplicationField.addFocusListener(this);      
    fmsdr2edrApplicationField.addFocusListener(this); 
    mergeEdrApplicationField.addFocusListener(this);   
    vippApplicationField.addFocusListener(this);   
    nedtMonitorApplicationField.addFocusListener(this); 
    nwpGenAnalysApplicationField.addFocusListener(this); 
    fwdApplicationField.addFocusListener(this);        
    determineBiasApplicationField.addFocusListener(this); 
    regressAlgApplicationField.addFocusListener(this); 
    applyRegressAlgApplicationField.addFocusListener(this);
    
    SymWindow aSymWindow = new SymWindow();
    this.addWindowListener(aSymWindow);

    //pack();
    setLocation(100,50);

  }

  
  /**
     * internal class to close the dialog window
     */
  class SymWindow extends java.awt.event.WindowAdapter
  {
          public void windowClosing(java.awt.event.WindowEvent event)
          {
        	  Object object = event.getSource();
        	  if (object == PathDialog.this)
        		  dispose();
          }
  }


  /**
   * What actions to take when focuc gained
   *
   * @param event	the focus event
   */
  public void focusGained(FocusEvent event)
  {
  }


  /**
   * What actions to take when focus lost
   *
   * @param event	the focus event
   */
  public void focusLost(FocusEvent event)
  {
    if ( event.getSource() == rootPathField ) {
	paths.put("rootPath",rootPathField.getText());
    }
    else if ( event.getSource() == dataPathField ) {
	paths.put("dataPath",dataPathField.getText());
    }
    else if ( event.getSource() == binPathField ) {
	paths.put("binPath",binPathField.getText());
    }
    else if ( event.getSource() == logPathField ) {
	paths.put("logPath",logPathField.getText());
    }
    else if ( event.getSource() == idlPathField ) {
	paths.put("IDL",idlPathField.getText());
    }
    else if ( event.getSource() == libPathField ) {
	paths.put("LD_LIBRARY_PATH",libPathField.getText());
    }
    

    else if ( event.getSource() == researchDataPathField ) {
	paths.put("researchDataPath",researchDataPathField.getText());
    }
    else if ( event.getSource() == fwdPathField ) {
	paths.put("fwdPath",fwdPathField.getText());
    }
    else if ( event.getSource() == out1dvarPathField ) {
	paths.put("out1dvarPath",out1dvarPathField.getText());
    }
    else if ( event.getSource() == monitorFileField ) {
	paths.put("monitorFile",monitorFileField.getText());
    }
    else if ( event.getSource() == modelNonErrPathField ) {
	paths.put("modelNonErrPath",modelNonErrPathField.getText());
    }
    

    else if ( event.getSource() == externalDataPathField ) {
	paths.put("externalDataPath",externalDataPathField.getText());
    }
    else if ( event.getSource() == rdrSensor1PathField ) {
	paths.put("rdrSensor1Path",rdrSensor1PathField.getText());
    }
    else if ( event.getSource() == rdrSensor2PathField ) {
	paths.put("rdrSensor2Path",rdrSensor2PathField.getText());
    }
   
    else if ( event.getSource() == rdrOrbitPathField ) {
	paths.put("rdrOrbitPath",rdrOrbitPathField.getText());
    }
    else if ( event.getSource() == nwpGdasGridPathField ) {
	paths.put("nwpGdasGridPath",nwpGdasGridPathField.getText());
    }
    else if ( event.getSource() == nwpEcmwfGridPathField ) {
	paths.put("nwpEcmwfGridPath",nwpEcmwfGridPathField.getText());
    }
    else if ( event.getSource() == nwpGfsGridPathField ) {
	paths.put("nwpGfsGridPath",nwpGfsGridPathField.getText());
    }

    
    else if ( event.getSource() == staticDataPathField ) {
	paths.put("staticDataPath",staticDataPathField.getText());
    }
    else if ( event.getSource() == instrumentPathField ) {
	paths.put("instrumentPath",instrumentPathField.getText());
    }
    else if ( event.getSource() == instrumentSensor1FileField ) {
	paths.put("instrumentSensor1File",instrumentSensor1FileField.getText());
    }
    else if ( event.getSource() == instrumentSensor2FileField ) {
	paths.put("instrumentSensor2File",instrumentSensor2FileField.getText());
    }
    else if ( event.getSource() == instrumentSensor1Sensor2FileField ) {
	paths.put("instrumentSensor1Sensor2File",instrumentSensor1Sensor2FileField.getText());
    }
    else if ( event.getSource() == topographyFileField ) {
	paths.put("topographyFile",topographyFileField.getText());
    }
    else if ( event.getSource() == antennaPathField ) {
	paths.put("antennaPath",antennaPathField.getText());
    }
    else if ( event.getSource() == antennaSensor1FileField ) {
	paths.put("antennaSensor1File",antennaSensor1FileField.getText());
    }
    else if ( event.getSource() == antennaSensor2FileField ) {
	paths.put("antennaSensor2File",antennaSensor2FileField.getText());
    }
    else if ( event.getSource() == tune1FileField ) {
	paths.put("tune1File",tune1FileField.getText());
    }
    else if ( event.getSource() == tune2FileField ) {
	paths.put("tune2File",tune2FileField.getText());
    }
    else if ( event.getSource() == nedtNominalFileField ) {
	paths.put("nedtNominalFile",nedtNominalFileField.getText());
    }
    else if ( event.getSource() == modelErrNominalFileField ) {
	paths.put("modelErrNominalFile",modelErrNominalFileField.getText());
    }
    //else if ( event.getSource() == covBkgAtmFileField ) {
	//paths.put("covBkgAtmFile",covBkgAtmFileField.getText());
    //}
    //else if ( event.getSource() == covBkgSfcFileField ) {
	//paths.put("covBkgSfcFile",covBkgSfcFileField.getText());
    //}
    else if ( event.getSource() == CRTMcoeffPathField ) {
	paths.put("CRTMcoeffPath",CRTMcoeffPathField.getText());
    }
    else if ( event.getSource() == siceEmissCatalogFileField ) {
	paths.put("siceEmissCatalogFile",siceEmissCatalogFileField.getText());
    }
    else if ( event.getSource() == snowEmissCatalogFileField ) {
	paths.put("snowEmissCatalogFile",snowEmissCatalogFileField.getText());
    }

    else if ( event.getSource() == semiStaticDataPathField ) {
	paths.put("semiStaticDataPath",semiStaticDataPathField.getText());
    }
    else if ( event.getSource() == biasPathField ) {
	paths.put("biasPath",biasPathField.getText());
    }
    else if ( event.getSource() == regressPathField ) {
	paths.put("regressPath",regressPathField.getText());
    }


    else if ( event.getSource() == testbedDataPathField ) {
	paths.put("testbedDataPath",testbedDataPathField.getText());
    }
    else if ( event.getSource() == nedtPathField ) {
	paths.put("nedtPath",nedtPathField.getText());
    }
    else if ( event.getSource() == nedtSensor1PathField ) {
	paths.put("nedtSensor1Path",nedtSensor1PathField.getText());
    }
    else if ( event.getSource() == nedtSensor2PathField ) {
	paths.put("nedtSensor2Path",nedtSensor2PathField.getText());
    }
    else if ( event.getSource() == nedtSensor1Sensor2PathField ) {
	paths.put("nedtSensor1Sensor2Path",nedtSensor1Sensor2PathField.getText());
    }
    else if ( event.getSource() == edrPathField ) {
	paths.put("edrPath",edrPathField.getText());
    }
    else if ( event.getSource() == depPathField ) {
	paths.put("depPath",depPathField.getText());
    }
    else if ( event.getSource() == figsPathField ) {
	paths.put("figsPath",figsPathField.getText());
    }
    else if ( event.getSource() == perfsMonitorPathField ) {
	paths.put("perfsMonitorPath",perfsMonitorPathField.getText());
    }
    else if ( event.getSource() == logFileField ) {
	paths.put("logFile",logFileField.getText());
    }


    else if ( event.getSource() == dynamicDataPathField ) {
	paths.put("dynamicDataPath",dynamicDataPathField.getText());
    }
    else if ( event.getSource() == tdrPathField ) {
	paths.put("tdrPath",tdrPathField.getText());
    }
    else if ( event.getSource() == tdrSensor1PathField ) {
	paths.put("tdrSensor1Path",tdrSensor1PathField.getText());
    }
    else if ( event.getSource() == tdrSensor2PathField ) {
	paths.put("tdrSensor2Path",tdrSensor2PathField.getText());
    }
    else if ( event.getSource() == sdrPathField ) {
	paths.put("sdrPath",sdrPathField.getText());
    }
    else if ( event.getSource() == sdrSensor1PathField ) {
	paths.put("sdrSensor1Path",sdrSensor1PathField.getText());
    }
    else if ( event.getSource() == sdrSensor2PathField ) {
	paths.put("sdrSensor2Path",sdrSensor2PathField.getText());
    }
    else if ( event.getSource() == fmsdrPathField ) {
	paths.put("fmsdrPath",fmsdrPathField.getText());
    }
    else if ( event.getSource() == choppPathField ) {
	paths.put("choppPath",choppPathField.getText());
    }
    else if ( event.getSource() == nwpAnalysPathField ) {
	paths.put("nwpAnalysPath",nwpAnalysPathField.getText());
    }
    else if ( event.getSource() == fwdAnalysPathField ) {
	paths.put("fwdAnalysPath",fwdAnalysPathField.getText());
    }
    else if ( event.getSource() == regressRetrPathField ) {
	paths.put("regressRetrPath",regressRetrPathField.getText());
    }


    else if ( event.getSource() == controlDataPathField ) {
	paths.put("controlDataPath",controlDataPathField.getText());
    }
    else if ( event.getSource() == rdr2tdrSensor1ControlFileField ) {
	paths.put("rdr2tdrSensor1ControlFile",rdr2tdrSensor1ControlFileField.getText());
    }
    else if ( event.getSource() == rdr2tdrSensor2ControlFileField ) {
	paths.put("rdr2tdrSensor2ControlFile",rdr2tdrSensor2ControlFileField.getText());
    }
    else if ( event.getSource() == mergeNedtControlFileField ) {
	paths.put("mergeNedtControlFile",mergeNedtControlFileField.getText());
    }
    else if ( event.getSource() == tdr2sdrSensor1ControlFileField ) {
	paths.put("tdr2sdrSensor1ControlFile",tdr2sdrSensor1ControlFileField.getText());
    }
    else if ( event.getSource() == tdr2sdrSensor2ControlFileField ) {
	paths.put("tdr2sdrSensor2ControlFile",tdr2sdrSensor2ControlFileField.getText());
    }
    else if ( event.getSource() == fmControlFileField ) {
	paths.put("fmControlFile",fmControlFileField.getText());
    }
    else if ( event.getSource() == fmsdr2edrControlFileField ) {
	paths.put("fmsdr2edrControlFile",fmsdr2edrControlFileField.getText());
    }
    else if ( event.getSource() == grid2nwpControlFileField ) {
	paths.put("grid2nwpControlFile",grid2nwpControlFileField.getText());
    }
    else if ( event.getSource() == fwdControlFileField ) {
	paths.put("fwdControlFile",fwdControlFileField.getText());
    }
    else if ( event.getSource() == regressControlFileField ) {
	paths.put("regressControlFile",regressControlFileField.getText());
    }
    else if ( event.getSource() == choppControlFileField ) {
	paths.put("choppControlFile",choppControlFileField.getText());
    }
    else if ( event.getSource() == mergeEdrControlFileField ) {
	paths.put("mergeEdrControlFile",mergeEdrControlFileField.getText());
    }
    else if ( event.getSource() == biasCompuControlFileField ) {
	paths.put("biasCompuControlFile",biasCompuControlFileField.getText());
    }
    else if ( event.getSource() == biasVerifControlFileField ) {
	paths.put("biasVerifControlFile",biasVerifControlFileField.getText());
    }
    else if ( event.getSource() == regressGenControlFileField ) {
	paths.put("regressGenControlFile",regressGenControlFileField.getText());
    }
    else if ( event.getSource() == modifyNedtControlFileField ) {
	paths.put("modifyNedtControlFile",modifyNedtControlFileField.getText());
    }
    else if ( event.getSource() == psFigsGenControlFileField ) {
	paths.put("psFigsGenControlFile",psFigsGenControlFileField.getText());
    }


    else if ( event.getSource() == inputDataPathField ) {
	paths.put("inputDataPath",inputDataPathField.getText());
    }
    else if ( event.getSource() == rdrSensor1ListField ) {
	paths.put("rdrSensor1List",rdrSensor1ListField.getText());
    }
    else if ( event.getSource() == rdrSensor2ListField ) {
	paths.put("rdrSensor2List",rdrSensor2ListField.getText());
    }
    else if ( event.getSource() == tdrSensor1ListField ) {
	paths.put("tdrSensor1List",tdrSensor1ListField.getText());
    }
    else if ( event.getSource() == tdrSensor2ListField ) {
	paths.put("tdrSensor2List",tdrSensor2ListField.getText());
    }
    else if ( event.getSource() == sdrSensor1ListField ) {
	paths.put("sdrSensor1List",sdrSensor1ListField.getText());
    }
    else if ( event.getSource() == sdrSensor2ListField ) {
	paths.put("sdrSensor2List",sdrSensor2ListField.getText());
    }
    else if ( event.getSource() == sdrSensor3ListField ) {
	paths.put("sdrSensor3List",sdrSensor3ListField.getText());
    }
    else if ( event.getSource() == sdrSensor4ListField ) {
	paths.put("sdrSensor4List",sdrSensor4ListField.getText());
    }
    else if ( event.getSource() == fmsdrListField ) {
	paths.put("fmsdrList",fmsdrListField.getText());
    }
    else if ( event.getSource() == fmsdr4BiasListField ) {
	paths.put("fmsdr4BiasList",fmsdr4BiasListField.getText());
    }
    else if ( event.getSource() == fmsdr4ChoppListField ) {
	paths.put("fmsdr4ChoppList",fmsdr4ChoppListField.getText());
    }
    else if ( event.getSource() == fmsdr4NwpListField ) {
	paths.put("fmsdr4NwpList",fmsdr4NwpListField.getText());
    }
    else if ( event.getSource() == fmsdr4RegressListField ) {
	paths.put("fmsdr4RegressList",fmsdr4RegressListField.getText());
    }
    else if ( event.getSource() == fmsdr4ApplyRegressListField ) {
	paths.put("fmsdr4ApplyRegressList",fmsdr4ApplyRegressListField.getText());
    }
    else if ( event.getSource() == edrListField ) {
	paths.put("edrList",edrListField.getText());
    }
    else if ( event.getSource() == edr4BiasListField ) {
	paths.put("edr4BiasList",edr4BiasListField.getText());
    }
    else if ( event.getSource() == edr4MergeListField ) {
	paths.put("edr4MergeList",edr4MergeListField.getText());
    }
    else if ( event.getSource() == nedtListField ) {
	paths.put("nedtList",nedtListField.getText());
    }
    else if ( event.getSource() == nedtSensor1ListField ) {
	paths.put("nedtSensor1List",nedtSensor1ListField.getText());
    }
    else if ( event.getSource() == nedtSensor2ListField ) {
	paths.put("nedtSensor2List",nedtSensor2ListField.getText());
    }
    else if ( event.getSource() == gridSfcNwpAnalysListField ) {
	paths.put("gridSfcNwpAnalysList",gridSfcNwpAnalysListField.getText());
    }
    else if ( event.getSource() == gridAtmNwpAnalysListField ) {
	paths.put("gridAtmNwpAnalysList",gridAtmNwpAnalysListField.getText());
    }
    else if ( event.getSource() == nwpAnalysListField ) {
	paths.put("nwpAnalysList",nwpAnalysListField.getText());
    }
    else if ( event.getSource() == nwpAnalysRetrListField ) {
	paths.put("nwpAnalysRetrList",nwpAnalysRetrListField.getText());
    }
    else if ( event.getSource() == nwpAnalys4BiasListField ) {
	paths.put("nwpAnalys4BiasList",nwpAnalys4BiasListField.getText());
    }
    else if ( event.getSource() == nwpAnalys4RegressListField ) {
	paths.put("nwpAnalys4RegressList",nwpAnalys4RegressListField.getText());
    }
    else if ( event.getSource() == fwdAnalys4BiasListField ) {
	paths.put("fwdAnalys4BiasList",fwdAnalys4BiasListField.getText());
    }


    else if ( event.getSource() == rdr2tdrSensor1ApplicationField ) {
	paths.put("rdr2tdrSensor1Src",rdr2tdrSensor1ApplicationField.getText());
    }
    else if ( event.getSource() == rdr2tdrSensor2ApplicationField ) {
	paths.put("rdr2tdrSensor2Src",rdr2tdrSensor2ApplicationField.getText());
    }
    else if ( event.getSource() == mergeNedtApplicationField ) {
	paths.put("mergeNedtSrc",mergeNedtApplicationField.getText());
    }
    else if ( event.getSource() == tdr2sdrApplicationField ) {
	paths.put("tdr2sdrSrc",tdr2sdrApplicationField.getText());
    }
    else if ( event.getSource() == fmApplicationField ) {
	paths.put("fmSrc",fmApplicationField.getText());
    }
    else if ( event.getSource() == choppApplicationField ) {
	paths.put("choppSrc",choppApplicationField.getText());
    }
    else if ( event.getSource() == fmsdr2edrApplicationField ) {
	paths.put("fmsdr2edrSrc",fmsdr2edrApplicationField.getText());
    }
    else if ( event.getSource() == mergeEdrApplicationField ) {
	paths.put("mergeEdrSrc",mergeEdrApplicationField.getText());
    }
    else if ( event.getSource() == vippApplicationField ) {
	paths.put("vippSrc",vippApplicationField.getText());
    }
    else if ( event.getSource() == nedtMonitorApplicationField ) {
	paths.put("nedtMonitorSrc",nedtMonitorApplicationField.getText());
    }
    else if ( event.getSource() == nwpGenAnalysApplicationField ) {
	paths.put("nwpGenAnalysSrc",nwpGenAnalysApplicationField.getText());
    }
    else if ( event.getSource() == fwdApplicationField ) {
	paths.put("fwdSrc",fwdApplicationField.getText());
    }
    else if ( event.getSource() == determineBiasApplicationField ) {
	paths.put("determineBiasSrc",determineBiasApplicationField.getText());
    }
    else if ( event.getSource() == regressAlgApplicationField ) {
	paths.put("regressAlgSrc",regressAlgApplicationField.getText());
    }
    else if ( event.getSource() == applyRegressAlgApplicationField ) {
	paths.put("applyRegressSrc",applyRegressAlgApplicationField.getText());
    }


  }

  
  /**
   * What actions to take when 3 buttons are clicked
   * OK and Cancel will close the window; while Default will reset back to
   * default values for current selected node(page), other pages won't be
   * affected.
   */
  public void actionPerformed(ActionEvent event) 
  { 

    if ( event.getSource() == okButton ) {
        setVisible(false);
	dispose();
    }
    else if ( event.getSource() == cancelButton ) {
        setVisible(false);
	dispose();
    }
    else if ( event.getSource() == defaultButton ) {
        
	// too much if we reset all nodes values
	//setDefaultValues();
    	
	
	// instead, we only reset current page values
	TreePath path = tree.getSelectionPath();
    	if (path == null) return;
    	DefaultMutableTreeNode selectedNode = (DefaultMutableTreeNode) path.getLastPathComponent();
	
	if ( selectedNode == majorNode ) {
		setDefaultMajor();
	}
	else if ( selectedNode == researchNode ) {
		setDefaultResearch();
	}
	else if ( selectedNode == externalNode ) {
		setDefaultExternal();
	}
	else if ( selectedNode == staticNode ) {
		setDefaultStatic();
	}
	else if ( selectedNode == semiStaticNode ) {
		setDefaultSemiStatic();
	}
	else if ( selectedNode == testbedNode ) {
		setDefaultTestbed();
	}
	else if ( selectedNode == dynamicNode ) {
		setDefaultDynamic();
	}
	else if ( selectedNode == controlFileNode ) {
		setDefaultControlFile();
	}
	else if ( selectedNode == fileListNode ) {
		setDefaultFileList();
	}
	else if ( selectedNode == applicationNode ) {
		setDefaultApplication();
	}
	
    } 

  }

  
  /**
   * What to do when different tree node get selected. Different fields will come up
   * according to left nodes you selected.
   *
   * @param event 	the TreeSelection Event	
   */  
  public void valueChanged(TreeSelectionEvent event) 
  { 
    TreePath path = tree.getSelectionPath();
    if (path == null) return;
    DefaultMutableTreeNode selectedNode = (DefaultMutableTreeNode) path.getLastPathComponent();
    
    if ( selectedNode == majorNode ) {
	
	loadMajorNode();
    }
    else if ( selectedNode == researchNode ) {
	
	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(5,1);
	gridLayout.setVgap(10);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	
	JPanel titlePanel = new JPanel();
	titlePanel.add(new JLabel("Research Data & Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	rightPanel.add(titlePanel);

	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(150));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(150));
	
	
	labelPanel.add(researchDataPathLabel);
	labelPanel.add(fwdPathLabel	    );
	labelPanel.add(out1dvarPathLabel    );
	labelPanel.add(monitorFileLabel     );
	labelPanel.add(modelNonErrPathLabel  );
   
	fieldPanel.add(researchDataPathField);
	fieldPanel.add(fwdPathField	    );
	fieldPanel.add(out1dvarPathField    );
	fieldPanel.add(monitorFileField     );
	fieldPanel.add(modelNonErrPathField  );
   
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == externalNode ) {
	
	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(6,1);
	gridLayout.setVgap(10);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();
	
	titlePanel.add(new JLabel("External Data & Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	rightPanel.add(titlePanel);
	
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
		
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(180));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(180));
	
	labelPanel.add(externalDataPathLabel );
	labelPanel.add(nwpGdasGridPathLabel);
	labelPanel.add(nwpEcmwfGridPathLabel);
	labelPanel.add(nwpGfsGridPathLabel);

	fieldPanel.add(externalDataPathField );
	fieldPanel.add(nwpGdasGridPathField);
	fieldPanel.add(nwpEcmwfGridPathField);
	fieldPanel.add(nwpGfsGridPathField);
	
	if ( processMode != null && processMode.equals("0") ) {
		labelPanel.add(rdrOrbitPathLabel);
		fieldPanel.add(rdrOrbitPathField);
	}	
	else {
		labelPanel.add(rdrSensor1PathLabel);
		if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") )
			labelPanel.add(rdrSensor2PathLabel  );
		fieldPanel.add(rdrSensor1PathField);
		
		if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") )
			fieldPanel.add(rdrSensor2PathField  );
	}

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);
	
    }
    else if ( selectedNode == staticNode ) {
	
	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(18,1);
	gridLayout.setVgap(5);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();
	
	titlePanel.add(new JLabel("Static Data & Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	rightPanel.add(titlePanel);
	
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(565));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(565));
	
	labelPanel.add(staticDataPathLabel	  );
	labelPanel.add(instrumentPathLabel	  );
	labelPanel.add(instrumentSensor1FileLabel   );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(instrumentSensor2FileLabel);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(instrumentSensor1Sensor2FileLabel);
	labelPanel.add(topographyFileLabel	  );
	labelPanel.add(antennaPathLabel 	  );
	labelPanel.add(antennaSensor1FileLabel	  );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(antennaSensor2FileLabel);
	labelPanel.add(tune1FileLabel		  );
	labelPanel.add(tune2FileLabel		  );
	labelPanel.add(nedtNominalFileLabel);
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) labelPanel.add(modelErrNominalFileLabel);
	//labelPanel.add(covBkgAtmFileLabel	  );
	//labelPanel.add(covBkgSfcFileLabel	  );
	labelPanel.add(CRTMcoeffPathLabel	  );
	labelPanel.add(siceEmissCatalogFileLabel  );
	labelPanel.add(snowEmissCatalogFileLabel  );
   
	fieldPanel.add(staticDataPathField	  );
	fieldPanel.add(instrumentPathField	  );
	fieldPanel.add(instrumentSensor1FileField   );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(instrumentSensor2FileField);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(instrumentSensor1Sensor2FileField);
	fieldPanel.add(topographyFileField	  );
	fieldPanel.add(antennaPathField 	  );
	fieldPanel.add(antennaSensor1FileField	  );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(antennaSensor2FileField);
	fieldPanel.add(tune1FileField		  );
	fieldPanel.add(tune2FileField		  );
	fieldPanel.add(nedtNominalFileField);
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) fieldPanel.add(modelErrNominalFileField);
	//fieldPanel.add(covBkgAtmFileField	  );
	//fieldPanel.add(covBkgSfcFileField	  );
	fieldPanel.add(CRTMcoeffPathField	  );
	fieldPanel.add(siceEmissCatalogFileField  );
	fieldPanel.add(snowEmissCatalogFileField  );
   
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == semiStaticNode ) {

	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(3,1);
	gridLayout.setVgap(5);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();
	
	titlePanel.add(new JLabel("Semi-Static Data & Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	rightPanel.add(titlePanel);
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(90));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(90));
	
	labelPanel.add(semiStaticDataPathLabel	);
	labelPanel.add(biasPathLabel	      	);
	labelPanel.add(regressPathLabel       	);
   
	fieldPanel.add(semiStaticDataPathField	);
	fieldPanel.add(biasPathField	      	);
	fieldPanel.add(regressPathField 	);
 
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);
	
    }
    else if ( selectedNode == testbedNode ) {

	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(10,1);
	gridLayout.setVgap(5);

	JPanel titlePanel = new JPanel();
	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	
	titlePanel.add(new JLabel("Testbed Data & Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	rightPanel.add(titlePanel);
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(300));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(300));
	
	labelPanel.add(testbedDataPathLabel );
	labelPanel.add(nedtPathLabel        );
	labelPanel.add(nedtSensor1PathLabel );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(nedtSensor2PathLabel       );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(nedtSensor1Sensor2PathLabel);
	labelPanel.add(edrPathLabel         );
	labelPanel.add(depPathLabel         );
	labelPanel.add(figsPathLabel        );
	labelPanel.add(perfsMonitorPathLabel);
	labelPanel.add(logFileLabel         );
	
	fieldPanel.add(testbedDataPathField );
	fieldPanel.add(nedtPathField	    );
	fieldPanel.add(nedtSensor1PathField );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(nedtSensor2PathField       );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(nedtSensor1Sensor2PathField);
	fieldPanel.add(edrPathField	    );
	fieldPanel.add(depPathField	    );
	fieldPanel.add(figsPathField	    );
	fieldPanel.add(perfsMonitorPathField);
	fieldPanel.add(logFileField	    );

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);
	
    }
    else if ( selectedNode == dynamicNode ) {
	
	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(14,1);
	gridLayout.setVgap(5);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();
	
	titlePanel.add(new JLabel("Dynamic Data & Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	rightPanel.add(titlePanel);
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(420));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(420));
	
	labelPanel.add(dynamicDataPathLabel);
	labelPanel.add(tdrPathLabel	   );
	labelPanel.add(tdrSensor1PathLabel );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(tdrSensor2PathLabel);
	labelPanel.add(sdrPathLabel	   );
	labelPanel.add(sdrSensor1PathLabel );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(sdrSensor2PathLabel);
	labelPanel.add(fmsdrPathLabel	   );
	labelPanel.add(choppPathLabel	   );
	labelPanel.add(nwpAnalysPathLabel  );
	labelPanel.add(fwdAnalysPathLabel  );
	labelPanel.add(regressRetrPathLabel);
   
	fieldPanel.add(dynamicDataPathField);
	fieldPanel.add(tdrPathField	   );
	fieldPanel.add(tdrSensor1PathField );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(tdrSensor2PathField);
	fieldPanel.add(sdrPathField	   );
	fieldPanel.add(sdrSensor1PathField );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(sdrSensor2PathField);
	fieldPanel.add(fmsdrPathField	   );
	fieldPanel.add(choppPathField	   );
	fieldPanel.add(nwpAnalysPathField  );
	fieldPanel.add(fwdAnalysPathField  );
	fieldPanel.add(regressRetrPathField);
  
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == controlFileNode ) {

	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(19,1);
	gridLayout.setVgap(5);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();
	
	titlePanel.add(new JLabel("Control Files"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	rightPanel.add(titlePanel);
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(475));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(475));
	
	labelPanel.add(controlDataPathLabel	    );
	labelPanel.add(rdr2tdrSensor1ControlFileLabel );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(rdr2tdrSensor2ControlFileLabel);
	labelPanel.add(mergeNedtControlFileLabel    );
	labelPanel.add(tdr2sdrSensor1ControlFileLabel );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(tdr2sdrSensor2ControlFileLabel);
	labelPanel.add(fmControlFileLabel	    );
	labelPanel.add(fmsdr2edrControlFileLabel  );
	labelPanel.add(grid2nwpControlFileLabel     );
	labelPanel.add(fwdControlFileLabel	    );
	labelPanel.add(regressControlFileLabel      );
	labelPanel.add(choppControlFileLabel	    );
	labelPanel.add(mergeEdrControlFileLabel     );
	labelPanel.add(biasCompuControlFileLabel    );
	labelPanel.add(biasVerifControlFileLabel    );
	labelPanel.add(regressGenControlFileLabel   );
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")  || satId.equals("trmm") ) labelPanel.add(modifyNedtControlFileLabel);
   
	fieldPanel.add(controlDataPathField	    );
	fieldPanel.add(rdr2tdrSensor1ControlFileField );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(rdr2tdrSensor2ControlFileField);
	fieldPanel.add(mergeNedtControlFileField    );
	fieldPanel.add(tdr2sdrSensor1ControlFileField );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(tdr2sdrSensor2ControlFileField);
	fieldPanel.add(fmControlFileField	    );
	fieldPanel.add(fmsdr2edrControlFileField  );
	fieldPanel.add(grid2nwpControlFileField     );
	fieldPanel.add(fwdControlFileField	    );
	fieldPanel.add(regressControlFileField      );
	fieldPanel.add(choppControlFileField	    );
	fieldPanel.add(mergeEdrControlFileField     );
	fieldPanel.add(biasCompuControlFileField    );
	fieldPanel.add(biasVerifControlFileField    );
	fieldPanel.add(regressGenControlFileField   );
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   ) fieldPanel.add(modifyNedtControlFileField);

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == fileListNode ) {

	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(32,1);
	gridLayout.setVgap(5);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();

	titlePanel.add(new JLabel("Input File List"));
	titlePanel.setBackground(new Color(176,196,222));

	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	
	rightPanel.add(titlePanel);
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(750));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(750));
	
	labelPanel.add(inputDataPathLabel		);
	labelPanel.add(rdrSensor1ListLabel		);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(rdrSensor2ListLabel);
	labelPanel.add(tdrSensor1ListLabel		);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(tdrSensor2ListLabel);
	
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   ) {
		sdrSensor1ListLabel.setText("SDR img List:");
		sdrSensor2ListLabel.setText("SDR env List:");
	}
	if ( satId.equals("trmm") ) {
		sdrSensor1ListLabel.setText("SDR LR List:");
		sdrSensor2ListLabel.setText("SDR HR List:");
	}
	
	labelPanel.add(sdrSensor1ListLabel		);
	labelPanel.add(sdrSensor2ListLabel		);
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   ) labelPanel.add(sdrSensor3ListLabel);
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   ) labelPanel.add(sdrSensor4ListLabel);
	labelPanel.add(fmsdrListLabel			);
	labelPanel.add(fmsdr4BiasListLabel  		);
	labelPanel.add(fmsdr4ChoppListLabel		);
	labelPanel.add(fmsdr4NwpListLabel 		);
	labelPanel.add(fmsdr4BiasListLabel 		);
	labelPanel.add(fmsdr4RegressListLabel		);
	labelPanel.add(fmsdr4ApplyRegressListLabel      );
	labelPanel.add(edrListLabel 			);
	labelPanel.add(edr4BiasListLabel		);
	labelPanel.add(edr4MergeListLabel		);
	labelPanel.add(nedtListLabel			);
	labelPanel.add(nedtSensor1ListLabel		);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) labelPanel.add(nedtSensor2ListLabel);
	labelPanel.add(gridSfcNwpAnalysListLabel	);
	labelPanel.add(gridAtmNwpAnalysListLabel	);
	labelPanel.add(nwpAnalysListLabel		);
	labelPanel.add(nwpAnalysRetrListLabel		);
	labelPanel.add(nwpAnalys4BiasListLabel		);
	labelPanel.add(nwpAnalys4RegressListLabel	);
	labelPanel.add(fwdAnalys4BiasListLabel		);
   
	fieldPanel.add(inputDataPathField 		);
	fieldPanel.add(rdrSensor1ListField	  	);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(rdrSensor2ListField);
	fieldPanel.add(tdrSensor1ListField	  	);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(tdrSensor2ListField);
	fieldPanel.add(sdrSensor1ListField	  	);
	fieldPanel.add(sdrSensor2ListField	  	);
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   ) fieldPanel.add(sdrSensor3ListField);
	if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   ) fieldPanel.add(sdrSensor4ListField);
	fieldPanel.add(fmsdrListField	  		);
	fieldPanel.add(fmsdr4BiasListField    		);
	fieldPanel.add(fmsdr4ChoppListField 		);
	fieldPanel.add(fmsdr4NwpListField   		);
	fieldPanel.add(fmsdr4BiasListField   		);
	fieldPanel.add(fmsdr4RegressListField 	);
	fieldPanel.add(fmsdr4ApplyRegressListField	);
	fieldPanel.add(edrListField 	  		);
	fieldPanel.add(edr4BiasListField	  	);
	fieldPanel.add(edr4MergeListField	  	);
	fieldPanel.add(nedtListField	  		);
	fieldPanel.add(nedtSensor1ListField	  	);
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) fieldPanel.add(nedtSensor2ListField);
	fieldPanel.add(gridSfcNwpAnalysListField 	);
	fieldPanel.add(gridAtmNwpAnalysListField 	);
	fieldPanel.add(nwpAnalysListField	  	);
	fieldPanel.add(nwpAnalysRetrListField 		);
	fieldPanel.add(nwpAnalys4BiasListField 		);
	fieldPanel.add(nwpAnalys4RegressListField 	);
	fieldPanel.add(fwdAnalys4BiasListField 		);

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);
	
    }
    else if ( selectedNode == applicationNode ) {

	rightPanel.removeAll();
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(19,1);
	gridLayout.setVgap(5);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);
	JPanel titlePanel = new JPanel();

	titlePanel.add(new JLabel("Application/Process Paths"));
	titlePanel.setBackground(new Color(176,196,222));

	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	
	rightPanel.add(titlePanel);
	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(475));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(475));
	
	labelPanel.add(rdr2tdrSensor1ApplicationLabel );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		labelPanel.add(rdr2tdrSensor2ApplicationLabel);
	labelPanel.add(mergeNedtApplicationLabel      );
	labelPanel.add(tdr2sdrApplicationLabel        );
	labelPanel.add(fmApplicationLabel	      );
	labelPanel.add(choppApplicationLabel	      );
	labelPanel.add(fmsdr2edrApplicationLabel    );
	labelPanel.add(mergeEdrApplicationLabel       );
	labelPanel.add(vippApplicationLabel           );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		labelPanel.add(nedtMonitorApplicationLabel);
	labelPanel.add(nwpGenAnalysApplicationLabel   );
	labelPanel.add(fwdApplicationLabel	      );
	labelPanel.add(determineBiasApplicationLabel  );
	labelPanel.add(regressAlgApplicationLabel     );
	labelPanel.add(applyRegressAlgApplicationLabel);
   
   
	fieldPanel.add(rdr2tdrSensor1ApplicationField );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		fieldPanel.add(rdr2tdrSensor2ApplicationField);
	fieldPanel.add(mergeNedtApplicationField      );
	fieldPanel.add(tdr2sdrApplicationField        );
	fieldPanel.add(fmApplicationField	      );
	fieldPanel.add(choppApplicationField	      );
	fieldPanel.add(fmsdr2edrApplicationField    );
	fieldPanel.add(mergeEdrApplicationField       );
	fieldPanel.add(vippApplicationField           );
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		fieldPanel.add(nedtMonitorApplicationField);
	fieldPanel.add(nwpGenAnalysApplicationField   );
	fieldPanel.add(fwdApplicationField	      );
	fieldPanel.add(determineBiasApplicationField  );
	fieldPanel.add(regressAlgApplicationField     );
	fieldPanel.add(applyRegressAlgApplicationField);

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else {
    	    Graphics g = rightPanel.getGraphics(); 
    	    rightPanel.removeAll();

	    rightPanel.paintAll(g);
    	    rightPanel.setVisible(true);

    }

  }



  /**
   *
   * Set default vaules and load those default values into paths. Currently, this
   * method is not used since it will make all whole sets values to change back to
   * default values and cost is too high. Instead, we implement a smaller methods
   * for every single tree node. For example, in Major Root Path, if you click
   * "Default", only values in this page will reset back to default values, while
   * other values in "External Data & Path" and other nodes won't get changed.
   *
   */
  public void setDefaultValues() {
  	
	////////////////////////////////////////////////////////////////
	// clear first
	////////////////////////////////////////////////////////////////
	paths.clear();
	
	////////////////////////////////////////////////////////////////
	// load paths key=value pair
	////////////////////////////////////////////////////////////////
	paths.put("satId", "n18");
	paths.put("sensor1", "amsua");
	paths.put("sensor2", "mhs");
	
	// default rootPath
	String rootPath = null;
	try { 
	  String tmpFile="compilerHelp.bash";
	  File aFile = new File(tmpFile);
	  PrintWriter out = new PrintWriter(new FileWriter(tmpFile));
	  out.println(bashPath);
	  out.println("pwd\n");
	  
	  out.close();
	  aFile.setExecutable(true, false);
	  
	  Runtime rt = Runtime.getRuntime();
	  Process proc = rt.exec(tmpFile);
	  
	  InputStream is = proc.getInputStream();
	  InputStreamReader isr = new InputStreamReader(is);
	  BufferedReader br = new BufferedReader(isr);
	
	  String line = br.readLine();
	  if ( line != null && line.endsWith("gui") ) {
	      rootPath = line.replaceAll("/gui", "");
	  }
	  
	  boolean deleted = aFile.delete();
	  
	} catch ( IOException ioe )
	{		  
	  System.out.println("" + ioe);
	}
	
	if ( rootPath != null ) 
	    paths.put("rootPath", rootPath);
	else
	    paths.put("rootPath", "../mirs");
	
	
	paths.put("dataPath", "${rootPath}/data");
	paths.put("binPath", "${rootPath}/bin");
	paths.put("logPath", "${rootPath}/logs");
	
	// search idl and fortran lib path in system environment
	String idlPath = null;
	String libFortranPath = null;
	try { 
	  String tmpFile="compilerHelp.bash";
	  File aFile = new File(tmpFile);
	  PrintWriter out = new PrintWriter(new FileWriter(tmpFile));
	  out.println(bashPath);
	  out.println("which idl\n");
	  out.println("which ifort\n");
	  
	  out.close();
	  aFile.setExecutable(true, false);
	  
	  Runtime rt = Runtime.getRuntime();
	  Process proc = rt.exec(tmpFile);
	  
	  InputStream is = proc.getInputStream();
	  InputStreamReader isr = new InputStreamReader(is);
	  BufferedReader br = new BufferedReader(isr);
	
	  String line = br.readLine();
	  if ( line != null && line.endsWith("idl") ) {
	      idlPath = line;
	  }
	  
	  line = br.readLine();
	  if ( line != null && line.endsWith("ifort") ) {
	      libFortranPath = line.replaceAll("bin/ifort", "lib");
	  }
	  
	  boolean deleted = aFile.delete();
	  
	} catch ( IOException ioe )
	{		  
	  System.out.println("" + ioe);
	}
	
	if ( idlPath != null ) 
	    paths.put("IDL", idlPath);
	else
	    paths.put("IDL", "/usr/local/bin/idl");
	    
	if ( libFortranPath != null )
	    paths.put("LD_LIBRARY_PATH", libFortranPath);
	else
	    paths.put("LD_LIBRARY_PATH", "/usr/local/intel_fc_9.1/lib");
	
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");
	
	//paths.put("rdrSensor1Path", "/net/orbit041L/disk2/pub/RawData/NOAA18");
	//paths.put("rdrSensor2Path", "/net/orbit041L/disk2/pub/RawData/NOAA18");
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	
	//paths.put("rdrOrbitPath", "/net/orbit041L/disk2/pub/RawData/NOAA18");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	
	//paths.put("nwpGdasGridPath", "/net/orbit063l/disk3/pub/GDAS_BE");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/${satId}_${sensor1}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/${satId}_${sensor2}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/${satId}_${sensor1}_${sensor2}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}/${satId}_${sensor2}_antennaPattern.dat");
	
	paths.put("tune1File", "${staticDataPath}/TuningData/${satId}_${sensor1}_${sensor2}/TunParams.in");
	paths.put("tune2File", "${staticDataPath}/TuningData/${satId}_${sensor1}_${sensor2}/TunParams.in");
	
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec/${satId}_${sensor1}_${sensor2}");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors/${satId}_${sensor1}_${sensor2}");
	
	
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	
	
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
	paths.put("choppPath", "${dynamicDataPath}/ecfmsdrchopp/${satId}_${sensor1}_${sensor2}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	
	
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr.in");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr.in");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT.in");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr.in");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr.in");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm.in");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar.in");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD.in");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd.in");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress.in");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp.in");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR.in");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation.in");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification.in");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4regressGen.in");
	paths.put("psFigsGenControlFile", "${controlDataPath}/${satId}_Inputs4psFigsGener.in");
	
	
	
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles.list");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles.list");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles.list");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles.list");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles.list");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles.list");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles.list");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias.list");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_ecfmsdrFiles_4Chopping.list");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_ecfmsdrFiles_4nwp.list");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_ecfmsdrFiles_4Bias.list");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_ecfmsdrFiles_4regress.list");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_ecfmsdrFiles_4ApplyRegress.list");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles.list");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias.list");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging.list");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}.list");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}.list");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}.list");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys.list");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys.list");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles.list");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr.list");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias.list");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress.list");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias.list");
	
	
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr/${satId}_${sensor1}_${sensor2}/${sensor1}");
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr/${satId}_${sensor1}_${sensor2}/${sensor2}");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm/${satId}_${sensor1}_${sensor2}");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");
	
	
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_rdr2tdrSensor2", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_tdr2sdrSensor2", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_regressAlg", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_verifBias", "0");
	paths.put("step_mapsGen", "0");
	paths.put("step_biasMapsGen", "0");
	paths.put("step_nedtMonitor", "0");
	paths.put("step_clean", "0");
	paths.put("processMode", "1");
	paths.put("sensorId", "1");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "1");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "1"); // confuse here, 0 should be no; 1 mean yes
	paths.put("retrErrChara", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "1");
	paths.put("geoLimit", "0");
	paths.put("minLat", "13.");
	paths.put("maxLat", "27.");
	paths.put("minLon", "-94.");
	paths.put("maxLon", "-66.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2006_02_01");
	paths.put("dayUsed4Alg", "2006_02_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("gifDensity", "100");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "1");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("biasComputeMethod", "0");
	paths.put("correctTBMethod", "0");
	paths.put("rhMaxAllowed", "120000");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "0");
	paths.put("fwdMatrix2Use", "0");
        paths.put("plotNWP",  "0");
        paths.put("plotBias", "0");
        paths.put("plotAsym", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "0");
	paths.put("makeClean", "0");
	paths.put("extUseT", "0");
	paths.put("extUseQ", "0");
	paths.put("extUseO3", "0");
	paths.put("extUseCLW", "0");
	paths.put("extUseRR", "0");
	paths.put("extUseSN", "0");
	paths.put("extUseIC", "0");
	paths.put("extUseGR", "0");
	paths.put("extUseEM", "0");
	paths.put("extUseRF", "0");
	paths.put("extUseWN", "0");
	paths.put("extUseTS", "0");
	paths.put("extUseDT", "0");
	paths.put("extUseSP", "0");
	paths.put("retrT", "0");
	paths.put("retrQ", "0");
	paths.put("retrO3", "0");
	paths.put("retrCLW", "0");
	paths.put("retrRR", "0");
	paths.put("retrSN", "0");
	paths.put("retrIC", "0");
	paths.put("retrGR", "0");
	paths.put("retrEM", "0");
	paths.put("retrRF", "0");
	paths.put("retrWN", "0");
	paths.put("retrTS", "0");
	paths.put("retrDT", "0");
	paths.put("retrSP", "0");

	
	////////////////////////////////////////////////////////////////
	// fill in all fields with values in paths
	////////////////////////////////////////////////////////////////
	
	setValues(paths);  
    
  }




  /**
   * set default values for Major root Node
   */
  public void setDefaultMajor() {
     
	// default rootPath
	String rootPath = null;
	try { 
	  String tmpFile="compilerHelp.bash";
	  File aFile = new File(tmpFile);
	  PrintWriter out = new PrintWriter(new FileWriter(tmpFile));
	  out.println(bashPath);
	  out.println("pwd\n");
	  
	  out.close();
	  aFile.setExecutable(true, false);
	  
	  Runtime rt = Runtime.getRuntime();
	  Process proc = rt.exec(tmpFile);
	  
	  InputStream is = proc.getInputStream();
	  InputStreamReader isr = new InputStreamReader(is);
	  BufferedReader br = new BufferedReader(isr);
	
	  String line = br.readLine();
	  if ( line != null && line.endsWith("gui") ) {
	      rootPath = line.replaceAll("/gui", "");
	  }
	  
	  boolean deleted = aFile.delete();
	  
	} catch ( IOException ioe )
	{		  
	  System.out.println("" + ioe);
	}
	
	if ( rootPath != null ) 
	    paths.put("rootPath", rootPath);
	else
	    paths.put("rootPath", "../mirs");
	
	
	paths.put("dataPath", "${rootPath}/data");
	paths.put("binPath", "${rootPath}/bin");
	paths.put("logPath", "${rootPath}/logs");
	
	// search idl and fortran lib path in system environment
	String idlPath = null;
	String libFortranPath = null;
	try { 
	  String tmpFile="compilerHelp.bash";
	  File aFile = new File(tmpFile);
	  PrintWriter out = new PrintWriter(new FileWriter(tmpFile));
	  out.println(bashPath);
	  out.println("which idl\n");
	  out.println("which ifort\n");
	  
	  out.close();
	  aFile.setExecutable(true, false);
	  
	  Runtime rt = Runtime.getRuntime();
	  Process proc = rt.exec(tmpFile);
	  
	  InputStream is = proc.getInputStream();
	  InputStreamReader isr = new InputStreamReader(is);
	  BufferedReader br = new BufferedReader(isr);
	
	  String line = br.readLine();
	  if ( line != null && line.endsWith("idl") ) {
	      idlPath = line;
	  }
	  
	  line = br.readLine();
	  if ( line != null && line.endsWith("ifort") ) {
	      libFortranPath = line.replaceAll("bin/ifort", "lib");
	  }
	  
	  boolean deleted = aFile.delete();
	  
	} catch ( IOException ioe )
	{		  
	  System.out.println("" + ioe);
	}
	
	if ( idlPath != null ) 
	    paths.put("IDL", idlPath);
	else
	    paths.put("IDL", "/usr/local/bin/idl");
	    
	if ( libFortranPath != null )
	    paths.put("LD_LIBRARY_PATH", libFortranPath);
	else
	    paths.put("LD_LIBRARY_PATH", "/usr/local/intel_fc_9.1/lib");
	    
	    
	rootPathField.setText(paths.get(rootPathKey));
	dataPathField.setText(paths.get(dataPathKey));
	binPathField.setText(paths.get(binPathKey));
	logPathField.setText(paths.get(logPathKey));
	idlPathField.setText(paths.get(idlPathKey));
	libPathField.setText(paths.get(libPathKey));
	    
  }   
 
 
  /**
   * set default values for Research Node
   */
  public void setDefaultResearch() {
  
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
 
	researchDataPathField.setText(paths.get(researchDataPathKey));
	fwdPathField.setText(paths.get(fwdPathKey));	     
	out1dvarPathField.setText(paths.get(out1dvarPathKey));    
	monitorFileField.setText(paths.get(monitorFileKey));     
	modelNonErrPathField.setText(paths.get(modelNonErrPathKey)); 
  }
  
  
  /**
   * set default values for External Node
   */
  public void setDefaultExternal() {
  
	paths.put("externalDataPath", "${dataPath}/ExternalData");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")   )
		paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}");
	
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
  
	externalDataPathField.setText(paths.get(externalDataPathKey)); 
	rdrSensor1PathField.setText(paths.get(rdrSensor1PathKey));  
	rdrSensor2PathField.setText(paths.get(rdrSensor2PathKey));
	rdrOrbitPathField.setText(paths.get(rdrOrbitPathKey));    
	nwpGdasGridPathField.setText(paths.get(nwpGdasGridPathKey));
	nwpEcmwfGridPathField.setText(paths.get(nwpEcmwfGridPathKey));
	nwpGfsGridPathField.setText(paths.get(nwpGfsGridPathKey));

  }   
 
 
  /**
   * set default values for Static Node
   */
  public void setDefaultStatic() {
  
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/${satId}_${sensor1}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/${satId}_${sensor2}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/${satId}_${sensor1}_${sensor2}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}/${satId}_${sensor2}_antennaPattern.dat");
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}.in");
		paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}_2.in");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) {
		paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in");
		paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in");
	}
	
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat");
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
		paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
		paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) {
		paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
		paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
		paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	}
	paths.put("cldOptPropFile", "${staticDataPath}/CRTMFiles/mw_cloud_opt.dat");

	staticDataPathField.setText(paths.get(staticDataPathKey));	   
	instrumentPathField.setText(paths.get(instrumentPathKey));	   
	instrumentSensor1FileField.setText(paths.get(instrumentSensor1FileKey));   
	instrumentSensor2FileField.setText(paths.get(instrumentSensor2FileKey));     
	instrumentSensor1Sensor2FileField.setText(paths.get(instrumentSensor1Sensor2FileKey));
	topographyFileField.setText(paths.get(topographyFileKey));	   
	antennaPathField.setText(paths.get(antennaPathKey));	   
	antennaSensor1FileField.setText(paths.get(antennaSensor1FileKey));	   
	antennaSensor2FileField.setText(paths.get(antennaSensor2FileKey));	   
	tune1FileField.setText(paths.get(tune1FileKey));  	   
	tune2FileField.setText(paths.get(tune2FileKey));  	   
	nedtNominalFileField.setText(paths.get(nedtNominalFileKey));  	 
	modelErrNominalFileField.setText(paths.get(modelErrNominalFileKey));  	 
	//covBkgAtmFileField.setText(paths.get(covBkgAtmFileKey));	   
	//covBkgSfcFileField.setText(paths.get(covBkgSfcFileKey));	   
	CRTMcoeffPathField.setText(paths.get(CRTMcoeffPathKey));	   
	siceEmissCatalogFileField.setText(paths.get(siceEmissCatalogFileKey));	   
 	snowEmissCatalogFileField.setText(paths.get(snowEmissCatalogFileKey));	   
 
  }   
 
 
  /**
   * set default values for Semi-Static Node
   */
  public void setDefaultSemiStatic() {
  
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("biasPath", "${semiStaticDataPath}/biasCorrec/${satId}_${sensor1}_${sensor2}");
		paths.put("regressPath", "${semiStaticDataPath}/regressAlgors/${satId}_${sensor1}_${sensor2}");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) {
		paths.put("biasPath", "${semiStaticDataPath}/biasCorrec/${satId}_${sensor1}");
		paths.put("regressPath", "${semiStaticDataPath}/regressAlgors/${satId}_${sensor1}");
	}
	
	semiStaticDataPathField.setText(paths.get(semiStaticDataPathKey));
	biasPathField.setText(paths.get(biasPathKey));	       
	regressPathField.setText(paths.get(regressPathKey));       
  }


  /**
   * set default values for Testbed Node
   */
  public void setDefaultTestbed() {

	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
		paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}_${sensor2}");
		paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
		paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm")  ) {
		paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
		paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
		paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
		paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	}
	paths.put("logFile", "${logPath}/${satId}_logFile");
  
	testbedDataPathField.setText(paths.get(testbedDataPathKey)); 
	nedtPathField.setText(paths.get(nedtPathKey));	   
	nedtSensor1PathField.setText(paths.get(nedtSensor1PathKey));  
	nedtSensor2PathField.setText(paths.get(nedtSensor2PathKey));   
	nedtSensor1Sensor2PathField.setText(paths.get(nedtSensor1Sensor2PathKey));
	edrPathField.setText(paths.get(edrPathKey));
	depPathField.setText(paths.get(depPathKey));
	figsPathField.setText(paths.get(figsPathKey));	   
	perfsMonitorPathField.setText(paths.get(perfsMonitorPathKey));
	logFileField.setText(paths.get(logFileKey));	      
  }
  
  

  /**
   * set default values for Dynamic Node
   */
  public void setDefaultDynamic() {
  
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
		paths.put("choppPath", "${dynamicDataPath}/ecfmsdrchopp/${satId}_${sensor1}_${sensor2}");
		paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
		paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
		paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) {
		paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
		paths.put("choppPath", "${dynamicDataPath}/ecfmsdrchopp/${satId}_${sensor1}");
		paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
		paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
		paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	}
	
	dynamicDataPathField.setText(paths.get(dynamicDataPathKey));
	tdrPathField.setText(paths.get(tdrPathKey));
	tdrSensor1PathField.setText(paths.get(tdrSensor1PathKey));       
	tdrSensor2PathField.setText(paths.get(tdrSensor2PathKey));   	      
	sdrPathField.setText(paths.get(sdrPathKey));	        	      
	sdrSensor1PathField.setText(paths.get(sdrSensor1PathKey));       
	sdrSensor2PathField.setText(paths.get(sdrSensor2PathKey));    	     
	fmsdrPathField.setText(paths.get(fmsdrPathKey));     	     
	choppPathField.setText(paths.get(choppPathKey));     	     
	nwpAnalysPathField.setText(paths.get(nwpAnalysPathKey));       
	fwdAnalysPathField.setText(paths.get(fwdAnalysPathKey));       
	regressRetrPathField.setText(paths.get(regressRetrPathKey));     
  }
  
  

  /**
   * set default values for Control Node
   */
  public void setDefaultControlFile() {
  
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr.in");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr.in");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT.in");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr.in");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr.in");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm.in");
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") )
		paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm.in");
	
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar.in");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD.in");
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") )
		paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD.in");
	
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd.in");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress.in");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp.in");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR.in");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation.in");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification.in");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4regressGen.in");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt.in");
	paths.put("psFigsGenControlFile", "${controlDataPath}/${satId}_Inputs4psFigsGener.in");

	controlDataPathField.setText(paths.get(controlDataPathKey));	   
	rdr2tdrSensor1ControlFileField.setText(paths.get(rdr2tdrSensor1ControlFileKey)); 
	rdr2tdrSensor2ControlFileField.setText(paths.get(rdr2tdrSensor2ControlFileKey)); 
	mergeNedtControlFileField.setText(paths.get(mergeNedtControlFileKey));  
	tdr2sdrSensor1ControlFileField.setText(paths.get(tdr2sdrSensor1ControlFileKey)); 
	tdr2sdrSensor2ControlFileField.setText(paths.get(tdr2sdrSensor2ControlFileKey)); 
	fmControlFileField.setText(paths.get(fmControlFileKey));	   
	fmsdr2edrControlFileField.setText(paths.get(fmsdr2edrControlFileKey)); 
	grid2nwpControlFileField.setText(paths.get(grid2nwpControlFileKey));   
	fwdControlFileField.setText(paths.get(fwdControlFileKey));	   
	regressControlFileField.setText(paths.get(regressControlFileKey));    
	choppControlFileField.setText(paths.get(choppControlFileKey));	   
	mergeEdrControlFileField.setText(paths.get(mergeEdrControlFileKey));   
	biasCompuControlFileField.setText(paths.get(biasCompuControlFileKey));  
	biasVerifControlFileField.setText(paths.get(biasVerifControlFileKey));  
	regressGenControlFileField.setText(paths.get(regressGenControlFileKey));
	modifyNedtControlFileField.setText(paths.get(modifyNedtControlFileKey));
	psFigsGenControlFileField.setText(paths.get(psFigsGenControlFileKey));  

  }
  
  
  /**
   * set default values for Control Node
   */
  public void setDefaultFileList() {
  
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles.list");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles.list");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles.list");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles.list");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles.list");
		paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles.list");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) {
		paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_img.list");
		paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles_evn.list");
	}
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_las.list");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_uas.list");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles.list");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias.list");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_ecfmsdrFiles_4Chopping.list");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_ecfmsdrFiles_4nwp.list");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_ecfmsdrFiles_4Bias.list");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_ecfmsdrFiles_4regress.list");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_ecfmsdrFiles_4ApplyRegress.list");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles.list");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias.list");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging.list");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") )
		paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}.list");
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") )
		paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}.list");
	
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}.list");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}.list");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys.list");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys.list");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles.list");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr.list");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias.list");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress.list");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias.list");

	inputDataPathField.setText(paths.get(inputDataPathKey));	   
	rdrSensor1ListField.setText(paths.get(rdrSensor1ListKey));	   
	rdrSensor2ListField.setText(paths.get(rdrSensor2ListKey)); 	   
	tdrSensor1ListField.setText(paths.get(tdrSensor1ListKey));	   
	tdrSensor2ListField.setText(paths.get(tdrSensor2ListKey)); 	   
	sdrSensor1ListField.setText(paths.get(sdrSensor1ListKey));	   
	sdrSensor2ListField.setText(paths.get(sdrSensor2ListKey));
	sdrSensor3ListField.setText(paths.get(sdrSensor3ListKey));	   
	sdrSensor4ListField.setText(paths.get(sdrSensor4ListKey));
	fmsdrListField.setText(paths.get(fmsdrListKey));
	fmsdr4BiasListField.setText(paths.get(fmsdr4BiasListKey));	   
	fmsdr4ChoppListField.setText(paths.get(fmsdr4ChoppListKey));     
	fmsdr4NwpListField.setText(paths.get(fmsdr4NwpListKey));	   
	fmsdr4BiasListField.setText(paths.get(fmsdr4BiasListKey));
	fmsdr4RegressListField.setText(paths.get(fmsdr4RegressListKey));   
	fmsdr4ApplyRegressListField.setText(paths.get(fmsdr4ApplyRegressListKey));
	edrListField.setText(paths.get(edrListKey));
	edr4BiasListField.setText(paths.get(edr4BiasListKey));	   
	edr4MergeListField.setText(paths.get(edr4MergeListKey));	   
	nedtListField.setText(paths.get(nedtListKey));		   
	nedtSensor1ListField.setText(paths.get(nedtSensor1ListKey));	   
	nedtSensor2ListField.setText(paths.get(nedtSensor2ListKey));	   
	gridSfcNwpAnalysListField.setText(paths.get(gridSfcNwpAnalysListKey));  
	gridAtmNwpAnalysListField.setText(paths.get(gridAtmNwpAnalysListKey));  
	nwpAnalysListField.setText(paths.get(nwpAnalysListKey));	   
	nwpAnalysRetrListField.setText(paths.get(nwpAnalysRetrListKey));     
	nwpAnalys4BiasListField.setText(paths.get(nwpAnalys4BiasListKey));    

  }
  
  
  
  /**
   * set default values for Application Node
   */
  public void setDefaultApplication() {
  	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) {
		paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr/${satId}_${sensor1}_${sensor2}/${sensor1}");
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") ) {
		paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr/${satId}_${sensor1}");
	}
	
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr/${satId}_${sensor1}_${sensor2}/${sensor2}");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	
	if ( satId.equals("n18") || satId.equals("metopA") || satId.equals("n19") || satId.equals("metopB") ) 
		paths.put("fmSrc", "${rootPath}/src/testbed/fm/${satId}_${sensor1}_${sensor2}");
	else  if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") || satId.equals("trmm") )
		paths.put("fmSrc", "${rootPath}/src/testbed/fm/${satId}_${sensor1}");
	
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");
  
	rdr2tdrSensor1ApplicationField.setText(paths.get(rdr2tdrSensor1ApplicationKey)); 
	rdr2tdrSensor2ApplicationField.setText(paths.get(rdr2tdrSensor2ApplicationKey)); 
	mergeNedtApplicationField.setText(paths.get(mergeNedtApplicationKey));  
	tdr2sdrApplicationField.setText(paths.get(tdr2sdrApplicationKey));    
	fmApplicationField.setText(paths.get(fmApplicationKey));	   
	choppApplicationField.setText(paths.get(choppApplicationKey));	   
	fmsdr2edrApplicationField.setText(paths.get(fmsdr2edrApplicationKey)); 
	mergeEdrApplicationField.setText(paths.get(mergeEdrApplicationKey));   
	vippApplicationField.setText(paths.get(vippApplicationKey));   
	nedtMonitorApplicationField.setText(paths.get(nedtMonitorApplicationKey)); 
	nwpGenAnalysApplicationField.setText(paths.get(nwpGenAnalysApplicationKey)); 
	fwdApplicationField.setText(paths.get(fwdApplicationKey));	   
	determineBiasApplicationField.setText(paths.get(determineBiasApplicationKey)); 
	regressAlgApplicationField.setText(paths.get(regressAlgApplicationKey)); 
	applyRegressAlgApplicationField.setText(paths.get(applyRegressAlgApplicationKey));
  
  } 



  /**
   *
   * Set  vaules according to key=value pairs contained in inputPaths argument
   *
   */
  public void setValues( Map<String,String> inputPaths ) {
	  
	rootPathField.setText(inputPaths.get(rootPathKey));
	dataPathField.setText(inputPaths.get(dataPathKey));
	binPathField.setText(inputPaths.get(binPathKey));
	logPathField.setText(inputPaths.get(logPathKey));
	idlPathField.setText(inputPaths.get(idlPathKey));
	libPathField.setText(inputPaths.get(libPathKey));

	researchDataPathField.setText(inputPaths.get(researchDataPathKey));
	fwdPathField.setText(inputPaths.get(fwdPathKey));
	out1dvarPathField.setText(inputPaths.get(out1dvarPathKey));
	monitorFileField.setText(inputPaths.get(monitorFileKey));
	modelNonErrPathField.setText(inputPaths.get(modelNonErrPathKey));

	externalDataPathField.setText(inputPaths.get(externalDataPathKey));
	rdrSensor1PathField.setText(inputPaths.get(rdrSensor1PathKey));
	rdrSensor2PathField.setText(inputPaths.get(rdrSensor2PathKey));
	rdrF16PathField.setText(inputPaths.get(rdrF16PathKey));
	rdrOrbitPathField.setText(inputPaths.get(rdrOrbitPathKey));
	nwpGdasGridPathField.setText(inputPaths.get(nwpGdasGridPathKey));
	nwpEcmwfGridPathField.setText(inputPaths.get(nwpEcmwfGridPathKey));
	nwpGfsGridPathField.setText(inputPaths.get(nwpGfsGridPathKey));

	staticDataPathField.setText(inputPaths.get(staticDataPathKey));
	instrumentPathField.setText(inputPaths.get(instrumentPathKey));
	instrumentSensor1FileField.setText(inputPaths.get(instrumentSensor1FileKey));
	instrumentSensor2FileField.setText(inputPaths.get(instrumentSensor2FileKey));
	instrumentSensor1Sensor2FileField.setText(inputPaths.get(instrumentSensor1Sensor2FileKey));
	topographyFileField.setText(inputPaths.get(topographyFileKey));
	antennaPathField.setText(inputPaths.get(antennaPathKey));
	antennaSensor1FileField.setText(inputPaths.get(antennaSensor1FileKey));
	antennaSensor2FileField.setText(inputPaths.get(antennaSensor2FileKey));
	tune1FileField.setText(inputPaths.get(tune1FileKey));
	tune2FileField.setText(inputPaths.get(tune2FileKey));
	nedtNominalFileField.setText(inputPaths.get(nedtNominalFileKey));
	modelErrNominalFileField.setText(inputPaths.get(modelErrNominalFileKey));
	//covBkgAtmFileField.setText(inputPaths.get(covBkgAtmFileKey));
	//covBkgSfcFileField.setText(inputPaths.get(covBkgSfcFileKey));
	CRTMcoeffPathField.setText(inputPaths.get(CRTMcoeffPathKey));
	siceEmissCatalogFileField.setText(inputPaths.get(siceEmissCatalogFileKey));
	snowEmissCatalogFileField.setText(inputPaths.get(snowEmissCatalogFileKey));

	semiStaticDataPathField.setText(inputPaths.get(semiStaticDataPathKey));
	biasPathField.setText(inputPaths.get(biasPathKey));	       
	regressPathField.setText(inputPaths.get(regressPathKey));       

	testbedDataPathField.setText(inputPaths.get(testbedDataPathKey)); 
	nedtPathField.setText(inputPaths.get(nedtPathKey));	   
	nedtSensor1PathField.setText(inputPaths.get(nedtSensor1PathKey));  
	nedtSensor2PathField.setText(inputPaths.get(nedtSensor2PathKey));   
	nedtSensor1Sensor2PathField.setText(inputPaths.get(nedtSensor1Sensor2PathKey));
	edrPathField.setText(inputPaths.get(edrPathKey));
	depPathField.setText(inputPaths.get(depPathKey));
	figsPathField.setText(inputPaths.get(figsPathKey));	   
	perfsMonitorPathField.setText(inputPaths.get(perfsMonitorPathKey));
	logFileField.setText(inputPaths.get(logFileKey));	      

	dynamicDataPathField.setText(inputPaths.get(dynamicDataPathKey));
	tdrPathField.setText(inputPaths.get(tdrPathKey));
	tdrSensor1PathField.setText(inputPaths.get(tdrSensor1PathKey));       
	tdrSensor2PathField.setText(inputPaths.get(tdrSensor2PathKey));   	      
	sdrPathField.setText(inputPaths.get(sdrPathKey));	        	      
	sdrSensor1PathField.setText(inputPaths.get(sdrSensor1PathKey));       
	sdrSensor2PathField.setText(inputPaths.get(sdrSensor2PathKey));    	     
	fmsdrPathField.setText(inputPaths.get(fmsdrPathKey));     	     
	choppPathField.setText(inputPaths.get(choppPathKey));     	     
	nwpAnalysPathField.setText(inputPaths.get(nwpAnalysPathKey));       
	fwdAnalysPathField.setText(inputPaths.get(fwdAnalysPathKey));       
	regressRetrPathField.setText(inputPaths.get(regressRetrPathKey));     

	controlDataPathField.setText(inputPaths.get(controlDataPathKey));	   
	rdr2tdrSensor1ControlFileField.setText(inputPaths.get(rdr2tdrSensor1ControlFileKey)); 
	rdr2tdrSensor2ControlFileField.setText(inputPaths.get(rdr2tdrSensor2ControlFileKey)); 
	mergeNedtControlFileField.setText(inputPaths.get(mergeNedtControlFileKey));  
	tdr2sdrSensor1ControlFileField.setText(inputPaths.get(tdr2sdrSensor1ControlFileKey)); 
	tdr2sdrSensor2ControlFileField.setText(inputPaths.get(tdr2sdrSensor2ControlFileKey)); 
	fmControlFileField.setText(inputPaths.get(fmControlFileKey));	   
	fmsdr2edrControlFileField.setText(inputPaths.get(fmsdr2edrControlFileKey)); 
	grid2nwpControlFileField.setText(inputPaths.get(grid2nwpControlFileKey));   
	fwdControlFileField.setText(inputPaths.get(fwdControlFileKey));	   
	regressControlFileField.setText(inputPaths.get(regressControlFileKey));    
	choppControlFileField.setText(inputPaths.get(choppControlFileKey));	   
	mergeEdrControlFileField.setText(inputPaths.get(mergeEdrControlFileKey));   
	biasCompuControlFileField.setText(inputPaths.get(biasCompuControlFileKey));  
	biasVerifControlFileField.setText(inputPaths.get(biasVerifControlFileKey));  
	regressGenControlFileField.setText(inputPaths.get(regressGenControlFileKey));
	modifyNedtControlFileField.setText(inputPaths.get(modifyNedtControlFileKey));
	psFigsGenControlFileField.setText(inputPaths.get(psFigsGenControlFileKey));  

	inputDataPathField.setText(inputPaths.get(inputDataPathKey));	   
	rdrSensor1ListField.setText(inputPaths.get(rdrSensor1ListKey));	   
	rdrSensor2ListField.setText(inputPaths.get(rdrSensor2ListKey)); 	   
	tdrSensor1ListField.setText(inputPaths.get(tdrSensor1ListKey));	   
	tdrSensor2ListField.setText(inputPaths.get(tdrSensor2ListKey)); 	   
	sdrSensor1ListField.setText(inputPaths.get(sdrSensor1ListKey));	   
	sdrSensor2ListField.setText(inputPaths.get(sdrSensor2ListKey));
	sdrSensor3ListField.setText(inputPaths.get(sdrSensor3ListKey));
	sdrSensor4ListField.setText(inputPaths.get(sdrSensor4ListKey));
	fmsdrListField.setText(inputPaths.get(fmsdrListKey));
	fmsdr4BiasListField.setText(inputPaths.get(fmsdr4BiasListKey));
	fmsdr4ChoppListField.setText(inputPaths.get(fmsdr4ChoppListKey));
	fmsdr4NwpListField.setText(inputPaths.get(fmsdr4NwpListKey));
	fmsdr4BiasListField.setText(inputPaths.get(fmsdr4BiasListKey));
	fmsdr4RegressListField.setText(inputPaths.get(fmsdr4RegressListKey));
	fmsdr4ApplyRegressListField.setText(inputPaths.get(fmsdr4ApplyRegressListKey));
	edrListField.setText(inputPaths.get(edrListKey));
	edr4BiasListField.setText(inputPaths.get(edr4BiasListKey));
	edr4MergeListField.setText(inputPaths.get(edr4MergeListKey));
	nedtListField.setText(inputPaths.get(nedtListKey));
	nedtSensor1ListField.setText(inputPaths.get(nedtSensor1ListKey));
	nedtSensor2ListField.setText(inputPaths.get(nedtSensor2ListKey));
	gridSfcNwpAnalysListField.setText(inputPaths.get(gridSfcNwpAnalysListKey));
	gridAtmNwpAnalysListField.setText(inputPaths.get(gridAtmNwpAnalysListKey));
	nwpAnalysListField.setText(inputPaths.get(nwpAnalysListKey));
	nwpAnalysRetrListField.setText(inputPaths.get(nwpAnalysRetrListKey));
	nwpAnalys4BiasListField.setText(inputPaths.get(nwpAnalys4BiasListKey));
	nwpAnalys4RegressListField.setText(inputPaths.get(nwpAnalys4RegressListKey));
	fwdAnalys4BiasListField.setText(inputPaths.get(fwdAnalys4BiasListKey));

	rdr2tdrSensor1ApplicationField.setText(inputPaths.get(rdr2tdrSensor1ApplicationKey)); 
	rdr2tdrSensor2ApplicationField.setText(inputPaths.get(rdr2tdrSensor2ApplicationKey)); 
	mergeNedtApplicationField.setText(inputPaths.get(mergeNedtApplicationKey));  
	tdr2sdrApplicationField.setText(inputPaths.get(tdr2sdrApplicationKey));    
	fmApplicationField.setText(inputPaths.get(fmApplicationKey));	   
	choppApplicationField.setText(inputPaths.get(choppApplicationKey));	   
	fmsdr2edrApplicationField.setText(inputPaths.get(fmsdr2edrApplicationKey)); 
	mergeEdrApplicationField.setText(inputPaths.get(mergeEdrApplicationKey));   
	vippApplicationField.setText(inputPaths.get(vippApplicationKey));   
	nedtMonitorApplicationField.setText(inputPaths.get(nedtMonitorApplicationKey)); 
	nwpGenAnalysApplicationField.setText(inputPaths.get(nwpGenAnalysApplicationKey)); 
	fwdApplicationField.setText(inputPaths.get(fwdApplicationKey));	   
	determineBiasApplicationField.setText(inputPaths.get(determineBiasApplicationKey)); 
	regressAlgApplicationField.setText(inputPaths.get(regressAlgApplicationKey)); 
	applyRegressAlgApplicationField.setText(inputPaths.get(applyRegressAlgApplicationKey));
	
	satId = inputPaths.get(satIdKey);
	processMode = inputPaths.get(processModeKey);
	
  }
 
  /**
   * load major root stuffs
   */
  private void loadMajorNode() {
    
	int componentCount = rightPanel.getComponentCount();
	if( componentCount >= 1 ) rightPanel.removeAll();
	
	Graphics g = rightPanel.getGraphics(); 
	
	SpringLayout rightLayout = new SpringLayout();
    	rightPanel.setLayout(rightLayout);
	
	GridLayout gridLayout = new GridLayout(6,1);
	gridLayout.setVgap(10);

	JPanel labelPanel = new JPanel(gridLayout);
	JPanel fieldPanel = new JPanel(gridLayout);

	JPanel titlePanel = new JPanel();
	titlePanel.add(new JLabel("Major Root Path"));
	titlePanel.setBackground(new Color(176,196,222));
	titlePanel.setForeground(Color.white);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	rightPanel.add(titlePanel);

	rightPanel.add(labelPanel);
	rightPanel.add(fieldPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(800));
        titleCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints labelCons = rightLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(25));
        labelCons.setY(Spring.constant(75));
        labelCons.setWidth(Spring.constant(200));
        labelCons.setHeight(Spring.constant(180));

	SpringLayout.Constraints fieldCons = rightLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(230));
        fieldCons.setY(Spring.constant(75));
        fieldCons.setWidth(Spring.constant(550));
        fieldCons.setHeight(Spring.constant(180));

	labelPanel.add(rootPathLabel);
	labelPanel.add(dataPathLabel);
	labelPanel.add(binPathLabel );
	labelPanel.add(logPathLabel );
	labelPanel.add(idlPathLabel );
	labelPanel.add(libPathLabel );

	fieldPanel.add(rootPathField); 
	fieldPanel.add(dataPathField);
	fieldPanel.add(binPathField );
	fieldPanel.add(logPathField );
	fieldPanel.add(idlPathField );
	fieldPanel.add(libPathField );
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

  }
  
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  //
  // Return values Section
  //
  //////////////////////////////////////////////////////////////////////////////////////////////////

  /**
   * only returned key=value pairs that you touched, type in, clicked, namely, those get focus
   */
  public Map<String, String> getPaths() { return paths; }
  

  //////////////////////////////////////////////////////////////////////////////////////////////////
  //
  // Member Definition Section Start Here:
  //
  //////////////////////////////////////////////////////////////////////////////////////////////////
 

  private DefaultMutableTreeNode root;
  
  private DefaultMutableTreeNode majorNode;
  private DefaultMutableTreeNode researchNode;
  private DefaultMutableTreeNode externalNode;
  private DefaultMutableTreeNode staticNode;
  private DefaultMutableTreeNode semiStaticNode;
  private DefaultMutableTreeNode testbedNode;
  private DefaultMutableTreeNode dynamicNode;
  private DefaultMutableTreeNode controlFileNode;
  private DefaultMutableTreeNode fileListNode;
  private DefaultMutableTreeNode applicationNode;
  //private DefaultMutableTreeNode imageNode;
  

  
  //////////////////////////////////////////////////////////////
  // Major Root Paths Identifiers
  //////////////////////////////////////////////////////////////
  		     
  private JLabel rootPathLabel = new JLabel("Root Path:");
  private JLabel dataPathLabel = new JLabel("Data Path:");
  private JLabel binPathLabel  = new JLabel("Bin Path:");
  private JLabel logPathLabel  = new JLabel("Log Path:");
  private JLabel idlPathLabel  = new JLabel("IDL Path:");
  private JLabel libPathLabel  = new JLabel("Fortran Lib Path:");

  private JTextField rootPathField = new JTextField(32);
  private JTextField dataPathField = new JTextField(32);
  private JTextField binPathField  = new JTextField(32);
  private JTextField logPathField  = new JTextField(32);
  private JTextField idlPathField  = new JTextField(32);
  private JTextField libPathField  = new JTextField(32);

 
  //////////////////////////////////////////////////////////////
  // Research Data Paths Identifiers
  //////////////////////////////////////////////////////////////
  		     
  private JLabel researchDataPathLabel 	= new JLabel("Research Data Path:");
  private JLabel fwdPathLabel		= new JLabel("Forward Path:");
  private JLabel out1dvarPathLabel  	= new JLabel("1dvat Output Path:");
  private JLabel monitorFileLabel  	= new JLabel("Monitor File:");
  private JLabel modelNonErrPathLabel  	= new JLabel("Non Err Model Dir:");

  private JTextField researchDataPathField = new JTextField(32);
  private JTextField fwdPathField 	   = new JTextField(32);
  private JTextField out1dvarPathField     = new JTextField(32);
  private JTextField monitorFileField  	   = new JTextField(32);
  private JTextField modelNonErrPathField  = new JTextField(32);


  //////////////////////////////////////////////////////////////
  // External Data Paths Identifiers
  //////////////////////////////////////////////////////////////
 
  private JLabel externalDataPathLabel  = new JLabel("External Data Path:");
  private JLabel rdrOrbitPathLabel 	= new JLabel("Orbit RDR Path:");
  private JLabel rdrSensor1PathLabel 	= new JLabel("Sensor1 RDR Path:");
  private JLabel rdrSensor2PathLabel 	= new JLabel("Sensor2 RDR Path:");
  private JLabel rdrF16PathLabel 	= new JLabel("F16 SSMI/S RDR Path:");
  private JLabel nwpGdasGridPathLabel   = new JLabel("NWP GDAS Path:");
  private JLabel nwpEcmwfGridPathLabel  = new JLabel("NWP ECMWF Path:");
  private JLabel nwpGfsGridPathLabel    = new JLabel("NWP GFS Path:");
 
  private JTextField externalDataPathField  	= new JTextField(32);
  private JTextField rdrOrbitPathField  	= new JTextField(32);
  private JTextField rdrSensor1PathField 	= new JTextField(32);
  private JTextField rdrSensor2PathField 	= new JTextField(32);
  private JTextField rdrF16PathField 		= new JTextField(32);
  private JTextField nwpGdasGridPathField 	= new JTextField(32); 
  private JTextField nwpEcmwfGridPathField 	= new JTextField(32); 
  private JTextField nwpGfsGridPathField 	= new JTextField(32); 
  


  //////////////////////////////////////////////////////////////
  // Static Data Paths Identifiers
  //////////////////////////////////////////////////////////////

  private JLabel staticDataPathLabel  		= new JLabel("Static Data Path:");
  private JLabel instrumentPathLabel  		= new JLabel("Instrument Config Path:");
  private JLabel instrumentSensor1FileLabel 	= new JLabel("Sensor1 Config File:");
  private JLabel instrumentSensor2FileLabel 	= new JLabel("Sensor2 Config File:");
  private JLabel instrumentSensor1Sensor2FileLabel = new JLabel("Sensor1-2 Config File:");
  private JLabel topographyFileLabel 		= new JLabel("Topography File:");
  private JLabel antennaPathLabel 	  	= new JLabel("Attena Pattern Path:");
  private JLabel antennaSensor1FileLabel 	= new JLabel("Sensor1 Antenna Path:");
  private JLabel antennaSensor2FileLabel   	= new JLabel("Sensor2 Antenna Path:");
  private JLabel tune1FileLabel   		= new JLabel("Tune File 1:");
  private JLabel tune2FileLabel   		= new JLabel("Tune File 2:");
  private JLabel nedtNominalFileLabel		= new JLabel("Nedt Nominal File:");
  private JLabel modelErrNominalFileLabel	= new JLabel("Model Err Nominal File:");
  //private JLabel covBkgAtmFileLabel   		= new JLabel("Atm Cov Matrix:");
  //private JLabel covBkgSfcFileLabel   		= new JLabel("Sfc Cov Matrix:");
  private JLabel CRTMcoeffPathLabel   		= new JLabel("CRTM Coeff Path:");
  private JLabel siceEmissCatalogFileLabel	= new JLabel("Sea Ice Emiss Catalog:");
  private JLabel snowEmissCatalogFileLabel	= new JLabel("Snow Cover Emiss Catalog:");

  private JTextField staticDataPathField  	= new JTextField(32);
  private JTextField instrumentPathField  	= new JTextField(32);
  private JTextField instrumentSensor1FileField = new JTextField(32);
  private JTextField instrumentSensor2FileField = new JTextField(32);
  private JTextField instrumentSensor1Sensor2FileField= new JTextField(32);
  private JTextField topographyFileField 	= new JTextField(32);
  private JTextField antennaPathField	  	= new JTextField(32);
  private JTextField antennaSensor1FileField 	= new JTextField(32);
  private JTextField antennaSensor2FileField   	= new JTextField(32);
  private JTextField tune1FileField   		= new JTextField(32);
  private JTextField tune2FileField   		= new JTextField(32);
  private JTextField nedtNominalFileField	= new JTextField(32);
  private JTextField modelErrNominalFileField	= new JTextField(32);
  //private JTextField covBkgAtmFileField   	= new JTextField(32);
  //private JTextField covBkgSfcFileField   	= new JTextField(32);
  private JTextField CRTMcoeffPathField   	= new JTextField(32);
  private JTextField siceEmissCatalogFileField	= new JTextField(32);
  private JTextField snowEmissCatalogFileField	= new JTextField(32);
  

  //////////////////////////////////////////////////////////////
  // Semi-Static Data Paths Identifiers
  //////////////////////////////////////////////////////////////

  private JLabel semiStaticDataPathLabel  	= new JLabel("Semi-Static Data Path:");
  private JLabel biasPathLabel  		= new JLabel("Bias Path:");
  private JLabel regressPathLabel 		= new JLabel("Regression Path:");

  private JTextField semiStaticDataPathField	= new JTextField(32);
  private JTextField biasPathField          	= new JTextField(32);
  private JTextField regressPathField       	= new JTextField(32);



  //////////////////////////////////////////////////////////////
  // Testbed Data Paths Identifiers
  //////////////////////////////////////////////////////////////

  private JLabel testbedDataPathLabel  		= new JLabel("Testbed Data Path:") ;
  private JLabel nedtPathLabel  		= new JLabel("NEDT Path:") ;
  private JLabel nedtSensor1PathLabel  		= new JLabel("Sensor1 NEDT Path:") ;
  private JLabel nedtSensor2PathLabel  		= new JLabel("Sensor2 NEDT Path:") ;
  private JLabel nedtSensor1Sensor2PathLabel  	= new JLabel("Sensor1-2 NEDT Path:") ;
  private JLabel edrPathLabel  			= new JLabel("EDR Path:") ;
  private JLabel depPathLabel  			= new JLabel("DEP Path:") ;
  private JLabel figsPathLabel  		= new JLabel("Figs Path:") ;
  private JLabel perfsMonitorPathLabel  	= new JLabel("Perf. Monitor Path:") ;
  private JLabel logFileLabel  			= new JLabel("Log File:") ;

  private JTextField testbedDataPathField  	= new JTextField(32) ;
  private JTextField nedtPathField  		= new JTextField(32) ;
  private JTextField nedtSensor1PathField  	= new JTextField(32) ;
  private JTextField nedtSensor2PathField  	= new JTextField(32) ;
  private JTextField nedtSensor1Sensor2PathField= new JTextField(32) ;
  private JTextField edrPathField  		= new JTextField(32) ;
  private JTextField depPathField  		= new JTextField(32) ;
  private JTextField figsPathField  		= new JTextField(32) ;
  private JTextField perfsMonitorPathField  	= new JTextField(32) ;
  private JTextField logFileField  		= new JTextField(32) ;


  //////////////////////////////////////////////////////////////
  // Dynamic Data Paths Identifiers
  //////////////////////////////////////////////////////////////
  
  private JLabel dynamicDataPathLabel	= new JLabel("Dynamic Data Path:") ;
  private JLabel tdrPathLabel		= new JLabel("TDR Path:") ;
  private JLabel tdrSensor1PathLabel	= new JLabel("Sensor1 TDR Path:") ;
  private JLabel tdrSensor2PathLabel	= new JLabel("Sensor2 TDR Path:") ;
  private JLabel sdrPathLabel		= new JLabel("SDR Path:") ;
  private JLabel sdrSensor1PathLabel	= new JLabel("Sensor1 SDR Path:") ;
  private JLabel sdrSensor2PathLabel	= new JLabel("Sensor2 SDR Path:") ;
  private JLabel fmsdrPathLabel		= new JLabel("FMSDR Path:") ;
  private JLabel choppPathLabel		= new JLabel("Chopp Path:") ;
  private JLabel nwpAnalysPathLabel   	= new JLabel("NWP Analysis Path:") ;
  private JLabel fwdAnalysPathLabel  	= new JLabel("FWD Analysis Path:") ;
  private JLabel regressRetrPathLabel  	= new JLabel("Regress. Retr. Path:") ;
  
  private JTextField dynamicDataPathField	= new JTextField(32);
  private JTextField tdrPathField		= new JTextField(32);
  private JTextField tdrSensor1PathField	= new JTextField(32);
  private JTextField tdrSensor2PathField	= new JTextField(32);
  private JTextField sdrPathField		= new JTextField(32);
  private JTextField sdrSensor1PathField	= new JTextField(32);
  private JTextField sdrSensor2PathField	= new JTextField(32);
  private JTextField fmsdrPathField		= new JTextField(32);
  private JTextField choppPathField		= new JTextField(32);
  private JTextField nwpAnalysPathField		= new JTextField(32);
  private JTextField fwdAnalysPathField		= new JTextField(32);
  private JTextField regressRetrPathField  	= new JTextField(32);



  //////////////////////////////////////////////////////////////
  // Control File Identifiers
  //////////////////////////////////////////////////////////////
  
  private JLabel controlDataPathLabel 		= new JLabel("Control File Path:");
  private JLabel rdr2tdrSensor1ControlFileLabel = new JLabel("Sensor1 rdr2tdr Control File:");
  private JLabel rdr2tdrSensor2ControlFileLabel = new JLabel("Sensor2 rdr2tdr Control File:");
  private JLabel mergeNedtControlFileLabel 	= new JLabel("Merge Nedt Control File:");
  private JLabel tdr2sdrSensor1ControlFileLabel = new JLabel("Sensor1 tdr2sdr Control File:");
  private JLabel tdr2sdrSensor2ControlFileLabel = new JLabel("Sensor2 tdr2sdr Control File:");
  private JLabel fmControlFileLabel 		= new JLabel("FM Control File:");
  private JLabel fmsdr2edrControlFileLabel 	= new JLabel("FMSDR2EDR Control File:");
  private JLabel grid2nwpControlFileLabel 	= new JLabel("Grid2NWP Control File:");
  private JLabel fwdControlFileLabel 		= new JLabel("FWD Control File:");
  private JLabel regressControlFileLabel 	= new JLabel("Regress. Control File:");
  private JLabel choppControlFileLabel 		= new JLabel("Chopp Control File:");
  private JLabel mergeEdrControlFileLabel 	= new JLabel("Merged EDR Control File:");
  private JLabel biasCompuControlFileLabel	= new JLabel("Bias Comput. Control File:");
  private JLabel biasVerifControlFileLabel 	= new JLabel("Bias Verifi. Control File:");
  private JLabel regressGenControlFileLabel 	= new JLabel("Regress. Gen. Control File:");
  private JLabel modifyNedtControlFileLabel 	= new JLabel("Modify Nedt Control File:");
  
  private JTextField controlDataPathField 		= new JTextField(32);
  private JTextField rdr2tdrSensor1ControlFileField 	= new JTextField(32);
  private JTextField rdr2tdrSensor2ControlFileField 	= new JTextField(32);
  private JTextField mergeNedtControlFileField 		= new JTextField(32);
  private JTextField tdr2sdrSensor1ControlFileField 	= new JTextField(32);
  private JTextField tdr2sdrSensor2ControlFileField 	= new JTextField(32);
  private JTextField fmControlFileField 		= new JTextField(32);
  private JTextField fmsdr2edrControlFileField 		= new JTextField(32);
  private JTextField grid2nwpControlFileField 		= new JTextField(32);
  private JTextField fwdControlFileField 		= new JTextField(32);
  private JTextField regressControlFileField 		= new JTextField(32);
  private JTextField choppControlFileField 		= new JTextField(32);
  private JTextField mergeEdrControlFileField 		= new JTextField(32);
  private JTextField biasCompuControlFileField 		= new JTextField(32);
  private JTextField biasVerifControlFileField 		= new JTextField(32);
  private JTextField regressGenControlFileField 	= new JTextField(32);
  private JTextField modifyNedtControlFileField 	= new JTextField(32);
  private JTextField psFigsGenControlFileField 		= new JTextField(32);



  //////////////////////////////////////////////////////////////
  // File List Identifiers
  //////////////////////////////////////////////////////////////
  
  private JLabel inputDataPathLabel 	        = new JLabel("Input Data Path:") ;
  private JLabel rdrSensor1ListLabel 	        = new JLabel("Sensor1 rdr List:") ;
  private JLabel rdrSensor2ListLabel 	        = new JLabel("Sensor2 rdr List:") ;
  private JLabel tdrSensor1ListLabel 	        = new JLabel("Sensor1 tdr List:") ;
  private JLabel tdrSensor2ListLabel 	        = new JLabel("Sensor2 tdr List:") ;
  private JLabel sdrSensor1ListLabel 	        = new JLabel("Sensor1 sdr List:") ;
  private JLabel sdrSensor2ListLabel 	        = new JLabel("Sensor2 sdr List:") ;
  private JLabel sdrSensor3ListLabel 	        = new JLabel("SDR las List:") ;
  private JLabel sdrSensor4ListLabel 	        = new JLabel("SDR uas List:") ;
  private JLabel fmsdrListLabel 	        = new JLabel("FMSDR List:") ;
  private JLabel fmsdr4BiasListLabel 		= new JLabel("Bias FMSDR List:") ;
  private JLabel fmsdr4ChoppListLabel 		= new JLabel("Chopped FMSDR List:") ;
  private JLabel fmsdr4NwpListLabel 		= new JLabel("NWP FMSDR List:") ;
  private JLabel fmsdr4RegressListLabel 	= new JLabel("Regression FMSDR List:") ;
  private JLabel fmsdr4ApplyRegressListLabel 	= new JLabel("Applied Regress. FMSDR List:") ;
  private JLabel edrListLabel 			= new JLabel("EDR List:") ;
  
  private JLabel edr4BiasListLabel 		= new JLabel("Bias EDR List:") ;
  private JLabel edr4MergeListLabel 		= new JLabel("Merged EDR List:") ;
  private JLabel nedtListLabel 			= new JLabel("NEDT List:") ;
  private JLabel nedtSensor1ListLabel 		= new JLabel("Sensor1 NEDT List:") ;
  private JLabel nedtSensor2ListLabel 		= new JLabel("Sensor2 NEDT List:") ;
  private JLabel gridSfcNwpAnalysListLabel 	= new JLabel("NWP SFC Grid Analysis List:") ;
  private JLabel gridAtmNwpAnalysListLabel 	= new JLabel("NWP ATM Grid Analysis List:") ;
  private JLabel nwpAnalysListLabel 		= new JLabel("NWP Analysis List:") ;
  private JLabel nwpAnalysRetrListLabel 	= new JLabel("Retrieved NWP List:") ;
  private JLabel nwpAnalys4BiasListLabel 	= new JLabel("Bias NWP Analysis List:") ;
  private JLabel nwpAnalys4RegressListLabel 	= new JLabel("NWP Regress. Analysis List:") ;
  private JLabel fwdAnalys4BiasListLabel 	= new JLabel("Bias FWD Analysis List:") ;
  
  private JTextField inputDataPathField 		= new JTextField(32);
  private JTextField rdrSensor1ListField 		= new JTextField(32);
  private JTextField rdrSensor2ListField 		= new JTextField(32);
  private JTextField tdrSensor1ListField 		= new JTextField(32);
  private JTextField tdrSensor2ListField 		= new JTextField(32);
  private JTextField sdrSensor1ListField 		= new JTextField(32);
  private JTextField sdrSensor2ListField 		= new JTextField(32);
  private JTextField sdrSensor3ListField 		= new JTextField(32);
  private JTextField sdrSensor4ListField 		= new JTextField(32);
  private JTextField fmsdrListField 			= new JTextField(32);
  private JTextField fmsdr4BiasListField 		= new JTextField(32);
  private JTextField fmsdr4ChoppListField 		= new JTextField(32);
  private JTextField fmsdr4NwpListField 		= new JTextField(32);
  private JTextField fmsdr4RegressListField 		= new JTextField(32);
  private JTextField fmsdr4ApplyRegressListField 	= new JTextField(32);
  private JTextField edrListField 			= new JTextField(32);
  private JTextField edr4BiasListField 			= new JTextField(32);
  private JTextField edr4MergeListField 		= new JTextField(32);
  private JTextField nedtListField 			= new JTextField(32);
  private JTextField nedtSensor1ListField 		= new JTextField(32);
  private JTextField nedtSensor2ListField 		= new JTextField(32);
  private JTextField gridSfcNwpAnalysListField 		= new JTextField(32);
  private JTextField gridAtmNwpAnalysListField 		= new JTextField(32);
  private JTextField nwpAnalysListField 		= new JTextField(32);
  private JTextField nwpAnalysRetrListField 		= new JTextField(32);
  private JTextField nwpAnalys4BiasListField 		= new JTextField(32);
  private JTextField nwpAnalys4RegressListField 	= new JTextField(32);
  private JTextField fwdAnalys4BiasListField 		= new JTextField(32);


  //////////////////////////////////////////////////////////////
  // Application / Process Identifiers
  //////////////////////////////////////////////////////////////

  private JLabel rdr2tdrSensor1ApplicationLabel 	= new JLabel("Sensor1 RDR to TDR:") ;
  private JLabel rdr2tdrSensor2ApplicationLabel 	= new JLabel("Sensor2 RDR to TDR:") ;
  private JLabel mergeNedtApplicationLabel 		= new JLabel("Merge NEDT:") ;
  private JLabel tdr2sdrApplicationLabel 		= new JLabel("TDR to SDR:") ;
  private JLabel fmApplicationLabel 			= new JLabel("FM:") ;
  private JLabel choppApplicationLabel 			= new JLabel("Chopp:") ;
  private JLabel fmsdr2edrApplicationLabel 		= new JLabel("FMSDR2EDR:") ;
  private JLabel mergeEdrApplicationLabel 		= new JLabel("merge EDR:") ;
  private JLabel vippApplicationLabel			= new JLabel("VIPP:") ;
  private JLabel nedtMonitorApplicationLabel 		= new JLabel("NEDT Monitor:") ;
  private JLabel nwpGenAnalysApplicationLabel 		= new JLabel("NWP Analys Gen.:") ;
  private JLabel fwdApplicationLabel 			= new JLabel("FWD") ;
  private JLabel determineBiasApplicationLabel 		= new JLabel("Determine Bias:") ;
  private JLabel regressAlgApplicationLabel 		= new JLabel("Regression Algorithm:") ;
  private JLabel applyRegressAlgApplicationLabel	= new JLabel("Apply Regress. Algorithm:") ;

  private JTextField rdr2tdrSensor1ApplicationField 	= new JTextField(32);
  private JTextField rdr2tdrSensor2ApplicationField 	= new JTextField(32);
  private JTextField mergeNedtApplicationField 		= new JTextField(32);
  private JTextField tdr2sdrApplicationField 		= new JTextField(32);
  private JTextField fmApplicationField 		= new JTextField(32);
  private JTextField choppApplicationField 		= new JTextField(32);
  private JTextField fmsdr2edrApplicationField 		= new JTextField(32);
  private JTextField mergeEdrApplicationField 		= new JTextField(32);
  private JTextField vippApplicationField 		= new JTextField(32);
  private JTextField nedtMonitorApplicationField 	= new JTextField(32);
  private JTextField nwpGenAnalysApplicationField 	= new JTextField(32);
  private JTextField fwdApplicationField 		= new JTextField(32);
  private JTextField determineBiasApplicationField 	= new JTextField(32);
  private JTextField regressAlgApplicationField 	= new JTextField(32);
  private JTextField applyRegressAlgApplicationField 	= new JTextField(32);

 
  private DefaultTreeModel model;
  private JTree tree;
  
  private JScrollPane treePanel;
  private JPanel rightPanel = new JPanel();
  SpringLayout springLayout = new SpringLayout();
  
  private JButton okButton = new JButton("OK");
  private JButton cancelButton = new JButton("Cancel");
  private JButton defaultButton = new JButton("Default");
  
  private static final long serialVersionUID = 1L;

}
