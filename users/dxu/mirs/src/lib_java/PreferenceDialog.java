/**
 * @version 1.00 2007-01-10
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


public class PreferenceDialog extends JDialog 
			implements ActionListener, TreeSelectionListener, ItemListener, FocusListener 
{

  private Map<String, String> preferences = new HashMap<String, String>();
  
  private String nAttemptsKey 			= new String("nAttempts");
  private String fmTypeKey 			= new String("fmType");
  private String outFMAccuracyKey 		= new String("outFMAccuracy");
  private String prefixFMAccuracyKey 		= new String("prefixFMAccuracy");
  private String addDeviceNoiseKey 		= new String("addDeviceNoise");

  private String monitorIterativeKey 		= new String("monitorIterative");
  private String monitorRetrievalKey 		= new String("monitorRetrieval");
  private String monitorFwdKey 	     		= new String("monitorFwd");

  private String externalDataAvailableKey 	= new String("externalDataAvailable");
  private String externalDataSrcKey 		= new String("externalDataSrc");
  
  private String nwpGdasUseKey 			= new String("nwpGdasUse");
  private String nwpEcmwfUseKey 		= new String("nwpEcmwfUse");
  private String nwpGfsUseKey 			= new String("nwpGfsUse");

  private String geoLimitKey 			= new String("geoLimit");
  private String minLatKey 			= new String("minLat");
  private String maxLatKey 			= new String("maxLat");
  private String minLonKey 			= new String("minLon");
  private String maxLonKey 			= new String("maxLon");
  private String cendKey 			= new String("cend");

  private String maxDaysArchivedKey 		= new String("maxDaysArchived"); 
  private String dayUsed4BiasKey    		= new String("dayUsed4Bias");
  private String dayUsed4AlgKey     		= new String("dayUsed4Alg");

  private String nScanLineSensor1SkipKey 	= new String("nScanLineSensor1Skip"); 
  private String nScanLineSensor2SkipKey 		= new String("nScanLineSensor2Skip");
  private String scanLineIndexSensor2TimeCollocKey 	= new String("scanLineIndexSensor2TimeColloc");

  private String biasComputeMethodKey 		= new String("biasComputeMethod");
  private String regressionBiasApplyDomainKey 	= new String("regressionBiasApplyDomain");
  private String nChoppedFilesPerOrbitKey 	= new String("nChoppedFilesPerOrbit");     
  private String retrOnOrbitOrSubOrbitKey 	= new String("retrOnOrbitOrSubOrbit");
  private String retrOnWhichSDRKey 		= new String("retrOnWhichSDR");
  private String fwdMatrix2UseKey 		= new String("fwdMatrix2Use");

  private String tdrFormatKey 			= new String("tdrFormat");
  private String gifDensityKey 			= new String("gifDensity");
  private String gridFactorKey 			= new String("gridFactor");
  private String emailKey 			= new String("email");
  private String websiteKey 			= new String("website");

  private String makeOrNotKey 			= new String("makeOrNot"); 
  private String useCPUKey 			= new String("useCPU");              
  private String makeCleanKey 			= new String("makeClean");              
  
  private String fwdCloudOffOrOnKey 		= new String("fwdCloudOffOrOn");              

  private String extBkgAtmUseKey 		= new String("extBkgAtmUse");              
  
  private String rdrTypeKey 		        = new String("rdrType");              

  private String nAttempts 			= null ;
  private String fmType 			= null ;
  private String outFMAccuracy 			= null ;
  private String prefixFMAccuracy 		= null ; 
  private String addDeviceNoise 		= null ;

  private String monitorIterative 		= null ; 
  private String monitorRetrieval 		= null ; 
  private String monitorFwd 	     		= null ; 

  private String externalDataAvailable 		= null ; 
  private String externalDataSrc 		= null ; 

  private String nwpGdasUse 			= null ; 
  private String nwpEcmwfUse 			= null ;
  private String nwpGfsUse 			= null ; 
   
  private String geoLimit 			= null ; 
  private String minLat 			= null ; 
  private String maxLat 			= null ; 
  private String minLon 			= null ; 
  private String maxLon 			= null ; 
  private String cend 				= null ; 

  private String maxDaysArchived 		= null ; 
  private String dayUsed4Bias    		= null ; 
  private String dayUsed4Alg     		= null ; 

  private String nScanLineSensor1Skip 		= null ; 
  private String nScanLineSensor2Skip 		= null ; 
  private String scanLineIndexSensor2TimeColloc = null ; 

  private String biasComputeMethod 		= null ; 
  private String regressionBiasApplyDomain 	= null ;
  
  private String nChoppedFilesPerOrbit 		= null ; 
  private String retrOnOrbitOrSubOrbit 		= null ; 
  private String retrOnWhichSDR 		= null ; 
  private String fwdMatrix2Use 			= null ; 

  private String tdrFormat 			= null ; 
  private String gifDensity 			= null ; 
  private String gridFactor 			= null ; 
  private String email                          = null ;
  private String website                        = null ;

  private String makeOrNot 			= null ; 
  private String useCPU 			= null ; 
  private String makeClean 			= null ; 
  private String fwdCloudOffOrOn 		= null ; 
  private String extBkgAtmUse 		        = null ; 
  
  private String rdrType 			= null ; 
  
  //nAttempts
  private JLabel nAttemptsLabel = new JLabel("Number of retrieval attempts in case of non-convergence");
  private JTextField nAttemptsField = new JTextField(16);

  //fmType
  private JRadioButton fmCoarseTypeButton  = new JRadioButton("Coarse resolution footprint matching");
  private JRadioButton fmLowTypeButton     = new JRadioButton("Low resolution footprint matching");
  private JRadioButton fmHighTypeButton    = new JRadioButton("High resolution footprint matching");
  
  private JRadioButton fmImgTypeButton  = new JRadioButton("Img resolution footprint matching");
  private JRadioButton fmEnvTypeButton  = new JRadioButton("Evn resolution footprint matching");
  private JRadioButton fmLasTypeButton  = new JRadioButton("Las resolution footprint matching");
  private JRadioButton fmUasTypeButton  = new JRadioButton("Uas resolution footprint matching");

  //outFMAccuracy
  private JCheckBox outFMAccuracyCheck = new JCheckBox("Output FM accuracy metric", true);

  //prefixFMAccuracy
  private JTextField prefixFMAccuracyField = new JTextField(16);

  //addDeviceNoise
  private JCheckBox addDeviceNoiseCheck = new JCheckBox("Add noise to the fwd simulation", true);

  //externalDataAvailable
  private JCheckBox externalDataAvailableCheck = new JCheckBox("External data available", false);

  //externalDataSrc ( depending on externalDataAvailable )
  private JRadioButton analysExternalDataButton  = new JRadioButton("NWP analyses");
  private JRadioButton regressExternalDataButton = new JRadioButton("Regression algorithms");

  //nwpGdasUse
  private JCheckBox nwpGdasUseCheck = new JCheckBox("Use GDAS", true);

  //nwpEcmwfUse
  private JCheckBox nwpEcmwfUseCheck = new JCheckBox("Use ECMWF", false);

  //nwpGfsUse
  private JCheckBox nwpGfsUseCheck = new JCheckBox("Use GFS", false);

  //extBkgAtmUse
  private JCheckBox extBkgAtmUseCheck = new JCheckBox("Use External (spatial/temporal varying) Background Atmosphere Yes/No", false);



  //geoLimit
  private JRadioButton geoAllLimitButton    = new JRadioButton("Process all data");
  private JRadioButton geoLatLonLimitButton = new JRadioButton("Process data in lat/lon box");
  private JRadioButton geoOceanLimitButton  = new JRadioButton("Process only ocean data");
  private JRadioButton geoLandLimitButton   = new JRadioButton("Process only land data");

  //minLat
  private JLabel minLatLabel = new JLabel("Minimum Latitude");
  private JTextField minLatField = new JTextField(32);

  //maxLat
  private JLabel maxLatLabel = new JLabel("Maximum Latitude");
  private JTextField maxLatField = new JTextField(32);

  //minLon
  private JLabel minLonLabel = new JLabel("Minimum Longitude");
  private JTextField minLonField = new JTextField(32);

  //maxLon
  private JLabel maxLonLabel = new JLabel("Maximum Longitude");
  private JTextField maxLonField = new JTextField(32);

  //cend
  private JRadioButton ascendButton  = new JRadioButton("Ascending");
  private JRadioButton descendButton  = new JRadioButton("Descending");
  private JRadioButton combineButton = new JRadioButton("Both");




  // 3 monitor options
  //monitorIterative
  private JCheckBox monitorIterativeCheck = new JCheckBox("On-file monitoring of iterative process", false);

  //monitorRetrieval
  private JCheckBox monitorRetrievalCheck = new JCheckBox("On-screen monitoring of retrieval", false);

  //monitorFwd
  private JCheckBox monitorFwdCheck 	= new JCheckBox("On-screen monitoring of fwd simulation", false);



  //maxDaysArchived
  private JLabel maxDaysArchivedLabel = new JLabel("Number of days data to be archived");
  private JTextField maxDaysArchivedField = new JTextField(32);

  //dayUsed4Bias
  private JLabel dayUsed4BiasLabel = new JLabel("Date to be used in empirical correction process");
  private JTextField dayUsed4BiasField = new JTextField(32);

  //dayUsed4Alg
  private JLabel dayUsed4AlgLabel = new JLabel("Date to be used in generating external data from regress");
  private JTextField dayUsed4AlgField = new JTextField(32);



  //tdrFormat
  private JRadioButton tdrAsciiFormatButton  = new JRadioButton("Ascii format");
  private JRadioButton tdrBinaryFormatButton = new JRadioButton("Binary format");

  //gifDensity
  private JLabel gifDensityLabel = new JLabel("Density in converting PS file into GIF image");
  private JTextField gifDensityField = new JTextField(8);

  //gridFactor
  private JLabel gridFactorLabel = new JLabel("Grid factor in gridding level III data");
  private JTextField gridFactorField = new JTextField(8);

  //email
  private JLabel emailLabel = new JLabel("Comma separated email addresses");
  private JTextField emailField = new JTextField(40);

  //website
  private JLabel websiteLabel = new JLabel("Website address to view result");
  private JTextField websiteField = new JTextField(40);

  //rdrType
  private JRadioButton rdrTypeTdrButton = new JRadioButton("TDR(1)");
  private JRadioButton rdrTypeSdrProxyButton = new JRadioButton("Proxy SDR from MIT(2)");
  private JRadioButton rdrTypeSdrButton = new JRadioButton("SDR(3)");
  private JRadioButton rdrTypeSdrRemapButton = new JRadioButton("Remapped SDR(4)");

  //nScanLineSensor1Skip
  private JLabel nScanLineSensor1SkipLabel = new JLabel("Number of Sensor1 scan lines to skip");
  private JTextField nScanLineSensor1SkipField = new JTextField(16);

  //nScanLineSensor2Skip
  private JLabel nScanLineSensor2SkipLabel = new JLabel("Number of Sensor2 scan lines to skip");
  private JTextField nScanLineSensor2SkipField = new JTextField(16);


  //scanLineIndexSensor2TimeColloc
  private JLabel sensor2ScanIndexLabel = new JLabel("Sensor2 scanline index that corresponds in time to Sensor1");
  private JRadioButton sensor2ScanIndex1Button = new JRadioButton("1");
  private JRadioButton sensor2ScanIndex2Button = new JRadioButton("2");
  private JRadioButton sensor2ScanIndex3Button = new JRadioButton("3");


  //biasComputeMethod
  private JRadioButton biasComputeSimpleMethodButton = new JRadioButton("Simple method");
  private JRadioButton biasComputeHistogramMethodButton = new JRadioButton("Histogram adjustment");

  //regressionBiasApplyDomain  - regression
  private JRadioButton noneRegressionBiasApplyDomainButton   = new JRadioButton("Nowhere");
  private JRadioButton allRegressionBiasApplyDomainButton    = new JRadioButton("Everywhere (which channel and which sfcType are controlled in tuning file)");


  //nChoppedFilesPerOrbit
  private JLabel nChoppedFilesPerOrbitLabel = new JLabel("Number of chopped sub-orbits pre orbit");
  private JTextField nChoppedFilesPerOrbitField = new JTextField(16);

  //retrOnOrbitOrSubOrbit
  private JRadioButton fullOrbitRetrievalButton    = new JRadioButton("Full orbits retrieval");
  private JRadioButton choppedOrbitRetrievalButton = new JRadioButton("Chopped orbits retrieval");

  //retrOnWhichSDR
  private JRadioButton empiricallyCorrectedTBButton = new JRadioButton("Empiri. corrected");
  private JRadioButton uncorrectedTBButton 	  = new JRadioButton("Uncorrected");
  private JRadioButton nwpTBButton 		  = new JRadioButton("NWP based simu.");

  //fwdMatrix2Use
  private JRadioButton dynamicFwdMatrixButton = new JRadioButton("Dynamicly generated by comparing with simulation");
  private JRadioButton nonErrFwdMatrixButton  = new JRadioButton("Non error");



 //makeOrNot
  private JCheckBox makeOrNotCheck = new JCheckBox("Make ( make is updated automatically )", false);

  //useCPU
  //private JCheckBox useCPUCheck = new JCheckBox("Use all available CPUs", false);
  private JRadioButton cpu0Button  = new JRadioButton("Use 1 CPU");
  private JRadioButton cpu1Button  = new JRadioButton("Use All CPUs");
  private JRadioButton cpu2Button  = new JRadioButton("Use QSUB");


  //makeClean
  private JCheckBox makeCleanCheck = new JCheckBox("Make Clean(only used when 'Cleaning' step turned on)", false);


  //fwdCloudOffOrOn
  private JCheckBox fwdCloudOffOrOnCheck = new JCheckBox("Forward Cloud On/Off", false);

  //satId
  private String satIdKey = new String("satId");
  private String satId  = null;
    
  
  private DefaultMutableTreeNode root;
  
  private DefaultMutableTreeNode geoNode;
  private DefaultMutableTreeNode retrievalNode;
  private DefaultMutableTreeNode externalNode;
  private DefaultMutableTreeNode extBkgNode;
  private DefaultMutableTreeNode monitorNode;
  private DefaultMutableTreeNode synchroNode;
  private DefaultMutableTreeNode semiStaticNode;
  private DefaultMutableTreeNode methodNode;
  private DefaultMutableTreeNode runTimeNode;
  private DefaultMutableTreeNode fmNode;
  private DefaultMutableTreeNode fwdNode;
  private DefaultMutableTreeNode flagNode;
  //private DefaultMutableTreeNode plotNode;
  private DefaultMutableTreeNode miscNode;
  private DefaultMutableTreeNode cleanNode;

  
  private DefaultTreeModel model;
  private JTree tree;

  private JScrollPane treePanel;
  private JPanel rightPanel = new JPanel();

  SpringLayout springLayout = new SpringLayout();
  SpringLayout rightLayout  = new SpringLayout();
 

  private JButton okButton = new JButton("OK");
  private JButton cancelButton = new JButton("Cancel");
  
  private static final long serialVersionUID = 1L;
  

  /**
   * Constructor
   */
  public PreferenceDialog(JFrame frame, String title, boolean model) 
  { 
    super(frame, title, model);
    setSize(800, 800);
    
    /**
    addWindowListener(new WindowAdapter() {
	public void windowClosing(WindowEvent e) {
	//System.exit(0);
	setVisible(false);
	}
	});
    */
    
    // the root of the class tree is Object
    root = new DefaultMutableTreeNode("Category");
    //model = new DefaultTreeModel(root);

    geoNode 	  = new DefaultMutableTreeNode("Geographic Selection");
    retrievalNode = new DefaultMutableTreeNode("Retrieval Options");
    externalNode  = new DefaultMutableTreeNode("External Data Use");
    extBkgNode    = new DefaultMutableTreeNode("External Background Use");
    monitorNode   = new DefaultMutableTreeNode("Monitoring Options");
    synchroNode   = new DefaultMutableTreeNode("Sensors Synchronization");
    semiStaticNode= new DefaultMutableTreeNode("Semi-Static Options");
    methodNode    = new DefaultMutableTreeNode("Bias Computation Options");
    fmNode        = new DefaultMutableTreeNode("Footprint Matching");
    fwdNode       = new DefaultMutableTreeNode("Forward Options");
    flagNode      = new DefaultMutableTreeNode("Control Flags");
    runTimeNode   = new DefaultMutableTreeNode("Make/Run Options");
    //plotNode    = new DefaultMutableTreeNode("Plot Options");
    miscNode      = new DefaultMutableTreeNode("Miscellaneous");
    cleanNode     = new DefaultMutableTreeNode("Cleaning");
    
    root.add(geoNode);
    root.add(retrievalNode);
    root.add(externalNode);
    root.add(extBkgNode);
    root.add(monitorNode);
    root.add(synchroNode);
    //root.add(semiStaticNode);
    root.add(fmNode);
    root.add(fwdNode);
    root.add(methodNode);
    root.add(runTimeNode);
    //root.add(plotNode);
    root.add(miscNode);
    root.add(cleanNode);
    
    //root.add(flagNode);
    
    //tree = new JTree(model);
    tree = new JTree(root);

    tree.addTreeSelectionListener(this);
    tree.setRootVisible(false);

    int mode = TreeSelectionModel.SINGLE_TREE_SELECTION;
    tree.getSelectionModel().setSelectionMode(mode);


    // add tree and text area to the content pane
    JPanel panel = new JPanel();
    panel.setLayout(springLayout);    	
    rightPanel.setLayout(rightLayout);
    treePanel = new JScrollPane(tree);
    
    panel.add(treePanel);
    panel.add(rightPanel);
    
    
    SpringLayout.Constraints treeCons = springLayout.getConstraints(treePanel);
    treeCons.setX(Spring.constant(0));
    treeCons.setY(Spring.constant(50));
    treeCons.setWidth(Spring.constant(175));
    treeCons.setHeight(Spring.constant(660));
    
    SpringLayout.Constraints rightCons = springLayout.getConstraints(rightPanel);
    rightCons.setX(Spring.constant(180));
    rightCons.setY(Spring.constant(0));
    rightCons.setWidth(Spring.constant(1125));
    rightCons.setHeight(Spring.constant(720));
    

    SpringLayout.Constraints okCons = springLayout.getConstraints(okButton);
    okCons.setX(Spring.constant(350));
    okCons.setY(Spring.constant(730));
    okCons.setWidth(Spring.constant(75));
    okCons.setHeight(Spring.constant(25));
    
    SpringLayout.Constraints cancelCons = springLayout.getConstraints(cancelButton);
    cancelCons.setX(Spring.constant(435));
    cancelCons.setY(Spring.constant(730));
    cancelCons.setWidth(Spring.constant(75));
    cancelCons.setHeight(Spring.constant(25));
    
    
    panel.add(okButton);
    panel.add(cancelButton);

    Container contentPane = getContentPane();
    contentPane.add(panel, "Center");
    
    ////////////////////////////////////////////////////////////////////
    // Initial States of some fields
    ////////////////////////////////////////////////////////////////////

    analysExternalDataButton.setEnabled(false);
    regressExternalDataButton.setEnabled(false);
    
    ////////////////////////////////////////////////////////////////////
    // Add various action listeners
    ////////////////////////////////////////////////////////////////////
   
    okButton.addActionListener(this);
    cancelButton.addActionListener(this);
    
    externalDataAvailableCheck.addItemListener(this);

    analysExternalDataButton.addActionListener(this);
    regressExternalDataButton.addActionListener(this);
    
    nwpGdasUseCheck.addItemListener(this);
    nwpEcmwfUseCheck.addItemListener(this);
    nwpGfsUseCheck.addItemListener(this);

    extBkgAtmUseCheck.addItemListener(this);

    geoAllLimitButton.addActionListener(this);   
    geoLatLonLimitButton.addActionListener(this);
    geoOceanLimitButton.addActionListener(this); 
    geoLandLimitButton.addActionListener(this); 

    minLatField.addFocusListener(this);
    maxLatField.addFocusListener(this);
    minLonField.addFocusListener(this);  
    maxLonField.addFocusListener(this);

    ascendButton.addActionListener(this); 
    descendButton.addActionListener(this); 
    combineButton.addActionListener(this);


    nScanLineSensor1SkipField.addFocusListener(this);
    nScanLineSensor2SkipField.addFocusListener(this);
    sensor2ScanIndex1Button.addActionListener(this);
    sensor2ScanIndex2Button.addActionListener(this);
    sensor2ScanIndex3Button.addActionListener(this);


    maxDaysArchivedField.addFocusListener(this);
    dayUsed4BiasField.addFocusListener(this);
    dayUsed4AlgField.addFocusListener(this);


    tdrAsciiFormatButton.addActionListener(this);
    tdrBinaryFormatButton.addActionListener(this);
    gifDensityField.addFocusListener(this);
    gridFactorField.addFocusListener(this);
    emailField.addFocusListener(this);
    websiteField.addFocusListener(this);
    
    rdrTypeTdrButton.addActionListener(this);
    rdrTypeSdrProxyButton.addActionListener(this);
    rdrTypeSdrButton.addActionListener(this);
    rdrTypeSdrRemapButton.addActionListener(this);


    nAttemptsField.addFocusListener(this);

    fmCoarseTypeButton.addActionListener(this); 
    fmLowTypeButton.addActionListener(this); 
    fmHighTypeButton.addActionListener(this);
    
    fmImgTypeButton.addActionListener(this); 
    fmEnvTypeButton.addActionListener(this);
    fmLasTypeButton.addActionListener(this);
    fmUasTypeButton.addActionListener(this);

    outFMAccuracyCheck.addItemListener(this);
    prefixFMAccuracyField.addFocusListener(this);
    
    biasComputeSimpleMethodButton.addActionListener(this); 
    biasComputeHistogramMethodButton.addActionListener(this);

    noneRegressionBiasApplyDomainButton.addActionListener(this);    
    allRegressionBiasApplyDomainButton.addActionListener(this);    
    //oceanRegressionBiasApplyDomainButton.addActionListener(this);    
    //landRegressionBiasApplyDomainButton.addActionListener(this);    

    nChoppedFilesPerOrbitField.addFocusListener(this);

    fullOrbitRetrievalButton.addActionListener(this);	
    choppedOrbitRetrievalButton.addActionListener(this);

    empiricallyCorrectedTBButton.addActionListener(this);
    uncorrectedTBButton.addActionListener(this);	
    nwpTBButton.addActionListener(this);		

    dynamicFwdMatrixButton.addActionListener(this);
    nonErrFwdMatrixButton.addActionListener(this); 

    monitorIterativeCheck.addItemListener(this);
    monitorRetrievalCheck.addItemListener(this);
    monitorFwdCheck.addItemListener(this);

    addDeviceNoiseCheck.addItemListener(this);

    makeOrNotCheck.addItemListener(this);
    //useCPUCheck.addItemListener(this);
    cpu0Button.addActionListener(this); 
    cpu1Button.addActionListener(this); 
    cpu2Button.addActionListener(this);

    makeCleanCheck.addItemListener(this);
    fwdCloudOffOrOnCheck.addItemListener(this);
    extBkgAtmUseCheck.addItemListener(this);
 
    SymWindow aSymWindow = new SymWindow();
    this.addWindowListener(aSymWindow);

   
    // load default     
    loadGeo();
    //pack();
    setLocation(200,100);
    
  }


  /**
     * internal class to close the dialog window
     */
  class SymWindow extends java.awt.event.WindowAdapter
  {
          public void windowClosing(java.awt.event.WindowEvent event)
          {
        	  Object object = event.getSource();
        	  if (object == PreferenceDialog.this)
        		  dispose();
          }
  }


  
  /**
   * All those CheckBoxes and all those Choice, when their state change
   */
  public void itemStateChanged(ItemEvent event) {

    if ( event.getSource() == outFMAccuracyCheck ) {
	String oldVal =  outFMAccuracy;

	if ( outFMAccuracyCheck.isSelected() ) {
	    outFMAccuracy="1";
	    prefixFMAccuracyField.setEditable(true);
	}	
	else {
	    outFMAccuracy="0";
	    prefixFMAccuracyField.setEditable(false);
	}
	    
	if ( ! outFMAccuracy.equals(oldVal) ) {
	    if ( preferences.containsKey(outFMAccuracyKey) ) 
	    	preferences.remove(outFMAccuracyKey);
	    preferences.put(outFMAccuracyKey, outFMAccuracy);
	}	     
    }


    else if ( event.getSource() == monitorIterativeCheck ) {

	String oldVal =  monitorIterative;

	if ( monitorIterativeCheck.isSelected() )
	    monitorIterative="1";
	else
	    monitorIterative="0";
	    
	if ( ! monitorIterative.equals(oldVal) ) {
	    if ( preferences.containsKey(monitorIterativeKey) ) 
	    	preferences.remove(monitorIterativeKey);
	    preferences.put(monitorIterativeKey, monitorIterative);
	}	     
    }
    else if ( event.getSource() == monitorRetrievalCheck ) {

	String oldVal =  monitorRetrieval;

	if ( monitorRetrievalCheck.isSelected() )
	    monitorRetrieval="1";
	else
	    monitorRetrieval="0";
	    
	if ( ! monitorRetrieval.equals(oldVal) ) {
	    if ( preferences.containsKey(monitorRetrievalKey) ) 
	    	preferences.remove(monitorRetrievalKey);
	    preferences.put(monitorRetrievalKey, monitorRetrieval);
	}	     
    }
    else if ( event.getSource() == monitorFwdCheck ) {

	String oldVal =  monitorFwd;

	if ( monitorFwdCheck.isSelected() )
	    monitorFwd="1";
	else
	    monitorFwd="0";
	    
	if ( ! monitorFwd.equals(oldVal) ) {
	    if ( preferences.containsKey(monitorFwdKey) ) 
	    	preferences.remove(monitorFwdKey);
	    preferences.put(monitorFwdKey, monitorFwd);
	}	     
    }


    else if ( event.getSource() == makeOrNotCheck ) {
        
	String oldVal =  makeOrNot;

	if ( makeOrNotCheck.isSelected() )
	    makeOrNot="1";
	else
	    makeOrNot="0";
	    
	if ( ! makeOrNot.equals(oldVal) ) {
	    if ( preferences.containsKey(makeOrNotKey) ) 
	    	preferences.remove(makeOrNotKey);
	    preferences.put(makeOrNotKey, makeOrNot);
	}	     
	     
    }
    /**
    else if ( event.getSource() == useCPUCheck ) {
    
	String oldVal =  useCPU;

	if ( useCPUCheck.isSelected() )
	    useCPU="1";
	else
	    useCPU="0";
	    
	if ( ! useCPU.equals(oldVal) ) {
	    if ( preferences.containsKey(useCPUKey) ) 
	    	preferences.remove(useCPUKey);
	    preferences.put(useCPUKey, useCPU);
	}	     
    }
    */
    else if ( event.getSource() == makeCleanCheck ) {
    
	String oldVal =  makeClean;

	if ( makeCleanCheck.isSelected() )
	    makeClean="1";
	else
	    makeClean="0";
	    
	if ( ! makeClean.equals(oldVal) ) {
	    if ( preferences.containsKey(makeCleanKey) ) 
	    	preferences.remove(makeCleanKey);
	    preferences.put(makeCleanKey, makeClean);
	}	     
    }

    else if ( event.getSource() == fwdCloudOffOrOnCheck ) {
    
	String oldVal =  fwdCloudOffOrOn;

	if ( fwdCloudOffOrOnCheck.isSelected() )
	    fwdCloudOffOrOn="1";
	else
	    fwdCloudOffOrOn="0";
	    
	if ( ! fwdCloudOffOrOn.equals(oldVal) ) {
	    if ( preferences.containsKey(fwdCloudOffOrOnKey) ) 
	    	preferences.remove(fwdCloudOffOrOnKey);
	    preferences.put(fwdCloudOffOrOnKey, fwdCloudOffOrOn);
	}	     
    }

    else if ( event.getSource() == addDeviceNoiseCheck ) {
    
	String oldVal =  addDeviceNoise;

	if ( addDeviceNoiseCheck.isSelected() )
	    addDeviceNoise="1";
	else
	    addDeviceNoise="0";
	    
	if ( ! addDeviceNoise.equals(oldVal) ) {
	    if ( preferences.containsKey(addDeviceNoiseKey) ) 
	    	preferences.remove(addDeviceNoiseKey);
	    preferences.put(addDeviceNoiseKey, addDeviceNoise);
	}	     
    }

    else if ( event.getSource() == externalDataAvailableCheck ) {
        
	String oldVal = externalDataAvailable;
        
	if ( externalDataAvailableCheck.isSelected() )
	{
		externalDataAvailable = "1";
		analysExternalDataButton.setEnabled(true);
		regressExternalDataButton.setEnabled(true);
	}
	else 
	{     
	   	externalDataAvailable = "0";
		analysExternalDataButton.setEnabled(false);
		regressExternalDataButton.setEnabled(false);
	}
	  
	if ( ! externalDataAvailable.equals(oldVal) ) {
	    if ( preferences.containsKey(externalDataAvailableKey) ) 
	    	preferences.remove(externalDataAvailableKey);
	    preferences.put(externalDataAvailableKey, externalDataAvailable);
	}	     

    }

    else if ( event.getSource() == nwpGdasUseCheck ) {
    
	String oldVal =  nwpGdasUse;

	if ( nwpGdasUseCheck.isSelected() )
	    nwpGdasUse="1";
	else
	    nwpGdasUse="0";
	    
	if ( ! nwpGdasUse.equals(oldVal) ) {
	    if ( preferences.containsKey(nwpGdasUseKey) ) 
	    	preferences.remove(nwpGdasUseKey);
	    preferences.put(nwpGdasUseKey, nwpGdasUse);
	}	     
    }

    else if ( event.getSource() == nwpEcmwfUseCheck ) {
    
	String oldVal =  nwpEcmwfUse;

	if ( nwpEcmwfUseCheck.isSelected() )
	    nwpEcmwfUse="1";
	else
	    nwpEcmwfUse="0";
	    
	if ( ! nwpEcmwfUse.equals(oldVal) ) {
	    if ( preferences.containsKey(nwpEcmwfUseKey) ) 
	    	preferences.remove(nwpEcmwfUseKey);
	    preferences.put(nwpEcmwfUseKey, nwpEcmwfUse);
	}	     
    }
    
    else if ( event.getSource() == nwpGfsUseCheck ) {
    
	String oldVal =  nwpGfsUse;

	if ( nwpGfsUseCheck.isSelected() )
	    nwpGfsUse="1";
	else
	    nwpGfsUse="0";
	    
	if ( ! nwpGfsUse.equals(oldVal) ) {
	    if ( preferences.containsKey(nwpGfsUseKey) ) 
	    	preferences.remove(nwpGfsUseKey);
	    preferences.put(nwpGfsUseKey, nwpGfsUse);
	}	     
    }

    else if ( event.getSource() == extBkgAtmUseCheck ) {
    
	String oldVal =  extBkgAtmUse;

	if ( extBkgAtmUseCheck.isSelected() )
	    extBkgAtmUse="1";
	else
	    extBkgAtmUse="0";
	    
	if ( ! extBkgAtmUse.equals(oldVal) ) {
	    if ( preferences.containsKey(extBkgAtmUseKey) ) 
	    	preferences.remove(extBkgAtmUseKey);
	    preferences.put(extBkgAtmUseKey, extBkgAtmUse);
	}	     
    }

    
  
  } 

  
  
  /**
   * Actions to take when those buttons are clicked
   */
  public void actionPerformed(ActionEvent event) 
  { 

    if ( event.getSource() == okButton ) {
        setVisible(false);
    }

    else if ( event.getSource() == cancelButton ) {
    
	preferences.clear();
	setVisible(false);
    }

    else if ( event.getSource() == analysExternalDataButton ) {
    	String oldVal = externalDataSrc;
	if ( oldVal == null || !externalDataSrc.equals("1") );
    	    preferences.put("externalDataSrc", "1");
     }
    else if ( event.getSource() == regressExternalDataButton ) {
     	String oldVal = externalDataSrc;
	if ( oldVal == null || !externalDataSrc.equals("2") );
   	    preferences.put("externalDataSrc", "2");
    }
    else if ( event.getSource() == geoLatLonLimitButton ) {

	minLatField.setEditable(true);
	maxLatField.setEditable(true);
  	minLonField.setEditable(true);
	maxLonField.setEditable(true);

	String oldVal = geoLimit;
	geoLimit = "1";
	
	if ( !geoLimit.equals(oldVal) ) {
	    if(preferences.containsKey(geoLimitKey))
		preferences.remove(geoLimitKey);
	    preferences.put(geoLimitKey, geoLimit);
	}		
    }
    else if ( event.getSource() == geoAllLimitButton ) {
    
	minLatField.setEditable(false);
	maxLatField.setEditable(false);
  	minLonField.setEditable(false);
	maxLonField.setEditable(false);
	
	String oldVal = geoLimit;
	geoLimit = "0";
	
	if ( !geoLimit.equals(oldVal) ) {
	    if(preferences.containsKey(geoLimitKey))
		preferences.remove(geoLimitKey);
	    preferences.put(geoLimitKey, geoLimit);
	}
    }
    else if ( event.getSource() == geoOceanLimitButton ) {
    
	minLatField.setEditable(false);
	maxLatField.setEditable(false);
  	minLonField.setEditable(false);
	maxLonField.setEditable(false);

	String oldVal = geoLimit;
	geoLimit = "2";
	
	if ( !geoLimit.equals(oldVal) ) {
	    if(preferences.containsKey(geoLimitKey))
		preferences.remove(geoLimitKey);
	    preferences.put(geoLimitKey, geoLimit);
	}
    }
    else if ( event.getSource() == geoLandLimitButton ) {
    
	minLatField.setEditable(false);
	maxLatField.setEditable(false);
  	minLonField.setEditable(false);
	maxLonField.setEditable(false);
	
 	String oldVal = geoLimit;
	geoLimit = "3";
	
	if ( !geoLimit.equals(oldVal) ) {
	    if(preferences.containsKey(geoLimitKey))
		preferences.remove(geoLimitKey);
	    preferences.put(geoLimitKey, geoLimit);
	}
    }


    else if ( event.getSource() == ascendButton ) {
	String oldVal = cend;
	cend = "0";	
	if ( !cend.equals(oldVal) ) {
	    if(preferences.containsKey(cendKey))
		preferences.remove(cendKey);
	    preferences.put(cendKey, cend);
	}
    }
    else if ( event.getSource() == descendButton ) {
	String oldVal = cend;
	cend = "1";
	if ( !cend.equals(oldVal) ) {
	    if(preferences.containsKey(cendKey))
		preferences.remove(cendKey);
	    preferences.put(cendKey, cend);
	}
    }
    else if ( event.getSource() == combineButton ) {
	String oldVal = cend;
	cend = "2";	
	if ( !cend.equals(oldVal) ) {
	    if(preferences.containsKey(cendKey))
		preferences.remove(cendKey);
	    preferences.put(cendKey, cend);
	}
    }


    else if ( event.getSource() == sensor2ScanIndex1Button ) { 
	String oldVal = scanLineIndexSensor2TimeColloc;
	scanLineIndexSensor2TimeColloc = "1";
	if ( !scanLineIndexSensor2TimeColloc.equals(oldVal) ) {
	    if(preferences.containsKey(scanLineIndexSensor2TimeCollocKey))
		preferences.remove(scanLineIndexSensor2TimeCollocKey);
	    preferences.put(scanLineIndexSensor2TimeCollocKey, scanLineIndexSensor2TimeColloc);
	}
    }
    else if ( event.getSource() == sensor2ScanIndex2Button ) { 
	String oldVal = scanLineIndexSensor2TimeColloc;
	scanLineIndexSensor2TimeColloc = "2";
	if ( !scanLineIndexSensor2TimeColloc.equals(oldVal) ) {
	    if(preferences.containsKey(scanLineIndexSensor2TimeCollocKey))
		preferences.remove(scanLineIndexSensor2TimeCollocKey);
	    preferences.put(scanLineIndexSensor2TimeCollocKey, scanLineIndexSensor2TimeColloc);
	}
    }
    else if ( event.getSource() == sensor2ScanIndex3Button ) {  
 	String oldVal = scanLineIndexSensor2TimeColloc;
	scanLineIndexSensor2TimeColloc = "3";
	if ( !scanLineIndexSensor2TimeColloc.equals(oldVal) ) {
	    if(preferences.containsKey(scanLineIndexSensor2TimeCollocKey))
		preferences.remove(scanLineIndexSensor2TimeCollocKey);
	    preferences.put(scanLineIndexSensor2TimeCollocKey, scanLineIndexSensor2TimeColloc);
	}
   }

    else if ( event.getSource() == biasComputeSimpleMethodButton ) {
 	String oldVal = biasComputeMethod;
	biasComputeMethod = "0";
	if ( !biasComputeMethod.equals(oldVal) ) {
	    if(preferences.containsKey(biasComputeMethodKey))
		preferences.remove(biasComputeMethodKey);
	    preferences.put(biasComputeMethodKey, biasComputeMethod);
	}
    }
    else if ( event.getSource() == biasComputeHistogramMethodButton ) {
 	String oldVal = biasComputeMethod;
	biasComputeMethod = "1";
	if ( !biasComputeMethod.equals(oldVal) ) {
	    if(preferences.containsKey(biasComputeMethodKey))
		preferences.remove(biasComputeMethodKey);
	    preferences.put(biasComputeMethodKey, biasComputeMethod);
	}
    }

    else if ( event.getSource() == noneRegressionBiasApplyDomainButton ) { 
 	String oldVal = regressionBiasApplyDomain;
	regressionBiasApplyDomain = "-2";
	if ( !regressionBiasApplyDomain.equals(oldVal) ) {
	    if(preferences.containsKey(regressionBiasApplyDomainKey))
		preferences.remove(regressionBiasApplyDomainKey);
	    preferences.put(regressionBiasApplyDomainKey, regressionBiasApplyDomain);
	}
    }
    else if ( event.getSource() == allRegressionBiasApplyDomainButton ) { 
 	String oldVal = regressionBiasApplyDomain;
	regressionBiasApplyDomain = "-1";
	if ( !regressionBiasApplyDomain.equals(oldVal) ) {
	    if(preferences.containsKey(regressionBiasApplyDomainKey))
		preferences.remove(regressionBiasApplyDomainKey);
	    preferences.put(regressionBiasApplyDomainKey, regressionBiasApplyDomain);
	}
    }
    /**
    else if ( event.getSource() == oceanRegressionBiasApplyDomainButton ) { 
 	String oldVal = regressionBiasApplyDomain;
	regressionBiasApplyDomain = "0";
	if ( !regressionBiasApplyDomain.equals(oldVal) ) {
	    if(preferences.containsKey(regressionBiasApplyDomainKey))
		preferences.remove(regressionBiasApplyDomainKey);
	    preferences.put(regressionBiasApplyDomainKey, regressionBiasApplyDomain);
	}
    }
    else if ( event.getSource() == landRegressionBiasApplyDomainButton ) { 
 	String oldVal = regressionBiasApplyDomain;
	regressionBiasApplyDomain = "1";
	if ( !regressionBiasApplyDomain.equals(oldVal) ) {
	    if(preferences.containsKey(regressionBiasApplyDomainKey))
		preferences.remove(regressionBiasApplyDomainKey);
	    preferences.put(regressionBiasApplyDomainKey, regressionBiasApplyDomain);
	}
    }
    */

    else if ( event.getSource() == fullOrbitRetrievalButton ) {
	String oldVal = retrOnOrbitOrSubOrbit;
	retrOnOrbitOrSubOrbit = "0";
	if ( !retrOnOrbitOrSubOrbit.equals(oldVal) ) {
	    if(preferences.containsKey(retrOnOrbitOrSubOrbitKey))
		preferences.remove(retrOnOrbitOrSubOrbitKey);
	    preferences.put(retrOnOrbitOrSubOrbitKey, retrOnOrbitOrSubOrbit);
	}
    }
    else if ( event.getSource() == choppedOrbitRetrievalButton ) {
	String oldVal = retrOnOrbitOrSubOrbit;
	retrOnOrbitOrSubOrbit = "1";
	if ( !retrOnOrbitOrSubOrbit.equals(oldVal) ) {
	    if(preferences.containsKey(retrOnOrbitOrSubOrbitKey))
		preferences.remove(retrOnOrbitOrSubOrbitKey);
	    preferences.put(retrOnOrbitOrSubOrbitKey, retrOnOrbitOrSubOrbit);
	}
    }

    else if ( event.getSource() == empiricallyCorrectedTBButton ) {
	String oldVal = retrOnWhichSDR;
	retrOnWhichSDR = "0";
	if ( !retrOnWhichSDR.equals(oldVal) ) {
	    if(preferences.containsKey(retrOnWhichSDRKey))
		preferences.remove(retrOnWhichSDRKey);
	    preferences.put(retrOnWhichSDRKey, retrOnWhichSDR);
	}
    }
    else if ( event.getSource() == uncorrectedTBButton ) {    
	String oldVal = retrOnWhichSDR;
	retrOnWhichSDR = "1";
	if ( !retrOnWhichSDR.equals(oldVal) ) {
	    if(preferences.containsKey(retrOnWhichSDRKey))
		preferences.remove(retrOnWhichSDRKey);
	    preferences.put(retrOnWhichSDRKey, retrOnWhichSDR);
	}
    }
    else if ( event.getSource() == nwpTBButton ) {
	String oldVal = retrOnWhichSDR;
	retrOnWhichSDR = "2";
	if ( !retrOnWhichSDR.equals(oldVal) ) {
	    if(preferences.containsKey(retrOnWhichSDRKey))
		preferences.remove(retrOnWhichSDRKey);
	    preferences.put(retrOnWhichSDRKey, retrOnWhichSDR);
	}
    }

    else if ( event.getSource() == dynamicFwdMatrixButton ) {
	String oldVal = fwdMatrix2Use;
	fwdMatrix2Use = "0";
	if ( !fwdMatrix2Use.equals(oldVal) ) {
	    if(preferences.containsKey(fwdMatrix2UseKey))
		preferences.remove(fwdMatrix2UseKey);
	    preferences.put(fwdMatrix2UseKey, fwdMatrix2Use);
	}
    }
    else if ( event.getSource() == nonErrFwdMatrixButton ) {
	String oldVal = fwdMatrix2Use;
	fwdMatrix2Use = "1";
	if ( !fwdMatrix2Use.equals(oldVal) ) {
	    if(preferences.containsKey(fwdMatrix2UseKey))
		preferences.remove(fwdMatrix2UseKey);
	    preferences.put(fwdMatrix2UseKey, fwdMatrix2Use);
	}
    }

    else if ( event.getSource() == tdrAsciiFormatButton ) { 
	String oldVal = tdrFormat;
	tdrFormat = "0";
	if ( !tdrFormat.equals(oldVal) ) {
	    if(preferences.containsKey(tdrFormatKey))
		preferences.remove(tdrFormatKey);
	    preferences.put(tdrFormatKey, tdrFormat);
	}
    }
    else if ( event.getSource() == tdrBinaryFormatButton ) {
	String oldVal = tdrFormat;
	tdrFormat = "1";
	if ( !tdrFormat.equals(oldVal) ) {
	    if(preferences.containsKey(tdrFormatKey))
		preferences.remove(tdrFormatKey);
	    preferences.put(tdrFormatKey, tdrFormat);
	}
    }


    else if ( event.getSource() == fmCoarseTypeButton ) {
	String oldVal = fmType;
	fmType = "-1";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }
    else if ( event.getSource() == fmLowTypeButton ) {
	String oldVal = fmType;
	fmType = "0";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }
    else if ( event.getSource() == fmHighTypeButton ) {
	String oldVal = fmType;
	fmType = "1";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }
    
    else if ( event.getSource() == fmUasTypeButton ) {
	String oldVal = fmType;
	fmType = "0";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }
    else if ( event.getSource() == fmLasTypeButton ) {
	String oldVal = fmType;
	fmType = "1";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }
    else if ( event.getSource() == fmEnvTypeButton ) {
	String oldVal = fmType;
	fmType = "2";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }
    else if ( event.getSource() == fmImgTypeButton ) {
	String oldVal = fmType;
	fmType = "3";
	if ( !fmType.equals(oldVal) ) {
	    if(preferences.containsKey(fmTypeKey))
		preferences.remove(fmTypeKey);
	    preferences.put(fmTypeKey, fmType);
	}
    }

    else if ( event.getSource() == cpu0Button ) {
	String oldVal = useCPU;
	useCPU = "0";	
	if ( !useCPU.equals(oldVal) ) {
	    if(preferences.containsKey(useCPUKey))
		preferences.remove(useCPUKey);
	    preferences.put(useCPUKey, useCPU);
	}
    }
    else if ( event.getSource() == cpu1Button ) {
	String oldVal = useCPU;
	useCPU = "1";
	if ( !useCPU.equals(oldVal) ) {
	    if(preferences.containsKey(useCPUKey))
		preferences.remove(useCPUKey);
	    preferences.put(useCPUKey, useCPU);
	}
    }
    else if ( event.getSource() == cpu2Button ) {
	String oldVal = useCPU;
	useCPU = "2";	
	if ( !useCPU.equals(oldVal) ) {
	    if(preferences.containsKey(useCPUKey))
		preferences.remove(useCPUKey);
	    preferences.put(useCPUKey, useCPU);
	}
    }

    else if ( event.getSource() == rdrTypeTdrButton ) {
    
	String oldVal = rdrType;
	rdrType = "1";
	
	if ( !rdrType.equals(oldVal) ) {
	    if(preferences.containsKey(rdrTypeKey))
		preferences.remove(rdrTypeKey);
	    preferences.put(rdrTypeKey, rdrType);
	}
    }
    else if ( event.getSource() == rdrTypeSdrProxyButton ) {
    
	String oldVal = rdrType;
	rdrType = "2";
	
	if ( !rdrType.equals(oldVal) ) {
	    if(preferences.containsKey(rdrTypeKey))
		preferences.remove(rdrTypeKey);
	    preferences.put(rdrTypeKey, rdrType);
	}
    }
    else if ( event.getSource() == rdrTypeSdrButton ) {
    
	String oldVal = rdrType;
	rdrType = "3";
	
	if ( !rdrType.equals(oldVal) ) {
	    if(preferences.containsKey(rdrTypeKey))
		preferences.remove(rdrTypeKey);
	    preferences.put(rdrTypeKey, rdrType);
	}
    }
    else if ( event.getSource() == rdrTypeSdrRemapButton ) {
    
	String oldVal = rdrType;
	rdrType = "4";
	
	if ( !rdrType.equals(oldVal) ) {
	    if(preferences.containsKey(rdrTypeKey))
		preferences.remove(rdrTypeKey);
	    preferences.put(rdrTypeKey, rdrType);
	}
    }

  }


  /**
   * Actions to take when text fields get focus
   */
  public void focusGained(FocusEvent event)
  {
  }


  /**
   * Actions to take when text fields lost focus
   */
  public void focusLost(FocusEvent event)
  {

    if ( event.getSource() == minLatField ) {
	preferences.remove(minLatKey);
	preferences.put(minLatKey, minLatField.getText());
    }
    else if ( event.getSource() == maxLatField ) {
	preferences.remove(maxLatKey);
	preferences.put(maxLatKey, maxLatField.getText());
    }
    else if ( event.getSource() == minLonField ) {
	preferences.remove(minLonKey);
	preferences.put(minLonKey, minLonField.getText());
    }
    else if ( event.getSource() == maxLonField ) {
	preferences.remove(maxLonKey);
	preferences.put(maxLonKey, maxLonField.getText());
    }

    else if ( event.getSource() == nScanLineSensor1SkipField ) {
	preferences.remove(nScanLineSensor1SkipKey);
	preferences.put(nScanLineSensor1SkipKey, nScanLineSensor1SkipField.getText());
    }
    else if ( event.getSource() == nScanLineSensor2SkipField ) {
	preferences.remove(nScanLineSensor2SkipKey);
	preferences.put(nScanLineSensor2SkipKey, nScanLineSensor2SkipField.getText());
    }

    else if ( event.getSource() == maxDaysArchivedField ) {
	preferences.remove(maxDaysArchivedKey);
	preferences.put(maxDaysArchivedKey, maxDaysArchivedField.getText());
    }
    else if ( event.getSource() == dayUsed4BiasField ) {
	preferences.remove(dayUsed4BiasKey);
	preferences.put(dayUsed4BiasKey, dayUsed4BiasField.getText());
    }
    else if ( event.getSource() == dayUsed4AlgField ) {
	preferences.remove(dayUsed4AlgKey);
	preferences.put(dayUsed4AlgKey, dayUsed4AlgField.getText());
    }

    else if ( event.getSource() == nChoppedFilesPerOrbitField ) {
	preferences.remove(nChoppedFilesPerOrbitKey);
	preferences.put(nChoppedFilesPerOrbitKey, nChoppedFilesPerOrbitField.getText());
    }

    else if ( event.getSource() == gifDensityField ) {
	preferences.remove(gifDensityKey);
	preferences.put(gifDensityKey, gifDensityField.getText());
    }

    else if ( event.getSource() == gridFactorField ) {
	preferences.remove(gridFactorKey);
	preferences.put(gridFactorKey, gridFactorField.getText());
    }

    else if ( event.getSource() == emailField ) {
	preferences.remove(emailKey);
	preferences.put(emailKey, emailField.getText());
    }

    else if ( event.getSource() == websiteField ) {
	preferences.remove(websiteKey);
	preferences.put(websiteKey, websiteField.getText());
    }

    else if ( event.getSource() == nAttemptsField ) {
	preferences.remove(nAttemptsKey);
	preferences.put(nAttemptsKey, nAttemptsField.getText());
    }
    else if ( event.getSource() == prefixFMAccuracyField ) {
	preferences.remove(prefixFMAccuracyKey);
	preferences.put(prefixFMAccuracyKey, prefixFMAccuracyField.getText());
    }

  }


  /**
   * Actions to take when different tree nodes are selected
   */
  public void valueChanged(TreeSelectionEvent event) 
  { 
    TreePath path = tree.getSelectionPath();
    if (path == null) return;
    DefaultMutableTreeNode selectedNode = (DefaultMutableTreeNode) path.getLastPathComponent();

    if ( selectedNode == geoNode ) {

	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(4,1);
	gridLayout.setVgap(5);

	JPanel titlePanel    = new JPanel();
	JPanel geoLimitPanel = new JPanel(new GridLayout(2,2));
	JPanel latLonPanel   = new JPanel();
	JPanel labelPanel    = new JPanel(gridLayout);
	JPanel fieldPanel    = new JPanel(gridLayout);
	JPanel cendPanel     = new JPanel(new GridLayout(3,1));

	titlePanel.add(new JLabel("Geographic Specification"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);

	String geoLimitTitle = new String("Select geographic limit");
	TitledBorder geoLimitTitleBorder=BorderFactory.createTitledBorder(blackline, geoLimitTitle);
	geoLimitPanel.setBorder(geoLimitTitleBorder);	
	geoLimitPanel.add(geoAllLimitButton);
	geoLimitPanel.add(geoLatLonLimitButton);
	geoLimitPanel.add(geoOceanLimitButton);
	geoLimitPanel.add(geoLandLimitButton);
	ButtonGroup buttonGroup = new ButtonGroup();
	buttonGroup.add(geoAllLimitButton);
	buttonGroup.add(geoLatLonLimitButton);
	buttonGroup.add(geoOceanLimitButton);
	buttonGroup.add(geoLandLimitButton);


	String latLonTitle = new String("Latitude/Longitude Box");
	TitledBorder latLonTitleBorder=BorderFactory.createTitledBorder(blackline, latLonTitle);
	latLonPanel.setBorder(latLonTitleBorder);

	String cendTitle = new String("Orbit Selection");
	TitledBorder cendTitleBorder=BorderFactory.createTitledBorder(blackline, cendTitle);
	cendPanel.setBorder(cendTitleBorder);

	
	rightPanel.add(titlePanel);
	rightPanel.add(geoLimitPanel);
	rightPanel.add(latLonPanel);
	rightPanel.add(cendPanel);

	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints geoLimitCons = rightLayout.getConstraints(geoLimitPanel);
        geoLimitCons.setX(Spring.constant(50));
        geoLimitCons.setY(Spring.constant(75));
        geoLimitCons.setWidth(Spring.constant(525));
        geoLimitCons.setHeight(Spring.constant(100));
	
	SpringLayout.Constraints latLonCons = rightLayout.getConstraints(latLonPanel);
        latLonCons.setX(Spring.constant(50));
        latLonCons.setY(Spring.constant(225));
        latLonCons.setWidth(Spring.constant(525));
        latLonCons.setHeight(Spring.constant(200));
	
	SpringLayout.Constraints cendCons = rightLayout.getConstraints(cendPanel);
        cendCons.setX(Spring.constant(50));
        cendCons.setY(Spring.constant(475));
        cendCons.setWidth(Spring.constant(525));
        cendCons.setHeight(Spring.constant(125));
	
	
	latLonPanel.add(labelPanel);
	latLonPanel.add(fieldPanel);
	
	SpringLayout latLonSpringLayout = new SpringLayout();
	latLonPanel.setLayout(latLonSpringLayout);
	SpringLayout.Constraints labelCons = latLonSpringLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(75));
        labelCons.setY(Spring.constant(25));
        labelCons.setWidth(Spring.constant(150));
        labelCons.setHeight(Spring.constant(100));

	SpringLayout.Constraints fieldCons = latLonSpringLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(250));
        fieldCons.setY(Spring.constant(25));
        fieldCons.setWidth(Spring.constant(200));
        fieldCons.setHeight(Spring.constant(100));
	
	labelPanel.add(minLatLabel);
	labelPanel.add(maxLatLabel);
	labelPanel.add(minLonLabel);
	labelPanel.add(maxLonLabel);

	fieldPanel.add(minLatField);
	fieldPanel.add(maxLatField);
	fieldPanel.add(minLonField);
	fieldPanel.add(maxLonField);


	ButtonGroup cendButtonGroup = new ButtonGroup();
	cendButtonGroup.add(ascendButton);
	cendButtonGroup.add(descendButton);
	cendButtonGroup.add(combineButton);
	
	cendPanel.add(ascendButton);
	cendPanel.add(descendButton);
	cendPanel.add(combineButton);


	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == externalNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(16,1);
	gridLayout.setVgap(5);

	JPanel titlePanel   = new JPanel();
	JPanel availablePanel = new JPanel();
	JPanel srcPanel = new JPanel();
	JPanel nwpPanel = new JPanel(new GridLayout(3,1));

	titlePanel.add(new JLabel("External Data Use"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);

	String availableTitle = new String("External Data Availability");
	TitledBorder availableTitleBorder=BorderFactory.createTitledBorder(blackline, availableTitle);
	availablePanel.setBorder(availableTitleBorder);	
	availablePanel.add(externalDataAvailableCheck);

	String srcTitle = new String("External Data Source");
	TitledBorder srcTitleBorder=BorderFactory.createTitledBorder(blackline, srcTitle);
	srcPanel.setBorder(srcTitleBorder);	
	srcPanel.add( analysExternalDataButton );
	srcPanel.add( regressExternalDataButton );
	ButtonGroup buttonGroup = new ButtonGroup();
	buttonGroup.add(analysExternalDataButton);
	buttonGroup.add(regressExternalDataButton);

	String nwpTitle = new String("External Data for NWP Collocation");
	TitledBorder nwpTitleBorder=BorderFactory.createTitledBorder(blackline, nwpTitle);
	nwpPanel.setBorder(nwpTitleBorder);	
	nwpPanel.add(nwpGdasUseCheck);
	nwpPanel.add(nwpEcmwfUseCheck);
	nwpPanel.add(nwpGfsUseCheck);

	rightPanel.add(titlePanel);
	rightPanel.add(availablePanel);
	rightPanel.add(srcPanel);
	rightPanel.add(nwpPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints availableCons = rightLayout.getConstraints(availablePanel);
        availableCons.setX(Spring.constant(50));
        availableCons.setY(Spring.constant(45));
        availableCons.setWidth(Spring.constant(525));
        availableCons.setHeight(Spring.constant(75));
	
	SpringLayout.Constraints srcCons = rightLayout.getConstraints(srcPanel);
        srcCons.setX(Spring.constant(50));
        srcCons.setY(Spring.constant(130));
        srcCons.setWidth(Spring.constant(525));
        srcCons.setHeight(Spring.constant(75));
	
	SpringLayout.Constraints nwpCons = rightLayout.getConstraints(nwpPanel);
        nwpCons.setX(Spring.constant(50));
        nwpCons.setY(Spring.constant(215));
        nwpCons.setWidth(Spring.constant(525));
        nwpCons.setHeight(Spring.constant(150));

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == extBkgNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(16,1);
	gridLayout.setVgap(5);

	JPanel titlePanel   = new JPanel();
	JPanel useExtBkgPanel = new JPanel(new GridLayout(3,1));

	titlePanel.add(new JLabel("External Background/Covariance Use"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);

	String useExtBkgTitle = new String("External Atm/Sfc Background Use");
	TitledBorder useExtBkgTitleBorder=BorderFactory.createTitledBorder(blackline, useExtBkgTitle);
	useExtBkgPanel.setBorder(useExtBkgTitleBorder);	
	useExtBkgPanel.add(extBkgAtmUseCheck);

	rightPanel.add(titlePanel);
	rightPanel.add(useExtBkgPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints useExtBkgCons = rightLayout.getConstraints(useExtBkgPanel);
        useExtBkgCons.setX(Spring.constant(50));
        useExtBkgCons.setY(Spring.constant(45));
        useExtBkgCons.setWidth(Spring.constant(525));
        useExtBkgCons.setHeight(Spring.constant(75));
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == retrievalNode ) {
   
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(8,1);
	gridLayout.setVgap(5);

	JPanel titlePanel = new JPanel();
	JPanel nAttempsPanel = new JPanel();
	JPanel nChoppedFilesPerOrbitPanel = new JPanel();
	JPanel retrOnOrbitOrSubOrbitPanel = new JPanel(new GridLayout(2,1));
	JPanel retrOnWhichSDRPanel = new JPanel(new GridLayout(1,3));
	JPanel fwdMatrix2UsePanel = new JPanel(new GridLayout(2,1));

	titlePanel.add(new JLabel("Retrieval Options"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	Border blackline = BorderFactory.createLineBorder(Color.black);


	String nAttempsTitle = new String("Number of retrieval attempts in case of non-convergence");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, nAttempsTitle);
	nAttempsPanel.setBorder(titleBorder);

	String nChoppedFilesPerOrbitTitle = new String("Number of chopped sub orbits per orbit");
	titleBorder=BorderFactory.createTitledBorder(blackline, nChoppedFilesPerOrbitTitle);
	nChoppedFilesPerOrbitPanel.setBorder(titleBorder);
	
	String retrOnOrbitOrSubOrbitTitle = new String("Full orbit or chopped ones");
	titleBorder=BorderFactory.createTitledBorder(blackline, retrOnOrbitOrSubOrbitTitle);
	retrOnOrbitOrSubOrbitPanel.setBorder(titleBorder);
	
	String fwdMatrix2UseTitle = new String("FWD model error matrix");
	titleBorder=BorderFactory.createTitledBorder(blackline, fwdMatrix2UseTitle);
	fwdMatrix2UsePanel.setBorder(titleBorder);
	
	String retrOnWhichSDRTitle = new String("Which SDR to retrieve");
	titleBorder=BorderFactory.createTitledBorder(blackline, retrOnWhichSDRTitle);
	retrOnWhichSDRPanel.setBorder(titleBorder);
	
	nAttempsPanel.add(nAttemptsField);
	nChoppedFilesPerOrbitPanel.add(nChoppedFilesPerOrbitField);

	retrOnOrbitOrSubOrbitPanel.add(fullOrbitRetrievalButton);
	retrOnOrbitOrSubOrbitPanel.add(choppedOrbitRetrievalButton);
	ButtonGroup fullChoppeButtonGroup = new ButtonGroup();
	fullChoppeButtonGroup.add(fullOrbitRetrievalButton);
	fullChoppeButtonGroup.add(choppedOrbitRetrievalButton);
	
	retrOnWhichSDRPanel.add(empiricallyCorrectedTBButton);
	retrOnWhichSDRPanel.add(uncorrectedTBButton);
	retrOnWhichSDRPanel.add(nwpTBButton);
	ButtonGroup sdrButtonGroup = new ButtonGroup();
	sdrButtonGroup.add(empiricallyCorrectedTBButton);
	sdrButtonGroup.add(uncorrectedTBButton);
	sdrButtonGroup.add(nwpTBButton);
	
	fwdMatrix2UsePanel.add(dynamicFwdMatrixButton);
	fwdMatrix2UsePanel.add(nonErrFwdMatrixButton);
	ButtonGroup fwdButtonGroup = new ButtonGroup();
	fwdButtonGroup.add(dynamicFwdMatrixButton);
	fwdButtonGroup.add(nonErrFwdMatrixButton);

	rightPanel.add(titlePanel);
	rightPanel.add(nChoppedFilesPerOrbitPanel);
	rightPanel.add(retrOnOrbitOrSubOrbitPanel);
	rightPanel.add(retrOnWhichSDRPanel);
	rightPanel.add(fwdMatrix2UsePanel);
	rightPanel.add(nAttempsPanel);

	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints retrOnOrbitOrSubOrbitCons = rightLayout.getConstraints(retrOnOrbitOrSubOrbitPanel);
        retrOnOrbitOrSubOrbitCons.setX(Spring.constant(50));
        retrOnOrbitOrSubOrbitCons.setY(Spring.constant(75));
        retrOnOrbitOrSubOrbitCons.setWidth(Spring.constant(525));
        retrOnOrbitOrSubOrbitCons.setHeight(Spring.constant(80));

	SpringLayout.Constraints nChoppedFilesPerOrbitCons = rightLayout.getConstraints(nChoppedFilesPerOrbitPanel);
        nChoppedFilesPerOrbitCons.setX(Spring.constant(50));
        nChoppedFilesPerOrbitCons.setY(Spring.constant(170));
        nChoppedFilesPerOrbitCons.setWidth(Spring.constant(525));
        nChoppedFilesPerOrbitCons.setHeight(Spring.constant(60));

	SpringLayout.Constraints retrOnWhichSDRCons = rightLayout.getConstraints(retrOnWhichSDRPanel);
        retrOnWhichSDRCons.setX(Spring.constant(50));
        retrOnWhichSDRCons.setY(Spring.constant(245));
        retrOnWhichSDRCons.setWidth(Spring.constant(525));
        retrOnWhichSDRCons.setHeight(Spring.constant(60));
	
	SpringLayout.Constraints fwdMatrix2UseCons = rightLayout.getConstraints(fwdMatrix2UsePanel);
        fwdMatrix2UseCons.setX(Spring.constant(50));
        fwdMatrix2UseCons.setY(Spring.constant(320));
        fwdMatrix2UseCons.setWidth(Spring.constant(525));
        fwdMatrix2UseCons.setHeight(Spring.constant(80));

	SpringLayout.Constraints nAttempsCons = rightLayout.getConstraints(nAttempsPanel);
        nAttempsCons.setX(Spring.constant(50));
        nAttempsCons.setY(Spring.constant(415));
        nAttempsCons.setWidth(Spring.constant(525));
        nAttempsCons.setHeight(Spring.constant(60));

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == monitorNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(3,1);
	gridLayout.setVgap(5);

	JPanel titlePanel = new JPanel();
	JPanel monitorPanel = new JPanel(gridLayout);
	
	titlePanel.add(new JLabel("Monitoring Options"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	String monitorTitle = new String("Monitoring Setting");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, monitorTitle);
	monitorPanel.setBorder(titleBorder);
	
	monitorPanel.add(monitorIterativeCheck);
	monitorPanel.add(monitorRetrievalCheck);
	monitorPanel.add(monitorFwdCheck);

	rightPanel.add(titlePanel);
	rightPanel.add(monitorPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints monitorCons = rightLayout.getConstraints(monitorPanel);
        monitorCons.setX(Spring.constant(50));
        monitorCons.setY(Spring.constant(150));
        monitorCons.setWidth(Spring.constant(525));
        monitorCons.setHeight(Spring.constant(150));
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == runTimeNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();

	JPanel titlePanel   = new JPanel();
	JPanel runTimePanel = new JPanel();
	JPanel cpuPanel     = new JPanel(new GridLayout(3,1));
	
	titlePanel.add(new JLabel("Make/Run Options"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	
	String runTimeTitle = new String("Make Setting");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, runTimeTitle);
	runTimePanel.setBorder(titleBorder);
	
	String cpuTitle = new String("CPU Usage Setting");
	TitledBorder cpuTitleBorder=BorderFactory.createTitledBorder(blackline, cpuTitle);
	cpuPanel.setBorder(cpuTitleBorder);
	
	runTimePanel.add(makeOrNotCheck);
	//runTimePanel.add(useCPUCheck);

	rightPanel.add(titlePanel);
	rightPanel.add(runTimePanel);
	rightPanel.add(cpuPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints runTimeCons = rightLayout.getConstraints(runTimePanel);
        runTimeCons.setX(Spring.constant(50));
        runTimeCons.setY(Spring.constant(150));
        runTimeCons.setWidth(Spring.constant(525));
        runTimeCons.setHeight(Spring.constant(60));
	
	SpringLayout.Constraints cpuCons = rightLayout.getConstraints(cpuPanel);
        cpuCons.setX(Spring.constant(50));
        cpuCons.setY(Spring.constant(275));
        cpuCons.setWidth(Spring.constant(525));
        cpuCons.setHeight(Spring.constant(125));
	
	ButtonGroup cpuButtonGroup = new ButtonGroup();
	cpuButtonGroup.add(cpu0Button);
	cpuButtonGroup.add(cpu1Button);
	cpuButtonGroup.add(cpu2Button);
	
	cpuPanel.add(cpu0Button);
	cpuPanel.add(cpu1Button);
	cpuPanel.add(cpu2Button);
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == miscNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(2,1);
	gridLayout.setVgap(5);

	JPanel titlePanel = new JPanel();
	JPanel addDeviceNoisePanel = new JPanel();
	JPanel tdrFormatPanel  = new JPanel(gridLayout);
	JPanel gifDensityPanel = new JPanel();
	JPanel gridFactorPanel = new JPanel();
	JPanel emailPanel = new JPanel();
	JPanel websitePanel = new JPanel();
	
	titlePanel.add(new JLabel("Miscellaneous Options"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	
	String addDeviceNoiseTitle = new String("Noise option");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, addDeviceNoiseTitle);
	addDeviceNoisePanel.setBorder(titleBorder);

	String tdrFormatTitle = new String("Format of TDR Data");
	TitledBorder tdrTitleBorder=BorderFactory.createTitledBorder(blackline, tdrFormatTitle);
	tdrFormatPanel.setBorder(tdrTitleBorder);
	
	String gifDensityTitle = new String("Density used to convert PS file into gif image (default is 100)");
	TitledBorder gifTitleBorder=BorderFactory.createTitledBorder(blackline, gifDensityTitle);
	gifDensityPanel.setBorder(gifTitleBorder);
	
	String gridFactorTitle = new String("Grid factor used to grid level III data (default is 4)");
	TitledBorder gridTitleBorder=BorderFactory.createTitledBorder(blackline, gridFactorTitle);
	gridFactorPanel.setBorder(gridTitleBorder);
	
	String emailTitle = new String("Comma separated email addresses to notify");
	TitledBorder emailTitleBorder=BorderFactory.createTitledBorder(blackline, emailTitle);
	emailPanel.setBorder(emailTitleBorder);
	
	String websiteTitle = new String("Website address to view result");
	TitledBorder websiteTitleBorder=BorderFactory.createTitledBorder(blackline, websiteTitle);
	websitePanel.setBorder(websiteTitleBorder);
	
        JPanel rdrTypePanel = new JPanel(new GridLayout(2,2));

	String rdrTypeTitle = new String("Select RDR for NPP/ATMS");
	TitledBorder rdrTypeTitleBorder=BorderFactory.createTitledBorder(blackline, rdrTypeTitle);
	rdrTypePanel.setBorder(rdrTypeTitleBorder);	
	
        rdrTypePanel.add(rdrTypeTdrButton);
	rdrTypePanel.add(rdrTypeSdrProxyButton);
	rdrTypePanel.add(rdrTypeSdrButton);
	rdrTypePanel.add(rdrTypeSdrRemapButton);
	ButtonGroup rdrTypeButtonGroup = new ButtonGroup();
	rdrTypeButtonGroup.add(rdrTypeTdrButton);
	rdrTypeButtonGroup.add(rdrTypeSdrProxyButton);
	rdrTypeButtonGroup.add(rdrTypeSdrButton);
	rdrTypeButtonGroup.add(rdrTypeSdrRemapButton);

	addDeviceNoisePanel.add(addDeviceNoiseCheck); 
	
	tdrFormatPanel.add(tdrAsciiFormatButton);
	tdrFormatPanel.add(tdrBinaryFormatButton);
	ButtonGroup buttonGroup = new ButtonGroup();
	buttonGroup.add(tdrAsciiFormatButton);
	buttonGroup.add(tdrBinaryFormatButton);
	
	gifDensityPanel.add(gifDensityField);
	gridFactorPanel.add(gridFactorField);
	emailPanel.add(emailField);
	websitePanel.add(websiteField);
	
	rightPanel.add(titlePanel);
	rightPanel.add(addDeviceNoisePanel);
	rightPanel.add(tdrFormatPanel);
	rightPanel.add(gifDensityPanel);
	rightPanel.add(gridFactorPanel);
	rightPanel.add(emailPanel);
	rightPanel.add(websitePanel);
	rightPanel.add(rdrTypePanel);
        
        
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints addDeviceNoiseCons = rightLayout.getConstraints(addDeviceNoisePanel);
        addDeviceNoiseCons.setX(Spring.constant(50));
        addDeviceNoiseCons.setY(Spring.constant(50));
        addDeviceNoiseCons.setWidth(Spring.constant(525));
        addDeviceNoiseCons.setHeight(Spring.constant(60));
	
	SpringLayout.Constraints tdrFormatCons = rightLayout.getConstraints(tdrFormatPanel);
        tdrFormatCons.setX(Spring.constant(50));
        tdrFormatCons.setY(Spring.constant(130));
        tdrFormatCons.setWidth(Spring.constant(525));
        tdrFormatCons.setHeight(Spring.constant(100));
	
	SpringLayout.Constraints gifDensityCons = rightLayout.getConstraints(gifDensityPanel);
        gifDensityCons.setX(Spring.constant(50));
        gifDensityCons.setY(Spring.constant(250));
        gifDensityCons.setWidth(Spring.constant(525));
        gifDensityCons.setHeight(Spring.constant(65));
	
	SpringLayout.Constraints gridFactorCons = rightLayout.getConstraints(gridFactorPanel);
        gridFactorCons.setX(Spring.constant(50));
        gridFactorCons.setY(Spring.constant(335));
        gridFactorCons.setWidth(Spring.constant(525));
        gridFactorCons.setHeight(Spring.constant(65));
	
	SpringLayout.Constraints emailCons = rightLayout.getConstraints(emailPanel);
        emailCons.setX(Spring.constant(50));
        emailCons.setY(Spring.constant(420));
        emailCons.setWidth(Spring.constant(525));
        emailCons.setHeight(Spring.constant(65));
	
	SpringLayout.Constraints websiteCons = rightLayout.getConstraints(websitePanel);
        websiteCons.setX(Spring.constant(50));
        websiteCons.setY(Spring.constant(505));
        websiteCons.setWidth(Spring.constant(525));
        websiteCons.setHeight(Spring.constant(65));
	
	SpringLayout.Constraints rdrTypeCons = rightLayout.getConstraints(rdrTypePanel);
        rdrTypeCons.setX(Spring.constant(50));
        rdrTypeCons.setY(Spring.constant(590));
        rdrTypeCons.setWidth(Spring.constant(525));
        rdrTypeCons.setHeight(Spring.constant(100));

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == synchroNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	
	JPanel titlePanel = new JPanel();
	JPanel sensor1SkipPanel = new JPanel();
	JPanel sensor2SkipPanel = new JPanel();
	JPanel sensor2ScanIndexPanel = new JPanel(new GridLayout(3,1));
	
	titlePanel.add(new JLabel("Sensors Synchronization"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	
	String sensor1SkipTitle = new String("Number of Sensor1 scan lines to skip upfront");
	TitledBorder sensor1TitleBorder=BorderFactory.createTitledBorder(blackline, sensor1SkipTitle);
	sensor1SkipPanel.setBorder(sensor1TitleBorder);
	
	String sensor2SkipTitle = new String("Number of Sensor2 scan lines to skip upfront");
	TitledBorder sensor2TitleBorder=BorderFactory.createTitledBorder(blackline, sensor2SkipTitle);
	sensor2SkipPanel.setBorder(sensor2TitleBorder);
	
	String sensor2ScanIndexTitle = new String("Sensor2 scanline index that corresponds in time to Sensor1");
	TitledBorder sensor2ScanTitleBorder=BorderFactory.createTitledBorder(blackline, sensor2ScanIndexTitle);
	sensor2ScanIndexPanel.setBorder(sensor2ScanTitleBorder);
	
	//sensor1SkipPanel.add(nScanLineSensor1SkipLabel);
	sensor1SkipPanel.add(nScanLineSensor1SkipField);

	//sensor2SkipPanel.add(nScanLineSensor2SkipLabel);
	sensor2SkipPanel.add(nScanLineSensor2SkipField);
	
	sensor2ScanIndexPanel.add(sensor2ScanIndex1Button);
	sensor2ScanIndexPanel.add(sensor2ScanIndex2Button);
	sensor2ScanIndexPanel.add(sensor2ScanIndex3Button);
	ButtonGroup buttonGroup = new ButtonGroup();
	buttonGroup.add(sensor2ScanIndex1Button);
	buttonGroup.add(sensor2ScanIndex2Button);
	buttonGroup.add(sensor2ScanIndex3Button);


	rightPanel.add(titlePanel);
	rightPanel.add(sensor1SkipPanel);
	rightPanel.add(sensor2SkipPanel);
	rightPanel.add(sensor2ScanIndexPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints sensor1SkipCons = rightLayout.getConstraints(sensor1SkipPanel);
        sensor1SkipCons.setX(Spring.constant(50));
        sensor1SkipCons.setY(Spring.constant(150));
        sensor1SkipCons.setWidth(Spring.constant(525));
        sensor1SkipCons.setHeight(Spring.constant(65));
	
	SpringLayout.Constraints sensor2SkipCons = rightLayout.getConstraints(sensor2SkipPanel);
        sensor2SkipCons.setX(Spring.constant(50));
        sensor2SkipCons.setY(Spring.constant(275));
        sensor2SkipCons.setWidth(Spring.constant(525));
        sensor2SkipCons.setHeight(Spring.constant(65));
	
	SpringLayout.Constraints sensor2ScanIndexCons = rightLayout.getConstraints(sensor2ScanIndexPanel);
        sensor2ScanIndexCons.setX(Spring.constant(50));
        sensor2ScanIndexCons.setY(Spring.constant(400));
        sensor2ScanIndexCons.setWidth(Spring.constant(525));
        sensor2ScanIndexCons.setHeight(Spring.constant(100));

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == methodNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	
	JPanel titlePanel = new JPanel();
	JPanel biasComputePanel = new JPanel(new GridLayout(2,1));
	JPanel regressionBiasApplyDomainPanel = new JPanel(new GridLayout(2,1));
	
	titlePanel.add(new JLabel("Bias Computation Method"));
	titlePanel.setBackground(new Color(176,196,222));
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	
	String biasComputeTitle = new String("Method to compute bias");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, biasComputeTitle);
	biasComputePanel.setBorder(titleBorder);
	
	String regressionBiasApplyDomainTitle = new String("Domain of application of bias (regression)");
	titleBorder=BorderFactory.createTitledBorder(blackline, regressionBiasApplyDomainTitle);
	regressionBiasApplyDomainPanel.setBorder(titleBorder);
	
	biasComputePanel.add(biasComputeSimpleMethodButton);
	biasComputePanel.add(biasComputeHistogramMethodButton);
	ButtonGroup biasButtonGroup = new ButtonGroup();
	biasButtonGroup.add(biasComputeSimpleMethodButton);
	biasButtonGroup.add(biasComputeHistogramMethodButton);

	regressionBiasApplyDomainPanel.add(noneRegressionBiasApplyDomainButton);
	regressionBiasApplyDomainPanel.add(allRegressionBiasApplyDomainButton);
	//regressionBiasApplyDomainPanel.add(oceanRegressionBiasApplyDomainButton);
	//regressionBiasApplyDomainPanel.add(landRegressionBiasApplyDomainButton);
	ButtonGroup regressionBiasApplyDomainButtonGroup = new ButtonGroup();
	regressionBiasApplyDomainButtonGroup.add(noneRegressionBiasApplyDomainButton);
	regressionBiasApplyDomainButtonGroup.add(allRegressionBiasApplyDomainButton);
	//regressionBiasApplyDomainButtonGroup.add(oceanRegressionBiasApplyDomainButton);
	//regressionBiasApplyDomainButtonGroup.add(landRegressionBiasApplyDomainButton);

	rightPanel.add(titlePanel);
	rightPanel.add(biasComputePanel);
	rightPanel.add(regressionBiasApplyDomainPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints biasComputeCons = rightLayout.getConstraints(biasComputePanel);
        biasComputeCons.setX(Spring.constant(50));
        biasComputeCons.setY(Spring.constant(80));
        biasComputeCons.setWidth(Spring.constant(525));
        biasComputeCons.setHeight(Spring.constant(80));

	SpringLayout.Constraints regressionBiasApplyDomainCons = rightLayout.getConstraints(regressionBiasApplyDomainPanel);
        regressionBiasApplyDomainCons.setX(Spring.constant(50));
        regressionBiasApplyDomainCons.setY(Spring.constant(210));
        regressionBiasApplyDomainCons.setWidth(Spring.constant(525));
        regressionBiasApplyDomainCons.setHeight(Spring.constant(80));
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == fmNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();

	JPanel titlePanel = new JPanel();
	JPanel nAttempsPanel = new JPanel();
	JPanel fmTypePanel = new JPanel(new GridLayout(4,1));
	JPanel outFMAccuracyPanel = new JPanel();
	JPanel prefixFMAccuracyPanel = new JPanel();
	JPanel addDeviceNoisePanel = new JPanel();
	
	titlePanel.add(new JLabel("Footprint Matching Options"));
	titlePanel.setBackground(new Color(176,196,222));
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	
	String fmTypeTitle = new String("Footprint matching type");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, fmTypeTitle);
	fmTypePanel.setBorder(titleBorder);

	String outFMAccuracyTitle = new String("Output the FM accuracy metric or not");
	titleBorder=BorderFactory.createTitledBorder(blackline, outFMAccuracyTitle);
	outFMAccuracyPanel.setBorder(titleBorder);

	String prefixFMAccuracyTitle = new String("Prefix of file with FM-acuracy metric");
	titleBorder=BorderFactory.createTitledBorder(blackline, prefixFMAccuracyTitle);
	prefixFMAccuracyPanel.setBorder(titleBorder);
	
	if ( satId.equals("n18")    || 
	     satId.equals("n19")    || 
	     satId.equals("metopA") || 
	     satId.equals("metopB") || 
	     satId.equals("aqua")   || 
	     satId.equals("npp") ) {
		fmTypePanel.add(fmLowTypeButton);
		fmTypePanel.add(fmHighTypeButton);
		ButtonGroup fmButtonGroup = new ButtonGroup();
		fmButtonGroup.add(fmLowTypeButton);
		fmButtonGroup.add(fmHighTypeButton);
	}
	else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18")) {
		fmTypePanel.add(fmImgTypeButton);
		fmTypePanel.add(fmEnvTypeButton);
		fmTypePanel.add(fmLasTypeButton);
		fmTypePanel.add(fmUasTypeButton);
		
		ButtonGroup fmButtonGroup = new ButtonGroup();
		fmButtonGroup.add(fmImgTypeButton);
		fmButtonGroup.add(fmEnvTypeButton);
		fmButtonGroup.add(fmLasTypeButton);
		fmButtonGroup.add(fmUasTypeButton);
	}
	else if ( satId.equals("trmm") || satId.equals("mtma") || satId.equals("mtsa") || satId.equals("gcomw1") ) {
		fmTypePanel.add(fmCoarseTypeButton);
		fmTypePanel.add(fmLowTypeButton);
		fmTypePanel.add(fmHighTypeButton);
		ButtonGroup fmButtonGroup = new ButtonGroup();
		fmButtonGroup.add(fmCoarseTypeButton);
		fmButtonGroup.add(fmLowTypeButton);
		fmButtonGroup.add(fmHighTypeButton);
	}
	
	outFMAccuracyPanel.add(outFMAccuracyCheck);
	prefixFMAccuracyPanel.add(prefixFMAccuracyField);

	rightPanel.add(titlePanel);
	rightPanel.add(fmTypePanel);
	rightPanel.add(outFMAccuracyPanel);
	rightPanel.add(prefixFMAccuracyPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints fmTypeCons = rightLayout.getConstraints(fmTypePanel);
        fmTypeCons.setX(Spring.constant(50));
        fmTypeCons.setY(Spring.constant(100));
        fmTypeCons.setWidth(Spring.constant(525));
        fmTypeCons.setHeight(Spring.constant(150));
	
	SpringLayout.Constraints outFMAccuracyCons = rightLayout.getConstraints(outFMAccuracyPanel);
        outFMAccuracyCons.setX(Spring.constant(50));
        outFMAccuracyCons.setY(Spring.constant(300));
        outFMAccuracyCons.setWidth(Spring.constant(525));
        outFMAccuracyCons.setHeight(Spring.constant(60));
	
	SpringLayout.Constraints prefixFMAccuracyCons = rightLayout.getConstraints(prefixFMAccuracyPanel);
        prefixFMAccuracyCons.setX(Spring.constant(50));
        prefixFMAccuracyCons.setY(Spring.constant(410));
        prefixFMAccuracyCons.setWidth(Spring.constant(525));
        prefixFMAccuracyCons.setHeight(Spring.constant(60));
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == cleanNode ) {
    
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(1,1);
	gridLayout.setVgap(5);

	JPanel titlePanel = new JPanel();
	JPanel maxDaysArchivedPanel = new JPanel();
	JPanel cleanPanel = new JPanel(gridLayout);
	
	titlePanel.add(new JLabel("Cleaning Options"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	String cleanTitle = new String("Cleaning Options");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, cleanTitle);
	cleanPanel.setBorder(titleBorder);
	
	String maxDaysArchivedTitle = new String("Maximum number of days data archived");
	titleBorder=BorderFactory.createTitledBorder(blackline, maxDaysArchivedTitle);
	maxDaysArchivedPanel.setBorder(titleBorder);
	
	maxDaysArchivedPanel.add(maxDaysArchivedField);
	
	cleanPanel.add(makeCleanCheck);

	rightPanel.add(titlePanel);
	rightPanel.add(maxDaysArchivedPanel);
	rightPanel.add(cleanPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints maxDaysArchivedCons = rightLayout.getConstraints(maxDaysArchivedPanel);
        maxDaysArchivedCons.setX(Spring.constant(50));
        maxDaysArchivedCons.setY(Spring.constant(80));
        maxDaysArchivedCons.setWidth(Spring.constant(525));
        maxDaysArchivedCons.setHeight(Spring.constant(75));

	SpringLayout.Constraints cleanCons = rightLayout.getConstraints(cleanPanel);
        cleanCons.setX(Spring.constant(50));
        cleanCons.setY(Spring.constant(190));
        cleanCons.setWidth(Spring.constant(525));
        cleanCons.setHeight(Spring.constant(75));
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);

    }
    else if ( selectedNode == fwdNode ) {

	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();

	JPanel titlePanel = new JPanel();
	JPanel fwdCloudOffOrOnPanel = new JPanel();
	
	titlePanel.add(new JLabel("Forward Options"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);

	Border blackline = BorderFactory.createLineBorder(Color.black);
	String fwdCloudOffOrOnTitle = new String("Forward Cloud On/Off Option");
	TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, fwdCloudOffOrOnTitle);
	fwdCloudOffOrOnPanel.setBorder(titleBorder);
	
	fwdCloudOffOrOnPanel.add(fwdCloudOffOrOnCheck);

	rightPanel.add(titlePanel);
	rightPanel.add(fwdCloudOffOrOnPanel);
	
	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints fwdCloudOffOrOnCons = rightLayout.getConstraints(fwdCloudOffOrOnPanel);
        fwdCloudOffOrOnCons.setX(Spring.constant(50));
        fwdCloudOffOrOnCons.setY(Spring.constant(100));
        fwdCloudOffOrOnCons.setWidth(Spring.constant(525));
        fwdCloudOffOrOnCons.setHeight(Spring.constant(75));
	
	rightPanel.paintAll(g);
	rightPanel.setVisible(true);
    }
    else {
    	    Graphics g = rightPanel.getGraphics(); 
    	    rightPanel.removeAll();
   	    rightPanel.paintAll(g);

	    g.drawString("Sorry! Not defined yet", 100,100);
    	    rightPanel.setVisible(true);

    }

  }

  /**
   * return changed preferences
   */
  public Map<String, String> getPreferences() 
  {
    return preferences; 
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////
  //
  // Get default vaules ( more strict check for invalid input(number,sensible,range check)
  //
  //////////////////////////////////////////////////////////////////////////////////////////////////
  /**
   * Get default vaules give an argument
   */
  public void getDefaultValues(Map<String,String> inputPreferences) {
    
    satId = inputPreferences.get(satIdKey);
    
    nAttempts = inputPreferences.get(nAttemptsKey);    
    if ( nAttempts != null )
    	nAttemptsField.setText(nAttempts);

    fmType = inputPreferences.get(fmTypeKey) ; 
    if ( satId.equals("n18")    || 
	 satId.equals("n19")    || 
         satId.equals("metopA") || 
	 satId.equals("metopB") || 
	 satId.equals("aqua")   || 
	 satId.equals("npp") ) { 
      if      ( fmType != null && fmType.equals("0") ) 	
    	fmLowTypeButton.setSelected(true);    
      else if ( fmType != null && fmType.equals("1") )	
    	fmHighTypeButton.setSelected(true);    
    }
    else if ( satId.equals("f16") || satId.equals("f17") || satId.equals("f18") ) { 
      if      ( fmType != null && fmType.equals("3") )
	fmImgTypeButton.setSelected(true);
      else if ( fmType != null && fmType.equals("2") )       
	fmEnvTypeButton.setSelected(true);
      else if ( fmType != null && fmType.equals("1") )       
	fmLasTypeButton.setSelected(true);
      else if ( fmType != null && fmType.equals("0") )       
	fmUasTypeButton.setSelected(true);
    }
    else if ( satId.equals("trmm") || satId.equals("mtma") || satId.equals("mtsa") || satId.equals("gcomw1") ) {
      if      ( fmType != null && fmType.equals("-1") ) 	
    	fmCoarseTypeButton.setSelected(true);    
      else if ( fmType != null && fmType.equals("0") ) 	
    	fmLowTypeButton.setSelected(true);    
      else if ( fmType != null && fmType.equals("1") )	
    	fmHighTypeButton.setSelected(true);    
    }
    

    outFMAccuracy = inputPreferences.get(outFMAccuracyKey);
    if ( outFMAccuracy != null && outFMAccuracy.equals("1") )  	
    	outFMAccuracyCheck.setSelected(true);
    else							
    	outFMAccuracyCheck.setSelected(false);
    
    prefixFMAccuracy = inputPreferences.get(prefixFMAccuracyKey);
    if ( prefixFMAccuracy != null )
    	prefixFMAccuracyField.setText(prefixFMAccuracy);

    addDeviceNoise = inputPreferences.get(addDeviceNoiseKey);
    if ( addDeviceNoise != null && addDeviceNoise.equals("1") )	
    	addDeviceNoiseCheck.setSelected(true);
    else 
    	addDeviceNoiseCheck.setSelected(false);


    externalDataAvailable = inputPreferences.get(externalDataAvailableKey);
    if ( externalDataAvailable!= null && externalDataAvailable.equals("1") )
	externalDataAvailableCheck.setSelected(true);
    else
    	externalDataAvailableCheck.setSelected(false);

    externalDataSrc = inputPreferences.get(externalDataSrcKey);
    if 	    ( externalDataSrc != null && externalDataSrc.equals("1") ) 	
    	analysExternalDataButton.setSelected(true);
    else if ( externalDataSrc != null && externalDataSrc.equals("2") ) 	
    	regressExternalDataButton.setSelected(true);

    nwpGdasUse = inputPreferences.get(nwpGdasUseKey);
    if ( nwpGdasUse!= null && nwpGdasUse.equals("1") )
	nwpGdasUseCheck.setSelected(true);
    else
    	nwpGdasUseCheck.setSelected(false);

    nwpEcmwfUse = inputPreferences.get(nwpEcmwfUseKey);
    if ( nwpEcmwfUse!= null && nwpEcmwfUse.equals("1") )
	nwpEcmwfUseCheck.setSelected(true);
    else
    	nwpEcmwfUseCheck.setSelected(false);

    nwpGfsUse = inputPreferences.get(nwpGfsUseKey);
    if ( nwpGfsUse!= null && nwpGfsUse.equals("1") )
	nwpGfsUseCheck.setSelected(true);
    else
    	nwpGfsUseCheck.setSelected(false);

    extBkgAtmUse = inputPreferences.get(extBkgAtmUseKey);
    if ( extBkgAtmUse!= null && extBkgAtmUse.equals("1") )
	extBkgAtmUseCheck.setSelected(true);
    else
    	extBkgAtmUseCheck.setSelected(false);

    minLat = inputPreferences.get(minLatKey);
    maxLat = inputPreferences.get(maxLatKey);
    minLon = inputPreferences.get(minLonKey);
    maxLon = inputPreferences.get(maxLonKey);
    minLatField.setText(inputPreferences.get(minLatKey));
    maxLatField.setText(inputPreferences.get(maxLatKey));
    minLonField.setText(inputPreferences.get(minLonKey));
    maxLonField.setText(inputPreferences.get(maxLonKey));
 
 
    geoLimit = inputPreferences.get(geoLimitKey);
    if 	( geoLimit != null && geoLimit.equals("0") ) {
  	    geoAllLimitButton.setSelected(true);
	    minLatField.setEditable(false);
	    maxLatField.setEditable(false);
	    minLonField.setEditable(false);
	    maxLonField.setEditable(false);
    }	
    else if ( geoLimit != null && geoLimit.equals("1") ) {
 	    geoLatLonLimitButton.setSelected(true);
	    minLatField.setEditable(true);
	    maxLatField.setEditable(true);
	    minLonField.setEditable(true);
	    maxLonField.setEditable(true);
	    
    }
    else if ( geoLimit != null && geoLimit.equals("2") ) {
	    geoOceanLimitButton.setSelected(true);
	    minLatField.setEditable(false);
	    maxLatField.setEditable(false);
	    minLonField.setEditable(false);
	    maxLonField.setEditable(false);
    }
    else if ( geoLimit != null && geoLimit.equals("3") ) {
	    geoLandLimitButton.setSelected(true);
	    minLatField.setEditable(false);
	    maxLatField.setEditable(false);
	    minLonField.setEditable(false);
	    maxLonField.setEditable(false);
    }

    rdrType = inputPreferences.get(rdrTypeKey);
    if 	( rdrType != null && rdrType.equals("1") ) {
  	    rdrTypeTdrButton.setSelected(true);
    }	
    else if ( rdrType != null && rdrType.equals("2") ) {
 	    rdrTypeSdrProxyButton.setSelected(true);
    }
    else if ( rdrType != null && rdrType.equals("3") ) {
	    rdrTypeSdrButton.setSelected(true);
    }
    else if ( rdrType != null && rdrType.equals("4") ) {
	    rdrTypeSdrRemapButton.setSelected(true);
    }

    cend = inputPreferences.get(cendKey);
    if 	    ( cend != null && cend.equals("0") ) { ascendButton.setSelected(true);  }	
    else if ( cend != null && cend.equals("1") ) { descendButton.setSelected(true); }
    else if ( cend != null && cend.equals("2") ) { combineButton.setSelected(true); }



    monitorIterative = inputPreferences.get(monitorIterativeKey);
    if ( monitorIterative != null && monitorIterative.equals("1") )	
    	monitorIterativeCheck.setSelected(true);
    else				
    	monitorIterativeCheck.setSelected(false);

    monitorRetrieval = inputPreferences.get(monitorRetrievalKey);
    if ( monitorRetrieval != null && monitorRetrieval.equals("1") )	
    	monitorRetrievalCheck.setSelected(true);
    else				
    	monitorRetrievalCheck.setSelected(false);

    monitorFwd = inputPreferences.get(monitorFwdKey);
    if ( monitorFwd != null && monitorFwd.equals("1") )	
    	monitorFwdCheck.setSelected(true);
    else				
    	monitorFwdCheck.setSelected(false);



    tdrFormat = inputPreferences.get(tdrFormatKey) ; 
    if      ( tdrFormat != null && tdrFormat.equals("0") ) 	tdrAsciiFormatButton.setSelected(true);    
    else if ( tdrFormat != null && tdrFormat.equals("1") )	tdrBinaryFormatButton.setSelected(true);    

    gifDensity = inputPreferences.get(gifDensityKey);
    if ( gifDensity != null )
    	gifDensityField.setText(gifDensity);

    gridFactor = inputPreferences.get(gridFactorKey);
    if ( gridFactor != null )
    	gridFactorField.setText(gridFactor);

    email = inputPreferences.get(emailKey);
    if ( email != null )
    	emailField.setText(email);

    website = inputPreferences.get(websiteKey);
    if ( website != null )
    	websiteField.setText(website);

    nScanLineSensor1Skip = inputPreferences.get(nScanLineSensor1SkipKey);
    if ( nScanLineSensor1Skip != null )
    	nScanLineSensor1SkipField.setText(nScanLineSensor1Skip);
    
    nScanLineSensor2Skip = inputPreferences.get(nScanLineSensor2SkipKey);
    if ( nScanLineSensor2Skip != null )
    	nScanLineSensor2SkipField.setText(nScanLineSensor2Skip);

    scanLineIndexSensor2TimeColloc = inputPreferences.get(scanLineIndexSensor2TimeCollocKey) ; 
    if ( scanLineIndexSensor2TimeColloc != null && scanLineIndexSensor2TimeColloc.equals("1") )
    	sensor2ScanIndex1Button.setSelected(true);    
    else if ( scanLineIndexSensor2TimeColloc != null && scanLineIndexSensor2TimeColloc.equals("2") )
    	sensor2ScanIndex2Button.setSelected(true);    
    else if ( scanLineIndexSensor2TimeColloc != null && scanLineIndexSensor2TimeColloc.equals("3") )
    	sensor2ScanIndex3Button.setSelected(true);    


    maxDaysArchived = inputPreferences.get(maxDaysArchivedKey);
    if ( maxDaysArchived != null ) 
    	maxDaysArchivedField.setText(maxDaysArchived);
	
    dayUsed4Bias = inputPreferences.get(dayUsed4BiasKey);
    if ( dayUsed4Bias != null )	
    	dayUsed4BiasField.setText(dayUsed4Bias);

    dayUsed4Alg	= inputPreferences.get(dayUsed4AlgKey);
    if (dayUsed4Alg != null )	
    	dayUsed4AlgField.setText(dayUsed4Alg);


    biasComputeMethod = inputPreferences.get(biasComputeMethodKey) ; 
    if      ( biasComputeMethod != null && biasComputeMethod.equals("0") ) 	
    	biasComputeSimpleMethodButton.setSelected(true);    
    else if ( biasComputeMethod != null && biasComputeMethod.equals("1") )
    	biasComputeHistogramMethodButton.setSelected(true);    

    regressionBiasApplyDomain = inputPreferences.get(regressionBiasApplyDomainKey) ; 
    if      ( regressionBiasApplyDomain != null && regressionBiasApplyDomain.equals("-2") )
    	noneRegressionBiasApplyDomainButton.setSelected(true);    
    else if      ( regressionBiasApplyDomain != null && regressionBiasApplyDomain.equals("-1") )
    	allRegressionBiasApplyDomainButton.setSelected(true);    
    //else if ( regressionBiasApplyDomain != null && regressionBiasApplyDomain.equals("0") )
    //	oceanRegressionBiasApplyDomainButton.setSelected(true);    
    //else if ( regressionBiasApplyDomain != null && regressionBiasApplyDomain.equals("1") )
    //	landRegressionBiasApplyDomainButton.setSelected(true);    


    nChoppedFilesPerOrbit = inputPreferences.get(nChoppedFilesPerOrbitKey);
    if ( nChoppedFilesPerOrbit != null )	
    	nChoppedFilesPerOrbitField.setText(nChoppedFilesPerOrbit);

    retrOnOrbitOrSubOrbit = inputPreferences.get(retrOnOrbitOrSubOrbitKey) ; 
    if      ( retrOnOrbitOrSubOrbit != null && retrOnOrbitOrSubOrbit.equals("0") )
    	fullOrbitRetrievalButton.setSelected(true);    
    else if ( retrOnOrbitOrSubOrbit != null && retrOnOrbitOrSubOrbit.equals("1") )
    	choppedOrbitRetrievalButton.setSelected(true);    

    retrOnWhichSDR = inputPreferences.get(retrOnWhichSDRKey) ; 
    if      ( retrOnWhichSDR != null && retrOnWhichSDR.equals("0") )
    	empiricallyCorrectedTBButton.setSelected(true);    
    else if ( retrOnWhichSDR != null && retrOnWhichSDR.equals("1") )
    	uncorrectedTBButton.setSelected(true);    
    else if ( retrOnWhichSDR != null && retrOnWhichSDR.equals("2") )
    	nwpTBButton.setSelected(true);    

    fwdMatrix2Use = inputPreferences.get(fwdMatrix2UseKey) ; 
    if      ( fwdMatrix2Use != null && fwdMatrix2Use.equals("0") )
    	dynamicFwdMatrixButton.setSelected(true);    
    else if ( fwdMatrix2Use != null && fwdMatrix2Use.equals("1") )
    	nonErrFwdMatrixButton.setSelected(true);    

    makeOrNot = inputPreferences.get(makeOrNotKey);
    if ( makeOrNot != null && makeOrNot.equals("1") )	
    	makeOrNotCheck.setSelected(true);
    else
    	makeOrNotCheck.setSelected(false);

    /**
    useCPU = inputPreferences.get(useCPUKey);
    if ( useCPU != null && useCPU.equals("1") )
    	useCPUCheck.setSelected(true);
    else
    	useCPUCheck.setSelected(false);
    */
    useCPU = inputPreferences.get(useCPUKey);
    if 	    ( useCPU != null && useCPU.equals("0") ) { cpu0Button.setSelected(true); }	
    else if ( useCPU != null && useCPU.equals("1") ) { cpu1Button.setSelected(true); }
    else if ( useCPU != null && useCPU.equals("2") ) { cpu2Button.setSelected(true); }


    makeClean = inputPreferences.get(makeCleanKey);
    if ( makeClean != null && makeClean.equals("1") )
    	makeCleanCheck.setSelected(true);
    else
    	makeCleanCheck.setSelected(false);
  
  
    fwdCloudOffOrOn = inputPreferences.get(fwdCloudOffOrOnKey);
    if ( fwdCloudOffOrOn != null && fwdCloudOffOrOn.equals("1") )
    	fwdCloudOffOrOnCheck.setSelected(true);
    else
    	fwdCloudOffOrOnCheck.setSelected(false);
 
  }
  
  
  /**
   * load default stuffs
   */
  private void loadGeo() {
  
	Graphics g = rightPanel.getGraphics(); 
	rightPanel.removeAll();
	GridLayout gridLayout = new GridLayout(4,1);
	gridLayout.setVgap(5);

	JPanel titlePanel    = new JPanel();
	JPanel geoLimitPanel = new JPanel(new GridLayout(2,2));
	JPanel latLonPanel   = new JPanel();
	JPanel labelPanel    = new JPanel(gridLayout);
	JPanel fieldPanel    = new JPanel(gridLayout);
	JPanel cendPanel     = new JPanel(new GridLayout(3,1));

	titlePanel.add(new JLabel("Geographic Specification"));
	titlePanel.setBackground(new Color(176,196,222));
	
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	titlePanel.setBorder(loweredbevel);
	
	Border blackline = BorderFactory.createLineBorder(Color.black);

	String geoLimitTitle = new String("Select geographic limit");
	TitledBorder geoLimitTitleBorder=BorderFactory.createTitledBorder(blackline, geoLimitTitle);
	geoLimitPanel.setBorder(geoLimitTitleBorder);	
	geoLimitPanel.add(geoAllLimitButton);
	geoLimitPanel.add(geoLatLonLimitButton);
	geoLimitPanel.add(geoOceanLimitButton);
	geoLimitPanel.add(geoLandLimitButton);
	ButtonGroup buttonGroup = new ButtonGroup();
	buttonGroup.add(geoAllLimitButton);
	buttonGroup.add(geoLatLonLimitButton);
	buttonGroup.add(geoOceanLimitButton);
	buttonGroup.add(geoLandLimitButton);


	String latLonTitle = new String("Latitude/Longitude Box");
	TitledBorder latLonTitleBorder=BorderFactory.createTitledBorder(blackline, latLonTitle);
	latLonPanel.setBorder(latLonTitleBorder);

	String cendTitle = new String("Orbit Selection");
	TitledBorder cendTitleBorder=BorderFactory.createTitledBorder(blackline, cendTitle);
	cendPanel.setBorder(cendTitleBorder);

	
	rightPanel.add(titlePanel);
	rightPanel.add(geoLimitPanel);
	rightPanel.add(latLonPanel);
	rightPanel.add(cendPanel);

	SpringLayout.Constraints titleCons = rightLayout.getConstraints(titlePanel);
        titleCons.setX(Spring.constant(0));
        titleCons.setY(Spring.constant(5));
        titleCons.setWidth(Spring.constant(635));
        titleCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints geoLimitCons = rightLayout.getConstraints(geoLimitPanel);
        geoLimitCons.setX(Spring.constant(50));
        geoLimitCons.setY(Spring.constant(75));
        geoLimitCons.setWidth(Spring.constant(525));
        geoLimitCons.setHeight(Spring.constant(100));
	
	SpringLayout.Constraints latLonCons = rightLayout.getConstraints(latLonPanel);
        latLonCons.setX(Spring.constant(50));
        latLonCons.setY(Spring.constant(225));
        latLonCons.setWidth(Spring.constant(525));
        latLonCons.setHeight(Spring.constant(200));
	
	SpringLayout.Constraints cendCons = rightLayout.getConstraints(cendPanel);
        cendCons.setX(Spring.constant(50));
        cendCons.setY(Spring.constant(475));
        cendCons.setWidth(Spring.constant(525));
        cendCons.setHeight(Spring.constant(125));
	
	
	latLonPanel.add(labelPanel);
	latLonPanel.add(fieldPanel);
	
	SpringLayout latLonSpringLayout = new SpringLayout();
	latLonPanel.setLayout(latLonSpringLayout);
	SpringLayout.Constraints labelCons = latLonSpringLayout.getConstraints(labelPanel);
        labelCons.setX(Spring.constant(75));
        labelCons.setY(Spring.constant(25));
        labelCons.setWidth(Spring.constant(150));
        labelCons.setHeight(Spring.constant(100));

	SpringLayout.Constraints fieldCons = latLonSpringLayout.getConstraints(fieldPanel);
        fieldCons.setX(Spring.constant(250));
        fieldCons.setY(Spring.constant(25));
        fieldCons.setWidth(Spring.constant(200));
        fieldCons.setHeight(Spring.constant(100));
	
	labelPanel.add(minLatLabel);
	labelPanel.add(maxLatLabel);
	labelPanel.add(minLonLabel);
	labelPanel.add(maxLonLabel);

	fieldPanel.add(minLatField);
	fieldPanel.add(maxLatField);
	fieldPanel.add(minLonField);
	fieldPanel.add(maxLonField);

	ButtonGroup cendButtonGroup = new ButtonGroup();
	cendButtonGroup.add(ascendButton);
	cendButtonGroup.add(descendButton);
	cendButtonGroup.add(combineButton);
	
	cendPanel.add(ascendButton);
	cendPanel.add(descendButton);
	cendPanel.add(combineButton);

	rightPanel.paintAll(g);
	rightPanel.setVisible(true);
  
  }
  
}
