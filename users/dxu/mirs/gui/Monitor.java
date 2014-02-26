/**
 * $Id: Monitor.java
 *
 * $Description:
 * This class is the main control and monitor of MIRS running.
 * Java 6 is required to run the program. As of 05/10/2007,
 * IBM AIX version of Java 1.6 is not available yet.
 *
 * $Usage:
 * 
 *	Linux:
 *	/jdk1.6.0/bin/javac Monitor.java
 *	/jdk1.6.0/bin/java  Monitor
 *
 * 
 *
 *
 * @version 1.0 $ $Date: Tue Dec 26 12:15:24 EST 2006 $
 *
 * @author Wanchun Chen  @ NOAA/NESDIS/STAR
 *
 */

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
import javax.swing.border.*;

import java.beans.*;
import java.util.Random;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;
import java.util.LinkedList;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.text.NumberFormat;

import java.net.*;


public class Monitor extends JPanel
                             implements ActionListener, ItemListener,
                                        PropertyChangeListener {
    private static final long serialVersionUID = 1L;
    
    /**
     * os name: Linux/Windows XP/AIX/Windows NT/Windows 95/
     */
    private String osName = null;

    /**
     * GUI current directoty
     */
    private String rootGUI = null;

    /**
     * bash path. most platform it's /bin/bash, some(AIX) /usr/bin/bash
     */
    private String bashPath="#!/bin/bash\n\n";
    
    /**
     * the maximum number of tasks 
     */
     private static final int MAXNUMTASK=32;
	
    /**
     * the paths hold all parameter=value pairs; it will change according to
     * different satellite selected.
     */
    private Map<String, String> paths = new HashMap<String, String>();
    
    /**
     * the comments hold all parameter and its comment parts; its values are 
     * fixed in the whole process and like a constant.
     */
    private Map<String, String> comments = new HashMap<String, String>();
    
    /**
     * the keys hold all parameter names(those one left side = in config.bash
     * All keys are include in keys and its values are fixed like a constant.
     */
    private LinkedList<String> keys = new LinkedList<String>();
    
    /**
     * the tasks hold all task=value pairs, kind of reduant respect to paths
     * but runs much faster, space/time is a good deal here.
     * It will change depending on satId selected.
     */
    private Map<String, String> tasks = new HashMap<String, String>();
 
    /**
     * the keys hold all task names, it will change depending on satId
     */
    private LinkedList<String> taskKeys = new LinkedList<String>();
    
    /**
     * the tasks in Task panel
     */
    private JCheckBox [] tasksCheckBox = new JCheckBox[MAXNUMTASK];   

    /**
     * the buffer to hold content of dynamically generated script
     */
    private StringBuffer taskBuffer = new StringBuffer(16384);
    
    /**
     * the mapping(associative array) hold task keys --> task strings
     * like, fm --> "Footpring Matching"
     */
    private Map<String, String> task2String = new HashMap<String, String>();

    /**
     * the mapping(associative array) hold task string --> key
     * like, "Footpring Matching" --> fm
     */
    private Map<String, String> string2Task = new HashMap<String, String>();

    /**
     * main menu bar stuff 
     */
    private JMenuBar menuBar;
    private JMenu fileMenu, editMenu;
    
    private JMenuItem openFileMenuItem;
    private JMenuItem saveFileMenuItem;
    private JMenuItem saveAsFileMenuItem;
    private JMenuItem exitMenuItem;
    
    private JMenuItem pathMenuItem;
    private JMenuItem preferenceMenuItem;
    private JMenuItem compileMenuItem;
    
    /**
     * Progress monitor bar
     */
    private JProgressBar progressBar;
    
    /**
     * the output area for task progress
     */
    private JTextArea taskOutput;

    /**
     * background task
     */
    private Task task; 
    
    /**
     * background task exit value
     */
    private int processExitValue = 0; 
    

    /**
     * global process
     */
    Process globalProcess;

    /**
     * config file with a default initial value
     */
    private String configFile = "n18_pcf.bash";

    /**
     * config file path
     */
    private String configPath = "../setup/";

    /**
     * dir_data String
     */
    private String dir_data = "2006-02-01";

    /**
     * date String
     */
    private String date = "2006-02-01";

    /**
     * sensor
     */
    private String sensor = "n18";
   
    /**
     * file name of would-be script file
     */
    private String scriptFile = "n18_scs.bash";
    
    /**
     * script path where we put scripts file
     */
    private String scriptPath = "../scripts/";

    /**
     * script path where we put scripts lib file
     */
    private String libScriptPath = "../scripts/";

    /**
     * raw RDR file to be processed in orbital mode
     */
    private String fileToProcess = null;
    
    /**
     * daily process button
     */
    private JRadioButton dailyButton;
    
    /**
     * orbit process button
     */
    private JRadioButton orbitButton;
    
    /**
     * the radio button group associated with task
     */
    private ButtonGroup displayButtonGroup;
    
    /**
     * Process NWP GDAS or not, 1 to use, 0 not use, default to use  
     */
    private String nwpGdasUse = "1";

    /**
     * Process NWP ECMWF or not, 1 to use, 0 not use 
     */
    private String nwpEcmwfUse = "0";

    /**
     * Process NWP GFS or not, 1 to use, 0 not use, default to use  
     */
    private String nwpGfsUse = "0";
    
    
    /**
     * Use External (spatial/temporal varying) background atmosphere or not, 1 to use, 0 not use, default not to use  
     */
    private String extBkgAtmUse = "0";
    
    
    ////////////////////////////////////////////////////////////////////
    // global string names
    ////////////////////////////////////////////////////////////////////
    
    // processing modes
    private String processModeKey    = "processMode";
    private String processModeString = "Process Mode";
    private String dailyModeString   = "Daily Process";
    private String orbitModeString   = "Orbit Process";
    
    // Sensors
    private String sensorIdKey   = "sensorId";
    private String n18String     = "NOAA-18/AMSUA&MHS";
    private String n19String     = "NOAA-19/AMSUA&MHS";
    private String moaString 	 = "MetOp-A/AMSUA&MHS";
    private String mobString 	 = "MetOp-B/AMSUA/MHS";
    private String f16String     = "F16/SSMIS";
    private String f18String     = "F18/SSMIS";
    private String nppString     = "NPP/ATMS";
    private String aquaString    = "AQUA/AMSRE";
    private String gcomw1String  = "GCOMW1/AMSR2";
    private String fy3riString   = "FY3/MWRI";
    private String fy3htString   = "FY3/MWHS/MWTS";
    private String trmmString    = "TRMM/TMI";
    private String gpmString     = "GPM/GMI";
    private String mtmaString    = "MT/MADRAS";
    private String mtsaString    = "MT/SAPHIR";
    private String f17String     = "F17/SSMIS";
    private String windSatString = "WindSat";
   
   
    //nProfs2Retr
    private String nProfs2RetrKey = "nProfs2Retr";

   //nProfs2Fwd=2000000
    private String nProfs2FwdKey = "nProfs2Fwd";

    //nDaysBack=2
    private String nDaysBackKey = "nDaysBack";
  
   //nOrbits2Process=50
    private String nOrbits2ProcessKey = "nOrbits2Process";

   
    /**
     * N18/MetopA tasks
     */
    private String [] tasksKeysN18 = {	"rdr2tdrSensor1", 
    					"rdr2tdrSensor2", 
					"mergeNedt", 
					"tdr2sdrSensor1", 
					"tdr2sdrSensor2", 
    					"fm", 
					"nwp", 
					"fwd", 
					"biasGen", 
					"choppRadFiles", 
					"regressAlg", 
					"externalDataFromRegress", 
					"fmsdr2edr", 
					"mergeEdr",
					"vipp",
					"grid",
					"nc",
					"figsGen", 
					"biasFigsGen", 
					"dataMonitor", 
					"clean"};
    /**
     * F16/F17/F18 tasks
     */
    private String [] tasksKeysF16 = {	"rdr2tdrSensor1", 
    					"mergeNedt", 
					"tdr2sdrSensor1", 
					"fm", 
					"nwp", 
					"fwd", 
					"biasGen", 
 					"choppRadFiles", 
					"regressAlg", 
					"externalDataFromRegress", 
					"fmsdr2edr", 
					"mergeEdr",
					"vipp",
					"grid",
					"nc",
					"figsGen", 
					"biasFigsGen", 
					"dataMonitor", 
					"clean"};
   
   
    /**
     * NPP tasks
     */
    private String [] tasksKeysNpp = {	"rdr2tdrSensor1", 
 					"mergeNedt", 
					"tdr2sdrSensor1", 
    					"fm", 
					"nwp", 
					"fwd",
					"bufr", 
					"biasGen", 
					"choppRadFiles", 
					"regressAlg", 
					"externalDataFromRegress", 
					"fmsdr2edr", 
					"mergeEdr",
					"vipp",
					"grid",
					"nc",
					"figsGen", 
					"biasFigsGen", 
					"dataMonitor", 
					"clean"};
   
    /**
     * AQUA tasks, shared by FY3 MWRI 
     */
    private String [] tasksKeysAqua = {	"rdr2tdrSensor1", 
 					"mergeNedt", 
					"tdr2sdrSensor1", 
					"mergeNedt", 
					"fm", 
					"nwp", 
					"fwd", 
					"biasGen", 
 					"choppRadFiles", 
					"regressAlg", 
					"externalDataFromRegress", 
					"fmsdr2edr", 
					"mergeEdr",
					"vipp",
					"grid",
					"nc",
					"figsGen", 
					"biasFigsGen", 
					"clean"};
   
    /**
     * WindSat tasks
    */
    private String [] tasksKeysWindSat=	{ "undefined1",
    					  "undefined2",
					  "undefined3",
					  "undefined4..." };
    
    
    
    
    private long startTime = 0;
    
    /**
     * to contain parsed output 
     */   
    JTextArea outputArea;
    
    /**
     * to contain parsed output 
     */   
    JScrollPane outScrollPane;
    
    
    /**
     * horizontal bar in outputArea
     */   
    JScrollBar hbar = new JScrollBar(JScrollBar.HORIZONTAL, 32,  32, 0, 128);
    
    /**
     * vertical bar in outputArea
     */   
    JScrollBar vbar = new JScrollBar(JScrollBar.VERTICAL,   32,  32, 0, 64);
    

    /**
     * horizontal bar in taskOutput
     */   
    JScrollBar taskHbar = new JScrollBar(JScrollBar.HORIZONTAL, 8,  8, 0, 64);
    
    /**
     * vertical bar in taskOutput
     */   
    JScrollBar taskVbar = new JScrollBar(JScrollBar.VERTICAL,   8,  8, 0, 64);

    
    /**
     * sensor chooser
     */   
    private Choice sensorChooser = new Choice();
    
    /**
     * process chooser
     */   
    private Choice processModeChooser = new Choice();
    
    /**
     * number of profiles(1dvar)
     */   
    private Choice profileNumberChooser = new Choice();
    
    /**
     * number of profiles(fwd)
     */   
    private Choice fwdProfileNumberChooser = new Choice();
   
    /**
     * number of orbits
     */   
    private Choice orbitNumberChooser = new Choice();
    
    /**
     * browse button for orbit mode
     */   
    private JButton browseButton = new JButton("Browse");
    
   
   
    /**
     * Button to generate script
     */   
    private JButton generateScriptButton = new JButton("Generate Script");

    /**
     * Button to generate script
     */   
    private JButton viewScriptButton = new JButton("View Script");

    /**
     * Button to save script using default script name: sensor + ".bash" = scriptFile
     */   
    private JButton viewConfigButton = new JButton("View Config");   

    /**
     * Button to save script as another name
     */   
    private JButton saveScriptAsButton = new JButton("Save Script As");

    /**
     * Button to run
     */   
    private JButton runButton = new JButton("Run");
    
    /**
     * Button to cancel
     */   
    private JButton cancelButton = new JButton("Cancel");
    
    /**
     * Button to quit
     */   
    private JButton quitButton = new JButton("Quit");
    
    
    /**
     * Button to clear output area
     */   
    private JButton clearButton = new JButton("Clear");
    
	
    /**
     * Button to Display
     */   
    private JButton displayButton = new JButton("Display");
    
    /**
     * date chooser
     */   
    private Choice dateChooser = new Choice();	
	
    // some labels
    private JLabel orbitLabel	   = new JLabel("Number of Orbits");
    private JLabel profileLabel    = new JLabel("Number of Profiles (1dv)");
    private JLabel fwdProfileLabel = new JLabel("Number of Profiles (fwd)");
    private JLabel outputLabel	   = new JLabel("Execution Monitoring");
    
    
    // main panels
    private JPanel controlPanel   = new JPanel();
    private JPanel titlePanel	  = new JPanel();
    private JPanel sensorPanel    = new JPanel(new FlowLayout()); 
    private JPanel stepPanel	  = new JPanel(new GridLayout(10,1)); 
    private JPanel modePanel 	  = new JPanel(); 
    private JPanel profilePanel   = new JPanel(); 
     
    private JPanel runPanel	  = new JPanel(new GridLayout(2,4));
    private JPanel outputPanel    = new JPanel(new BorderLayout() );
    private JPanel monitorPanel   = new JPanel(new BorderLayout() );

    private SpringLayout controlLayout = new SpringLayout();
    private SpringLayout modeLayout    = new SpringLayout();
    private SpringLayout profileLayout = new SpringLayout();
   
    ////////////////////////////////////////////////////////////////////////////
    //
    // task names definiton
    //
    ////////////////////////////////////////////////////////////////////////////
    private String rdr2tdrSensor1String 	= new String("rdr2tdrSensor1");
    private String rdr2tdrSensor2String 	= new String("rdr2tdrSensor2");
    private String mergeNedtString		= new String("mergeNedt");
    private String tdr2sdrSensor1String 	= new String("tdr2sdrSensor1");
    private String tdr2sdrSensor2String 	= new String("tdr2sdrSensor2");
    private String fmString 			= new String("fm");
    private String nwpString 			= new String("nwp");
    private String fwdString 			= new String("fwd");
    private String bufrString 			= new String("bufr");
    private String biasGenString 		= new String("biasGen");
    private String choppRadFilesString 		= new String("choppRadFiles");
    private String externalDataFromRegressString= new String("externalDataFromRegress");
    private String fmsdr2edrString 		= new String("fmsdr2edr");
    private String mergeEdrString 		= new String("mergeEdr");
    private String vippString 			= new String("vipp");
    private String gridString 			= new String("grid");
    private String ncString 			= new String("nc");
    private String figsGenString 		= new String("figsGen");
    private String biasFigsGenString            = new String("biasFigsGen");
    private String dataMonitorString	 	= new String("dataMonitor");
    private String cleanString	 		= new String("clean");



    ////////////////////////////////////////////////////////////////////////////
    //
    // some pre-task definiton
    //
    ////////////////////////////////////////////////////////////////////////////
    private String headerString
    	= new String(
	bashPath
	+ "#################################################################################################################\n"
	+ "#\n"
	+ "# Description:\n"
	+ "#        This is the GUI generated bash script used to run the MIRS testbed.\n"
	+ "#\n"
	+ "# Record of revisions:\n"
	+ "#           Date        Ver.        Programmer               Description of change\n"
	+ "#       ============   =====   =======================   ============================================\n"
	+ "#        09/03/2005     v0      Sid-Ahmed Boukabara       Original script created\n"
	+ "#                               (NOAA/NESDIS/ORA/IMSG)\n"
	+ "#\n"
	+ "#        02/20/2006     v1      Ninghai Sun               Modify script for operational testbed\n"
	+ "#                               (NOAA/NESDIS/ORA/IMSG)\n"
	+ "#\n"
	+ "#        03/20/2006     v2      Ninghai Sun               Modify script to compromise to SSM/IS\n" 
	+ "#                               (NOAA/NESDIS/ORA/IMSG)    Change the way to find GDAS data to standard\n"
	+ "#\n"
	+ "#        03/31/2006     v3      Sid Ahmed Boukabara       Changes related to :\n"
	+ "#                                                         (1) new footprint-matching, \n"
	+ "#                                                         (2) addition of new covariance matrix\n" 
	+ "#                                                         (3) sensor ID (to distinguish sensor-dependent classifiers),\n" 
	+ "#                                                         (4) flexible handling of scanlines shift (mhs vs amsu)\n"
	+ "#                                                         (5) bias correction method (bias removal or slope/intercept)\n"
	+ "#                                                         (6) Added threshold checking of relative humidity\n"
	+ "#        05/31/2006     v4      Sid Ahmed Boukabara       Changes related to directory structure more in line with oper.\n"
	+ "#\n"
	+ "#\n"
	+ "#        12/19/2006     v5      Sid Ahmed Boukabara       Added the capability to run the scripts in\n" 
	+ "#                              (IMSG@NOAA/NESDIS/STAR)    orbital mode in addition to daily mode.\n"
	+ "#\n"
	+ "#        12/22/2006     v6      Sid Ahmed Boukabara       Major changes to make the script as simple as possible to \n"
	+ "#                                                         maintain and ultimately to be generated through the JAVA GUI.\n"
	+ "#\n"
	+ "#        12/28/2006     v7      Sid Ahmed Boukabara       Extensive revision to make all functions general.\n"
	+ "#                              (IMSG@NOAA/NESDIS/STAR)    Scripts to be generated automatically through a Java GUI.\n"
	+ "#                                                         This removes the need to maintain as many scripts as sensors.\n"
	+ "#        03/10/2010     v8      Wanchun Chen              Many functions previously in script are moved into library.\n"
	+ "#                              (QSS/PSGS/DELL)\n"                                                                    
	+ "#\n"
	+ "#\n"
	+ "#################################################################################################################\n\n"
	+ "#------Store arguments & set starting date\n"
	+ "script=$0\n"
	+ "oldargs=(\"$@\")\n"
	+ "nargs=$#\n"
	+ "sdate=`date`\n"
	+ "MIRS_ROOT=`grep ^MIRS_ROOT "  + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n" 
	+ "HDF4LIB=`grep HDF4LIB "       + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n"
	+ "HDF5LIB=`grep HDF5LIB "       + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n"
	+ "HDFEOSLIB=`grep HDFEOSLIB "   + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n"
	+ "SZIPLIB=`grep SZIPLIB "       + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n"
	+ "ZLIBLIB=`grep ZLIBLIB "       + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n"
	+ "NETCDF4LIB=`grep NETCDF4LIB " + configPath + "paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`\n"
	+ "\n\n");
   
   private String source2ScriptString
   	= new String("#-----Include libraries and setup Info\n"
	+ ". " + libScriptPath + "script_functions.bash\n"
	+ ". " + configPath + configFile + "\n"
	+ "# Add more colon-separated shared libraries:\n"
	+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
	+ "# set stack size to unlimited\n"
	+ "ulimit -s unlimited\n\n" );
   
    private String orbitPathChangeString
   	= new String ( "rdrSensor1Path=${rdrOrbitPath}\n"
	+ "rdrSensor2Path=${rdrOrbitPath}\n\n" );

    private String displayVerifTaskString 
	= new String("displayVerif ${script} ${logFile} ${processMode} ${sensorId}" 	+ "\\\n" +
	"	${outFMAccuracy} ${prefixFMAccuracy} ${nProfs2Retr} ${nProfs2Fwd}" 	+ "\\\n" +
	"	${addDeviceNoise} ${monitorIterative} ${nAttempts} ${extBkgAtmUse}"	+ "\\\n" +
	"	${externalDataAvailable} ${monitorRetrieval} ${monitorFwd} ${geoLimit}" + "\\\n" + 
	"	${minLat} ${maxLat} ${minLon} ${maxLon} ${maxDaysArchived} ${nDaysBack}"+ "\\\n" +
	"	${tdrFormat} ${cend} ${dayUsed4Bias} ${dayUsed4Alg} ${nOrbits2Process}" + "\\\n" +
	"	${gifDensity} ${externalDataSrc} ${fmType} ${biasComputeMethod} "     	+ "\\\n" +
        "	${nChoppedFilesPerOrbit} ${retrOnOrbitOrSubOrbit} ${retrOnWhichSDR}"	+ "\\\n" + 
	"	${fwdMatrix2Use} ${makeOrNot} ${useCPU}\n\n" );

    private String resolutionTaskString
	= new String("extResol=`determineSpatialResol ${satId} ${fmType}`\n\n");
	
    private String versionTaskString
    	= new String(
	"version='9999'\n" +
	"if [[ -s ${rootPath}/version.txt ]] ; then\n" +
	"    version=`cat ${rootPath}/version.txt`\n"  +       
	"fi\n\n" +
	"os=`uname -s`\n" +
	"#Linux g95 and gfortran is the same as AIX xlf90/95(stream,unformatted)\n" +
	"#Linux ifort is (sequential,binary)\n" +
	"accessStr='sequential'\n" +
	"formStr='binary'\n" +
	"if [[ ${os} == 'Linux' ]] ; then\n" +
  	"  accessStr='sequential'\n" +
  	"  formStr='binary'\n" +
	"elif [[ ${os} == 'AIX' ]] ; then\n" +
  	"  accessStr='stream'\n" +
  	"  formStr='unformatted'\n" +
	"fi\n\n");
	
    private String orbitModeTaskString
	= new String(
	"if [[ ${nargs} -ne 1 ]] ; then\n" +
	"   ErrMessDue2UsageInOrbitalMode $0 \"Example: $0 NSS.AMBX.NN.D04260.S1110.E1256.B2054344.GC\"\n" +
	"elif [[ ${satId} == \"f16\" || ${satId} == \"f17\" || ${satId} == \"f18\" || ${satId} == \"fy3ri\" ]] ; then\n" +
	"   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}\n" +
	"   orbitInfo=`ExtractRdrFileNamesfromOrbit1 $1 ${rdrSensor1Path}`\n" +
	"elif [[ ${satId} == \"npp\" ]] ; then\n" +
	"   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}\n" +
	"   orbitInfo=`ExtractRdrFileNamesfromOrbitNpp $1 ${rdrSensor1Path}`\n" +
	"elif [[ ${satId} == \"trmm\" ]] ; then\n" +
	"   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}\n" +
	"   orbitInfo=`ExtractRdrFileNamesfromOrbitTrmm $1 ${rdrSensor1Path}`\n" +
	"elif [[ ${satId} == \"mtma\" ||  ${satId} == \"mtsa\" ]] ; then\n" +
	"   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}\n" +
	"   orbitInfo=`ExtractRdrFileNamesfromOrbitMT $1`\n" +
	"elif [[ ${satId} == \"gcomw1\" ]] ; then\n" +
	"   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}\n" +
	"   orbitInfo=`ExtractRdrFileNamesfromOrbitGcomw1 $1`\n" +
        "else\n" +
	"   SanityChecksOrbitProcess2 $1 ${rdrSensor1Path} ${rdrSensor2Path}\n" +
	"   orbitInfo=`ExtractRdrFileNamesfromOrbit2 $1 ${rdrSensor1Path} ${rdrSensor2Path}`\n" +
	"fi\n" +
	"#----Names of directories (specific to orbital processing)\n" +
	"rdrSensor1Dir=${rdrSensor1Path}\n" +
	"rdrSensor2Dir=${rdrSensor2Path}\n" +
	"#----set the generic extension (of file and directory)\n" +
	"fileExt=${orbitInfo}\n" +
	"rdirExt=''\n" +
	"rdirAnalysExt=''\n" +
	"rdirNextAnalysExt=''\n\n" );

    private String dailyModeTaskString 
	= new String(
	"gdasData=1\n" +
	"ecmwfData=2\n" +
	"gfsData=3\n\n" +
	"#----Construct date extension(s) for building directories\n" +
	"if [[ ${nargs} -eq 0 ]] ; then\n" +
	"   extensions=`DetermineYesterdExtAndAlanysExt ${nDaysBack}`\n" +
	"   date=$(echo ${extensions}|cut -c1-10)\n" +
	"   rdrSensor1Dir=${rdrSensor1Path}/${date}\n" +
	"   rdrSensor2Dir=${rdrSensor2Path}/${date}\n" +
	"elif [[ ${nargs} -eq 1 ]] ; then\n" +
	"   #----Check 3 different paths (absolute/relative/a nude dir name)\n" +
	"   indx_slash=`echo ${oldargs} | awk -v slash=\"/\" '{printf index($1,slash)}'`\n" +
	"   if [[ ${indx_slash} -eq 1 ]] ; then # =1: absolute path\n" +
	"     inputPath=${oldargs}\n" +
	"   elif [[ ${indx_slash} -gt 1 ]] ; then # >1: relative path\n" +
	"     inputPath=$(readlink -f ${oldargs}) # to get absolute full path\n" +
	"   else # <1: a nude dir name with no slash in it\n" +
	"     inputPath=${rdrSensor1Path}/${oldargs}\n" +
	"   fi\n" +
	"   date=`getDateFromFile ${satId} ${inputPath}`\n" +
	"   if [[ ${date} == 'xxxx-xx-xx' ]] ; then # something wrong\n" +
	"     op_msg \"Error: date $date is incorrect\"\n" +
	"     exit 1\n" +
	"   fi\n" +
	"   rdrSensor1Dir=${inputPath}\n" +
	"   rdrSensor2Dir=${inputPath}\n" +
	"   extensions=`DetermineExtAndAlanysExtFromArgument ${date}`\n" +
	"else\n" +
	"   ErrMessDue2UsageInDailyMode\n" + 
	"fi\n" +
	"rdirExt=$(echo ${extensions}|cut -c1-10)\n" +
	"rdirAnalysExt=$(echo ${extensions}|cut -c12-21)\n" +
	"rdirNextAnalysExt=$(echo ${extensions}|cut -c23-32)\n" +
	"#----set the generic extension\n" +
	"fileExt=`determineFileExt ${rdirExt}`\n" +
	"fileAnalysExt=`determineFileExt ${rdirAnalysExt}`\n" +
	"orbitInfo=Dummy\n\n" ) ; 

    private String directoryString 
	= new String(
	"#----Names of directories for both orbital and Daily processing\n" +
      	"tdrSensor1Dir=${tdrSensor1Path}/${rdirExt}\n" +
      	"tdrSensor2Dir=${tdrSensor2Path}/${rdirExt}\n" +
      	"sdrSensor1Dir=${sdrSensor1Path}/${rdirExt}\n" +
      	"sdrSensor2Dir=${sdrSensor2Path}/${rdirExt}\n" +
	"fmsdrDir=${fmsdrPath}/${rdirExt}\n" +
      	"fmsdrChoppDir=${choppPath}/${rdirExt}\n" +
      	"edrDir=${edrPath}/${rdirExt}\n" +
      	"depDir=${depPath}/${rdirExt}\n" +
      	"gridDir=${gridPath}/${rdirExt}\n" +
      	"ncDir=${ncPath}/${rdirExt}\n" +
	"figsDir=${figsPath}/${rdirExt}\n" +
      	"regressRetrDir=${regressRetrPath}/${rdirExt}\n" +
      	"orbitMonPath=${perfsMonitorPath}/orbitmon/\n" +
	"#----control file and input file identifiers depending on processing mode\n" +
	"if [[ ${processMode} -eq 0 ]] ; then\n" +
	"  identifier=${orbitInfo}\n" +
	"else\n" +
	"  identifier=${rdirExt}\n" +
	"fi\n" +
	"#----Control file\n" +
	"rdr2tdrSensor1ControlFile=${rdr2tdrSensor1ControlFile}_${identifier}.in\n" +
	"rdr2tdrSensor2ControlFile=${rdr2tdrSensor2ControlFile}_${identifier}.in\n" +
	"mergeNedtControlFile=${mergeNedtControlFile}_${identifier}.in\n" +
	"tdr2sdrSensor1ControlFile=${tdr2sdrSensor1ControlFile}_${identifier}.in\n" +
	"tdr2sdrSensor2ControlFile=${tdr2sdrSensor2ControlFile}_${identifier}.in\n" +
	"fmControlFile=${fmControlFile}_${identifier}.in\n" +
	"modifyNedtControlFile=${modifyNedtControlFile}_${identifier}.in\n" +
	"fmsdr2edrControlFile=${fmsdr2edrControlFile}_${identifier}.in\n" +
	"grid2nwpControlFile=${grid2nwpControlFile}_${identifier}.in\n" +
	"fwdControlFile=${fwdControlFile}_${identifier}.in\n" +
	"regressControlFile=${regressControlFile}_${identifier}.in\n" +
	"choppControlFile=${choppControlFile}_${identifier}.in\n" +
	"mergeEdrControlFile=${mergeEdrControlFile}_${identifier}.in\n" +
	"vippControlFile=${vippControlFile}_${identifier}.in\n" +
	"gridControlFile=${gridControlFile}_${identifier}.in\n" +
	"nwpGridControlFile=${nwpGridControlFile}_${identifier}.in\n" +
	"fwdGridControlFile=${fwdGridControlFile}_${identifier}.in\n" +
	"biasGridControlFile=${biasGridControlFile}_${identifier}.in\n" +
	"biasCompuControlFile=${biasCompuControlFile}_${identifier}.in\n" +
	"biasVerifControlFile=${biasVerifControlFile}_${identifier}.in\n" +
	"regressGenControlFile=${regressGenControlFile}_${identifier}.in\n" +
	"figsGenControlFile=${figsGenControlFile}_${identifier}.in\n" +
	
	"#---- Input file list\n" + 
	"rdrSensor1List=${rdrSensor1List}_${identifier}.list\n" +
	"rdrSensor2List=${rdrSensor2List}_${identifier}.list\n" +
	"tdrSensor1List=${tdrSensor1List}_${identifier}.list\n" +
	"tdrSensor2List=${tdrSensor2List}_${identifier}.list\n" +
	"sdrSensor1List=${sdrSensor1List}_${identifier}.list\n" +
	"sdrSensor2List=${sdrSensor2List}_${identifier}.list\n" +
	"fmsdrList=${fmsdrList}_${identifier}.list\n" +
	"fmsdr4BiasList=${fmsdr4BiasList}_${identifier}.list\n" +
	"fmsdr4ChoppList=${fmsdr4ChoppList}_${identifier}.list\n" +
	"fmsdr4NwpList=${fmsdr4NwpList}_${identifier}.list\n" +
	"fmsdr4BiasList=${fmsdr4BiasList}_${identifier}.list\n" +
	"fmsdr4RegressList=${fmsdr4RegressList}_${identifier}.list\n" +
	"fmsdr4ApplyRegressList=${fmsdr4ApplyRegressList}_${identifier}.list\n" +
	"edrList=${edrList}_${identifier}.list\n" +
	"edr4BiasList=${edr4BiasList}_${identifier}.list\n" +
	"dep4BiasList=${dep4BiasList}_${identifier}.list\n" +
	"edr4MergeList=${edr4MergeList}_${identifier}.list\n" +
	"depList=${depList}_${identifier}.list\n" +
	"nedtList=${nedtList}_${identifier}.list\n" +
	"nedtSensor1List=${nedtSensor1List}_${identifier}.list\n" +
	"nedtSensor2List=${nedtSensor2List}_${identifier}.list\n" +
	"gridSfcNwpAnalysList=${gridSfcNwpAnalysList}_${identifier}.list\n" +
	"gridAtmNwpAnalysList=${gridAtmNwpAnalysList}_${identifier}.list\n" +
	"nwpAnalysList=${nwpAnalysList}_${identifier}.list\n" +
	"nwpAnalysRetrList=${nwpAnalysRetrList}_${identifier}.list\n" +
	"nwpAnalys4BiasList=${nwpAnalys4BiasList}_${identifier}.list\n" +
	"nwpAnalys4RegressList=${nwpAnalys4RegressList}_${identifier}.list\n" +
	"fwdAnalys4BiasList=${fwdAnalys4BiasList}_${identifier}.list\n\n" +
	 
	"#----Names of directories exclusively for Daily processing\n" +
	"figs4BiasDir=${perfsMonitorPath}/${rdirAnalysExt}\n" +
	"qcCheckDir=${perfsMonitorPath}/${rdirExt}\n" +
	"figs4RegressDir=${perfsMonitorPath}/${rdirAnalysExt}\n" +
	"nwpAnalysDir=${nwpAnalysPath}/${rdirAnalysExt}\n" +
	"fwdAnalysDir=${fwdAnalysPath}/${rdirAnalysExt}\n" +
	"fwd2hdf5Dir=${externalDataPath}/rdr/npp_atms/${rdirAnalysExt}\n" +
	"fmsdr4BiasDir=${fmsdrPath}/${rdirAnalysExt}\n" +
	"edr4BiasDir=${edrPath}/${rdirAnalysExt}\n" +
	"dep4BiasDir=${depPath}/${rdirAnalysExt}\n" +
	"grid4BiasDir=${gridPath}/${rdirAnalysExt}\n" +
	
	"#----Names of the files dynamically generated\n" +
	"sensor1Nedt=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}.dat\n" +
	"sensor2Nedt=${nedtSensor2Path}/${satId}_${sensor2}_nedt_${fileExt}.dat\n" +
	"nedtExt=${fileExt}\n" +
	"if [[ ${satId} == \"f16\" || ${satId} == \"aqua\" || ${satId} == \"gcomw1\" || ${satId} == \"f17\" || ${satId} == \"f18\" || ${satId} == \"fy3ri\" || ${satId} == \"trmm\" || ${satId} == \"gpm\" || ${satId} == \"mtma\" || ${satId} == \"mtsa\" ]] ; then\n" +
	"  nedtBefFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}_befFM.dat\n" + 
	"  nedtAftFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}_aftFM.dat\n" +
	"  nedtDir=${nedtSensor1Path}/${rdirExt}\n" +
	"elif [[ ${satId} == \"npp\" ]]; then\n" +
	"  nedtExt=`determineNedtExt ${satId} ${rdrSensor1Dir} ${rdrType}`\n" +
	"  nedtBefFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${nedtExt}_befFM.dat\n" +
	"  nedtAftFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${nedtExt}_aftFM.dat\n" +
	"  nedtDir=${nedtSensor1Path}/${rdirExt}\n" +
	"else\n" +
	"  nedtBefFMFile=${nedtSensor1Sensor2Path}/${satId}_${sensor1}_${sensor2}_nedt_${fileExt}_befFM.dat\n" +
	"  nedtAftFMFile=${nedtSensor1Sensor2Path}/${satId}_${sensor1}_${sensor2}_nedt_${fileExt}_aftFM.dat\n" +
	"  nedtDir=${nedtSensor1Sensor2Path}/${rdirExt}\n" +
	"fi\n" +
	"sensor1Wt=${nedtSensor1Path}/${satId}_${sensor1}_wt_${fileExt}.dat\n" +
	"sensor2Wt=${nedtSensor2Path}/${satId}_${sensor2}_wt_${fileExt}.dat\n" +
	"sensor1Sensor2Wt=${nedtSensor1Sensor2Path}/${satId}_${sensor1}_${sensor2}_wt_${fileExt}.dat\n" +
	"modelErrFile=${biasPath}/ModelErrFile_${satId}_${fileExt}.dat	      #Model err file that will be generated\n" +
	"if [[ ${satId} == \"npp\" ]] ; then\n" +
	"  modelErrFile=${biasPath}/ModelErrFile_${satId}_${nedtExt}.dat\n" +
	"fi\n" +
	"logFile=${logFile}_${fileExt}.dat\n" +
	"#----Names of files dynamically generated only for daily processing \n" +
	"biasFile=${biasPath}/biasCorrec_${satId}_${fileAnalysExt}.dat	      #bias file that will be generated\n" +
	"if [[ ${satId} == \"npp\" ]] ; then\n" +
	"  biasFile=${biasPath}/biasCorrec_${satId}_${nedtExt}.dat\n" +
	"fi\n" +
	"biasCheckFile=${biasPath}/biasAfterCorr_${satId}_${fileAnalysExt}.dat #bias residual file\n" +
	"#----EDR/DEP/NWP grid/p2p products list\n" +
	"edrGridStr=\"angle,chisq,em,nattempt,niter,psfc,qc,scanday,scanpos,sfc,tbf,tbu,tbc,tbl,temp,tskin,wv,clwp,rainp,graupelp,pressure\"\n" +
	"edrP2PStr=\"em,scanpos,sfc,tbu,tskin,wspd,temp,psfc,wv\"\n" +
	"depGridStr=\"clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw\"\n" +
	"depP2PStr=\"angle,clw,iwp,lat,lon,lwp,rr,rwp,sfc2,swe,tpw\"\n" +
	"nwpGridStr=\"angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wv,clwp,rainp,graupelp,pressure\"\n" +
	"nwpP2PStr=\"clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,psfc,wv\"\n" +
	"if [[ ${satId} == \"f16\" || ${satId} == \"f17\" || ${satId} == \"f18\" || ${satId} == \"trmm\" || ${satId} == \"gpm\" || ${satId} == \"mtma\" || ${satId} == \"mtsa\" ]] ; then\n" +
	"  depGridStr=\"clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw,wspd\"\n" +
	"  depP2PStr=\"angle,clw,iwp,lat,lon,lwp,rr,rwp,sfc2,swe,tpw,wspd\"\n" +
	"  nwpGridStr=\"angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wspd,wv,clwp,rainp,graupelp,pressure\"\n" +
	"  nwpP2PStr=\"clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,wspd,psfc,wv\"\n" +
	"fi\n\n" );

    private String fwdMatrix2UseErrString 
	= new String("#---- fwd model error matrix\n" + 
	"modelErrFile1ToUse=${biasPath}/ModelErrFile_${satId}.dat\n" +
	"modelErrFile2ToUse=${biasPath}/ModelErrFile_${satId}.dat\n\n" );

    private String fwdMatrix2UseNonErrString 
	= new String("#---- fwd model non error matrix\n" +
	"modelErrFile1ToUse=${biasPath}/ModelNonErrFile_${satId}.dat\n" +
	"modelErrFile1ToUse=${biasPath}/ModelNonErrFile_${satId}.dat\n\n" );

    private String directoryGenTaskString
	= new String(
	"#---Check existence of the directories. If negative, make them\n" +
	"DirGen ${tdrSensor1Dir} \"TDR-" + "${sensor1}\"\n" +
	"if [[ $sensor2 != \"dummy\" ]] ; then \n" +
	"  DirGen ${tdrSensor2Dir} \"TDR-" + "${sensor2}\"\n" +
	"fi\n" +
	"DirGen ${sdrSensor1Dir} \"SDR-" + "${sensor1}\"\n" +
	"if [[ $sensor2 != \"dummy\" ]] ; then \n" +
	"  DirGen ${sdrSensor2Dir} \"SDR-" + "${sensor2}\"\n" +
	"fi\n" +
	"if [[ ${satId} == \"npp\" ]] ; then\n" +
	"  DirGen ${nedtDir} \"NEDT\"\n" +
	"fi\n" +
	"DirGen ${fmsdrDir} \"FM-SDR\"\n" +
	"DirGen ${fmsdrChoppDir} \"CHOPPED FMSDRs\"\n" +
	"DirGen ${edrDir} \"EDR\"\n" +
	"DirGen ${depDir} \"DEP\"\n" +
	"DirGen ${gridDir} \"GRID\"\n" +
	"DirGen ${ncDir} \"NETCDF4\"\n" +
	"DirGen ${figsDir} \"FIGS\"\n" +
	"DirGen ${figs4BiasDir} \"FIGS\"\n" +
	"DirGen ${grid4BiasDir} \"GRID-BIAS\"\n" +
	"DirGen ${qcCheckDir} \"QCCheck\"\n" +
	"DirGen ${figs4RegressDir} \"regr-FIGS\"\n" +
	"DirGen ${nwpAnalysDir} \"NWP-ANALYSIS\"\n" +
	"DirGen ${fwdAnalysDir} \"FWD SIMUL on Analyses\"\n" +
	"DirGen ${orbitMonPath} \"QC Monitor\"\n" +
	"DirGen ${regressRetrDir} \"Regression-Based Retrievals\"\n\n");

    ////////////////////////////////////////////////////////////////////////////
    //
    // task definiton section
    //
    ////////////////////////////////////////////////////////////////////////////

    private String rdr2tdrSensor1TaskString	    	
	= new String(
        "#--------------------------------------------------------------------------------\n" +
        "#      NPP/ATMS special case: to generate NEDT from SDR files\n" +
        "#--------------------------------------------------------------------------------\n" +
        "if [[ \"${step_rdr2tdrSensor1}\" -eq 1 && ${satId} == \"npp\" && ${rdrType} -eq 1 ]] ; then\n" +
          "  rdrType2=3\n" +
          "  nfile_gatmo=`ls -1 ${rdrSensor1Dir}/GATMO_npp* 2> /dev/null | wc -l`\n" +
          "  nfile_satms=`ls -1 ${rdrSensor1Dir}/SATMS_npp* 2> /dev/null | wc -l`\n" +
          "  #---- only do this when SDR files exist and have equal numer of GATMO files\n" +
          "  if [[ ${nfile_satms} -ge 1 && ${nfile_gatmo} -eq ${nfile_satms} ]] ; then\n" +
          "    rdr2tdr ${rdrSensor1Dir} ${rdrSensor1List} ${sensor1} ${tdrSensor1Dir} ${sensor1Nedt}\\\n" +
          "    ${instrumentSensor1File} ${sensor1Wt} ${nOrbits2Process} ${logFile}\\\n" +
          "    ${rdr2tdrSensor1ControlFile} ${rdr2tdrSensor1Src} ${makeOrNot} ${binPath}\\\n" +
          "    ${processMode} ${orbitInfo} ${satId} \"${rdirExt}\" ${controlDataPath}\\\n" +
          "    ${calibBiasFitFile} ${calibDTRlutFile} ${accessStr} ${formStr} ${rdrType2}\n" +
          "    #---- clean up ( only need remove TDR to avoid conflict )\n" +
          "    rm -f ${tdrSensor1Dir}/TDR*\n" +
          "  fi\n" +
        "fi\n\n" +
        
	"#--------------------------------------------------------------------------------\n" +
	"#      step: RDR to TDR conversion (sensor1)\n" +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_rdr2tdrSensor1}\" -eq 1 ]] ; then\n" + 
	"  rdr2tdr ${rdrSensor1Dir} ${rdrSensor1List} ${sensor1} ${tdrSensor1Dir} ${sensor1Nedt}" + "\\\n" +
	"  ${instrumentSensor1File} ${sensor1Wt} ${nOrbits2Process} ${logFile}" + "\\\n" +
	"  ${rdr2tdrSensor1ControlFile} ${rdr2tdrSensor1Src} ${makeOrNot} ${binPath}" + "\\\n" + 
	"  ${processMode} ${orbitInfo} ${satId} \"${rdirExt}\" ${controlDataPath}" + "\\\n" +
	"  ${calibBiasFitFile} ${calibDTRlutFile} ${accessStr} ${formStr} ${rdrType}\n"  +
	"fi\n\n" );

    private String rdr2tdrSensor2TaskString 	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: RDR to TDR conversion (sensor2)\n" +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_rdr2tdrSensor2}\" -eq 1 ]] ; then\n" + 
	"  rdr2tdr ${rdrSensor2Dir} ${rdrSensor2List} ${sensor2} ${tdrSensor2Dir} ${sensor2Nedt}" + "\\\n" +
	"  ${instrumentSensor2File} ${sensor2Wt} ${nOrbits2Process} ${logFile}" + "\\\n" + 
	"  ${rdr2tdrSensor2ControlFile} ${rdr2tdrSensor2Src} ${makeOrNot} ${binPath}" +  "\\\n" +
	"  ${processMode} ${orbitInfo} ${satId} \"${rdirExt}\" ${controlDataPath}" +  "\\\n" +
	"  ${calibBiasFitFile} ${calibDTRlutFile} ${accessStr} ${formStr} ${rdrType}\n"  + 
	"fi\n\n" );
    
    private String mergeNedtTaskString  	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Merge Different Sensor NEDT files into a single file\n" +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_mergeNedt}\" -eq 1 ]] ; then\n" + 
	"  mergeNedt ${sensor1Nedt} ${sensor2Nedt} ${nedtBefFMFile} ${logFile}" +  "\\\n" +
	"  ${mergeNedtControlFile} ${mergeNedtSrc} ${makeOrNot} ${binPath} ${satId}" +  "\\\n" + 
	"  \"${nedtNominalFile}\"  \"${nedtDir}\" \n"  + 
	"fi\n\n" );
    
    private String tdr2sdrSensor1TaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: TDR to SDR conversion (sensor1)\n" +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_tdr2sdrSensor1}\" -eq 1 ]] ; then\n" + 
	"  tdr2sdr ${tdrSensor1Dir} ${tdrSensor1List} ${sensor1} ${tdrFormat} ${sdrSensor1Dir}" + "\\\n" +
	"  ${antennaSensor1File} ${nOrbits2Process} ${logFile} ${tdr2sdrSensor1ControlFile}" + "\\\n" +
	"  ${makeOrNot} ${binPath} ${processMode} ${orbitInfo} ${tdr2sdrSrc}\n" +
	"fi\n\n" );
    
    private String tdr2sdrSensor2TaskString 	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: TDR to SDR conversion (sensor2)\n" 					+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_tdr2sdrSensor2}\" -eq 1 ]] ; then\n" + 
	"  tdr2sdr ${tdrSensor2Dir} ${tdrSensor2List} ${sensor2} ${tdrFormat} ${sdrSensor2Dir}" + "\\\n" +
	"  ${antennaSensor2File} ${nOrbits2Process} ${logFile} ${tdr2sdrSensor2ControlFile}"    + "\\\n" +
	"  ${makeOrNot} ${binPath} ${processMode} ${orbitInfo} ${tdr2sdrSrc}\n" +
	"fi\n\n" );
    
    private String fmTaskString 		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Footprint Matching (FM) sensor1/sensor2\n" 				+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fm}\" -eq 1 ]] ; then\n" + 
	"  fm ${sdrSensor1Dir} ${sdrSensor1List} ${sdrSensor2Dir} ${sdrSensor2List}" 	     + "\\\n" +
       	"  ${sensor1} ${sensor2} ${fmsdrDir} ${outFMAccuracy} ${perfsMonitorPath}"   	     + "\\\n" +
	"  ${prefixFMAccuracy} ${fmType} ${nScanLineSensor1Skip} ${nScanLineSensor2Skip}"    + "\\\n" +
       	"  ${scanLineIndexSensor2TimeColloc} ${nOrbits2Process} ${logFile} ${fmControlFile}" + "\\\n" +
       	"  ${fmSrc} ${makeOrNot} ${binPath} ${processMode} ${orbitInfo} ${satId}" 	     + "\\\n" + 
       	"  ${nedtBefFMFile} ${nedtAftFMFile} ${modifyNedtControlFile} ${figs4BiasDir}"       + "\\\n" +
	"  ${identifier} ${outFMAccuracy} ${inputDataPath} ${controlDataPath}" 		     + "\\\n" +
	"  ${geoLimit} ${minLat} ${maxLat} ${minLon} ${maxLon}\n" +
	"fi\n\n" );
	
    private String nwpGdasTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: colocation of NWP GDAS analyses with radiances (for bias)\n" 		+
	"#	note: In this step, if you get data allocate problem, you need do this:\n"    	+
	"#		ulimit -d unlimited  ( on IBM or Linux )\n"			      	+
	"#	Or	limit datasize unlimited ( on other platforms )\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_nwp}\" -eq 1 ]] ; then\n" +
	"  checkNWP=`prepNWP ${fmsdrPath} ${rdirAnalysExt} \"${extResol}\" ${satId} ${gdasData}"        + "\\\n" +
	"  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${controlDataPath} ${nwpGdasGridPath}" 		+ "\\\n" +
	"  ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}`"                                            + "\n"   +
	"  if [[ ${checkNWP} == \"1\" ]] ; then\n"     +
	"    echo NWP files needed are complete\n"     +
	"  else\n"                                     +
	"    echo NWP files needed are NOT complete\n" + 
	"    exit 1\n" +
	"  fi\n"       +
	"  nwp ${nwpGdasGridPath} ${rdirAnalysExt} ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}" 	+ "\\\n" +
        "  ${fmsdrPath} \"${extResol}\" ${fmsdr4NwpList} ${nwpAnalysDir} ${topographyFile}" 		+ "\\\n" +
	"  ${covBkgAtm1File} ${nOrbits2Process} ${logFile} ${nProfs2Fwd} ${grid2nwpControlFile}" 	+ "\\\n" +
	"  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${sensorId} ${rdirNextAnalysExt}" 		+ "\\\n" +
	"  ${CRTMcoeffPath} ${gdasData}\n" +
	"fi\n\n" );
    
    private String nwpEcmwfTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: colocation of NWP ECMWF analyses with radiances (for bias)\n" 		+
	"#	note: In this step, if you get data allocate problem, you need do this:\n"    	+
	"#		ulimit -d unlimited  ( on IBM or Linux )\n"			      	+
	"#	Or	limit datasize unlimited ( on other platforms )\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_nwp}\" -eq 1 ]] ; then\n" + 
	"  checkNWP=`prepNWP ${fmsdrPath} ${rdirAnalysExt} \"${extResol}\" ${satId} ${ecmwfData}"       + "\\\n" +
	"  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${controlDataPath} ${nwpEcmwfGridPath}" 		+ "\\\n" +
	"  ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}`"                                            + "\n"   +
	"  if [[ ${checkNWP} == \"1\" ]] ; then\n"     +
	"    echo NWP files needed are complete\n"     +
	"  else\n"                                     +
	"    echo NWP files needed are NOT complete\n" + 
	"    exit 1\n" +
	"  fi\n"       +
	"  nwp ${nwpEcmwfGridPath} ${rdirAnalysExt} ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}" 	+ "\\\n" +
        "  ${fmsdrPath} \"${extResol}\" ${fmsdr4NwpList} ${nwpAnalysDir} ${topographyFile}" 		+ "\\\n" +
	"  ${covBkgAtm1File} ${nOrbits2Process} ${logFile} ${nProfs2Fwd} ${grid2nwpControlFile}" 	+ "\\\n" +
	"  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${sensorId} ${rdirNextAnalysExt}" 		+ "\\\n" +
	"  ${CRTMcoeffPath} ${ecmwfData}\n" +
	"fi\n\n" );

    private String nwpGfsTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: colocation of NWP GFS analyses with radiances (for bias)\n" 		+
	"#	note: In this step, if you get data allocate problem, you need do this:\n"    	+
	"#		ulimit -d unlimited  ( on IBM or Linux )\n"			      	+
	"#	Or	limit datasize unlimited ( on other platforms )\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_nwp}\" -eq 1 ]] ; then\n" + 
	"  checkNWP=`prepNWP ${fmsdrPath} ${rdirAnalysExt} \"${extResol}\" ${satId} ${gfsData}"        + "\\\n" +
	"  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${controlDataPath} ${nwpGfsGridPath}" 	       + "\\\n" +
	"  ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}`"                                           + "\n"   +
	"  if [[ ${checkNWP} == \"1\" ]] ; then\n"     +
	"    echo NWP files needed are complete\n"     +
	"  else\n"                                     +
	"    echo NWP files needed are NOT complete\n" + 
	"    exit 1\n" +
	"  fi\n"       +
	"  nwp ${nwpGfsGridPath} ${rdirAnalysExt} ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}" 	+ "\\\n" +
        "  ${fmsdrPath} \"${extResol}\" ${fmsdr4NwpList} ${nwpAnalysDir} ${topographyFile}" 		+ "\\\n" +
	"  ${covBkgAtm1File} ${nOrbits2Process} ${logFile} ${nProfs2Fwd} ${grid2nwpControlFile}" 	+ "\\\n" +
	"  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${sensorId} ${rdirNextAnalysExt}" 		+ "\\\n" +
	"  ${CRTMcoeffPath} ${gfsData}\n" +
	"fi\n\n" );
    
    private String fwdGdasTaskString
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Application of forward operator on NWP GDAS analyses\n" 		      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	  + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd}" 	  + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile}"        + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${gdasData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );

    private String fwdEcmwfTaskString
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Application of forward operator on NWP ECMWF analyses\n" 		      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	  + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd}" 	  + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile}"        + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${ecmwfData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
    
    private String fwdGfsTaskString
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Application of forward operator on NWP GFS analyses\n" 		      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	  + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd}" 	  + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile}"        + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${gfsData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );

    private String bufrTaskString
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: convert into Bufr format\n" 		      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  bufr ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	  + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd} "	  + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile}"         + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${ecmwfData}\n" +
	"fi\n\n" );
    
    private String biasGenGdasTaskString
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Determination of the bias (by comparing FWD simul and meas)\n" 	      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_biasGen}\" -eq 1 ]] ; then\n" +
	"  biasGen ${determineBiasSrc} ${nwpAnalysDir} \"${extResol}\" ${nwpAnalys4BiasList}" + "\\\n" +
	"  ${fwdAnalysDir} ${fwdAnalys4BiasList} ${fmsdr4BiasDir} ${fmsdr4BiasList}" 	  + "\\\n" +
	"  ${biasCompuControlFile} ${biasComputeMethod} ${biasFile} ${modelErrFile}" 	  + "\\\n" +
	"  ${figs4BiasDir} ${nOrbits2Process} ${satId} ${IDL} ${sensorId}" 		  + "\\\n" +
	"  ${topographyFile} ${binPath} \"${modelErrNominalFile}\" ${controlDataPath} ${gdasData} ${minLat} ${maxLat} ${minLon} ${maxLon}\n" +
	"fi\n\n" );

    private String biasGenEcmwfTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Determination of the bias (by comparing FWD simul and meas)\n" 	      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_biasGen}\" -eq 1 ]] ; then\n" +
	"  biasGen ${determineBiasSrc} ${nwpAnalysDir} \"${extResol}\" ${nwpAnalys4BiasList}" + "\\\n" +
	"  ${fwdAnalysDir} ${fwdAnalys4BiasList} ${fmsdr4BiasDir} ${fmsdr4BiasList}" 	  + "\\\n" +
	"  ${biasCompuControlFile} ${biasComputeMethod} ${biasFile} ${modelErrFile}" 	  + "\\\n" +
	"  ${figs4BiasDir} ${nOrbits2Process} ${satId} ${IDL} ${sensorId}" 		  + "\\\n" +
	"  ${topographyFile} ${binPath} \"${modelErrNominalFile}\" ${controlDataPath} ${ecmwfData} ${minLat} ${maxLat} ${minLon} ${maxLon}\n" +
	"fi\n\n" );

    private String biasGenGfsTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Determination of the bias (by comparing FWD simul and meas)\n" 	      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_biasGen}\" -eq 1 ]] ; then\n" +
	"  biasGen ${determineBiasSrc} ${nwpAnalysDir} \"${extResol}\" ${nwpAnalys4BiasList}" + "\\\n" +
	"  ${fwdAnalysDir} ${fwdAnalys4BiasList} ${fmsdr4BiasDir} ${fmsdr4BiasList}" 	  + "\\\n" +
	"  ${biasCompuControlFile} ${biasComputeMethod} ${biasFile} ${modelErrFile}" 	  + "\\\n" +
	"  ${figs4BiasDir} ${nOrbits2Process} ${satId} ${IDL} ${sensorId}" 		  + "\\\n" +
	"  ${topographyFile} ${binPath} \"${modelErrNominalFile}\" ${controlDataPath} ${gfsData} ${minLat} ${maxLat} ${minLon} ${maxLon}\n" +
	"fi\n\n" );

    private String choppRadFilesTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Chopping the FMSDR file  into pieces for faster processing\n"           +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_choppRadFiles}\" -eq 1 ]] ; then\n" +
	"  chopp ${fmsdrDir} ${fmsdr4ChoppList} ${sensor1}${sensor2} ${fmsdrChoppDir}" 	      + "\\\n" +
	"  ${nChoppedFilesPerOrbit} ${logFile} ${choppControlFile} ${choppSrc} ${makeOrNot}"  + "\\\n" +
	"  ${binPath} ${processMode} ${orbitInfo} \"${extResol}\"\n" +
	"fi\n\n" ); 
    
    private String externalDataFromRegressTaskString
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: Apply regression-based algorithms on FM-SDR radiances\n"   	      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_externalDataFromRegress}\" -eq 1 ]] ; then\n" +
	"  applyRegress ${fmsdrDir} \"${extResol}\" ${fmsdr4ApplyRegressList} ${regressRetrDir}"   + "\\\n" +
	"  ${topographyFile} ${covBkgAtm1File} ${covBkgSfc1File} ${logFile} ${regressControlFile}" + "\\\n" +
        "  ${applyRegressAlgSrc} ${makeOrNot} ${binPath} ${processMode} ${orbitInfo}"   	   + "\\\n" +
	"  ${regressCoeffOceanClwFile}   ${regressCoeffSeaIceClwFile}   ${regressCoeffLandClwFile}   ${regressCoeffSnowClwFile}"   + "\\\n" + 
	"  ${regressCoeffOceanTskinFile} ${regressCoeffSeaIceTskinFile} ${regressCoeffLandTskinFile} ${regressCoeffSnowTskinFile}" + "\\\n" + 
	"  ${regressCoeffOceanTpwFile}   ${regressCoeffSeaIceTpwFile}   ${regressCoeffLandTpwFile}   ${regressCoeffSnowTpwFile}"   + "\\\n" + 
	"  ${regressCoeffOceanEmFile}    ${regressCoeffSeaIceEmFile}    ${regressCoeffLandEmFile}    ${regressCoeffSnowEmFile}"    + "\\\n" + 
	"  ${regressCoeffOceanWvFile}    ${regressCoeffSeaIceWvFile}    ${regressCoeffLandWvFile}    ${regressCoeffSnowWvFile}"    + "\\\n" + 
	"  ${regressCoeffOceanTempFile}  ${regressCoeffSeaIceTempFile}  ${regressCoeffLandTempFile}  ${regressCoeffSnowTempFile}"  + "\\\n" +
	"  ${regressCoeffOceanGwpFile}   ${regressCoeffSeaIceGwpFile}   ${regressCoeffLandGwpFile}   ${regressCoeffSnowGwpFile}"   + "\\\n" + 
	"  ${retrOnOrbitOrSubOrbit} ${fmsdrChoppDir} ${sensorId}"  	     + "\\\n" +
	"  ${nOrbits2Process} ${biasFileToUse} ${tune1File} ${version}\n"    +
	"fi\n\n" ); 
    
    private String fmsdr2edrTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#      step: EDRs retrieval from the FM-SDRs\n" 				      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fmsdr2edr}\" -eq 1 ]] ; then\n" +
	"  fmsdr2edr ${fmsdr2edrSrc} ${makeOrNot} ${retrOnWhichSDR} ${fmsdrDir}"   	       	+ "\\\n" +
	"  ${fwdAnalysDir} ${retrOnOrbitOrSubOrbit} ${fmsdrChoppDir} ${fmsdrList}" 	       	+ "\\\n" +
	"  ${nOrbits2Process} ${externalDataAvailable} ${externalDataSrc} ${nwpAnalysDir}"     	+ "\\\n" + 
	"  ${nwpAnalysRetrList} ${regressRetrDir} ${edrDir} ${nProfs2Retr}"         	       	+ "\\\n" +
	"  ${monitorIterative} ${nAttempts} ${monitorRetrieval} ${geoLimit}"   			+ "\\\n" +
	"  ${minLat} ${maxLat} ${minLon} ${maxLon} ${cend} ${sensorId}"	       			+ "\\\n" +
	"  ${tune1File} ${tune2File} ${covBkgAtm1File} ${covBkgAtm2File}"	       		+ "\\\n" +
	"  ${covBkgSfc1File} ${covBkgSfc2File} ${modelErrFile1ToUse} ${modelErrFile2ToUse} "    + "\\\n" + 
	"  ${extBkgAtmFile} ${extBkgAtmUse}"                              			+ "\\\n" + 
	"  ${nedtAftFMFile} ${monitorFile} ${topographyFile}"    	                       	+ "\\\n" +
	"  ${CRTMcoeffPath} ${logFile} ${fmsdr2edrControlFile} ${useCPU} ${processMode}" 	+ "\\\n" +
	"  ${orbitInfo} ${binPath} ${biasFileToUse} ${version} \"${extResol}\" \"${rdirExt}\"\n"    +
	"fi\n\n" );
	 
    private String mergeEdrTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Merge the mini EDR files into full orbit file\n" 				+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_mergeEdr}\" -eq 1 ]] ; then\n" +
	"  mergeEdr ${edrDir} \"${extResol}\" ${edr4MergeList} ${sensor1}${sensor2}"      	+ "\\\n"+ 
	"  ${nChoppedFilesPerOrbit} ${logFile} ${mergeEdrControlFile} ${mergeEdrSrc}" + "\\\n"  +
	"  ${makeOrNot} ${binPath} ${processMode} ${orbitInfo}\n" +
	"fi\n\n" );

    private String vippTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: To generate vertical integrated products\n" 			      	+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_vipp}\" -eq 1 ]] ; then\n" +
	"  vipp ${edrList} ${edrDir} ${depDir}  ${logFile} ${nOrbits2Process} ${nProfs2Retr}" 	+ "\\\n" + 
	"  ${sensorId} ${processMode} ${orbitInfo} \"${extResol}\" ${vippControlFile}" 	      	+ "\\\n" +
	"  ${vippSrc} ${binPath} ${siceEmissCatalogFile} ${snowEmissCatalogFile}\n" +
	"fi\n\n" );
	
    private String gridTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: To generate gridded products\n" 			      			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_grid}\" -eq 1 ]] ; then\n" +
	"  gridGen ${processMode} ${edrDir} ${depDir} ${orbitInfo} ${edrList} ${depList}" 	+ "\\\n" +
	"  \"${extResol}\" ${satId} ${fileExt} ${gridSrc} ${gridDir} ${makeOrNot} ${binPath}"   + "\\\n" +
	"  ${gridControlFile} ${gridFactor} ${minLat} ${maxLat} ${minLon} ${maxLon}"            + "\\\n" +
	"  ${fmType} \"${edrGridStr}\" \"${edrP2PStr}\" \"${depGridStr}\" \"${depP2PStr}\"\n"   +
	"fi\n\n" );
	    
    private String ncTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: To convert EDR & DEP into netcdf4 format\n" 				+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_nc}\" -eq 1 ]] ; then\n" +
	"  mirs2nc ${edrDir} ${depDir} ${ncDir} ${binPath} ${satId}\n"                          +
	"fi\n\n" );
	    
    private String figsGenTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: EDRs Figures Generation\n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_figsGen}\" -eq 1 ]] ; then\n" +
	"  figsGen ${satId} ${gridFactor} ${gridDir} ${gridSrc} ${figsDir} ${IDL}"  		+ "\\\n" + 
	"  ${identifier} ${figsGenControlFile} ${processMode} ${controlDataPath} ${version}"    + "\\\n" +
	"  ${minLat} ${maxLat} ${minLon} ${maxLon}\n" +  
	"fi\n\n" );

    private String biasFigsGenGdasTaskString
        = new String(
        "#--------------------------------------------------------------------------------\n" 	+
        "#      step: Bias Figures Generation (NWP GDAS)\n" 						+
        "#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_biasFigsGen}\" -eq 1 ]] ; then\n" +
	"  biasFigsGen ${edr4BiasDir} ${nwpAnalysDir} ${fwdAnalysDir} ${edr4BiasList}"  	+ "\\\n" +
	"  ${nwpAnalys4BiasList} ${fwdAnalys4BiasList} ${processMode} ${orbitInfo}" 		+ "\\\n" +
	"  \"${extResol}\" ${satId} ${rdirAnalysExt} ${gridSrc} ${grid4BiasDir} ${makeOrNot}"	+ "\\\n" +
 	"  ${binPath} ${nwpGridControlFile} ${fwdGridControlFile} ${biasGridControlFile}"       + "\\\n" +
 	"  ${gridFactor} ${gridSrc} ${figs4BiasDir} ${IDL} ${rdirAnalysExt}"       		+ "\\\n" +
	"  ${figsGenControlFile} ${controlDataPath}  ${dep4BiasDir} ${dep4BiasList}"		+ "\\\n" +
	"  ${version} ${biasPath} ${minLat} ${maxLat} ${minLon} ${maxLon} ${gdasData}"          + "\\\n" +
	"  ${fmType} \"${nwpGridStr}\" \"${nwpP2PStr}\" \"${nedtExt}\"\n" +
	"fi\n\n" );

    private String biasFigsGenEcmwfTaskString
        = new String(
        "#--------------------------------------------------------------------------------\n" 	+
        "#      step: Bias Figures Generation (NWP ECMWF) \n" 						+
        "#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_biasFigsGen}\" -eq 1 ]] ; then\n" +
	"  biasFigsGen ${edr4BiasDir} ${nwpAnalysDir} ${fwdAnalysDir} ${edr4BiasList}"  	+ "\\\n" +
	"  ${nwpAnalys4BiasList} ${fwdAnalys4BiasList} ${processMode} ${orbitInfo}" 		+ "\\\n" +
	"  \"${extResol}\" ${satId} ${rdirAnalysExt} ${gridSrc} ${grid4BiasDir} ${makeOrNot}"	+ "\\\n" +
 	"  ${binPath} ${nwpGridControlFile} ${fwdGridControlFile} ${biasGridControlFile}"       + "\\\n" +
 	"  ${gridFactor} ${gridSrc} ${figs4BiasDir} ${IDL} ${rdirAnalysExt}"       		+ "\\\n" +
	"  ${figsGenControlFile} ${controlDataPath}  ${dep4BiasDir} ${dep4BiasList}"		+ "\\\n" +
	"  ${version} ${biasPath} ${minLat} ${maxLat} ${minLon} ${maxLon} ${ecmwfData}"		+ "\\\n" +
	"  ${fmType} \"${nwpGridStr}\" \"${nwpP2PStr}\" \"${nedtExt}\"\n" +   
	"fi\n\n" );

    private String biasFigsGenGfsTaskString
        = new String(
        "#--------------------------------------------------------------------------------\n" 	+
        "#      step: Bias Figures Generation (NWP GFS)\n" 						+
        "#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_biasFigsGen}\" -eq 1 ]] ; then\n" +
	"  biasFigsGen ${edr4BiasDir} ${nwpAnalysDir} ${fwdAnalysDir} ${edr4BiasList}"  	+ "\\\n" +
	"  ${nwpAnalys4BiasList} ${fwdAnalys4BiasList} ${processMode} ${orbitInfo}" 		+ "\\\n" +
	"  \"${extResol}\" ${satId} ${rdirAnalysExt} ${gridSrc} ${grid4BiasDir} ${makeOrNot}"	+ "\\\n" +
 	"  ${binPath} ${nwpGridControlFile} ${fwdGridControlFile} ${biasGridControlFile}"       + "\\\n" +
 	"  ${gridFactor} ${gridSrc} ${figs4BiasDir} ${IDL} ${rdirAnalysExt}"       		+ "\\\n" +
	"  ${figsGenControlFile} ${controlDataPath}  ${dep4BiasDir} ${dep4BiasList}"		+ "\\\n" +
	"  ${version} ${biasPath} ${minLat} ${maxLat} ${minLon} ${maxLon} ${gfsData}"		+ "\\\n" +
	"  ${fmType} \"${nwpGridStr}\" \"${nwpP2PStr}\" \"${nedtExt}\"\n" +   
	"fi\n\n" );

    private String dataMonitorTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Data monitoring (plots)\n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_dataMonitor}\" -eq 1 ]] ; then\n" +
	"  qcRetrieval ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_abnormal_${satId}_${fileExt}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_namelist_${satId}_${fileExt}" + "\\\n" +
	"  \"${email}\" \"${website}\" ${controlDataPath}" + "\n\n" +
	"  dataQualityMonitor ${nedtSensor1Sensor2Path} ${nedtList} ${nedtMonitorSrc} ${IDL}" + "\\\n" + 
	"  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} ${perfsMonitorPath}\n" +
	"fi\n\n" );
    
    private String cleanTaskString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Cleaning Up\n" 								+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_clean}\" -eq 1 ]] ; then\n"   	+
	"  if [[ \"${satId}\" == \"f16\" || \"${satId}\" == \"f17\" || \"${satId}\" == \"f18\" ]] ; then\n" 	+
	"	tdrSensor2Path='Dummy'\n" 		+
	"	sdrSensor2Path='Dummy'\n" 		+
	"	rdr2tdrSensor2Src='Dummy'\n" 		+
	"	nedtSensor2Path='Dummy'\n" 		+
	"	nedtSensor1Sensor2Path='Dummy'\n" 	+
	"  fi\n" 	+
	"  clean ${satId} ${maxDaysArchived} ${tdrSensor1Path} ${tdrSensor2Path} ${sdrSensor1Path}" + "\\\n" +
	"  ${sdrSensor2Path} ${fmsdrPath} ${choppPath} ${edrPath} ${depPath}"        		    + "\\\n" +
	"  ${figsPath} ${nwpAnalysPath} ${fwdAnalysPath} ${regressRetrPath}" 	    		    + "\\\n" +
	"  ${perfsMonitorPath} ${makeOrNot} ${rdr2tdrSensor1Src} ${rdr2tdrSensor2Src}"  	    + "\\\n" +
	"  ${mergeNedtSrc} ${tdr2sdrSrc} ${fmSrc} ${nwpGenAnalysSrc} ${fwdSrc} " 		    + "\\\n" +
	"  ${applyRegressAlgSrc} ${fmsdr2edrSrc} ${controlDataPath} ${inputDataPath} ${binPath}"    + "\\\n" +
	"  ${logPath} ${prefixFMAccuracy} ${determineBiasSrc} ${regressAlgSrc}" 		    + "\\\n" +
	"  ${applyRegressAlgSrc} ${rootPath} ${processMode} ${makeClean} ${nedtSensor1Path}"        + "\\\n" + 
	"  ${nedtSensor2Path} ${nedtSensor1Sensor2Path} ${regressPath} ${biasPath} ${gridPath}\n"   +
	"fi\n\n" );

    private String finalCommentString		    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#	Final comments -End of script \n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"DisplayEndDatenTime\n\n");


    
    /**
     * MIRS web site URL
     */
    private URI uri = null; 
    private String uriString = new String("http://mirs.nesdis.noaa.gov");
    
    /**
     * MIRS web site URL Label
     */
    private JLabel uriLabel = new JLabel("<html><u>mirs.nesdis.noaa.gov</u></html>");
    
    
    /**
     * Inner class Task to do background work
     */
    class Task extends SwingWorker<Void, Void> {
        /*
         * Main task. Executed in background thread.
         */
        @Override
        public Void doInBackground() {
	
	int progress = 0;
	setProgress(0);
	
	try {

	      Runtime rt = Runtime.getRuntime();
	      
	      String [] cmd = new String[2];
	      String processMode = paths.get(processModeKey);
	      
	      if ( processMode.equals("0") ) {
	          cmd[0] = scriptPath + scriptFile ;
		  cmd[1] = fileToProcess ;
	      }
	      else if ( processMode.equals("1") ) {
		  cmd[0] = scriptPath + scriptFile ;
		  //##cmd[1] = date ;
		  cmd[1] = dir_data;
	      }
	      
              if( osName.indexOf("win") >= 0 ) {
                // need convert into unix format to remove \r for scs and pcf files if in wondows platform
                Process proc1 = new ProcessBuilder("cmd.exe", "/C dos2unix " + cmd[0] ).redirectErrorStream(true).start();
                proc1.waitFor();
                Process proc2 = new ProcessBuilder("cmd.exe", "/C dos2unix " + configPath + configFile ).redirectErrorStream(true).start();
                proc2.waitFor();

                globalProcess = new ProcessBuilder("cmd.exe", "/C bash " + cmd[0] + " " + cmd[1] ).redirectErrorStream(true).start();
              }
              else {
                globalProcess = rt.exec(cmd);
              }

	      InputStream is = globalProcess.getInputStream();
	      InputStreamReader isr = new InputStreamReader(is);
	      BufferedReader br = new BufferedReader(isr, 1024*1024);
	      
	      InputStream es = globalProcess.getErrorStream();
	      InputStreamReader esr = new InputStreamReader(es);
	      BufferedReader ber = new BufferedReader(esr);

	      //outputArea.setLineWrap(true);
	      taskOutput.setLineWrap(true);

	      String line=null;
              while ( (line = br.readLine()) != null)
              {
		outputArea.append(line + "\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		
		if ( line.startsWith("End of step") )  {
		    String stepFinished = line.substring(12,line.length());	
		    progress += 5 ;
		    setProgress( progress );
		    long now = System.currentTimeMillis();
		    double elapsedTime = ( now - startTime ) * 0.001;
		    NumberFormat formatter = NumberFormat.getNumberInstance();
		    formatter.setMaximumFractionDigits(2);
		    formatter.setMinimumFractionDigits(2);		    
		    String elapsedTimeString = formatter.format(elapsedTime);
		    taskVbar.setValues(taskVbar.getMaximum(), 8, 0, taskVbar.getMaximum());
		    taskOutput.append( stepFinished + " -- time elapsed: " + elapsedTimeString + "  seconds.\n");
		}
	      }
	      br.close();
	      
              while ( (line = ber.readLine()) != null)
              {
		outputArea.append(line + "\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
			
	      }
	      ber.close();
	      
	      processExitValue = globalProcess.exitValue();
	      //if ( processExitValue > 0 ) {
	      	//task.cancel(true);
	        //globalProcess.destroy();
	      //}
	      
        } catch (Throwable t)
        {
          //t.printStackTrace();
        }
	
            return null;
        }

        /**
         * Executed in event dispatching thread
         */
        @Override
        public void done() {
	    Toolkit.getDefaultToolkit().beep();
	    runButton.setEnabled(true);
	    generateScriptButton.setEnabled(true);
	    cancelButton.setEnabled(false);
	    setCursor(null); //turn off the wait cursor
	    
	    progressBar.setIndeterminate(false);
	    
	    if ( processExitValue == 0 ) {
	    	JOptionPane.showMessageDialog(new JFrame(),
    		"OK: Selected tasks are completed fine.",
    		"Task Complete Normally.",
    		JOptionPane.INFORMATION_MESSAGE);
	    }
	    else {
	    	JOptionPane.showMessageDialog(new JFrame(),
    		"Warning: Tasks ended abnormally or interrupted.",
    		"Task got error or interrupted.",
    		JOptionPane.ERROR_MESSAGE);
	    }

        }
    }
    
    /**
     * Constructor
     * To generate a GUI with N18 and daily mode as default.
     *
     */ 
    public Monitor() {
        
	// get osName
        osName = System.getProperty("os.name").toLowerCase();

        // get GUI current directory
        rootGUI = System.getProperty("user.dir");
        
    	// add those task = string pair into task2String map
	task2String.put("rdr2tdrSensor1", "lev1b 2 lev1b(local fmt) (Sensor1)");
	task2String.put("rdr2tdrSensor2", "lev1b 2 lev1b(local fmt) (Sensor2)");
	task2String.put("mergeNedt", "Produce NedT File");
	task2String.put("tdr2sdrSensor1", "lev1b 2 lev1c (Sensor1)");
	task2String.put("tdr2sdrSensor2", "lev1b 2 lev1c (Sensor2)");
	task2String.put("fm", "Footprint Matching");
	task2String.put("nwp", "NWP Collocation");
	task2String.put("fwd", "Forward Simulation");
	task2String.put("bufr", "To Bufr Format");
	task2String.put("biasGen", "Bias Generation");
	task2String.put("choppRadFiles", "Chop Orbits Into Granules");
	task2String.put("externalDataFromRegress", "Apply Regression");
	task2String.put("fmsdr2edr", "1dvar Advanced Algorithm");
	task2String.put("mergeEdr", "Merge Retrieved Granules");
	task2String.put("vipp", "Generate Derived Products");
	task2String.put("grid", "Generate Gridded Products");
	task2String.put("nc", "To NETCDF4 fmt");
	task2String.put("figsGen", "Figs Generation");
	task2String.put("biasFigsGen", "Bias Figs Generation");
	task2String.put("dataMonitor", "Data Monitoring");
	task2String.put("clean", "Cleaning");

    	// add those string = key pair into string2Task
	string2Task.put("lev1b 2 lev1b(local fmt) (Sensor1)", "rdr2tdrSensor1");
	string2Task.put("lev1b 2 lev1b(local fmt) (Sensor2)", "rdr2tdrSensor2");
	string2Task.put("Produce NedT File", "mergeNedt");
	string2Task.put("lev1b 2 lev1c (Sensor1)", "tdr2sdrSensor1");
	string2Task.put("lev1b 2 lev1c (Sensor2)", "tdr2sdrSensor2");
	string2Task.put("Footprint Matching", "fm");
	string2Task.put("NWP Collocation", "nwp");
	string2Task.put("Forward Simulation", "fwd");
	string2Task.put("To Bufr Format", "bufr");
	string2Task.put("Bias Generation", "biasGen");
	string2Task.put("Chop Orbits Into Granules", "choppRadFiles");
	string2Task.put("Apply Regression", "externalDataFromRegress");
	string2Task.put("1dvar Advanced Algorithm", "fmsdr2edr");
	string2Task.put("Merge Retrieved Granules", "mergeEdr");
	string2Task.put("Generate Derived Products", "vipp");
	string2Task.put("Generate Gridded Products", "grid");
	string2Task.put("To NETCDF4 fmt", "nc");
	string2Task.put("Figs Generation", "figsGen");
	string2Task.put("Bias Figs Generation", "biasFigsGen");
	string2Task.put("Data Monitoring", "dataMonitor");
	string2Task.put("Cleaning", "clean");


	// load all keys
	keys.add("satId");
	keys.add("sensor1");
	keys.add("sensor2");
	keys.add("date");
	
	keys.add("rootPath");
	keys.add("dataPath");
	keys.add("binPath");
	keys.add("logPath");
	keys.add("IDL");
	keys.add("LD_LIBRARY_PATH");
	
	keys.add("researchDataPath");
	keys.add("fwdPath");
	keys.add("out1dvarPath");
	keys.add("monitorFile");
	keys.add("modelNonErrPath");
	
	keys.add("externalDataPath");
	keys.add("rdrSensor1Path");
	keys.add("rdrSensor2Path");
	keys.add("rdrOrbitPath");
	keys.add("nwpGdasGridPath");
	keys.add("nwpEcmwfGridPath");
	keys.add("nwpGfsGridPath");
	
	keys.add("staticDataPath");
	keys.add("instrumentPath");
	keys.add("instrumentSensor1File");
	keys.add("instrumentSensor2File");
	keys.add("instrumentSensor1Sensor2File");
	keys.add("topographyFile");
	keys.add("antennaPath");
	keys.add("antennaSensor1File");
	keys.add("antennaSensor2File");
	keys.add("tune1File");
	keys.add("tune2File");
	keys.add("nedtNominalFile");
	keys.add("modelErrNominalFile");
	keys.add("covBkgAtm1File");
	keys.add("covBkgAtm2File");
	keys.add("covBkgSfc1File");
	keys.add("covBkgSfc2File");
	keys.add("extBkgAtmFile");
	keys.add("siceEmissCatalogFile");
	keys.add("snowEmissCatalogFile");
	keys.add("CRTMcoeffPath");
	
	keys.add("semiStaticDataPath");
	keys.add("biasPath");
	keys.add("regressPath");
	
	keys.add("regressCoeffOceanClwFile");
	keys.add("regressCoeffSeaIceClwFile");
	keys.add("regressCoeffLandClwFile");
	keys.add("regressCoeffSnowClwFile");
	
	keys.add("regressCoeffOceanTskinFile");
	keys.add("regressCoeffSeaIceTskinFile");
	keys.add("regressCoeffLandTskinFile");
	keys.add("regressCoeffSnowTskinFile");
	
	keys.add("regressCoeffOceanTpwFile");
	keys.add("regressCoeffSeaIceTpwFile");
	keys.add("regressCoeffLandTpwFile");
	keys.add("regressCoeffSnowTpwFile");
	
	keys.add("regressCoeffOceanEmFile");
	keys.add("regressCoeffSeaIceEmFile");
	keys.add("regressCoeffLandEmFile");
	keys.add("regressCoeffSnowEmFile");
	
	keys.add("regressCoeffOceanWvFile");
	keys.add("regressCoeffSeaIceWvFile");
	keys.add("regressCoeffLandWvFile");
	keys.add("regressCoeffSnowWvFile");
	
	keys.add("regressCoeffOceanTempFile");
	keys.add("regressCoeffSeaIceTempFile");
	keys.add("regressCoeffLandTempFile");
	keys.add("regressCoeffSnowTempFile");

	keys.add("regressCoeffOceanGwpFile");
	keys.add("regressCoeffSeaIceGwpFile");
	keys.add("regressCoeffLandGwpFile");
	keys.add("regressCoeffSnowGwpFile");
	
	keys.add("regressCoeffDesertFile");
	keys.add("biasFileToUse");
	keys.add("calibBiasFitFile");
	keys.add("calibDTRlutFile");
	
	keys.add("testbedDataPath");
	keys.add("nedtPath");
	keys.add("nedtSensor1Path");
	keys.add("nedtSensor2Path");
	keys.add("nedtSensor1Sensor2Path");
	
	keys.add("edrPath");
	keys.add("depPath");
	keys.add("gridPath");
	keys.add("ncPath");
	keys.add("figsPath");
	keys.add("perfsMonitorPath");
	keys.add("logFile");
	
	keys.add("dynamicDataPath");
	keys.add("tdrPath");
	keys.add("tdrSensor1Path");
	keys.add("tdrSensor2Path");
	keys.add("sdrPath");
	keys.add("sdrSensor1Path");
	keys.add("sdrSensor2Path");
	keys.add("fmsdrPath");
	keys.add("choppPath");
	keys.add("nwpAnalysPath");
	keys.add("fwdAnalysPath");
	keys.add("regressRetrPath");

	keys.add("controlDataPath");
	keys.add("rdr2tdrSensor1ControlFile");
	keys.add("rdr2tdrSensor2ControlFile");
	keys.add("mergeNedtControlFile");
	keys.add("tdr2sdrSensor1ControlFile");
	keys.add("tdr2sdrSensor2ControlFile");
	keys.add("fmControlFile");
	keys.add("fmsdr2edrControlFile");
	keys.add("grid2nwpControlFile");
	keys.add("fwdControlFile");
	keys.add("regressControlFile");
	keys.add("choppControlFile");
	keys.add("mergeEdrControlFile");
	keys.add("vippControlFile");
	keys.add("gridControlFile");
	keys.add("nwpGridControlFile");
	keys.add("fwdGridControlFile");
	keys.add("biasGridControlFile");
	keys.add("biasCompuControlFile");
	keys.add("biasVerifControlFile");
	keys.add("regressGenControlFile");
	keys.add("modifyNedtControlFile");
	keys.add("figsGenControlFile");
	
	keys.add("inputDataPath");
	keys.add("rdrSensor1List");
	keys.add("rdrSensor2List");
	keys.add("tdrSensor1List");
	keys.add("tdrSensor2List");
	keys.add("sdrSensor1List");	// for F16, is img; for N18,MetopA, is AMSUA
	keys.add("sdrSensor2List");	// for F16, is evn; for N18,MetopA, is MHS
	
	keys.add("sdrSensor3List");	// only to F16,las
	keys.add("sdrSensor4List");	// only to F16,uas
	
	keys.add("fmsdrList");
	keys.add("fmsdr4BiasList");
	keys.add("fmsdr4ChoppList");
	keys.add("fmsdr4NwpList");
	keys.add("fmsdr4BiasList");
	keys.add("fmsdr4RegressList");
	keys.add("fmsdr4ApplyRegressList");
	keys.add("edrList");
	keys.add("edr4BiasList");
	keys.add("dep4BiasList");
	keys.add("edr4MergeList");
	keys.add("depList");
	keys.add("nedtList");
	keys.add("nedtSensor1List");
	keys.add("nedtSensor2List");
	keys.add("gridSfcNwpAnalysList");
	keys.add("gridAtmNwpAnalysList");
	keys.add("nwpAnalysList");
	keys.add("nwpAnalysRetrList");
	keys.add("nwpAnalys4BiasList");
	keys.add("nwpAnalys4RegressList");
	keys.add("fwdAnalys4BiasList");
	
	keys.add("rdr2tdrSensor1Src");
	keys.add("rdr2tdrSensor2Src");
	keys.add("mergeNedtSrc");
	keys.add("tdr2sdrSrc");
	keys.add("fmSrc");
	keys.add("choppSrc");
	keys.add("fmsdr2edrSrc");
	keys.add("mergeEdrSrc");
	keys.add("vippSrc");
	keys.add("gridSrc");
	keys.add("ncSrc");
	keys.add("nedtMonitorSrc");
	keys.add("nwpGenAnalysSrc");
	keys.add("fwdSrc");
	keys.add("fwd2hdf5Src");
	keys.add("bufrSrc");
	keys.add("determineBiasSrc");
	keys.add("regressAlgSrc");
	keys.add("applyRegressAlgSrc");
	
	keys.add("step_rdr2tdrSensor1");
	keys.add("step_rdr2tdrSensor2");
	keys.add("step_mergeNedt");
	keys.add("step_tdr2sdrSensor1");
	keys.add("step_tdr2sdrSensor2");
	keys.add("step_fm");
	keys.add("step_nwp");
	keys.add("step_fwd");
	keys.add("step_bufr");
	keys.add("step_biasGen");
	keys.add("step_choppRadFiles");
	keys.add("step_externalDataFromRegress");
	keys.add("step_fmsdr2edr");
	keys.add("step_mergeEdr");
	keys.add("step_vipp");
	keys.add("step_grid");
	keys.add("step_nc");
	keys.add("step_figsGen");
 	keys.add("step_biasFigsGen");
	keys.add("step_dataMonitor");	
	keys.add("step_clean");
	
	keys.add("processMode");
	keys.add("sensorId");
	keys.add("outFMAccuracy");
	keys.add("prefixFMAccuracy");
	keys.add("nProfs2Retr");
	keys.add("nProfs2Fwd");
	keys.add("nAttempts");
	keys.add("fmType");
	keys.add("addDeviceNoise");
	keys.add("monitorIterative");
	keys.add("monitorRetrieval");
	keys.add("monitorFwd");
	keys.add("externalDataAvailable");
	keys.add("externalDataSrc");
	keys.add("nwpGdasUse");
	keys.add("nwpEcmwfUse");
	keys.add("nwpGfsUse");
	keys.add("extBkgAtmUse");
	keys.add("geoLimit");
	keys.add("minLat");
	keys.add("maxLat");
	keys.add("minLon");
	keys.add("maxLon");
	keys.add("cend");
	keys.add("nDaysBack");
	keys.add("maxDaysArchived");
	keys.add("dayUsed4Bias");
	keys.add("dayUsed4Alg");
	keys.add("nOrbits2Process");
	keys.add("tdrFormat");
	keys.add("rdrType");
	keys.add("gifDensity");
	keys.add("gridFactor");
	keys.add("nScanLineSensor1Skip");
	keys.add("nScanLineSensor2Skip");
	keys.add("scanLineIndexSensor2TimeColloc");
	keys.add("fwdCloudOffOrOn");
	keys.add("biasComputeMethod");
	keys.add("regressionBiasApplyDomain");
	keys.add("nChoppedFilesPerOrbit");
	keys.add("retrOnOrbitOrSubOrbit");
	keys.add("retrOnWhichSDR");
	keys.add("fwdMatrix2Use");
	keys.add("makeOrNot");
	keys.add("useCPU");
	keys.add("makeClean");
	keys.add("email");
	keys.add("website");
	
	// load taskKeys for all satellites
	taskKeys.add("step_rdr2tdrSensor1");
	taskKeys.add("step_rdr2tdrSensor2");
	taskKeys.add("step_mergeNedt");
	taskKeys.add("step_tdr2sdrSensor1");
	taskKeys.add("step_tdr2sdrSensor2");
	taskKeys.add("step_fm");
	taskKeys.add("step_nwp");
	taskKeys.add("step_fwd");
	//taskKeys.add("step_bufr");
	taskKeys.add("step_biasGen");
	taskKeys.add("step_choppRadFiles");
	taskKeys.add("step_externalDataFromRegress");
	taskKeys.add("step_fmsdr2edr");
	taskKeys.add("step_mergeEdr");
	taskKeys.add("step_vipp");
	taskKeys.add("step_grid");
	taskKeys.add("step_nc");
	taskKeys.add("step_figsGen");
	taskKeys.add("step_biasFigsGen");
	taskKeys.add("step_dataMonitor");
	taskKeys.add("step_clean");

	// load all task=value default values
  	tasks.put("step_rdr2tdrSensor1", "0");
	tasks.put("step_rdr2tdrSensor2", "0");
	tasks.put("step_mergeNedt", "0");
	tasks.put("step_tdr2sdrSensor1", "0");
	tasks.put("step_tdr2sdrSensor2", "0");
	tasks.put("step_fm", "0");
	tasks.put("step_nwp", "0");
	tasks.put("step_fwd", "0");
	//tasks.put("step_bufr", "0");
	tasks.put("step_biasGen", "0");
	tasks.put("step_choppRadFiles", "0");
	tasks.put("step_externalDataFromRegress", "0");
	tasks.put("step_fmsdr2edr", "0");
	tasks.put("step_mergeEdr", "0");
	tasks.put("step_vipp", "0");
	tasks.put("step_grid", "0");
	tasks.put("step_nc", "0");
	tasks.put("step_figsGen", "0");
	tasks.put("step_biasFigsGen", "0");
	tasks.put("step_dataMonitor", "0");
	tasks.put("step_clean", "0");

	// load comments, if any
	comments.put("LD_LIBRARY_PATH", "#OS dependent(HP is SHLIB_PATH; AIX is LIBPATH; Linux is ID_LIBRARAY_PATH)");
	comments.put("step_rdr2tdrSensor1" , "#RDR->TDR (Sensor1)");
	comments.put("step_rdr2tdrSensor2" , "#RDR->TDR (Sensor2)");
	comments.put("step_mergeNedt" , "#MERGE NEDTs (Sensor1 and Sensor2)");
	comments.put("step_tdr2sdrSensor1" , "#TDR->SDR (Sensor1)");
	comments.put("step_tdr2sdrSensor2" , "#TDR->SDR (Sensor2)");
	comments.put("step_fm" , "#FOOTPRINT MATCHING");
	comments.put("step_nwp" , "#CREATE NWP SCENE (GDAS)");
	comments.put("step_fwd" , "#USE FWD OPERATOR ON NWP SCENE");
	comments.put("step_bufr" , "#CONVERT INTO BUFR FORMAT");
	comments.put("step_biasGen" , "#GENERATE A NEW TB EC");
	comments.put("step_choppRadFiles" , "#CHOPPING RADIANCE FILES FOR MULTIPLE PROCESS SUBMISSION");
	comments.put("step_externalDataFromRegress" , "#USE OF REGRESSION ALGORIHMS TO GENERATE EXTERN DATA");
	comments.put("step_fmsdr2edr" , "#FMSDR->EDR");
	comments.put("step_mergeEdr" , "#MERGE THE MINI EDR FILES INTO A FULL ORBITAL EDR FILE ");
	comments.put("step_vipp" , "#VERTICAL INTEGRATION AND POST PROCESSING");
	comments.put("step_grid" , "#Gridded LEVEL III DATA GENERATION");
	comments.put("step_nc" , "#Convert EDR & DEP into NETCDF4 format");
	comments.put("step_figsGen" , "#FIGS GENERATION");
	comments.put("step_biasFigsGen" , "#BIAS FIGS GENERATION");
	comments.put("step_dataMonitor" , "#MONITORING OF DATA QUALITY");
	comments.put("step_clean" , "#DISK CLEANING/PURGING");
	comments.put("processMode" , "#0:Orbit processing  1:Daily processing");
	comments.put("sensorId" ,"#1:N18,2:MetopA,3:F16,4:N19,5:F18,6:ATMS,7:AMSRE,8:FY3/MWRI,9:TMI,10:GMI,12:MADRAS,13:SAPHIR,14:MetopB,15:AMSR2,18:F17");
	comments.put("outFMAccuracy" , "#Flag to output of the FM accuracy metric (DeltaTB @89)");
	comments.put("prefixFMAccuracy" , "#Prefix of file(s) w FM-acuracy metric (only if outFMaccur=1)");
	comments.put("nProfs2Retr" , "#Maximum number of profiles to process in retrieval");
	comments.put("nProfs2Fwd" , "#Maximum number of profiles to simulate using the fwd operator (over analyses)");
	comments.put("nAttempts" , "#Number of retrieval attempts in case of non-convergence ");
	comments.put("fmType" , "#POES/NPP:0(low),1(high); DMSP:0(UAS-low),1(LAS),2(ENV),3(IMG-high); TMI/AMSR2:-1(coarse),0(low),1(high)");
	comments.put("addDeviceNoise" , "#=1 Flag to add noise to the fwd simulations (over analyses), =0->no Noise added ");
	comments.put("monitorIterative" , "#=1->Yes, monitor iterative process, =0-> Do not");
	comments.put("monitorRetrieval" , "#=1->Yes, on-screen-monitoring of retrieval,  =0-> Do not.");
	comments.put("monitorFwd" , "#=1->Yes, on-screen-monitoring of fwd-simul,  =0-> Do not.");
	comments.put("externalDataAvailable" , "#=1->Ext data available, use ExtDataUse =0-> No Ext data available,");
	comments.put("externalDataSrc" , "#Source of external Data (only if externDataAvailable=1). =1-> ANALYS, =2->REGRESS");
	comments.put("nwpGdasUse"  , "#=1->To Use GDAS in NWP Collocation,  =0-> Not Use GDAS");
	comments.put("nwpEcmwfUse" , "#=1->To Use ECMWF in NWP Collocation, =0-> Not Use ECMWF");
	comments.put("nwpGfsUse" , "#=1->To Use GFS in NWP Collocation,  =0-> Not Use GFS");
	comments.put("extBkgAtmUse" , "#=1->To use spat/temp variable Atm Background, =0-> Do not use spat/temp variable Atm Background (use glbl climatology)");
	comments.put("geoLimit" , "#=0-> Process all data. =1-> only in the lat/lon box. 2->Only ocean, 3->Only land");
	comments.put("minLat" , "#Min latitude of data to be processed (only if GeogrLimits=1)");
	comments.put("maxLat" , "#Max latitude of data to be processed (only if GeogrLimits=1)");
	comments.put("minLon" , "#Min longitude of data to be processed (only if GeogrLimits=1)");
	comments.put("maxLon" , "#Max longitude of data to be processed (only if GeogrLimits=1)");
	comments.put("cend" , "#Orbit(s) 2 process: 0:ascending, 1:descending, 2:both");
	comments.put("nDaysBack" , "#TB-Bias assessed w. data from NdayBack, due to reference data delay. NdayBack between 0 & mxDaysAllowed-1");
	comments.put("maxDaysArchived" , "#Max number of days allowed for archiving (purged regularily)");
	comments.put("dayUsed4Bias" , "#Extension used to determine which bias to use in the ec process.");
	comments.put("dayUsed4Alg" , "#Extension used to determine which algorithms to use in the externDataFromRegress step");
	comments.put("nOrbits2Process" , "#Number of orbits/suborbits to process: overwrites the existing number of orbits");
	comments.put("tdrFormat" , "#Format of TDR (depends on the RDR decoder at hand):0->ascii, 1->binary");
	comments.put("rdrTYpe" , "#Raw RDR type, mainly for NPP/ATMS(1:TDR; 2:SDR-proxy from MIT; 3:SDR; 4:SDR-remap),other sensors set as 0");
	comments.put("gifDensity" , "#density used when converting PS files into Gif images");
	comments.put("gridFactor" , "#grid factor used when gridding level III data");
	comments.put("nScanLineSensor1Skip" , "#Number of sensor 1 scan lines to skip upfront (to accomodate geolocation issues) ");
	comments.put("nScanLineSensor2Skip" , "#Number of sensor 2 scan lines to skip upfront (to accomodate geolocation issues) ");
	comments.put("scanLineIndexSensor2TimeColloc" , "#Sensor2 ScanLine index (1,2 or 3) that corresponds in time to sensor1");
	comments.put("fwdCloudOffOrOn" , "#Set Hydrometeors to Zero or retain values (FWD step only).  0->Set to Zero, 1->Retain");
	comments.put("biasComputeMethod" , "#Method for computing the bias. 0->Simple bias, 1->Histogram adjustment");
	comments.put("regressionBiasApplyDomain" , "#Domain of application of bias(regression).-2 nowhere, -1->everywhere");
	comments.put("nChoppedFilesPerOrbit" , "#Number of chopped sub-orbits per orbit. If 1 no chopping is done");
	comments.put("retrOnOrbitOrSubOrbit" , "#Switches between performing retr on full-orbits (0) or chopped ones (1)");
	comments.put("retrOnWhichSDR" , "#Switches between retr on uncorrected TBs (1) or NWP-based simuls(2)");
	comments.put("fwdMatrix2Use" , "#Switches fwd model error matrix (0:dynamically generated by compar with simul, 1:Non-error)");
	comments.put("makeOrNot" , "#Switches between making (1) the executables on the fly or not (0)");
	comments.put("useCPU" , "#0->Use one CPU; 1->Use all CPUs; 2->Use QSUB");
	comments.put("makeClean" , "#During cleaning step, do we want make-clean as well (=1) or not (=0)");
	comments.put("email" , "#comma separated email addresses to notify");
	comments.put("website" , "#website address to view result");
	//comments.put("biasFileToUse" , "#bias file that will be used");


	//Create the menu bar.
	menuBar = new JMenuBar();
        
	//Build the main menu.
	fileMenu = new JMenu("File");
	editMenu = new JMenu("Edit");
	
	menuBar.add(fileMenu);
	menuBar.add(editMenu);

	//a group of JMenuItems
	openFileMenuItem = new JMenuItem("Load Config File");
	saveFileMenuItem = new JMenuItem("Save Config File");
	saveAsFileMenuItem = new JMenuItem("Save Config File As...");
	exitMenuItem = new JMenuItem("Exit");

	pathMenuItem       = new JMenuItem("Path Settings");
	preferenceMenuItem = new JMenuItem("Preference Settings");
	compileMenuItem    = new JMenuItem("Compile Options");
	
	fileMenu.add(openFileMenuItem);
	fileMenu.insertSeparator(1);
	fileMenu.add(saveFileMenuItem);
	fileMenu.add(saveAsFileMenuItem);
	fileMenu.insertSeparator(4);
	fileMenu.add(exitMenuItem);
	
	editMenu.add(pathMenuItem);
	editMenu.add(preferenceMenuItem);
	//editMenu.add(compileMenuItem);
	
	progressBar = new JProgressBar(0, 100);
	progressBar.setValue(0);	
	progressBar.setStringPainted(true);

	taskOutput = new JTextArea(3, 50);
	taskOutput.setMargin(new Insets(5,5,5,5));
	taskOutput.setEditable(false);

	// image panel
	ImagePanel imagePanel = new ImagePanel();
	imagePanel.setBackground(Color.white);
	
	JLabel titleLabel = new JLabel("MIRS Control Panel");
	titleLabel.setFont( new Font( "Nimbus Mono L", Font.BOLD, 30) ); 
	
	orbitLabel.setFont(new Font("Dialog",Font.PLAIN,12));
	profileLabel.setFont(new Font("Dialog",Font.PLAIN,12));
	fwdProfileLabel.setFont(new Font("Dialog",Font.PLAIN,12));
	
	SpringLayout titleLayout = new SpringLayout();
	titlePanel.setLayout(titleLayout);
	titlePanel.add( imagePanel );
	titlePanel.add( titleLabel );
        titlePanel.setBackground(Color.white);
	
	titleLayout.putConstraint(SpringLayout.WEST, titleLabel ,
                             240,
                             SpringLayout.EAST, imagePanel);

	SpringLayout.Constraints imageCons = titleLayout.getConstraints(imagePanel);
	imageCons.setWidth(Spring.constant(58));
	imageCons.setHeight(Spring.constant(58));
	
	SpringLayout.Constraints buttonCons = titleLayout.getConstraints(titleLabel);
	buttonCons.setWidth(Spring.constant(942));
	buttonCons.setHeight(Spring.constant(58));
        
        // mirs Link button        
        uriLabel.setHorizontalAlignment(SwingConstants.LEFT);
        uriLabel.setToolTipText("Clik here to open MiRS web site if you have a web browser");
        //uriLabel.setBorderPainted(false);
        uriLabel.setOpaque(false);
        uriLabel.setBackground(Color.lightGray);
        uriLabel.setForeground(Color.blue);
        uriLabel.addMouseListener(new MouseListener() {
                        public void mouseClicked(MouseEvent e) {
                                if (Desktop.isDesktopSupported()) {
                                        Desktop desktop = Desktop.getDesktop();
                                        try {
                                                uri = new URI(uriString);
                                                desktop.browse(uri);
                                        } catch (Exception ex) {
                                        }
                                }
                        }
                        
                        public void mouseEntered(MouseEvent e)  {}
                        public void mouseExited(MouseEvent e)   {}
                        public void mousePressed(MouseEvent e)  {}
                        public void mouseReleased(MouseEvent e) {}

                });

        titlePanel.add(uriLabel);
        SpringLayout.Constraints linkCons = titleLayout.getConstraints(uriLabel);
        //titleLayout.putConstraint(SpringLayout.SOUTH, uriLabel,
        //                     10,
        //                     SpringLayout.NORTH, titleLabel);
        
 	linkCons.setX(Spring.constant(675));
	linkCons.setY(Spring.constant(20));
	//linkCons.setWidth(Spring.constant(300));
	//linkCons.setHeight(Spring.constant(25));
        
        
	// sensor panel 
	sensorChooser.add(n18String);
  	sensorChooser.add(n19String);
	sensorChooser.add(moaString);
  	sensorChooser.add(mobString);
 	sensorChooser.add(f16String);
	sensorChooser.add(f17String);
 	sensorChooser.add(f18String);
 	sensorChooser.add(nppString);
 	sensorChooser.add(aquaString);
 	sensorChooser.add(gcomw1String);
 	sensorChooser.add(fy3riString);
 	sensorChooser.add(trmmString);
 	sensorChooser.add(gpmString);
 	sensorChooser.add(mtmaString);
 	sensorChooser.add(mtsaString);
  	//sensorChooser.add(fy3htString);
	//sensorChooser.add(windSatString);
	
	JLabel sensorLabel = new JLabel("Sensor:");
	sensorPanel.add( sensorLabel );
	sensorPanel.add( sensorChooser );
	
	// daily/orbit process
 	//processModeChooser.add(processModeString);
  	processModeChooser.add(orbitModeString);
	processModeChooser.add(dailyModeString);
	
	// dateChooser
	//dateChooser.add("Choose Date");
	dateChooser.add("Choose Directory");
	dateChooser.add("Yesterday");
	dateChooser.add("N Days Back");
	dateChooser.add("Browse");
	
	// number of 1dvar profiles
	profileNumberChooser.add("All");
	profileNumberChooser.add("1");
	profileNumberChooser.add("20");
	profileNumberChooser.add("100");
	profileNumberChooser.add("1000");
	profileNumberChooser.add("1000000");
	profileNumberChooser.add("Other");
	
	// number of fwd profiles
	fwdProfileNumberChooser.add("All");
	fwdProfileNumberChooser.add("1");
	fwdProfileNumberChooser.add("20");
	fwdProfileNumberChooser.add("100");
	fwdProfileNumberChooser.add("1000");
	fwdProfileNumberChooser.add("1000000");
	fwdProfileNumberChooser.add("Other");
	
	// number of orbits
	orbitNumberChooser.add("All");
	orbitNumberChooser.add("1");
	orbitNumberChooser.add("2");
	orbitNumberChooser.add("5");
	orbitNumberChooser.add("10");
	orbitNumberChooser.add("20");
	orbitNumberChooser.add("50");
	orbitNumberChooser.add("100");
	orbitNumberChooser.add("Other");
	
	
	////////////////////////////////////////////////////////////////
	// step panel
	////////////////////////////////////////////////////////////////
	//stepPanel.setBorder(BorderFactory.createLineBorder(Color.black));
	//stepPanel.setBorder(BorderFactory.createLoweredBevelBorder());
	//stepPanel.setBorder(BorderFactory.createTitledBorder("Task"));
	Border blackline = BorderFactory.createLineBorder(Color.black);
	Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	
	//TitledBorder titleBorder=BorderFactory.createTitledBorder(blackline, "Task Selection");;
	TitledBorder titleBorder=BorderFactory.createTitledBorder(loweredbevel, "Task Selection");;
	
	titleBorder.setTitleJustification(TitledBorder.CENTER);
	stepPanel.setBorder(titleBorder);
	
	
	////////////////////////////////////////////////////////////////
	// process mode panel.
	////////////////////////////////////////////////////////////////			
	TitledBorder modeBorder=BorderFactory.createTitledBorder(loweredbevel, "Process Mode");;	
	modeBorder.setTitleJustification(TitledBorder.CENTER);
	modePanel.setBorder(modeBorder);
	
	////////////////////////////////////////////////////////////////
	// profile panel
	////////////////////////////////////////////////////////////////			
	TitledBorder profileBorder=BorderFactory.createTitledBorder(loweredbevel, "Profile Number");;	
	profileBorder.setTitleJustification(TitledBorder.CENTER);
	profilePanel.setBorder(profileBorder);
	

	controlPanel.setLayout(controlLayout);
	
	SpringLayout.Constraints sensorCons = controlLayout.getConstraints(sensorPanel);
	sensorCons.setX(Spring.constant(0));
	sensorCons.setY(Spring.constant(0));
	sensorCons.setWidth(Spring.constant(300));
	sensorCons.setHeight(Spring.constant(40));
	
	SpringLayout.Constraints stepCons = controlLayout.getConstraints(stepPanel);
	stepCons.setX(Spring.constant(0));
	stepCons.setY(Spring.constant(40));
	stepCons.setWidth(Spring.constant(200));
	stepCons.setHeight(Spring.constant(370));

	SpringLayout.Constraints modeCons = controlLayout.getConstraints(modePanel);
	modeCons.setX(Spring.constant(0));
	modeCons.setY(Spring.constant(435));
	modeCons.setWidth(Spring.constant(285));
	modeCons.setHeight(Spring.constant(100));

	SpringLayout.Constraints profileCons = controlLayout.getConstraints(profilePanel);
	profileCons.setX(Spring.constant(0));
	profileCons.setY(Spring.constant(555));
	profileCons.setWidth(Spring.constant(285));
	profileCons.setHeight(Spring.constant(100));
		
	SpringLayout.Constraints profileLabelCons = profileLayout.getConstraints(profileLabel);
	profileLabelCons.setX(Spring.constant(5));
	profileLabelCons.setY(Spring.constant(5));
	profileLabelCons.setWidth(Spring.constant(150));
	profileLabelCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints profileNumberCons = profileLayout.getConstraints(profileNumberChooser);
	profileNumberCons.setX(Spring.constant(165));
	profileNumberCons.setY(Spring.constant(5));
	profileNumberCons.setWidth(Spring.constant(100));
	profileNumberCons.setHeight(Spring.constant(25));

	profilePanel.add(profileLabel);
	profilePanel.add(profileNumberChooser);

	SpringLayout.Constraints fwdProfileLabelCons = profileLayout.getConstraints(fwdProfileLabel);
	fwdProfileLabelCons.setX(Spring.constant(5));
	fwdProfileLabelCons.setY(Spring.constant(35));
	fwdProfileLabelCons.setWidth(Spring.constant(150));
	fwdProfileLabelCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints fwdProfileNumberCons = profileLayout.getConstraints(fwdProfileNumberChooser);
	fwdProfileNumberCons.setX(Spring.constant(165));
	fwdProfileNumberCons.setY(Spring.constant(35));
	fwdProfileNumberCons.setWidth(Spring.constant(100));
	fwdProfileNumberCons.setHeight(Spring.constant(25));

	profilePanel.add(fwdProfileLabel);
	profilePanel.add(fwdProfileNumberChooser);

	modePanel.add(processModeChooser); 
	modePanel.setLayout(modeLayout);    
	SpringLayout.Constraints processModeCons = modeLayout.getConstraints(processModeChooser);
	processModeCons.setX(Spring.constant(5));
	processModeCons.setY(Spring.constant(5));
	processModeCons.setWidth(Spring.constant(125));
	processModeCons.setHeight(Spring.constant(25));
	
	profilePanel.setLayout(profileLayout);
	
	// control panel add sensor panel, step panel and mode panel into it
	controlPanel.add(sensorPanel); 
	controlPanel.add(stepPanel); 
	controlPanel.add(modePanel); 
	controlPanel.add(profilePanel); 

	runButton.setEnabled(false);
	cancelButton.setEnabled(false);
	//quitButton.setEnabled(false);
	viewScriptButton.setEnabled(false);
	viewConfigButton.setEnabled(true);
	saveScriptAsButton.setEnabled(false);
	
	runPanel.add( generateScriptButton );
	runPanel.add( viewScriptButton );
	runPanel.add( saveScriptAsButton );
	runPanel.add( viewConfigButton );
	runPanel.add( runButton );
	runPanel.add( cancelButton );
	runPanel.add( clearButton );
	runPanel.add( quitButton );
	
	pathMenuItem.setEnabled(false);
	preferenceMenuItem.setEnabled(false);
	
	// output panel
	outputArea = new JTextArea(41,82);
	outScrollPane = new JScrollPane(outputArea);
    	hbar.setUnitIncrement(1);
    	hbar.setBlockIncrement(16);
 	outScrollPane.setHorizontalScrollBar(hbar);
	outScrollPane.setVerticalScrollBar(vbar);
	outScrollPane.setVerticalScrollBarPolicy(
                        JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	outScrollPane.setHorizontalScrollBarPolicy(
                        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	outputPanel.add ("Center", outScrollPane );
	
	
	// monitor panel
	monitorPanel.add ("North", new JLabel("Progress Monitoring") );
	progressBar.setValue(0);
	monitorPanel.add ("Center", progressBar );
	JScrollPane taskoutScrollPane = new JScrollPane(taskOutput);
	taskoutScrollPane.setHorizontalScrollBar(taskHbar);
	taskoutScrollPane.setVerticalScrollBar(taskVbar);
	taskoutScrollPane.setHorizontalScrollBarPolicy(
                        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	taskoutScrollPane.setVerticalScrollBarPolicy(
                        JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	monitorPanel.add(taskoutScrollPane, BorderLayout.SOUTH);
	
	SpringLayout totalLayout = new SpringLayout();
	setLayout(totalLayout);

	add(menuBar);
	add(titlePanel);
	add(controlPanel);
	
	add(runPanel);
	add(monitorPanel);
	add(outputLabel);
	add(outputPanel);
        
	SpringLayout.Constraints controlCons = totalLayout.getConstraints(controlPanel);
        controlCons.setX(Spring.constant(0));
        controlCons.setY(Spring.constant(100));
        controlCons.setWidth(Spring.constant(320));
        controlCons.setHeight(Spring.constant(700));
	
	SpringLayout.Constraints menuCons = totalLayout.getConstraints(menuBar);
	menuCons.setX(Spring.constant(0));
	menuCons.setY(Spring.constant(0));
	menuCons.setWidth(Spring.constant(1280));
	menuCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints titleCons = totalLayout.getConstraints(titlePanel);
	titleCons.setX(Spring.constant(0));
	titleCons.setY(Spring.constant(25));
	titleCons.setWidth(Spring.constant(1280));
	titleCons.setHeight(Spring.constant(58));
			
	SpringLayout.Constraints outputLabelCons = totalLayout.getConstraints(outputLabel);
	outputLabelCons.setX(Spring.constant(325));
	outputLabelCons.setY(Spring.constant(95));
	outputLabelCons.setWidth(Spring.constant(150));
	outputLabelCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints outputCons = totalLayout.getConstraints(outputPanel);
	outputCons.setX(Spring.constant(325));
	outputCons.setY(Spring.constant(120));
	outputCons.setWidth(Spring.constant(570));
	outputCons.setHeight(Spring.constant(450));
	
	SpringLayout.Constraints runCons = totalLayout.getConstraints(runPanel);
	runCons.setX(Spring.constant(325));
	runCons.setY(Spring.constant(585));
	runCons.setWidth(Spring.constant(570));
	runCons.setHeight(Spring.constant(50));
	
	SpringLayout.Constraints monitorCons = totalLayout.getConstraints(monitorPanel);
	monitorCons.setX(Spring.constant(325));
	monitorCons.setY(Spring.constant(655));
	monitorCons.setWidth(Spring.constant(570));
	monitorCons.setHeight(Spring.constant(100));

		
	////////////////////////////////////////////////////////////////
	// add Listeners
	////////////////////////////////////////////////////////////////
	openFileMenuItem.addActionListener( this );
	saveFileMenuItem.addActionListener( this );
	saveAsFileMenuItem.addActionListener( this );
	exitMenuItem.addActionListener( this );
	
	pathMenuItem.addActionListener(this);
	preferenceMenuItem.addActionListener( this );
	//compileMenuItem.addActionListener( this );
	
	generateScriptButton.addActionListener( this );
	viewScriptButton.addActionListener( this );
	viewConfigButton.addActionListener( this );
	saveScriptAsButton.addActionListener( this );
	runButton.addActionListener( this );
	cancelButton.addActionListener( this );
	clearButton.addActionListener( this );
	quitButton.addActionListener(this);
	//displayButton.addActionListener(this);
	
	sensorChooser.addItemListener( this );
	processModeChooser.addItemListener( this );
	dateChooser.addItemListener( this );
	browseButton.addActionListener( this );
	
	orbitNumberChooser.addItemListener( this );
	profileNumberChooser.addItemListener( this );
	fwdProfileNumberChooser.addItemListener( this );
	
	// load default config file if it found
	File file = new File(configPath+configFile);
	if ( file.exists() ) loadConfig(configFile);
	else		     loadDefaultConfig();
	
    }


    /**
     * Invoked when the user presses the Run button.
     */
    public void actionPerformed(ActionEvent evt) {
        
      if ( evt.getSource() == runButton) { 	
	
	//generateScript();
	
	String processMode = paths.get(processModeKey);
	if ( processMode != null && processMode.equals("0") && fileToProcess == null ) {
		
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: No file selected! Pls click Browse button to select a file first.",
    			"File missing",
    			JOptionPane.ERROR_MESSAGE);
			
		return;
	}     
	
	if ( fileToProcess != null  &&  processMode.equals("0") )  {
	  
	    if ( sensor.equals("n18") && fileToProcess.indexOf(".NN.") == -1 )  {
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Selected file is not NOAA-18 file.\n(Example: NSS.AMAX.NN.D07099.S1501.E1634.B0971819.WI)",
    			"Wrong File Selected",
    			JOptionPane.ERROR_MESSAGE);
		
		return;
	    }
	    else if ( sensor.equals("metopA") && fileToProcess.indexOf(".M2.") == -1 )  {
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Selected file is not MetOp-A file.\n(Example: NSS.AMAX.M2.D06305.S0203.E0347.B0017677.SV)",
    			"Wrong File Selected",
    			JOptionPane.ERROR_MESSAGE);
		
		return;
	    }
	    else if ( sensor.equals("metopB") && fileToProcess.indexOf(".M1.") == -1 )  {
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Selected file is not MetOp-B file.\n(Example: NSS.AMAX.M1.D06305.S0203.E0347.B0017677.SV)",
    			"Wrong File Selected",
    			JOptionPane.ERROR_MESSAGE);
		
		return;
	    }
	    else if ( sensor.equals("f16") && fileToProcess.indexOf(".SA.") == -1 )  {
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Selected file is not F16 SSMI/S file.\n(Example: NPR.TDRN.SA.D06032.S0426.E0604.B1181617.NS)",
    			"Wrong File Selected",
    			JOptionPane.ERROR_MESSAGE);
		
		return;
	    }
	    else if ( sensor.equals("f17") && fileToProcess.indexOf(".SB.") == -1 )  {
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Selected file is not F17 SSMI/S file.\n(Example: NPR.TDRN.SB.D13152.S0140.E0320.B3390910.NS)",
    			"Wrong File Selected",
    			JOptionPane.ERROR_MESSAGE);
		
		return;
	    }
	    else if ( sensor.equals("f18") && fileToProcess.indexOf(".SC.") == -1 )  {
		JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Selected file is not F18 SSMI/S file.\n(Example: NPR.TDRN.SC.D10151.S0434.E0611.B0317071.NS)",
    			"Wrong File Selected",
    			JOptionPane.ERROR_MESSAGE);
		
		return;
	    }
	}
	
	startTime=System.currentTimeMillis();
	
	runButton.setEnabled(false);
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	
	progressBar.setValue(0);
	
        //Instances of javax.swing.SwingWorker are not reusuable, so
        //we create new instances as needed.
        task = new Task();
        task.addPropertyChangeListener(this);
        task.execute(); 

	cancelButton.setEnabled(true);
	generateScriptButton.setEnabled(false);
	viewScriptButton.setEnabled(true);
	saveScriptAsButton.setEnabled(true);
	
	progressBar.setIndeterminate(true);
      }
         
      else if ( evt.getSource() == cancelButton) {
        
	task.cancel(true);
	globalProcess.destroy();
	processExitValue=1;
	cancelButton.setEnabled(false);

     }
      
      else if ( evt.getSource() == quitButton) { 	
	  int answer = JOptionPane.showConfirmDialog(null, "Are you sure you want to quit the system?", 
			"Exit the program", JOptionPane.YES_NO_OPTION);
	  
	  taskOutput.setText(answer + "\n");
	  
	  if ( answer == JOptionPane.YES_OPTION ) {
	  	if ( task != null )task.cancel(true);
	  	System.exit(0);
	  }
      }
  
      else if ( evt.getSource() == clearButton) { 
	
		progressBar.setValue(0);
		outputArea.setText(null);
		taskOutput.setText(null);
      
      }
      
      else if (evt.getSource() == generateScriptButton) {
           
	  Vector<String> vector = new Vector<String>();
	  
	  for(int i=0; i<taskKeys.size(); i++) {
	  
	  	if ( tasksCheckBox[i].isSelected() ) {
			//vector.add(tasksCheckBox[i].getText());
			vector.add( string2Task.get(tasksCheckBox[i].getText()) );
			//outputArea.append( tasksCheckBox[i].getText()+ "\n");
		}
	  }
	  
	  String sensorSelected = sensorChooser.getSelectedItem();
	  
	  if ( sensorSelected.equals(n18String) ) {
	  	sensor="n18";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(moaString) ) {
	  	sensor="metopA";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(mobString) ) {
	  	sensor="metopB";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(f16String) ) {
	  	sensor="f16";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(n19String) ) {
	  	sensor="n19";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(f18String) ) {
	  	sensor="f18";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(nppString) ) {
	  	sensor="npp";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(aquaString) ) {
	  	sensor="aqua";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(gcomw1String) ) {
	  	sensor="gcomw1";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(fy3riString) ) {
	  	sensor="fy3ri";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(trmmString) ) {
	  	sensor="trmm";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(gpmString) ) {
	  	sensor="gpm";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(mtmaString) ) {
	  	sensor="mtma";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(mtsaString) ) {
	  	sensor="mtsa";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(fy3htString) ) {
	  	sensor="fy3ht";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(f17String) ) {
	  	sensor="f17";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  else if ( sensorSelected.equals(windSatString) ) {
	  	sensor="windsat";
	  	paths.remove("satId");
		paths.put("satId", sensor);
	  }
	  
	  configFile = sensor + "_pcf.bash";
	  scriptFile = sensor + "_scs.bash";
	  
	  source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );

	  String processSelected = processModeChooser.getSelectedItem();
	  if ( processSelected.equals(dailyModeString) )
	  {     
	  	saveConfig(configPath+configFile);
	  	generateDailyScript(vector);
		saveScript();
	  }
	  else if ( processSelected.equals(orbitModeString) )
	  { 
	  	saveConfig(configPath+configFile);
	  	generateOrbitScript(vector);
		saveScript();
	  }

	  runButton.setEnabled(true);
	  viewScriptButton.setEnabled(true);
	  viewConfigButton.setEnabled(true);
	  saveScriptAsButton.setEnabled(true);
	   
	  outputArea.append("Sensor: " + sensorSelected + "\n");
	  vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
      }

      else if (evt.getSource() == viewScriptButton) {        
	  viewScript() ;
      }

      else if (evt.getSource() == viewConfigButton) {
          viewConfig(); 
      }

      else if (evt.getSource() == saveScriptAsButton) {         
	  saveScriptAs();
      }
      
      else if (evt.getSource() == browseButton) {
          
	  String rdrPath = getBashVariable("rdrOrbitPath") ;
	  
	  JFileChooser chooser = new JFileChooser(rdrPath);
	  int returnVal = chooser.showOpenDialog(this);
	  
	  if(returnVal == JFileChooser.APPROVE_OPTION) {
	  
		fileToProcess = chooser.getSelectedFile().getName();
	  	outputArea.append("\nRDR File selected: " + fileToProcess + "\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	  }
	  else {
	  	outputArea.append("\nOpen command cancelled by user.\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	  }
	   
      }

      else if (evt.getSource() == openFileMenuItem) {
      
	  JFileChooser chooser = new JFileChooser(".");
	  FileNameExtensionFilter filter = new FileNameExtensionFilter(
          "Bash file", "bash");
    	  chooser.setFileFilter(filter);
     	  int returnVal = chooser.showOpenDialog(this);
	  
	  if(returnVal == JFileChooser.APPROVE_OPTION) {
	  
		configFile = chooser.getSelectedFile().getName();
		loadConfig(configFile);
	  }
	  else {
	  	outputArea.append("\nOpen command cancelled by user.\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	  }
      }
      
      else if (evt.getSource() == saveFileMenuItem) {
	  
	  configFile = sensor + "_pcf.bash";
	  saveConfig(configPath+configFile);
	  outputArea.append("\nConfig file saved: " + configPath+configFile);
	  vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
      }
      
      else if (evt.getSource() == saveAsFileMenuItem) {
	  JFileChooser chooser = new JFileChooser(configPath);
	  FileNameExtensionFilter filter = new FileNameExtensionFilter(
          "Bash file", "bash");
    	  chooser.setFileFilter(filter);
     	  int returnVal = chooser.showSaveDialog(this);
	  
	  if(returnVal == JFileChooser.APPROVE_OPTION) {
 	  	configFile = chooser.getSelectedFile().getName();
		String currentPath = chooser.getCurrentDirectory().getPath() + "/" ;
		saveConfig(currentPath+configFile); 
		outputArea.append("\nConfig File Saved As: " + currentPath+configFile);
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		
	  }
	  else {
	  	outputArea.append("\nOpen command cancelled by user.\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	  }
      }
      
      else if (evt.getSource() == compileMenuItem) {

          JOptionPane.showMessageDialog(null, "Compile Option not defined yet",
    		"Compile Option.", JOptionPane.ERROR_MESSAGE); 
	  
      }
      
      else if (evt.getSource() == exitMenuItem) {
	  int answer = JOptionPane.showConfirmDialog(null, "Are you sure you want to quit the system?", 
			"Exit the program", JOptionPane.YES_NO_OPTION);
	  
	  taskOutput.setText(answer + "\n");
	  
	  if ( answer == JOptionPane.YES_OPTION ) {
	  	if ( task != null ) task.cancel(true);
	  	System.exit(0);
	  }
	  
      }

      else if (evt.getSource() == pathMenuItem) {
	  PathDialog pathDialog = new PathDialog(new JFrame(), "Paths Setting", true);
	  pathDialog.setValues(paths);
	  pathDialog.setVisible(true);
	  
	  Map<String, String> editPaths = pathDialog.getPaths();
	  Set<String> editKeys = editPaths.keySet(); 
	  Iterator it = editKeys.iterator();
    	  while (it.hasNext()) {
	    String key = (String)it.next();
	    String value = editPaths.get(key);  
	    //outputArea.append("key: "      + key    + "\n");
	    //outputArea.append("value: "    + value  + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	    // update total paths
	    paths.remove(key);
	    paths.put(key,value);
	  }
	  pathDialog.pack();
      }

      else if (evt.getSource() == preferenceMenuItem) {
      	  
	  PreferenceDialog preferenceDialog = new PreferenceDialog(new JFrame(), "Preferences", true);
	  preferenceDialog.getDefaultValues(paths);
	  preferenceDialog.setVisible(true);
	  
	  Map<String, String> editPreferences = preferenceDialog.getPreferences(); 
	  Set<String> editKeys = editPreferences.keySet(); 
	  Iterator it = editKeys.iterator();
	  //outputArea.append("\n");
    	  while (it.hasNext()) {
	    String key = (String)it.next();
	    String value = editPreferences.get(key);  
	    outputArea.append("\nkey: "	   + key    + "\n");
	    outputArea.append("value: "    + value  + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	    // update total paths
	    paths.remove(key);
	    paths.put(key,value);
	    // update global variable nwpGdasUse/nwpEcmwfUse/nwpGfsUse
	    if ( key.equals("nwpGdasUse") )  nwpGdasUse  = value;
	    if ( key.equals("nwpEcmwfUse") ) nwpEcmwfUse = value;
	    if ( key.equals("nwpGfsUse") )   nwpGfsUse   = value;
	  }
	  preferenceDialog.pack();
      }

    }
    
    
    /**
     * Action to take when Orbit/Daily selection switched
     */
    public void itemStateChanged(ItemEvent e)
    {
	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with orbit/daily change and populate different number sets
   	////////////////////////////////////////////////////////////////////////////////////////////
	if ( e.getSource() == processModeChooser )
	{
	    String processSelected = processModeChooser.getSelectedItem();
	    
	    if ( processSelected.equals(dailyModeString) )
	    { 
		String oldMode = paths.get(processModeKey);
		if ( oldMode == null || oldMode.equals("0") ) {
			paths.remove(processModeKey);
			paths.put(processModeKey,"1");
			loadDaily();
			//saveConfig(configPath+configFile); //??????
		}
	    }
	    else if ( processSelected.equals(orbitModeString) )
	    { 
		String oldMode = paths.get(processModeKey);
		if ( oldMode == null || oldMode.equals("1") ) {
			paths.remove(processModeKey);
			paths.put(processModeKey,"0");
			loadOrbit();
			//saveConfig(configPath+configFile);; //??????
		}
	    }
    	}
 
    	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with sensor change and populate corresponding different sets of tasks
   	////////////////////////////////////////////////////////////////////////////////////////////
	else if ( e.getSource() == sensorChooser )
	{	
	    String sensorSelected = sensorChooser.getSelectedItem();
	    if ( sensorSelected.equals(n18String) )
	    { 
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"1"); 
		sensor="n18";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		// update taskKeys and tasks
		tasks2N18();
		fwdChopp2N18();
		
		File file = new File(configPath+configFile);
		
		if ( file.exists() ) loadConfig(configFile);
		else		     loadDefaultConfig();
		
	    }
	    else if ( sensorSelected.equals(moaString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"2"); 
		sensor="metopA";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$NETCDF4LIB\n"
		+ "if [[ ${satId} == \"trmm\" ]] ; then\n"
		+ "  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB\n"
		+ "fi\n\n"
		+ "if [[ ${satId} == \"npp\" ]] ; then\n"
		+ "  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF5LIB\n"
		+ "fi\n\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		// update taskKeys and tasks
		tasks2Moa();
		fwdChopp2Moa();
		
		File file = new File(configPath+configFile);
		
		if ( file.exists() ) loadConfig(configFile);
		else		     loadMetopAConfig();
		
	    }
	    else if ( sensorSelected.equals(mobString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"14"); 
		sensor="metopB";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$NETCDF4LIB\n"
		+ "if [[ ${satId} == \"trmm\" ]] ; then\n"
		+ "  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB\n"
		+ "fi\n\n"
		+ "if [[ ${satId} == \"npp\" ]] ; then\n"
		+ "  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF5LIB\n"
		+ "fi\n\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		// update taskKeys and tasks
		tasks2Mob();
		fwdChopp2Mob();
		
		File file = new File(configPath+configFile);
		
		if ( file.exists() ) loadConfig(configFile);
		else		     loadMetopBConfig();
		
	    }
	    else if ( sensorSelected.equals(f16String) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"3"); 
		sensor="f16";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2F16();
		fwdChopp2F16();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadF16Config();

	    }
	    else if ( sensorSelected.equals(n19String) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"4"); 
		sensor="n19";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2N19();
		fwdChopp2N19();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadN19Config();
		
	    }
	    else if ( sensorSelected.equals(f18String) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"5"); 
		sensor="f18";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2F18();
		fwdChopp2F18();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadF18Config();
	    }
	    else if ( sensorSelected.equals(nppString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"6"); 
		sensor="npp";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Npp();
		fwdChopp2Npp();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadNppConfig();
	    }
	    else if ( sensorSelected.equals(aquaString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"7"); 
		sensor="aqua";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Aqua();
		fwdChopp2Aqua();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadAquaConfig();
	    }
	    else if ( sensorSelected.equals(gcomw1String) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"15"); 
		sensor="gcomw1";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Gcomw1();
		fwdChopp2Gcomw1();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadGcomw1Config();
	    }
	    else if ( sensorSelected.equals(fy3riString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"8"); 
		sensor="fy3ri";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Fy3ri();
		fwdChopp2Fy3ri();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadFy3riConfig();
	    }
	    else if ( sensorSelected.equals(trmmString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"9"); 
		sensor="trmm";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Trmm();
		fwdChopp2Trmm();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadTrmmConfig();
	    }
	    else if ( sensorSelected.equals(gpmString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"10"); 
		sensor="gpm";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Gpm();
		fwdChopp2Gpm();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadGpmConfig();
	    }
	    else if ( sensorSelected.equals(mtmaString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"11"); 
		sensor="mtma";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Mtma();
		fwdChopp2Mtma();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadMtmaConfig();
	    }
	    else if ( sensorSelected.equals(mtsaString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"12"); 
		sensor="mtsa";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Mtsa();
		fwdChopp2Mtsa();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadMtsaConfig();
	    }
	    else if ( sensorSelected.equals(fy3htString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"10"); 
		sensor="fy3ht";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2Fy3ht();
		fwdChopp2Fy3ht();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadFy3htConfig();
	    }
	    else if ( sensorSelected.equals(f17String) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"18"); 
		sensor="f17";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		tasks2F17();
		fwdChopp2F17();
		
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadF17Config();
	    }
	    else if ( sensorSelected.equals(windSatString) )
	    {
		paths.remove(sensorIdKey);
		paths.put(sensorIdKey,"12"); 
		sensor="windsat";
		paths.remove("satId");
		paths.put("satId", sensor);
		configFile=sensor + "_pcf.bash";
		scriptFile=sensor + "_scs.bash";
		
		source2ScriptString = 
		new String("#-----Include libraries and setup Info\n"
		+ ". " + libScriptPath + "script_functions.bash\n"
		+ ". " + configPath + configFile + "\n"
		+ "# Add more colon-separated shared libraries here:\n"
		+ "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB\n"
		+ "# set stack size to unlimited\n"
		+ "ulimit -s unlimited\n\n" );
		
		/**
		File file = new File(configPath+configFile);
		if ( file.exists() ) loadConfig(configFile);
		else		     loadWindsat6Config();
		*/
	    }	
	}

	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with profileNumberChooser change
   	////////////////////////////////////////////////////////////////////////////////////////////  
	else if ( e.getSource() == profileNumberChooser ) {
	
	    String oldVal = paths.get(nProfs2RetrKey);
	    String nProfs2Retr = profileNumberChooser.getSelectedItem();
	    
	    if ( nProfs2Retr.equals("Other") ) {
	        
		String str = (String)JOptionPane.showInputDialog(
                    new JFrame(),"Number of Profiles (1dvar) (must be positive integer):\n", 
		    "Input Number of Profiles (1dvar)",
                    JOptionPane.PLAIN_MESSAGE, null, null, "");
	    	
		try {
		    int nprofs2retr = Integer.parseInt(str);
		    outputArea.append("\nInput Number of Profiles (1dvar): " + nprofs2retr + "\n" );
		    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		    paths.remove(nProfs2RetrKey);
		    paths.put(nProfs2RetrKey, str);
		    
		    if ( ! alreadyContains(profileNumberChooser, str) ) 
		    	profileNumberChooser.add(str);
		    profileNumberChooser.select(str);
		} catch(NumberFormatException nfe) {
		
		    JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Number of Profiles (1dvar) must be a positive integer!",
    			"Profile Number (1dvar) Error",
    			JOptionPane.ERROR_MESSAGE);
			
		    profileNumberChooser.select(oldVal);
		}
		
	    }
	    else {
	    	outputArea.append("Profile Number Select (1dvar): " + nProfs2Retr + "\n" );
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		if ( nProfs2Retr != null && !nProfs2Retr.equals(oldVal)) {
	    	    paths.remove(nProfs2RetrKey);
	    	    paths.put(nProfs2RetrKey, nProfs2Retr);
		}
	    }
	}

	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with fwdProfileNumberChooser change
   	////////////////////////////////////////////////////////////////////////////////////////////  
	else if ( e.getSource() == fwdProfileNumberChooser ) {
	
	    String oldVal = paths.get(nProfs2FwdKey);
	    String nProfs2Fwd = fwdProfileNumberChooser.getSelectedItem();
	    
	    if ( nProfs2Fwd.equals("Other") ) {
	        
		String str = (String)JOptionPane.showInputDialog(
                    new JFrame(),"Number of Profiles (fwd) (must be positive integer):\n", "Input Number of Profiles (fwd)",
                    JOptionPane.PLAIN_MESSAGE, null, null, "");
	    	
		try {
		    int nprofs2fwd = Integer.parseInt(str);
		    outputArea.append("\nInput Number of Profiles (fwd): " + nprofs2fwd + "\n" );
		    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		    paths.remove(nProfs2FwdKey);
		    paths.put(nProfs2FwdKey, str);
		    
		    if ( ! alreadyContains(fwdProfileNumberChooser, str) ) 
		    	fwdProfileNumberChooser.add(str);
		    fwdProfileNumberChooser.select(str);
		    
		} catch(NumberFormatException nfe) {
		
		    JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Number of Profiles (fwd) must be a positive integer!",
    			"Profile Number (fwd) Error",
    			JOptionPane.ERROR_MESSAGE);
			
		    fwdProfileNumberChooser.select(oldVal);
		}
		
	    }
	    else {
	    	outputArea.append("Profile Number Select: " + nProfs2Fwd + "\n" );
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		if ( nProfs2Fwd != null && !nProfs2Fwd.equals(oldVal)) {
	    	    paths.remove(nProfs2FwdKey);
	    	    paths.put(nProfs2FwdKey, nProfs2Fwd);
		}
	    }
	}


	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with orbitNumberChooser change
   	////////////////////////////////////////////////////////////////////////////////////////////  
	else if ( e.getSource() == orbitNumberChooser ) {
	    
	    String oldVal = paths.get(nOrbits2ProcessKey);
	    String nOrbits2Process = orbitNumberChooser.getSelectedItem();
	    
	    if ( nOrbits2Process.equals("Other") ) {
	        
		String str = (String)JOptionPane.showInputDialog(
                    new JFrame(),"Number of Orbits (must be positive integer):\n", 
		    "Input Number of Orbits",
                    JOptionPane.PLAIN_MESSAGE, null, null, "");
	    	
		try {
		    int norbits2process = Integer.parseInt(str);
		    outputArea.append("\nInput Number of Orbits: " + norbits2process + "\n" );
		    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		    paths.remove(nOrbits2ProcessKey);
		    paths.put(nOrbits2ProcessKey, str);
		    
		    if ( ! alreadyContains(orbitNumberChooser, str) ) 
		    	orbitNumberChooser.add(str);
		    orbitNumberChooser.select(str);
		    
		} catch(NumberFormatException nfe) {
		
		    JOptionPane.showMessageDialog(new JFrame(),
    			"Error: Number of Orbits must be a positive integer!",
    			"Number of Orbits Error",
    			JOptionPane.ERROR_MESSAGE);
		    
		     orbitNumberChooser.select(oldVal);  
		}
		
	    }
	    else {
	    	outputArea.append("\nNumber of Orbits Selected: " + nOrbits2Process + "\n" );
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		if ( nOrbits2Process != null && !nOrbits2Process.equals(oldVal)) {
		    paths.remove(nOrbits2ProcessKey);
	    	    paths.put(nOrbits2ProcessKey, nOrbits2Process);
		}
	    }

	}

    	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with sensor change and populate corresponding different sets of tasks
   	////////////////////////////////////////////////////////////////////////////////////////////
	else if ( e.getSource() == dateChooser )
	{
	    String dateChoiceSelected = dateChooser.getSelectedItem();
	    if ( dateChoiceSelected.equals("Yesterday") ) {
		Date today = new Date();
		long todayTime = today.getTime();
		long yesterdayTime  = todayTime - 24*3600*1000;
		Date yesterday = new Date(yesterdayTime);
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		date = df.format(yesterday);
		
		paths.remove("date");
		paths.put("date", date);
		
		dir_data = date;
		
		outputArea.append("\nDate is Yesterday: " + date + "\n");
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	    }
	    else if ( dateChoiceSelected.equals("N Days Back") ) {
		String oldNDaysBack = paths.get(nDaysBackKey);

		Object[] possibilities = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
					"11", "12", "13", "14", "15", "16", "17", "18", "19","20",
					"21", "22", "23", "24", "25", "26", "27", "28", "29","30","31"}; 
		String nDaysBack = (String)JOptionPane.showInputDialog( new JFrame(), 
			"Number of days agos' reference data (GDAS/ECMWF/GFS) used in TB bias computation:\n", "nDaysBack(reference data)", 
			JOptionPane.PLAIN_MESSAGE, null, possibilities, oldNDaysBack ); 
		
		if ( nDaysBack != null ) {
			paths.remove(nDaysBackKey);
			paths.put(nDaysBackKey, nDaysBack);
			outputArea.append("\nnDaysBack: " + nDaysBack + "\n");
		}
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	    }
	    else if ( dateChoiceSelected.equals("Browse") ) {
		
		String rdrPath = getBashVariable("rdrSensor1Path");
		
		JFileChooser chooser = new JFileChooser(rdrPath);
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		int returnVal = chooser.showOpenDialog(this);
		
		if(returnVal == JFileChooser.APPROVE_OPTION) {
		    //##date = chooser.getSelectedFile().getName();
		    dir_data = chooser.getSelectedFile().getName();
		    try { 
		      String tmpFile = configPath + "dateHelp.bash";
		      File aFile = new File(tmpFile);
		      PrintWriter out = new PrintWriter(new FileWriter(tmpFile));
		      out.println(bashPath);
		      out.println(". " + scriptPath + "script_functions.bash\n");
		      //##out.println("getDateFromFile " + sensor + " " + rdrPath + "/" + date +"\n");
		      out.println("getDateFromFile " + sensor + " " + rdrPath + "/" + dir_data +"\n");
		      out.close();
		      aFile.setExecutable(true, false);

		      Process proc = null;
		      
                      if( osName.indexOf("win") >= 0 ) {
		      	  try 
			  {
                                Process proc2 = new ProcessBuilder("cmd.exe", "/C dos2unix " + tmpFile ).redirectErrorStream(true).start();
                                proc2.waitFor();
                          } catch( InterruptedException iex )
                          { 
		        	System.out.println(iex.toString());
		          }						
                          proc = new ProcessBuilder("cmd.exe", "/C bash "+ tmpFile ).redirectErrorStream(true).start();
                      }
                      else {
                          Runtime rt = Runtime.getRuntime();
                          proc = rt.exec(tmpFile);
                      }

		      InputStream is = proc.getInputStream();
		      InputStreamReader isr = new InputStreamReader(is);
		      BufferedReader br = new BufferedReader(isr);

		      String line = br.readLine();
		      if ( line != null && ! line.equals("xxxx-xx-xx") ) {
		          //##outputArea.append("\nData Directory=" + date + "\n");
		          outputArea.append("\nData Directory=" + dir_data + "\n");
			  date = line;
		          paths.remove("date");
		          paths.put("date", date);
		          outputArea.append("\nDate Selected: " + date + "\n");
		          vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		      }
		      else {
		          outputArea.append("\nError: Date is not correct.\n");
		          vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		      }

		      boolean deleted = aFile.delete();

		    } catch ( IOException ioe )
		    { 		
		      outputArea.append("" + ioe);
		      vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());		
		    }
		    
		}
		else {
		    outputArea.append("\nDate Select cancelled by user.\n");
		    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
		}
		
	    }
	    /**
	    else if ( dateChoiceSelected.equals("Browse") && sensor.equals("f16") ) {
		
		DateDialog dateDialog = new DateDialog(new JFrame(), "Date Selection", true);
		dateDialog.setVisible(true);
		date = dateDialog.getDate();
		paths.remove("date");
		paths.put("date", date);
		outputArea.append("\nDate Selected: " + date + "\n");
	    } */

 	}
	
    	////////////////////////////////////////////////////////////////////////////////////////////
	// to deal with task status changes
   	////////////////////////////////////////////////////////////////////////////////////////////
	else {
		for(int i=0; i<taskKeys.size(); i++) {
			if ( e.getSource() == tasksCheckBox[i] ) {
 
 				//String label = e.getActionCommand(); we need convert into task key
				//String label = tasksCheckBox[i].getText();
				String label = string2Task.get(tasksCheckBox[i].getText());
				
				String pathKey = new String("step_").concat(label);
				paths.remove( pathKey );
				tasks.remove( label );
				if ( tasksCheckBox[i].isSelected() ) {
					paths.put(pathKey, "1");
					tasks.put(label,"1");
					
					if ( label.equals(fmsdr2edrString) )
					{	
						profileLabel.setEnabled(true);
						profileNumberChooser.setEnabled(true);
						
						Graphics g = profilePanel.getGraphics(); 
	    					profilePanel.paintAll(g);
	    					profilePanel.setVisible(true);
					}
					else if ( label.equals(fwdString) )
					{	
						fwdProfileLabel.setEnabled(true);
						fwdProfileNumberChooser.setEnabled(true);
						
						Graphics g = profilePanel.getGraphics(); 
	    					profilePanel.paintAll(g);
	    					profilePanel.setVisible(true);		
					}
				}
				else {
					paths.put(pathKey, "0");
					tasks.put(label,"0");
					
					if ( label.equals(fmsdr2edrString) ) 
					{
						profileLabel.setEnabled(false);
						profileNumberChooser.setEnabled(false);
						Graphics g = profilePanel.getGraphics(); 
	    					profilePanel.paintAll(g);
	    					profilePanel.setVisible(true);		
					}
					else if ( label.equals(fwdString) )
					{
						fwdProfileLabel.setEnabled(false);
						fwdProfileNumberChooser.setEnabled(false);
						Graphics g = profilePanel.getGraphics(); 
	    					profilePanel.paintAll(g);
	    					profilePanel.setVisible(true);
					}
				}
			}
		}
	}
	
    }
    
    
    /**
     * Generate daily script based on a vector of tasks
     */
    private void generateDailyScript(Vector vector) {
        
	int len=taskBuffer.length();
	taskBuffer.delete(0, len);
	
	taskBuffer.append(headerString);
	taskBuffer.append(source2ScriptString);
	taskBuffer.append(displayVerifTaskString);
	taskBuffer.append(resolutionTaskString);
	taskBuffer.append(versionTaskString);
	taskBuffer.append(dailyModeTaskString);
	taskBuffer.append(directoryString);

	String fwdMatrix2Use = paths.get("fwdMatrix2Use");
	if ( fwdMatrix2Use != null && fwdMatrix2Use.equals("0") )
		taskBuffer.append(fwdMatrix2UseErrString);
	else if ( fwdMatrix2Use != null && fwdMatrix2Use.equals("1") )
		taskBuffer.append(fwdMatrix2UseNonErrString);
	
	taskBuffer.append(directoryGenTaskString);
		
	try {
	      
	    for ( int i=0; i<vector.size(); i++ ) 
	    {
		String taskName = (String)vector.elementAt(i);
		
		if ( taskName.equals(rdr2tdrSensor1String) ) {
			taskBuffer.append(rdr2tdrSensor1TaskString);
		}
		else if ( taskName.equals(rdr2tdrSensor2String) ) {
			taskBuffer.append(rdr2tdrSensor2TaskString);
		}
		else if ( taskName.equals(mergeNedtString) ) {
			taskBuffer.append(mergeNedtTaskString);
		}
		else if ( taskName.equals(tdr2sdrSensor1String) ) {
			taskBuffer.append(tdr2sdrSensor1TaskString);
		}
		else if ( taskName.equals(tdr2sdrSensor2String) ) {
			taskBuffer.append(tdr2sdrSensor2TaskString);
		}
		else if ( taskName.equals(fmString) ) {
			taskBuffer.append(fmTaskString);
		}
		else if ( taskName.equals(nwpString) ) {
		    if ( nwpGdasUse.equals("1") )
			taskBuffer.append(nwpGdasTaskString);
		    if ( nwpEcmwfUse.equals("1") ) 
			taskBuffer.append(nwpEcmwfTaskString);
		    if ( nwpGfsUse.equals("1") )
			taskBuffer.append(nwpGfsTaskString);
		}
		else if ( taskName.equals(fwdString) ) {
		    if ( nwpGdasUse.equals("1") )
			taskBuffer.append(fwdGdasTaskString);
		    if ( nwpEcmwfUse.equals("1") )
			taskBuffer.append(fwdEcmwfTaskString);
		    if ( nwpGfsUse.equals("1") )
			taskBuffer.append(fwdGfsTaskString);
		}
		else if ( taskName.equals(bufrString) ) {
			taskBuffer.append(bufrTaskString);
		}
		else if ( taskName.equals(biasGenString) ) {
		    if ( nwpGdasUse.equals("1") )
			taskBuffer.append(biasGenGdasTaskString);
		    if ( nwpEcmwfUse.equals("1") )
			taskBuffer.append(biasGenEcmwfTaskString);
		    if ( nwpGfsUse.equals("1") )
			taskBuffer.append(biasGenGfsTaskString);
		}
		else if ( taskName.equals(choppRadFilesString) ) {
			taskBuffer.append(choppRadFilesTaskString);
		}
		else if ( taskName.equals(externalDataFromRegressString) ) {
			taskBuffer.append(externalDataFromRegressTaskString);
		}
		else if ( taskName.equals(fmsdr2edrString) ) {
			taskBuffer.append(fmsdr2edrTaskString);
		}
		else if ( taskName.equals(mergeEdrString) ) {
			taskBuffer.append(mergeEdrTaskString);
		}
		else if ( taskName.equals(vippString) ) {
			taskBuffer.append(vippTaskString);
		}
		else if ( taskName.equals(gridString) ) {
			taskBuffer.append(gridTaskString);
		}
		else if ( taskName.equals(ncString) ) {
			taskBuffer.append(ncTaskString);
		}
		else if ( taskName.equals(figsGenString) ) {
			taskBuffer.append(figsGenTaskString);
		}
                else if ( taskName.equals(biasFigsGenString) ) {
		    if ( nwpGdasUse.equals("1") )
                        taskBuffer.append(biasFigsGenGdasTaskString);
		    if ( nwpEcmwfUse.equals("1") )
                        taskBuffer.append(biasFigsGenEcmwfTaskString);
		    if ( nwpGfsUse.equals("1") )
                        taskBuffer.append(biasFigsGenGfsTaskString);
                }
		else if ( taskName.equals(dataMonitorString) ) {
			taskBuffer.append(dataMonitorTaskString);
		}
		else if ( taskName.equals(cleanString) ) {
			taskBuffer.append(cleanTaskString);
		}
	    }

        } catch (Throwable t)
        {
          t.printStackTrace();
        }
       
    }
 
   
    /**
     * Generate orbit script based on a vector of tasks
     */
    private void generateOrbitScript(Vector vector) {
        
	int len=taskBuffer.length();
	taskBuffer.delete(0,len);
	taskBuffer.append(headerString);
	taskBuffer.append(source2ScriptString);
	taskBuffer.append(orbitPathChangeString);
	taskBuffer.append(displayVerifTaskString);
	taskBuffer.append(resolutionTaskString);	
	taskBuffer.append(versionTaskString);	
	taskBuffer.append(orbitModeTaskString);
	taskBuffer.append(directoryString);
	
	String fwdMatrix2Use = paths.get("fwdMatrix2Use");
	if ( fwdMatrix2Use != null && fwdMatrix2Use.equals("0") )
		taskBuffer.append(fwdMatrix2UseErrString);
	else if ( fwdMatrix2Use != null && fwdMatrix2Use.equals("1") )
		taskBuffer.append(fwdMatrix2UseNonErrString);

	taskBuffer.append(directoryGenTaskString);
		
	try {
	      
	    for ( int i=0; i<vector.size(); i++ ) 
	    {
		String taskName = (String)vector.elementAt(i);
		
		if ( taskName.equals(rdr2tdrSensor1String) ) {
			taskBuffer.append(rdr2tdrSensor1TaskString);
		}
		else if ( taskName.equals(rdr2tdrSensor2String) ) {
			taskBuffer.append(rdr2tdrSensor2TaskString);
		}
		else if ( taskName.equals(mergeNedtString) ) {
			taskBuffer.append(mergeNedtTaskString);
		}
		else if ( taskName.equals(tdr2sdrSensor1String) ) {
			taskBuffer.append(tdr2sdrSensor1TaskString);
		}
		else if ( taskName.equals(tdr2sdrSensor2String) ) {
			taskBuffer.append(tdr2sdrSensor2TaskString);
		}
		else if ( taskName.equals(fmString) ) {
			taskBuffer.append(fmTaskString);
		}
		else if ( taskName.equals(choppRadFilesString) ) {
			taskBuffer.append(choppRadFilesTaskString);
		}
		else if ( taskName.equals(externalDataFromRegressString) ) {
			taskBuffer.append(externalDataFromRegressTaskString);
		}
		else if ( taskName.equals(fmsdr2edrString) ) {
			taskBuffer.append(fmsdr2edrTaskString);
		}
		else if ( taskName.equals(mergeEdrString) ) {
			taskBuffer.append(mergeEdrTaskString);
		}
		else if ( taskName.equals(vippString) ) {
			taskBuffer.append(vippTaskString);
		}
		else if ( taskName.equals(gridString) ) {
			taskBuffer.append(gridTaskString);
		}
		else if ( taskName.equals(ncString) ) {
			taskBuffer.append(ncTaskString);
		}
		else if ( taskName.equals(figsGenString) ) {
			taskBuffer.append(figsGenTaskString);
		}
		else if ( taskName.equals(cleanString) ) {
			taskBuffer.append(cleanTaskString);
		}
	    }

        } catch (Throwable t)
        {
          t.printStackTrace();
        }
       
    }

    
    /**
     * view script file into another popup window from which you can edit script and save the changes.
     */
    private void viewScript() {
        
	StringBuffer stringBuffer = new StringBuffer(16384);
	ViewDialog scriptDialog = new ViewDialog(new JFrame(),
		"Script File ( You can edit after you click 'Edit' button in the bottom )", true);
	try{
	    BufferedReader br = new BufferedReader(new FileReader(scriptPath+scriptFile));
	    String line;
	    while (( line = br.readLine()) != null ) {
	    	stringBuffer.append(line + "\n");
	    }
	    br.close();	
	}
	catch ( FileNotFoundException ex )
	{
	    JOptionPane.showMessageDialog(new JFrame(),
		"Script File Not found:" + scriptPath+scriptFile,
		"Script file not exist",
		JOptionPane.ERROR_MESSAGE);
	}
	catch ( IOException ioe )
	{
	    outputArea.append("\nViewScript Error:" + ioe + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	}
	
	//scriptDialog.dumpContent(taskBuffer);
	//String oldScript = taskBuffer.toString();
	scriptDialog.dumpContent(stringBuffer);
	String oldScript = stringBuffer.toString();
	scriptDialog.setVisible(true);

	if ( scriptDialog.isSaveButtonPressed() ) {
		String newScript = scriptDialog.getContent();
		if ( oldScript.equals(newScript) ) 
			JOptionPane.showMessageDialog(new JFrame(),
			"NO Changes to script file, so no save action taken.",
			"Script file not changed",
			JOptionPane.ERROR_MESSAGE);
		else { 
			//outputArea.append( scriptDialog.getContent() );
			dumpContent(scriptDialog.getContent(), scriptPath+scriptFile);
		}
	}

    }
 
   
    /**
     * view config file in main control panel's output area without editing it
     */
    private void viewConfig2() {
    
	try{
	    BufferedReader br = new BufferedReader(new FileReader(configPath+configFile));
	    String line;
	    outputArea.append("\n\n");
	    while (( line = br.readLine()) != null ) {
	    	outputArea.append(line + "\n");
		
		if ( line.startsWith("LD_LIBRARY_PATH")      || line.startsWith("modelNonErrPath") 	|| 
		     line.startsWith("nwpGfsGridPath")       || line.startsWith("cldOptPropFile")  	|| 
		     line.startsWith("logFile") 	     || line.startsWith("regressRetrPath") 	|| 
		     line.startsWith("figsGenControlFile")   || line.startsWith("fwdAnalys4BiasList") 	|| 
		     line.startsWith("applyRegressAlgSrc")   || line.startsWith("step_clean") 		|| 
		     line.startsWith("makeClean")            || line.startsWith("extUseSTYP") )
		      
		outputArea.append("\n");
	    }
	
	    br.close();
	
	}
	catch ( FileNotFoundException ex )
	{
	    outputArea.append("\nConfig File Not found:" + configPath+configFile + "\n");
	}
	catch ( IOException ioe )
	{
	    outputArea.append("\nError:" + ioe + "\n");
	}
	
	vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
    }


    /**
     * view config file in a popup window with more functionalities: edit and save changes
     */
    private void viewConfig() {
    
	StringBuffer stringBuffer = new StringBuffer(16384);
	ViewDialog configDialog = new ViewDialog(new JFrame(),
		"Config File ( You can edit after click 'Edit' button in the bottom )", true);
	try{
	    BufferedReader br = new BufferedReader(new FileReader(configPath+configFile));
	    String line;
	    while (( line = br.readLine()) != null ) {
	    	stringBuffer.append(line + "\n");
	    }
	    br.close();	
	}
	catch ( FileNotFoundException ex )
	{
	    JOptionPane.showMessageDialog(new JFrame(),
		"Config File Not found:" + configPath+configFile,
		"Config file not exist",
		JOptionPane.ERROR_MESSAGE);
	}
	catch ( IOException ioe )
	{
	    outputArea.append("\nViewConfig Error:" + ioe + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	}
	
	configDialog.dumpContent(stringBuffer);
	String oldConfig = stringBuffer.toString();
	configDialog.setVisible(true);

	if ( configDialog.isSaveButtonPressed() ) {
		String newConfig = configDialog.getContent();
		if ( oldConfig.equals(newConfig) ) 
			JOptionPane.showMessageDialog(new JFrame(),
			"NO Changes to config file, so no save action taken.",
			"Config file not changed",
			JOptionPane.ERROR_MESSAGE);
		else { 
			//outputArea.append( configDialog.getContent() );
			dumpContent( configDialog.getContent(), configPath+configFile);
		}
	}
    	
    }


    /**
     * dump aString into aFile(aFile with path included)
     */
    private void dumpContent(String aString, String aFile) {
    
	try {	
	    PrintWriter out = new PrintWriter(new FileWriter(aFile));
    	    out.println(aString);
	    out.close();
	
	} catch ( IOException ioe )
	{		    
	    outputArea.append("" + ioe);
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());	    
	} 
    }
    
    
    /**
     * save config file, file name saved is configFile(including path in it).
     */
    private void saveConfig(String aConfigFile) {

	try {	
	    PrintWriter out = new PrintWriter(new FileWriter(aConfigFile));
	    
	    out.println("\n####################################################################################################");
	    out.println("#");
	    out.println("#");
	    out.println("#   S E C T I O N   O F   D A T A   A N D   P A T H S");
	    out.println("#");
	    out.println("#");
	    out.println("####################################################################################################\n");
	    
	    
	    out.println("#-------------------------------------------------------------------------------");
	    out.println("#        Satellite, sensor and default date used in GUI");
	    out.println("#-------------------------------------------------------------------------------");
	    
	    Iterator it = keys.iterator();
    	    while (it.hasNext()) {
	      String key = (String)it.next();
	      String value = paths.get(key);
	      String comment = comments.get(key);
	      
	      
	      // handle 3 "All" here, we can NOT put a ALL to config file, it must convert into a big integer
	      if ( ( key.equals(nOrbits2ProcessKey) || key.equals(nProfs2RetrKey) || key.equals(nProfs2FwdKey) ) && value.equals("All") )
	      	  value = "1000000"; 
	      
	      if ( value != null ) {
	        if ( comment == null || comment.length() < 1 )
	          out.println(key + "=" + value );  
	        else
	          out.println(key + "=" + value + "\t" + comment);
	      }    

	      if( key.equals("date") ) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Major root paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if( key.equals("LD_LIBRARY_PATH") ) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Research data & Paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("modelNonErrPath")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        External data & Paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("nwpGfsGridPath")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Static data & Paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("CRTMcoeffPath")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Semi-Static data & Paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("calibDTRlutFile")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Testbed data & Paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("logFile")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Dynamic data & Paths");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("regressRetrPath")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        Control Files");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("figsGenControlFile")) {
	    	out.println("\n#-------------------------------------------------------------------------------");
	    	out.println("#        File List");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("fwdAnalys4BiasList")) {
	    	out.println("\n\n####################################################################################################");
	    	out.println("#");
	    	out.println("#");
	    	out.println("#   S E C T I O N   O F   A P P L I C A T I O N S   A N D   P R O C E S S E S");
	    	out.println("#");
	    	out.println("#");
	    	out.println("####################################################################################################\n");
	      
	    	out.println("#-------------------------------------------------------------------------------");
	    	out.println("#        Source Directories");
	    	out.println("#-------------------------------------------------------------------------------");
	      }
	      else if (key.equals("applyRegressAlgSrc")) {
	    	out.println("\n\n####################################################################################################");
	    	out.println("#");
	    	out.println("#");
	    	out.println("#   S E C T I O N   O F   S W I T C H E S (W H I C H   A P P L I C A T I O N   T O   R U N)");
	    	out.println("#");
	    	out.println("#");
	    	out.println("####################################################################################################\n");
	      }
	      else if (key.equals("step_clean")) {
	    	out.println("\n\n####################################################################################################");
	    	out.println("#");
	    	out.println("#");
	    	out.println("#   S E C T I O N  OF  C O N T R O L L I N G  F L A G S");
	    	out.println("#");
	    	out.println("#");
	    	out.println("####################################################################################################\n");
	      }
 	 
	          
	    }
	    
	    out.close();
	
	} catch ( IOException ioe )
	{		    
	    outputArea.append("" + ioe);
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());	    
	} 

    }


    /**
     * save script file, file name saved is scriptFile.
     */
    private void saveScript() {
          
	scriptFile = sensor + "_scs.bash" ;
	try {   

            PrintWriter out = new PrintWriter(new FileWriter(scriptPath + scriptFile));
            out.println(taskBuffer.toString());  
            out.close();

            if( osName.indexOf("win") >= 0 ) {
                Process proc1 = new ProcessBuilder("cmd.exe", "/C chmod a+x " + scriptPath + scriptFile ).redirectErrorStream(true).start();
                proc1.waitFor();

                Process proc2 = new ProcessBuilder("cmd.exe", "/C dos2unix " + scriptPath + scriptFile ).redirectErrorStream(true).start();
                proc2.waitFor();
            }
            else {
                Runtime rt = Runtime.getRuntime();
                Process proc1 = rt.exec("chmod a+x " + scriptPath+scriptFile);
            }
	
	}
	catch ( IOException ioe )
	{ 
	    	outputArea.append("" + ioe);
		vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum()); 
	}
	catch (Throwable t)
	{
		t.printStackTrace();
	}
 
	outputArea.append("\nScript File saved: " + scriptPath+scriptFile + "\n");
	vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
    }


    /**
     * save script file, file name saved is scriptFile. User has more flexibility here
     * to save to any place, any file name.
     */
    private void saveScriptAs() {
        
	JFileChooser chooser = new JFileChooser(scriptPath);
	FileNameExtensionFilter filter = new FileNameExtensionFilter("Bash file", "bash");
	chooser.setFileFilter(filter);
	int returnVal = chooser.showSaveDialog(this);
	  
	if(returnVal == JFileChooser.APPROVE_OPTION) {
 	    scriptFile = chooser.getSelectedFile().getName();
	    String currentPath = chooser.getCurrentDirectory().getPath() + "/" ;
	    try {   
	    	PrintWriter out = new PrintWriter(new FileWriter(currentPath+scriptFile));
	    	out.println(taskBuffer.toString());
	    	out.close();

                if( osName.indexOf("win") >= 0 ) {
                    Process proc1 = new ProcessBuilder("cmd.exe", "/C chmod a+x " + scriptPath + scriptFile ).redirectErrorStream(true).start();
                    proc1.waitFor();

                    Process proc2 = new ProcessBuilder("cmd.exe", "/C dos2unix " + scriptPath + scriptFile ).redirectErrorStream(true).start();
                    proc2.waitFor();
                }
                else {
                    Runtime rt = Runtime.getRuntime();
                    Process proc1 = rt.exec("chmod a+x " + currentPath+scriptFile);
                }

	    }
	    catch ( IOException ioe )
	    { 
	    	outputArea.append("\n" + ioe); 
	    }
	    catch (Throwable t)
	    {
		t.printStackTrace();
	    }
 
	    outputArea.append("\nScript File saved as: " + currentPath+scriptFile + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());

	}
	else {
	    outputArea.append("\nSave ScriptFile command cancelled by user.\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	}

    }


    /**
     * to generate script
     */
    private void generateScript()
    {
	Vector<String> vector = new Vector<String>();
	
	for(int i=0; i<taskKeys.size(); i++) {
	
	      if ( tasksCheckBox[i].isSelected() ) {
	              //vector.add(tasksCheckBox[i].getText());
		      vector.add( string2Task.get(tasksCheckBox[i].getText()) );
	              //outputArea.append( tasksCheckBox[i].getText()+ "\n");
	      }
	}

	String sensorSelected = sensorChooser.getSelectedItem();
	if ( sensorSelected.equals(n18String) ) {
	      sensor="n18";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(moaString) ) {
	      sensor="metopA";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(mobString) ) {
	      sensor="metopB";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(f16String) ) {
	      sensor="f16";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(n19String) ) {
	      sensor="n19";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(f18String) ) {
	      sensor="f18";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(nppString) ) {
	      sensor="npp";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(aquaString) ) {
	      sensor="aqua";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(gcomw1String) ) {
	      sensor="gcomw1";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(fy3riString) ) {
	      sensor="fy3ri";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(trmmString) ) {
	      sensor="trmm";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(gpmString) ) {
	      sensor="gpm";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(mtmaString) ) {
	      sensor="mtma";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(mtsaString) ) {
	      sensor="mtsa";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(fy3htString) ) {
	      sensor="fy3ht";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(f17String) ) {
	      sensor="f17";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	else if ( sensorSelected.equals(windSatString) ) {
	      sensor="windsat";
	      paths.remove("satId");
	      paths.put("satId", sensor);
	}
	
	String processSelected = processModeChooser.getSelectedItem();
	if ( processSelected.equals(dailyModeString) )
	{     
	      saveConfig(configPath+configFile);
	      generateDailyScript(vector);
	      saveScript();
	}
	else if ( processSelected.equals(orbitModeString) )
	{ 
	      saveConfig(configPath+configFile);
	      generateOrbitScript(vector);
	      saveScript();
	}

    }


    /**
     * in order for java to deference a defined bash variable,
     * we generate a temp file and let temp file to echo
     * defined bash variable and then we get result. it's kinda workround here.
     */
    private String getBashVariable(String key) {

        String value = null;        
        try {

            String tmpFile = configPath+"bashVariableHelp.bash";
            File aFile = new File(tmpFile);
            PrintWriter out = new PrintWriter(new FileWriter(tmpFile));
            out.println(bashPath);
            out.println("source  " + configPath + configFile + "\n");
            out.println("echo ${" + key + "}\n");

            out.close();
            aFile.setExecutable(true, false);

            Process proc = null;
            if( osName.indexOf("win") >= 0 ) {
                // remove annoying carriage return \r for both pcf and temp file
                Process proc1 = new ProcessBuilder("cmd.exe", "/C dos2unix " + configPath + configFile ).redirectErrorStream(true).start();
                proc1.waitFor();

                Process proc2 = new ProcessBuilder("cmd.exe", "/C dos2unix " + tmpFile ).redirectErrorStream(true).start();
                proc2.waitFor();

                proc = new ProcessBuilder("cmd.exe", "/C bash " + tmpFile ).redirectErrorStream(true).start();
            }
            else {
                Runtime rt = Runtime.getRuntime();
                proc = rt.exec(tmpFile);
            }

            InputStream is = proc.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
        
            String line = br.readLine();
            
            if ( line != null && line.length() > 1 ) {
                value = line;
            }
            
            boolean deleted = aFile.delete();
            
        } catch ( IOException ioe )
        {                    
            outputArea.append("" + ioe);
            vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());            
        } catch (Throwable t)
        {
            t.printStackTrace();
        }

        return value;
        
    }


   /**
     * load all default config condition values if no config file provided.(here, N18 is default satellite).
     * namely, load vector paths with key=value.
     */
    public void loadDefaultConfig() {
	
	paths.clear();
	
	// Major Root Path and System Library Path
	loadPathsSystem();

	// Default Date for daily mode
	date="2006-02-01";
	
	// Satellite ID, Sensor ID 
	paths.put("satId", "n18");
	paths.put("sensor1", "amsua");
	paths.put("sensor2", "mhs");
	paths.put("date", date);
	
	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");
	
	// External Data Path
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// Static Data Path
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}.in");
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}_2.in");
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// Semi-Static Data Path
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat");

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// Testbed Data Path
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}_${sensor2}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}_${sensor2}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}_${sensor2}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// Dynamic Data Path
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}_${sensor2}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	
	// Control Data Path
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "Dummy");
	
	// Input Data Path
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");
	
	// Source Directories
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");
	
	// Steps
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
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// Controlling Parameters
	paths.put("processMode", "1");
	paths.put("sensorId", "1");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2006_02_01");
	paths.put("dayUsed4Alg", "2006_02_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "1");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";
	
	outputArea.append("\nNOAA-18 values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }
    
         
   /**
     * load all default config condition values if no config file provided.
     * namely, load vector paths with key=value
     */
    public void loadMetopAConfig() {
	
	paths.clear();
	
	// Major Root Path and System Library Path
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2006-11-01";

	// Satellite ID, Sensor ID and Default Date
	paths.put("satId", "metopA");
	paths.put("sensor1", "amsua");
	paths.put("sensor2", "mhs");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// External Data Path
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}" );
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// Static Data Path
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}.in");
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}_2.in");
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");

	// Semi-Static Data Path
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat");

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// Testbed Data Path
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}_${sensor2}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}_${sensor2}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}_${sensor2}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// Dynamic Data Path
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}_${sensor2}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	
	// Control Data Path
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "Dummy");
	
	// Input Data Path
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// Source Directories
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// Steps
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
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");

	// Controlling Parameters
	paths.put("processMode", "1");
	paths.put("sensorId", "2");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2006_11_01");
	paths.put("dayUsed4Alg", "2006_11_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "2");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nMetop-A config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


   /**
     * load all default config condition values if no config file provided.
     * namely, load vector paths with key=value
     */
    public void loadMetopBConfig() {
	
	paths.clear();
	
	// Major Root Path and System Library Path
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2012-10-11";

	// Satellite ID, Sensor ID and Default Date
	paths.put("satId", "metopB");
	paths.put("sensor1", "amsua");
	paths.put("sensor2", "mhs");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// External Data Path
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}" );
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// Static Data Path
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}.in");
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}_2.in");
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");

	// Semi-Static Data Path
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat");

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// Testbed Data Path
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}_${sensor2}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}_${sensor2}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}_${sensor2}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// Dynamic Data Path
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}_${sensor2}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	
	// Control Data Path
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "Dummy");
	
	// Input Data Path
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// Source Directories
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// Steps
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
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");

	// Controlling Parameters
	paths.put("processMode", "1");
	paths.put("sensorId", "14");
	paths.put("outFMAccuracy", "1");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2012_10_11");
	paths.put("dayUsed4Alg", "2012_10_11");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "2");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nMetop-B config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }
 
    
    /**
     * load F16 default config values into paths.
     */
    public void loadF16Config() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2006-02-01";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "f16");
	paths.put("sensor1", "ssmis");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_img");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_evn");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_las");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_uas");

	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "3");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2008_03_06");
	paths.put("dayUsed4Alg",  "2008_03_06");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nF16 config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }

    /**
     * load N19 default config values into paths.
     */
    public void loadN19Config() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2006-02-01";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "n19");
	paths.put("sensor1", "amsua");
	paths.put("sensor2", "mhs");
	paths.put("date", date);
	
	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// External Data Path
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}" );
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// Static Data Path
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}.in");
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}_2.in");
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");

	// Semi-Static Data Path
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// Testbed Data Path
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}_${sensor2}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}_${sensor2}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}_${sensor2}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// Dynamic Data Path
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}_${sensor2}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	
	// Control Data Path
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "Dummy");
	
	// Input Data Path
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// Source Directories
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// Steps
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
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");

	// Controlling Parameters
	paths.put("processMode", "1");
	paths.put("sensorId", "4");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2006_02_01");
	paths.put("dayUsed4Alg", "2006_02_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "2");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nN19 config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
    }


    /**
     * load F18 default config values into paths.
     */
    public void loadF18Config() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2006-02-01";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "f18");
	paths.put("sensor1", "ssmis");
	paths.put("sensor2", "dummy");
	paths.put("date", date);
	
	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_img");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_evn");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_las");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_uas");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "5");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2008_03_06");
	paths.put("dayUsed4Alg",  "2008_03_06");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nF18 config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


   /**
     * load all default config condition values if no config file provided.
     * namely, load vector paths with key=value
     */
    public void loadNppConfig() {
	
	paths.clear();
	
	// Major Root Path and System Library Path
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2006-11-01";

	// Satellite ID, Sensor ID and Default Date
	paths.put("satId", "npp");
	paths.put("sensor1", "atms");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// External Data Path
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// Static Data Path
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");

	// Semi-Static Data Path
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// Testbed Data Path
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// Dynamic Data Path
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// Control Data Path
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input Data Path
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_img");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_evn");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_las");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_uas");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// Source Directories
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("bufrSrc", "${rootPath}/src/bufr");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// Steps
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_bufr", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");

	// Controlling Parameters
	paths.put("processMode", "1");
	paths.put("sensorId", "6");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2006_11_01");
	paths.put("dayUsed4Alg", "2006_11_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "1");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "1");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nNpp config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }
 

    /**
     * load AQUA default config values into paths.
     */
    public void loadAquaConfig() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2008-09-28";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "aqua");
	paths.put("sensor1", "amsre");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");

	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "7");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2008_03_06");
	paths.put("dayUsed4Alg",  "2008_03_06");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nAqua config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


    /**
     * load GCOM-W1/AMSR2 default config values into paths.
     */
    public void loadGcomw1Config() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2012-11-01";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "gcomw1");
	paths.put("sensor1", "amsr2");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");

	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "15");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2012_11_01");
	paths.put("dayUsed4Alg",  "2012_11_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nGCOMW1 AMSR2 config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


    /**
     * load FY3RI default config values into paths.
     */
    public void loadFy3riConfig() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2008-09-28";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "fy3ri");
	paths.put("sensor1", "mwri");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");

	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "8");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2008_03_06");
	paths.put("dayUsed4Alg",  "2008_03_06");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nFY3 MWRI config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }

    /**
     * load TRMM TMI default config values into paths.
     */
    public void loadTrmmConfig() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2010-07-20";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "trmm");
	paths.put("sensor1", "tmi");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 

	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_lr");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_hr");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "9");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2010_07_20");
	paths.put("dayUsed4Alg",  "2010_07_20");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nTRMM TMI config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


    /**
     * load GPM GMI default config values into paths.
     */
    public void loadGpmConfig() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2010-08-25";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "gpm");
	paths.put("sensor1", "gmi");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_lr");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_hr");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "10");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2010_08_25");
	paths.put("dayUsed4Alg",  "2010_08_25");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nGPM GMI config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


    /**
     * load MADRAS default config values into paths.
     */
    public void loadMtmaConfig() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2010-07-20";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "mtma");
	paths.put("sensor1", "madras");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 

	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_lr");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_hr");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "12");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2010_07_20");
	paths.put("dayUsed4Alg",  "2010_07_20");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nMADRAS config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


    /**
     * load MADRAS default config values into paths.
     */
    public void loadMtsaConfig() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();

	// Default Date for daily mode
	date="2010-07-20";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "mtsa");
	paths.put("sensor1", "saphir");
	paths.put("sensor2", "dummy");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 

	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_lr");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_hr");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "13");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "1");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "0");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2010_07_20");
	paths.put("dayUsed4Alg",  "2010_07_20");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nSAPHIR config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


   /**
     * load FY3 MWHS/MWTS config
     */
    public void loadFy3htConfig() {
	
	paths.clear();
	
	// Major Root Path and System Library Path
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2006-11-01";

	// Satellite ID, Sensor ID and Default Date
	paths.put("satId", "fy3ht");
	paths.put("sensor1", "mwhs");
	paths.put("sensor2", "mwts");
	paths.put("date", date);

	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// External Data Path
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}" );
	paths.put("rdrSensor2Path", "${externalDataPath}/rdr/${satId}_${sensor1}_${sensor2}");
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// Static Data Path
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("instrumentSensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor2}.dat");
	paths.put("instrumentSensor1Sensor2File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("antennaSensor2File", "${antennaPath}/${satId}_${sensor2}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}.in");
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_${sensor2}_2.in");
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}_${sensor2}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");

	// Semi-Static Data Path
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// Testbed Data Path
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("nedtSensor2Path", "${nedtPath}/${satId}_${sensor2}");
	paths.put("nedtSensor1Sensor2Path", "${nedtPath}/${satId}_${sensor1}_${sensor2}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}_${sensor2}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}_${sensor2}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}_${sensor2}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}_${sensor2}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}_${sensor2}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}_${sensor2}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// Dynamic Data Path
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("tdrSensor2Path", "${tdrPath}/${satId}_${sensor2}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("sdrSensor2Path", "${sdrPath}/${satId}_${sensor2}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}_${sensor2}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}_${sensor2}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}_${sensor2}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}_${sensor2}");
	
	// Control Data Path
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("rdr2tdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("tdr2sdrSensor2ControlFile", "${controlDataPath}/${satId}_${sensor2}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_${sensor2}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "Dummy");
	
	// Input Data Path
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("rdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("tdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor2}_sdrFiles");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}_${sensor2}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor2List", "${inputDataPath}/${satId}_nedtDirs_${sensor2}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// Source Directories
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("rdr2tdrSensor2Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// Steps
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
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_dataMonitor", "0");
	paths.put("step_clean", "0");

	// Controlling Parameters
	paths.put("processMode", "1");
	paths.put("sensorId", "9");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2006_11_01");
	paths.put("dayUsed4Alg", "2006_11_01");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "1");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "4");
	paths.put("nScanLineSensor1Skip", "0");
	paths.put("nScanLineSensor2Skip", "2");
	paths.put("scanLineIndexSensor2TimeColloc", "2");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nFY3 MWHS/MWTS config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
    }
 

    /**
     * load F17 default config values into paths.
     */
    public void loadF17Config() {
	
	paths.clear();
	
	// Major rootPath and system environment paths
	loadPathsSystem();
	
	// Default Date for daily mode
	date="2008-10-16";
	
	// Sat id ,sensor, default date changes
	paths.put("satId", "f17");
	paths.put("sensor1", "ssmis");
	paths.put("sensor2", "dummy");
	paths.put("date", date);
	
	// Research Data Path
	paths.put("researchDataPath", "/net/orbit006L/home/sidb/ResearchData");
	paths.put("fwdPath", "${researchDataPath}/FwdSimulOutputs");
	paths.put("out1dvarPath", "${researchDataPath}/1dvarOutputs");
	paths.put("monitorFile", "${researchDataPath}/IterProcessMonitor/Monitoring.dat");
	paths.put("modelNonErrPath", "${researchDataPath}/ModelErrStats/amsua_mhs");
	paths.put("externalDataPath", "${dataPath}/ExternalData");

	// external data path change
	paths.put("rdrSensor1Path", "${externalDataPath}/rdr/${satId}_${sensor1}" );
	paths.put("rdrOrbitPath", "${externalDataPath}/rdr/OrbitalMode");
	paths.put("nwpGdasGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpEcmwfGridPath", "${externalDataPath}/gridNWP_analys");
	paths.put("nwpGfsGridPath", "${externalDataPath}/gridNWP_analys");
	
	// static data path change
	paths.put("staticDataPath", "${dataPath}/StaticData");
	paths.put("instrumentPath", "${staticDataPath}/InstrConfigInfo");
	paths.put("instrumentSensor1File", "${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat");
	paths.put("topographyFile", "${staticDataPath}/Topography/topography.bin_sgi");
	paths.put("antennaPath", "${staticDataPath}/AntennaPatterns");
	paths.put("antennaSensor1File", "${antennaPath}/${satId}_${sensor1}_antennaPattern.dat");
	paths.put("tune1File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in" );
	paths.put("tune2File", "${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in" );
	paths.put("nedtNominalFile", "${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat" );
	paths.put("modelErrNominalFile", "${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat" );
	paths.put("covBkgAtm1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgAtm2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat");
	paths.put("covBkgSfc1File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("covBkgSfc2File", "${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat" );
	paths.put("extBkgAtmFile", "${staticDataPath}/CovBkgStats/atmBkg_ECMWF.dat");
	paths.put("siceEmissCatalogFile", "${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("snowEmissCatalogFile", "${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat");
	paths.put("CRTMcoeffPath", "${staticDataPath}/CRTMFiles/");
	
	// semi-static data path change
	paths.put("semiStaticDataPath", "${dataPath}/SemiStaticData");
	paths.put("biasPath", "${semiStaticDataPath}/biasCorrec");
	paths.put("regressPath", "${semiStaticDataPath}/regressAlgors");
	
	paths.put("regressCoeffOceanClwFile", "${regressPath}/Oc_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSeaIceClwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffLandClwFile", "${regressPath}/Land_regressCoeffs_${satId}_clw.dat");
	paths.put("regressCoeffSnowClwFile", "${regressPath}/Snow_regressCoeffs_${satId}_clw.dat"); 
	
	paths.put("regressCoeffOceanTskinFile", "${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSeaIceTskinFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffLandTskinFile", "${regressPath}/Land_regressCoeffs_${satId}_tskin.dat");
	paths.put("regressCoeffSnowTskinFile", "${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat"); 
	
	paths.put("regressCoeffOceanTpwFile", "${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSeaIceTpwFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffLandTpwFile", "${regressPath}/Land_regressCoeffs_${satId}_tpw.dat");
	paths.put("regressCoeffSnowTpwFile", "${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat"); 
	
	paths.put("regressCoeffOceanEmFile", "${regressPath}/Oc_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSeaIceEmFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffLandEmFile", "${regressPath}/Land_regressCoeffs_${satId}_em.dat");
	paths.put("regressCoeffSnowEmFile", "${regressPath}/Snow_regressCoeffs_${satId}_em.dat"); 
	
	paths.put("regressCoeffOceanWvFile", "${regressPath}/Oc_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSeaIceWvFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffLandWvFile", "${regressPath}/Land_regressCoeffs_${satId}_wv.dat");
	paths.put("regressCoeffSnowWvFile", "${regressPath}/Snow_regressCoeffs_${satId}_wv.dat"); 
	
	paths.put("regressCoeffOceanTempFile", "${regressPath}/Oc_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSeaIceTempFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffLandTempFile", "${regressPath}/Land_regressCoeffs_${satId}_temp.dat");
	paths.put("regressCoeffSnowTempFile", "${regressPath}/Snow_regressCoeffs_${satId}_temp.dat"); 

	paths.put("regressCoeffOceanGwpFile", "${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSeaIceGwpFile", "${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffLandGwpFile", "${regressPath}/Land_regressCoeffs_${satId}_gwp.dat");
	paths.put("regressCoeffSnowGwpFile", "${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat"); 
	
	paths.put("regressCoeffDesertFile",  "${regressPath}/Desert_regressCoeffs_${satId}.dat");
	paths.put("biasFileToUse", "${biasPath}/biasCorrec_${satId}.dat"); 
	paths.put("calibBiasFitFile",  "${biasPath}/calibBiasFit_${satId}.dat");
	paths.put("calibDTRlutFile",  "${biasPath}/calibDTRlut_${satId}.dat");
	
	// testbed data path change
	paths.put("testbedDataPath", "${dataPath}/TestbedData");
	paths.put("nedtPath", "${testbedDataPath}/nedt");
	paths.put("nedtSensor1Path", "${nedtPath}/${satId}_${sensor1}");
	paths.put("edrPath", "${testbedDataPath}/Outputs/edr/${satId}_${sensor1}");
	paths.put("depPath", "${testbedDataPath}/Outputs/dep/${satId}_${sensor1}");
	paths.put("gridPath", "${testbedDataPath}/Outputs/grid/${satId}_${sensor1}");
	paths.put("ncPath", "${testbedDataPath}/Outputs/nc/${satId}_${sensor1}");
	paths.put("figsPath", "${testbedDataPath}/Outputs/figs/${satId}_${sensor1}");
	paths.put("perfsMonitorPath", "${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}");
	paths.put("logFile", "${logPath}/${satId}_logFile");
	
	// dynamic data path change
	paths.put("dynamicDataPath", "${testbedDataPath}/DynamicData");
	paths.put("tdrPath", "${dynamicDataPath}/tdr");
	paths.put("tdrSensor1Path", "${tdrPath}/${satId}_${sensor1}");
	paths.put("sdrPath", "${dynamicDataPath}/sdr");
	paths.put("sdrSensor1Path", "${sdrPath}/${satId}_${sensor1}");
	paths.put("fmsdrPath", "${dynamicDataPath}/fmsdr/${satId}_${sensor1}");
	paths.put("choppPath", "${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}");
	paths.put("nwpAnalysPath", "${dynamicDataPath}/nwp_analys/${satId}_${sensor1}");
	paths.put("fwdAnalysPath", "${dynamicDataPath}/fwd_analys/${satId}_${sensor1}");
	paths.put("regressRetrPath", "${dynamicDataPath}/regress_retr/${satId}_${sensor1}");
	
	// control file path change
	paths.put("controlDataPath", "${dataPath}/ControlData");
	paths.put("rdr2tdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_rdr2tdr");
	paths.put("mergeNedtControlFile", "${controlDataPath}/${satId}_mergeNEDT");
	paths.put("tdr2sdrSensor1ControlFile", "${controlDataPath}/${satId}_${sensor1}_tdr2sdr");
	paths.put("fmControlFile", "${controlDataPath}/${satId}_${sensor1}_fm");
	paths.put("fmsdr2edrControlFile", "${controlDataPath}/${satId}_CntrlConfig_1dvar");
	paths.put("grid2nwpControlFile", "${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD");
	paths.put("fwdControlFile", "${controlDataPath}/${satId}_cntrl_fwd");
	paths.put("regressControlFile", "${controlDataPath}/${satId}_ApplyRegress");
	paths.put("choppControlFile", "${controlDataPath}/${satId}_Chopp");
	paths.put("mergeEdrControlFile", "${controlDataPath}/${satId}_MergeEDR");
	paths.put("vippControlFile", "${controlDataPath}/${satId}_Vipp");
	paths.put("gridControlFile", "${controlDataPath}/${satId}_Grid");
	paths.put("nwpGridControlFile", "${controlDataPath}/${satId}_NWPGrid");
	paths.put("fwdGridControlFile", "${controlDataPath}/${satId}_FWDGrid");
	paths.put("biasGridControlFile", "${controlDataPath}/${satId}_BiasGrid");
	paths.put("biasCompuControlFile", "${controlDataPath}/${satId}_Inputs4BiasComputation");
	paths.put("biasVerifControlFile", "${controlDataPath}/${satId}_Inputs4BiasVerification");
	paths.put("regressGenControlFile", "${controlDataPath}/${satId}_Inputs4RegressGen");
	paths.put("figsGenControlFile", "${controlDataPath}/${satId}_Inputs4FigsGener");
	paths.put("modifyNedtControlFile", "${controlDataPath}/${satId}_modifyNedt");
	
	// Input file list
	paths.put("inputDataPath", "${dataPath}/InputsData");
	paths.put("rdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_rdrFiles");
	paths.put("tdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_tdrFiles");
	paths.put("sdrSensor1List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_img");
	paths.put("sdrSensor2List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_evn");
	paths.put("sdrSensor3List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_las");
	paths.put("sdrSensor4List", "${inputDataPath}/${satId}_${sensor1}_sdrFiles_uas");
	paths.put("fmsdrList", "${inputDataPath}/${satId}_fmsdrFiles");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4ChoppList", "${inputDataPath}/${satId}_fmsdrFiles_4Chopping");
	paths.put("fmsdr4NwpList", "${inputDataPath}/${satId}_fmsdrFiles_4nwp");
	paths.put("fmsdr4BiasList", "${inputDataPath}/${satId}_fmsdrFiles_4Bias");
	paths.put("fmsdr4RegressList", "${inputDataPath}/${satId}_fmsdrFiles_4regress");
	paths.put("fmsdr4ApplyRegressList", "${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress");
	paths.put("edrList", "${inputDataPath}/${satId}_edrFiles");
	paths.put("edr4BiasList", "${inputDataPath}/${satId}_edrFiles_4Bias");
	paths.put("dep4BiasList", "${inputDataPath}/${satId}_depFiles_4Bias");
	paths.put("edr4MergeList", "${inputDataPath}/${satId}_FullOrbitEDR_4Merging");
	paths.put("depList", "${inputDataPath}/${satId}_depFiles");
	paths.put("nedtList", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("nedtSensor1List", "${inputDataPath}/${satId}_nedtDirs_${sensor1}");
	paths.put("gridSfcNwpAnalysList", "${inputDataPath}/${satId}_sfcNWPanalys");
	paths.put("gridAtmNwpAnalysList", "${inputDataPath}/${satId}_atmNWPanalys");
	paths.put("nwpAnalysList", "${inputDataPath}/${satId}_NWPanalysFiles");
	paths.put("nwpAnalysRetrList", "${inputDataPath}/${satId}_NWPanalysFiles_4retr");
	paths.put("nwpAnalys4BiasList", "${inputDataPath}/${satId}_NWPanalysFiles_4Bias");
	paths.put("nwpAnalys4RegressList", "${inputDataPath}/${satId}_NWPanalysFiles_4Regress");
	paths.put("fwdAnalys4BiasList", "${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias");

	// source directories change
	paths.put("rdr2tdrSensor1Src", "${rootPath}/src/testbed/rdr2tdr");
	paths.put("mergeNedtSrc", "${rootPath}/src/testbed/mergeNEDTofDiffInstr");
	paths.put("tdr2sdrSrc", "${rootPath}/src/testbed/tdr2sdr");
	paths.put("fmSrc", "${rootPath}/src/testbed/fm");
	paths.put("choppSrc", "${rootPath}/src/testbed/chopp");
	paths.put("fmsdr2edrSrc", "${rootPath}/src/1dvar");
	paths.put("mergeEdrSrc", "${rootPath}/src/testbed/mergeEDR");
	paths.put("vippSrc", "${rootPath}/src/testbed/vipp");
	paths.put("gridSrc", "${rootPath}/src/testbed/grid");
	paths.put("ncSrc", "${rootPath}/src/testbed/mirs2nc");
	paths.put("nedtMonitorSrc", "${rootPath}/src/testbed/nedtMonitoring");
	paths.put("nwpGenAnalysSrc", "${rootPath}/src/testbed/nwp");
	paths.put("fwdSrc", "${rootPath}/src/fwd");
	paths.put("fwd2hdf5Src", "${rootPath}/src/testbed/fwd2hdf5");
	paths.put("determineBiasSrc", "${rootPath}/src/testbed/biasGenerAndMonit");
	paths.put("regressAlgSrc", "${rootPath}/src/testbed/regressAlgors");
	paths.put("applyRegressAlgSrc", "${rootPath}/src/testbed/retrRegress");

	// step change
	paths.put("step_rdr2tdrSensor1", "0");
	paths.put("step_mergeNedt", "0");
	paths.put("step_tdr2sdrSensor1", "0");
	paths.put("step_fm", "0");
	paths.put("step_nwp", "0");
	paths.put("step_fwd", "0");
	paths.put("step_biasGen", "0");
	paths.put("step_choppRadFiles", "0");
	paths.put("step_externalDataFromRegress", "0");
	paths.put("step_fmsdr2edr", "0");
	paths.put("step_mergeEdr", "0");
	paths.put("step_vipp", "0");
	paths.put("step_grid", "0");
	paths.put("step_nc", "0");
	paths.put("step_figsGen", "0");
	paths.put("step_biasFigsGen", "0");
	paths.put("step_clean", "0");
	
	// section of controling flags change
	paths.put("processMode", "1");
	paths.put("sensorId", "18");
	paths.put("outFMAccuracy", "0");
	paths.put("prefixFMAccuracy", "QCcheck");
	paths.put("nProfs2Retr", "All");
	paths.put("nProfs2Fwd", "All");
	paths.put("nAttempts", "2");
	paths.put("fmType", "0");
	paths.put("addDeviceNoise", "0");
	paths.put("monitorIterative", "0");
	paths.put("monitorRetrieval", "0");
	paths.put("monitorFwd", "0");
	paths.put("externalDataAvailable", "1");
	paths.put("externalDataSrc", "2");
	paths.put("nwpGdasUse", "1");
	paths.put("nwpEcmwfUse", "0");
	paths.put("nwpGfsUse", "0");
	paths.put("extBkgAtmUse", "0");
	paths.put("geoLimit", "0");
	paths.put("minLat", "-90.");
	paths.put("maxLat", "90.");
	paths.put("minLon", "-180.");
	paths.put("maxLon", "180.");
	paths.put("cend", "2");
	paths.put("nDaysBack", "2");
	paths.put("maxDaysArchived", "0");
	paths.put("dayUsed4Bias", "2008_03_06");
	paths.put("dayUsed4Alg",  "2008_03_06");
	paths.put("nOrbits2Process", "All");
	paths.put("tdrFormat", "0");
	paths.put("rdrType", "0");
	paths.put("gifDensity", "100");
	paths.put("gridFactor", "2");
	paths.put("nScanLineSensor1Skip", "-99");
	paths.put("nScanLineSensor2Skip", "-99");
	paths.put("scanLineIndexSensor2TimeColloc", "-99");
	paths.put("fwdCloudOffOrOn", "0");
	paths.put("biasComputeMethod", "1");
	paths.put("regressionBiasApplyDomain", "-2");
	paths.put("nChoppedFilesPerOrbit", "10");
	paths.put("retrOnOrbitOrSubOrbit", "0");
	paths.put("retrOnWhichSDR", "1");
	paths.put("fwdMatrix2Use", "0");
	paths.put("makeOrNot", "0");
	paths.put("useCPU", "1");
	paths.put("makeClean", "0");
	paths.put("email", "Wanchun.Chen@noaa.gov");
	paths.put("website", "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php");
	
	nwpGdasUse  = "1";
	nwpEcmwfUse = "0";
	nwpGfsUse   = "0";

	outputArea.append("\nF17 config values are loaded.\n");
	
	// only enabled after successful loading of all default config values
	loadTasks();
	loadMainGUI();
	
	// automatically generate a config file if no one exist
	saveConfig(configPath+configFile);
	
	outputArea.append("\nA default config file is generated: " + configPath+configFile + "\n");

	// become enabled 	
	pathMenuItem.setEnabled(true);
	preferenceMenuItem.setEnabled(true);
	
    }


   /**
     * load all config conditions from parameter aConfigFile
     */
    public void loadConfig(String aConfigFile) { 
	
	configFile=aConfigFile;
		
	try{
	    BufferedReader br = new BufferedReader(new FileReader(configPath+configFile));
		
	    //keys.clear();
	    taskKeys.clear();
	    tasks.clear();
	    paths.clear();
	
	    String line;
	    while (( line = br.readLine()) != null ) {
	    	//outputArea.append(line + "\n");
		if ( ! line.startsWith("#") ) {
		    String keyvalue=null;
		    String comment=null;
		    int poundIndex=line.indexOf('#');
		    if ( poundIndex > 0 ) {
		    	String usefulPart=line.substring(0,poundIndex);
			comment = line.substring(poundIndex, line.length());
			keyvalue = usefulPart.trim();
		    } 
		    else {
		    	
		    	keyvalue = line.trim(); 
		    }
		    
		    int index = keyvalue.indexOf('=');
		    
		    if ( index > 0 ) {
		    
			String key = keyvalue.substring(0,index);
		    	String value= keyvalue.substring(index+1,keyvalue.length());
		    
		    	if ( key != null && value != null && key.length() > 0 && value.length() > 0 ) {
				//keys.add(key);
		    		paths.put(key, value);
				//comments.put(key,comment);
				if ( key.startsWith("step_") ) {
				    taskKeys.add(key); 
				    tasks.put(key, value); 
				}
				
				if ( key.equals("date")        ) date = value;
				if ( key.equals("nwpGdasUse")  ) nwpGdasUse = value;
				if ( key.equals("nwpEcmwfUse") ) nwpEcmwfUse = value;
				if ( key.equals("nwpGfsUse")   ) nwpGfsUse = value;
			}
		    } 
		}
	    }
	
	    br.close();
	    outputArea.append("\nLoad Config file OK: " + configPath+configFile + "\n");
	
	    // only enabled after successful loading of config file
	    loadTasks();
	    loadMainGUI();

	    pathMenuItem.setEnabled(true);
	    preferenceMenuItem.setEnabled(true);
		    
	}
	catch ( FileNotFoundException ex )
	{
	    outputArea.append("\nFile Not found:" + configFile + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	}
	catch ( IOException ioe )
	{
	    outputArea.append("\nError:" + ioe + "\n");
	    vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());
	}
		
    }
    
    
   /**
     * populate all tasks and load into taskPanel ( N18 and MetopA )
     */
    private void loadTasks()
    {	
 	stepPanel.removeAll();
	Graphics g=stepPanel.getGraphics();
	
	int rowNum=tasksKeysN18.length;
	
	if ( sensor.equals("n18") || sensor.equals("n19") || sensor.equals("fy3ht") ) 
	        rowNum=tasksKeysN18.length;
	else if ( sensor.equals("f16") || sensor.equals("f17") || sensor.equals("f18") || sensor.equals("trmm") ) 
		rowNum=tasksKeysF16.length;
	else if ( sensor.equals("npp") )
		rowNum=tasksKeysNpp.length; 
	else if ( sensor.equals("aqua") || sensor.equals("fy3ri") )
		rowNum=tasksKeysAqua.length; 
	else if ( sensor.equals("windsat") )
		rowNum=tasksKeysWindSat.length; 
		
	stepPanel.setLayout(new GridLayout(rowNum,1));
	
	Iterator it = taskKeys.iterator();
	int i=0;
	while (it.hasNext()) {
		String taskKey = (String)it.next();
		String taskValue = tasks.get(taskKey);
		
		String keyLabel = taskKey.substring(5,taskKey.length());	
		if (taskValue != null && taskValue.equals("0")) {
			tasksCheckBox[i] = new JCheckBox(task2String.get(keyLabel),false);
			tasksCheckBox[i].setFont(new Font("Dialog",Font.PLAIN,10));
			tasksCheckBox[i].addItemListener(this);
			stepPanel.add(tasksCheckBox[i]);
		}
		else if (taskValue != null && taskValue.equals("1")) {
			tasksCheckBox[i] = new JCheckBox(task2String.get(keyLabel),true);
			tasksCheckBox[i].setFont(new Font("Dialog",Font.PLAIN,10));
			tasksCheckBox[i].addItemListener(this);
			stepPanel.add(tasksCheckBox[i]);
		}
		i++;		    
	}
	
	stepPanel.paintAll(g);
	stepPanel.setVisible(true);	
    }


    /**
     * helper function to test whethe choice already contains str or not
     */
    private boolean alreadyContains(Choice choice, String str) {
	
	if ( choice == null || str == null ) return false;
	
	int count = choice.getItemCount();
	for(int i = 0; i < count; i++ )
	    if ( choice.getItem(i).equals(str) ) 
	    	return true;
	
	return false;
    }


    /**
     * load 1dvar Options
     */
    private void load1dvarOption() {
	
	for ( int j=0; j<taskKeys.size(); j++ ) {
	    //String step = tasksCheckBox[j].getText();
	    String step = string2Task.get(tasksCheckBox[j].getText());
	    
	    if ( step.equals(fmsdr2edrString) && tasksCheckBox[j].isSelected() ) 
	    {   
		profileLabel.setEnabled(true);
		profileNumberChooser.setEnabled(true);
	    }
	    else if ( step.equals(fmsdr2edrString) && !tasksCheckBox[j].isSelected() ) 
	    {   
		profileLabel.setEnabled(false);
		profileNumberChooser.setEnabled(false);
	    }
	}
	
	Graphics g = profilePanel.getGraphics(); 
	profilePanel.paintAll(g);
	profilePanel.setVisible(true);		
		
    }
 
 
    /**
     * load FWD Options
     */
    private void loadFwdOption() {
	
	for ( int j=0; j<taskKeys.size(); j++ ) {
	    //String step = tasksCheckBox[j].getText();
	    String step = string2Task.get(tasksCheckBox[j].getText());
	    if ( step.equals(fwdString) && tasksCheckBox[j].isSelected() ) 
	    {   
		fwdProfileLabel.setEnabled(true);
		fwdProfileNumberChooser.setEnabled(true);
	    }
	    else if ( step.equals(fwdString) && !tasksCheckBox[j].isSelected() ) 
	    {   
		fwdProfileLabel.setEnabled(false);
		fwdProfileNumberChooser.setEnabled(false);
	    }
	}	

	Graphics g = profilePanel.getGraphics(); 
	profilePanel.paintAll(g);
	profilePanel.setVisible(true);		
		
    }
 
 
    /**
     * load sensorId, processMode, nProfs2Retr, nDaysBack, nOrbits2Process, nProfs2Fwd
     */
    private void loadMainGUI()
    {
	String processMode = paths.get(processModeKey);
	if ( processMode.equals("0") ) {
		loadOrbit();
	}
	else if ( processMode.equals("1") ) {
		loadDaily();
	}
	
	String sensorId = paths.get(sensorIdKey);
	if 	( sensorId.equals("1") )  { sensorChooser.select(n18String);	sensor="n18";	   }
	else if ( sensorId.equals("2") )  { sensorChooser.select(moaString);	sensor="metopA";   }
	else if ( sensorId.equals("3") )  { sensorChooser.select(f16String);	sensor="f16";	   }
	else if ( sensorId.equals("4") )  { sensorChooser.select(n19String);	sensor="n19";	   }
	else if ( sensorId.equals("5") )  { sensorChooser.select(f18String);	sensor="f18";	   }
	else if ( sensorId.equals("6") )  { sensorChooser.select(nppString);	sensor="npp";	   }
	else if ( sensorId.equals("7") )  { sensorChooser.select(aquaString);   sensor="aqua";     }
	else if ( sensorId.equals("8") )  { sensorChooser.select(fy3riString);  sensor="fy3ri";    }
	else if ( sensorId.equals("9") )  { sensorChooser.select(trmmString);   sensor="trmm";     }
	else if ( sensorId.equals("10") ) { sensorChooser.select(gpmString);    sensor="gpm";      }
	else if ( sensorId.equals("12") ) { sensorChooser.select(mtmaString);   sensor="mtma";     }
	else if ( sensorId.equals("13") ) { sensorChooser.select(mtsaString);   sensor="mtsa";     }
	else if ( sensorId.equals("14") ) { sensorChooser.select(mobString);	sensor="metopB";   }
	else if ( sensorId.equals("15") ) { sensorChooser.select(gcomw1String); sensor="gcomw1";   }
	else if ( sensorId.equals("18") ) { sensorChooser.select(f17String);    sensor="f17";      }

       /** 
	* else if ( sensorId.equals("16") ) { sensorChooser.select(windSatString); sensor="windsat"; }
	* else if ( sensorId.equals("17") ) { sensorChooser.select(fy3htString);   sensor="fy3ht";   }
        */
	
	String nProfs2Retr = paths.get(nProfs2RetrKey);
	String nProfs2Fwd  = paths.get(nProfs2FwdKey);
	String nOrbits2Process = paths.get(nOrbits2ProcessKey);
	
	
	if ( ! alreadyContains(orbitNumberChooser, nOrbits2Process) ) 
		orbitNumberChooser.add(nOrbits2Process);
	
	orbitNumberChooser.select(nOrbits2Process);
	
	load1dvarOption();
	loadFwdOption();
		
	if ( ! alreadyContains(profileNumberChooser,nProfs2Retr) )
		profileNumberChooser.add(nProfs2Retr);
		
	if ( ! alreadyContains(fwdProfileNumberChooser, nProfs2Fwd) )	
		fwdProfileNumberChooser.add(nProfs2Fwd);
	
	profileNumberChooser.select(nProfs2Retr);
	fwdProfileNumberChooser.select(nProfs2Fwd);

	String nDaysBack = paths.get(nDaysBackKey);

    }

    /**
     * load orbit related stuff and its dependents
     */
    private void loadOrbit() {

	processModeChooser.select(orbitModeString);
	
	Graphics g = modePanel.getGraphics(); 
	modePanel.remove(dateChooser);
	modePanel.remove(orbitLabel);
	modePanel.remove(orbitNumberChooser);

	SpringLayout.Constraints browseButtonCons = modeLayout.getConstraints(browseButton);
	browseButtonCons.setX(Spring.constant(145));
	browseButtonCons.setY(Spring.constant(5));
	browseButtonCons.setWidth(Spring.constant(125));
	browseButtonCons.setHeight(Spring.constant(25));

	int countComponent = modePanel.getComponentCount();
	if ( countComponent < 2 ) 
		modePanel.add(browseButton);
	
	//String nOrbits2Process = paths.get(nOrbits2ProcessKey); 
	//orbitNumberChooser.select(nOrbits2Process );
	
	for ( int j=0; j<taskKeys.size(); j++ ){
	    //String step = tasksCheckBox[j].getText();
	    String step = string2Task.get(tasksCheckBox[j].getText());
	    if (step.equals(nwpString)       	|| step.equals(fwdString) || step.equals(bufrString) ||
	    	step.equals(biasGenString)   	|| 
		step.equals(biasFigsGenString)  || step.equals(dataMonitorString))  
			tasksCheckBox[j].setEnabled(false);
	}	

	modePanel.paintAll(g);
	modePanel.setVisible(true);		
    }


    /**
     * load daily stuff and its dependents
     */
    private void loadDaily() {

	processModeChooser.select(dailyModeString);
	
	Graphics g = modePanel.getGraphics(); 
	modePanel.remove(browseButton);

 	SpringLayout.Constraints dateChooserCons = modeLayout.getConstraints(dateChooser);
	dateChooserCons.setX(Spring.constant(145));
	dateChooserCons.setY(Spring.constant(5));
	dateChooserCons.setWidth(Spring.constant(125));
	dateChooserCons.setHeight(Spring.constant(25));
	
	SpringLayout.Constraints orbitLabelCons = modeLayout.getConstraints(orbitLabel);
	orbitLabelCons.setX(Spring.constant(5));
	orbitLabelCons.setY(Spring.constant(35));
	orbitLabelCons.setWidth(Spring.constant(125));
	orbitLabelCons.setHeight(Spring.constant(25));

	SpringLayout.Constraints orbitNumberCons = modeLayout.getConstraints(orbitNumberChooser);
	orbitNumberCons.setX(Spring.constant(145));
	orbitNumberCons.setY(Spring.constant(35));
	orbitNumberCons.setWidth(Spring.constant(125));
	orbitNumberCons.setHeight(Spring.constant(25));

	int countComponent = modePanel.getComponentCount();
	if ( countComponent < 2 ) { 
		modePanel.add(dateChooser);
		modePanel.add(orbitLabel);
		modePanel.add(orbitNumberChooser);
	}
	
	for ( int j=0; j<taskKeys.size(); j++ ){
	    //String step = tasksCheckBox[j].getText();
	    String step = string2Task.get(tasksCheckBox[j].getText());
	    if( step.equals(nwpString) 	       || step.equals(fwdString) 
	     || step.equals(bufrString)        || step.equals(biasGenString)
	     || step.equals(biasFigsGenString) || step.equals(dataMonitorString)) 	
		tasksCheckBox[j].setEnabled(true);
	}	

	modePanel.paintAll(g);
	modePanel.setVisible(true);
    }


    /**
     * to load System stuffs into paths
     */
    private void loadPathsSystem() {
    
	// Major rootPath and system environment paths
	String rootPath = null;
	try {

          Process proc = null;
          if( osName.indexOf("win") >= 0 ) {
              proc = new ProcessBuilder("cmd.exe", "/C pwd" ).redirectErrorStream(true).start();
          }
          else {
              Runtime rt = Runtime.getRuntime();
              proc = rt.exec("pwd");
          }
	  
	  InputStream is = proc.getInputStream();
	  InputStreamReader isr = new InputStreamReader(is);
	  BufferedReader br = new BufferedReader(isr);
	
	  String line = br.readLine();
	  if ( line != null && line.endsWith("gui") ) {
	      rootPath = line.replaceAll("/gui", "");
	      // assume cygwin is installed into C:/cygwin
	      if( osName.indexOf("win") >= 0 ) rootPath = "C:/cygwin" + rootPath ;
	      outputArea.append("rootPath=" + rootPath + "\n" );
	  }
	  
	} catch ( IOException ioe )
	{		  
	  outputArea.append("" + ioe);
	  vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());  	  
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
            Process proc = null;
            if( osName.indexOf("win") >= 0 ) {
                proc = new ProcessBuilder("cmd.exe", "/C which idl" ).redirectErrorStream(true).start();
            }
            else {
                Runtime rt = Runtime.getRuntime();
                proc = rt.exec("which idl");
            }

            InputStream is = proc.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);

            String line = br.readLine();
            if ( line != null && line.endsWith("idl") ) {
                idlPath = line;
                outputArea.append("idlPath=" + idlPath + "\n");
            }

        } catch ( IOException ioe )
        {                  
          outputArea.append("" + ioe);
          vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());            
        }
	
	if ( idlPath != null ) 
	    paths.put("IDL", idlPath);
	else
	    paths.put("IDL", "/usr/local/bin/idl");

/**	    
        // search grads path in system environment
        String gradsPath = null;
        try {
            Process proc = null;
            if( osName.indexOf("win") >= 0 ) {
                proc = new ProcessBuilder("cmd.exe", "/C which grads" ).redirectErrorStream(true).start();
            }
            else {
                Runtime rt = Runtime.getRuntime();
                proc = rt.exec("which grads");
            }
            InputStream is = proc.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);

            String line = br.readLine();
            if ( line != null && line.endsWith("grads") ) {
                gradsPath = line;
                outputArea.append("gradsPath=" + gradsPath + "\n");
            }
        } catch ( IOException ioe )
        {
          outputArea.append("" + ioe);
          vbar.setValues(vbar.getMaximum(), 32, 0, vbar.getMaximum());            
        }

        if ( gradsPath != null ) 
            paths.put("GRADS", "\""+gradsPath+" -lbxc\"");
        else
            paths.put("GRADS", "\"/misc/grads/bin/grads -lbxc\"");
*/

        // fortran lib really non-standard and should be specified in config.in
	if ( libFortranPath != null)
	    paths.put("LD_LIBRARY_PATH", "$LD_LIBRARY_PATH:"+libFortranPath);
	else if ( osName.equals("linux") )
	    paths.put("LD_LIBRARY_PATH", "$LD_LIBRARY_PATH:/usr/lib");
	else if ( osName.equals("aix")   )
	    paths.put("LD_LIBRARY_PATH", "$LD_LIBRARY_PATH:/usr/local/lib");
	else
	    paths.put("LD_LIBRARY_PATH", "$LD_LIBRARY_PATH:/usr/local/lib");
    
    }
    
    
    /**
     * to load N18 tasks
     */
    private void tasks2N18() {
    
	taskKeys.clear();
	taskKeys.add("step_rdr2tdrSensor1");
	taskKeys.add("step_rdr2tdrSensor2");
	taskKeys.add("step_mergeNedt");
	taskKeys.add("step_tdr2sdrSensor1");
	taskKeys.add("step_tdr2sdrSensor2");
	taskKeys.add("step_fm");
	taskKeys.add("step_nwp");
	taskKeys.add("step_fwd");
	taskKeys.add("step_biasGen");
	taskKeys.add("step_choppRadFiles");
	taskKeys.add("step_externalDataFromRegress");
	taskKeys.add("step_fmsdr2edr");
	taskKeys.add("step_mergeEdr");
	taskKeys.add("step_vipp");
	taskKeys.add("step_grid");
	taskKeys.add("step_nc");
	taskKeys.add("step_figsGen");
	taskKeys.add("step_biasFigsGen");
	taskKeys.add("step_dataMonitor");
	taskKeys.add("step_clean");

	tasks.clear();
  	tasks.put("step_rdr2tdrSensor1", "0");
	tasks.put("step_rdr2tdrSensor2", "0");
	tasks.put("step_mergeNedt", "0");
	tasks.put("step_tdr2sdrSensor1", "0");
	tasks.put("step_tdr2sdrSensor2", "0");
	tasks.put("step_fm", "0");
	tasks.put("step_nwp", "0");
	tasks.put("step_fwd", "0");
	tasks.put("step_biasGen", "0");
	tasks.put("step_choppRadFiles", "0");
	tasks.put("step_externalDataFromRegress", "0");
	tasks.put("step_fmsdr2edr", "0");
	tasks.put("step_mergeEdr", "0");
	tasks.put("step_vipp", "0");
	tasks.put("step_grid", "0");
	tasks.put("step_nc", "0");
	tasks.put("step_figsGen", "0");
	tasks.put("step_biasFigsGen", "0");
	tasks.put("step_dataMonitor", "0");
	tasks.put("step_clean", "0");
    }

    
    /**
     * to load Metop-A tasks ( the same as N18 )
     */
    private void tasks2Moa() {
     	tasks2N18();
    }
    
    /**
     * to load Metop-B tasks ( the same as N18 )
     */
    private void tasks2Mob() {
     	tasks2N18();
    }
    
    
    /**
     * to load N19 tasks ( the same as N18 )
     */
    private void tasks2N19() {
     	tasks2N18();
    }
    
   
    /**
     * to load FY3 HT tasks ( the same as N18 )
     */
    private void tasks2Fy3ht() {
     	tasks2N18();
    }
    
    
    /**
     * to load F16 tasks
     */
    private void tasks2F16() {
    
 	taskKeys.clear();
	taskKeys.add("step_rdr2tdrSensor1");
	taskKeys.add("step_mergeNedt");
	taskKeys.add("step_tdr2sdrSensor1");
	taskKeys.add("step_fm");
	taskKeys.add("step_nwp");
	taskKeys.add("step_fwd");
	taskKeys.add("step_biasGen");
	taskKeys.add("step_choppRadFiles");
	taskKeys.add("step_externalDataFromRegress");
	taskKeys.add("step_fmsdr2edr");
	taskKeys.add("step_mergeEdr");
	taskKeys.add("step_vipp");
	taskKeys.add("step_grid");
	taskKeys.add("step_nc");
	taskKeys.add("step_figsGen");
	taskKeys.add("step_biasFigsGen");
	taskKeys.add("step_dataMonitor");
	taskKeys.add("step_clean");
	
	tasks.clear();
  	tasks.put("step_rdr2tdrSensor1", "0");
	tasks.put("step_mergeNedt", "0");
	tasks.put("step_tdr2sdrSensor1", "0");
	tasks.put("step_fm", "0");
	tasks.put("step_nwp", "0");
	tasks.put("step_fwd", "0");
	tasks.put("step_biasGen", "0");
	tasks.put("step_choppRadFiles", "0");
	tasks.put("step_externalDataFromRegress", "0");
	tasks.put("step_fmsdr2edr", "0");
	tasks.put("step_mergeEdr", "0");
	tasks.put("step_vipp", "0");
	tasks.put("step_grid", "0");
	tasks.put("step_nc", "0");
	tasks.put("step_figsGen", "0");
	tasks.put("step_biasFigsGen", "0");
	tasks.put("step_dataMonitor", "0");
	tasks.put("step_clean", "0");
    }

    
    /**
     * to load F18 tasks
     */
    private void tasks2F18() {
	tasks2F16();
    }
    

    /**
     * to load F17 tasks
     */
    private void tasks2F17() {
	tasks2F16();
    }
    
    
    /**
     * to load Npp tasks ( one more step of bufr than N18 )
     */
    private void tasks2Npp() {
    
	taskKeys.clear();
	taskKeys.add("step_rdr2tdrSensor1");
	taskKeys.add("step_mergeNedt");
	taskKeys.add("step_tdr2sdrSensor1");
	taskKeys.add("step_fm");
	taskKeys.add("step_nwp");
	taskKeys.add("step_fwd");
	taskKeys.add("step_bufr");
	taskKeys.add("step_biasGen");
	taskKeys.add("step_choppRadFiles");
	taskKeys.add("step_externalDataFromRegress");
	taskKeys.add("step_fmsdr2edr");
	taskKeys.add("step_mergeEdr");
	taskKeys.add("step_vipp");
	taskKeys.add("step_grid");
	taskKeys.add("step_nc");
	taskKeys.add("step_figsGen");
	taskKeys.add("step_biasFigsGen");
	taskKeys.add("step_dataMonitor");
	taskKeys.add("step_clean");

	tasks.clear();
  	tasks.put("step_rdr2tdrSensor1", "0");
	tasks.put("step_mergeNedt", "0");
	tasks.put("step_tdr2sdrSensor1", "0");
	tasks.put("step_fm", "0");
	tasks.put("step_nwp", "0");
	tasks.put("step_fwd", "0");
	tasks.put("step_bufr", "0");
	tasks.put("step_biasGen", "0");
	tasks.put("step_choppRadFiles", "0");
	tasks.put("step_externalDataFromRegress", "0");
	tasks.put("step_fmsdr2edr", "0");
	tasks.put("step_mergeEdr", "0");
	tasks.put("step_vipp", "0");
	tasks.put("step_grid", "0");
	tasks.put("step_nc", "0");
	tasks.put("step_figsGen", "0");
	tasks.put("step_biasFigsGen", "0");
	tasks.put("step_dataMonitor", "0");
	tasks.put("step_clean", "0");
    }

    
    /**
     * to load AQUA tasks
     */
    private void tasks2Aqua() {
    
 	taskKeys.clear();
	taskKeys.add("step_rdr2tdrSensor1");
	taskKeys.add("step_mergeNedt");
	taskKeys.add("step_tdr2sdrSensor1");
	taskKeys.add("step_fm");
	taskKeys.add("step_nwp");
	taskKeys.add("step_fwd");
	taskKeys.add("step_biasGen");
	taskKeys.add("step_choppRadFiles");
	taskKeys.add("step_externalDataFromRegress");
	taskKeys.add("step_fmsdr2edr");
	taskKeys.add("step_mergeEdr");
	taskKeys.add("step_vipp");
	taskKeys.add("step_grid");
	taskKeys.add("step_nc");
	taskKeys.add("step_figsGen");
	taskKeys.add("step_biasFigsGen");
	taskKeys.add("step_clean");
	
	tasks.clear();
  	tasks.put("step_rdr2tdrSensor1", "0");
	tasks.put("step_mergeNedt", "0");
	tasks.put("step_tdr2sdrSensor1", "0");
	tasks.put("step_fm", "0");
	tasks.put("step_nwp", "0");
	tasks.put("step_fwd", "0");
	tasks.put("step_biasGen", "0");
	tasks.put("step_choppRadFiles", "0");
	tasks.put("step_externalDataFromRegress", "0");
	tasks.put("step_fmsdr2edr", "0");
	tasks.put("step_mergeEdr", "0");
	tasks.put("step_vipp", "0");
	tasks.put("step_grid", "0");
	tasks.put("step_nc", "0");
	tasks.put("step_figsGen", "0");
	tasks.put("step_biasFigsGen", "0");
	tasks.put("step_clean", "0");
    }

    
    /**
     * to load FY3 MWRI tasks ( same as AQUA AMSRE )
     */
    private void tasks2Fy3ri() {
    	tasks2Aqua();
    }
    
    /**
     * to load GCOMW1 AMSR2 tasks ( same as AQUA AMSRE )
     */
    private void tasks2Gcomw1() {
    	tasks2Aqua();
    }
    
    /**
     * to load TRMM TMI tasks ( same as F16 SSMIS )
     */
    private void tasks2Trmm() {
    	tasks2F16();
    }
    
    /**
     * to load GPM GMI tasks ( same as F16 SSMIS )
     */
    private void tasks2Gpm() {
    	tasks2F16();
    }
    
    /**
     * to load MADRAS tasks ( same as F16 SSMIS )
     */
    private void tasks2Mtma() {
    	tasks2F16();
    }
    
    /**
     * to load SAPHIR tasks ( same as F16 SSMIS )
     */
    private void tasks2Mtsa() {
    	tasks2F16();
    }
    
    /**
     * fwd task string change for N18
     */
    private void fwdChopp2N18() {

	fwdGdasTaskString = new String(
	"#--------------------------------------------------------------------------------\n" +
	"#	step: Application of forward operator on NWP GDAS analyses\n"		      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}"    + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd}"    + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "    + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${gdasData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdEcmwfTaskString = new String(
	"#--------------------------------------------------------------------------------\n" +
	"#	step: Application of forward operator on NWP ECMWF analyses\n"  	      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}"    + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd}"    + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "    + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${ecmwfData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );

	fwdGfsTaskString = new String(
	"#--------------------------------------------------------------------------------\n" +
	"#	step: Application of forward operator on NWP GFS analyses\n"		      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}"    + "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1Sensor2File} ${nProfs2Fwd}"    + "\\\n" + 
	"  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "    + "\\\n" +
        "  ${binPath} ${satId} ${sensorId} ${gfsData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	choppRadFilesTaskString     
    	= new String(
	"#--------------------------------------------------------------------------------\n" +
	"#	step: Chopping the FMSDR file  into pieces for faster processing\n"	      +
	"#--------------------------------------------------------------------------------\n" +
	"if [[ \"${step_choppRadFiles}\" -eq 1 ]] ; then\n" +
	"  chopp ${fmsdrDir} ${fmsdr4ChoppList} ${sensor1}${sensor2} ${fmsdrChoppDir}"        + "\\\n" +
	"  ${nChoppedFilesPerOrbit} ${logFile} ${choppControlFile} ${choppSrc} ${makeOrNot}"  + "\\\n" +
	"  ${binPath} ${processMode} ${orbitInfo} \"${extResol}\"\n" +
	"fi\n\n" ); 
	
	dataMonitorTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Data monitoring (plots)\n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_dataMonitor}\" -eq 1 ]] ; then\n" +
	"  qcRetrieval ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_abnormal_${satId}_${fileExt}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_namelist_${satId}_${fileExt}" + "\\\n" +
	"  \"${email}\" \"${website}\" ${controlDataPath}" + "\n\n" +
	"  dataQualityMonitor ${nedtSensor1Sensor2Path} ${nedtList} ${nedtMonitorSrc} ${IDL}" + "\\\n" + 
	"  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} ${perfsMonitorPath}\n" +
	"fi\n\n" );
    }


    /**
     * fwd task string change for Metop-A
     */
    private void fwdChopp2Moa() {
    	fwdChopp2N18();
    }

    /**
     * fwd task string change for Metop-B
     */
    private void fwdChopp2Mob() {
    	fwdChopp2N18();
    }


    /**
     * fwd task string change for N19
     */
    private void fwdChopp2N19() {
    	fwdChopp2N18();
    }


    /**
     * fwd task string change for FY3 MWHS/MWTS
     */
    private void fwdChopp2Fy3ht() {
    	fwdChopp2N18();
    }


    /**
     * fwd task string change for F16
     */
    private void fwdChopp2F16() {
    
	fwdGdasTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP GDAS analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd}"		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} " 	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${gdasData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdEcmwfTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP ECMWF analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd}"		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} " 	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${ecmwfData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdGfsTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP GFS analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd}"		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} " 	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${gfsData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	choppRadFilesTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Chopping the FMSDR file  into pieces for faster processing\n" 		+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_choppRadFiles}\" -eq 1 ]] ; then\n" +
	"  chopp ${fmsdrDir} ${fmsdr4ChoppList} ${sensor1} ${fmsdrChoppDir}" 		        + "\\\n" +
	"  ${nChoppedFilesPerOrbit} ${logFile} ${choppControlFile} ${choppSrc} ${makeOrNot}" 	+ "\\\n" + 
	"  ${binPath} ${processMode} ${orbitInfo} \"${extResol}\"\n" +
	"fi\n\n" ); 
	
	dataMonitorTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Data monitoring (plots)\n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_dataMonitor}\" -eq 1 ]] ; then\n" +
	"  qcRetrieval ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_abnormal_${satId}_${fileExt}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_namelist_${satId}_${fileExt}" + "\\\n" +
	"  \"${email}\" \"${website}\" ${controlDataPath}" + "\n\n" +
	"  dataQualityMonitor ${nedtSensor1Path} ${nedtList} ${nedtMonitorSrc} ${IDL}" + "\\\n" + 
	"  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} ${perfsMonitorPath}\n" +
	"fi\n\n" );
    }


    /**
     * fwd task string change for F18
     */
    private void fwdChopp2F18() {
	fwdChopp2F16();
    }

    /**
     * fwd task string change for F17
     */
    private void fwdChopp2F17() {
	fwdChopp2F16();
    }

    
    /**
     * fwd task string change for Npp
     */
    private void fwdChopp2Npp() {
    	fwdChopp2F16();
    }


    /**
     * fwd task string change for AQUA
     */
    private void fwdChopp2Aqua() {
    
	fwdGdasTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP GDAS analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd} " 		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${gdasData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdEcmwfTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP ECMWF analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd} " 		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${ecmwfData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdGfsTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP GFS analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd} " 		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${gfsData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	choppRadFilesTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Chopping the FMSDR file  into pieces for faster processing\n" 		+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_choppRadFiles}\" -eq 1 ]] ; then\n" +
	"  chopp ${fmsdrDir} ${fmsdr4ChoppList} ${sensor1} ${fmsdrChoppDir}" 		        + "\\\n" +
	"  ${nChoppedFilesPerOrbit} ${logFile} ${choppControlFile} ${choppSrc} ${makeOrNot}" 	+ "\\\n" + 
	"  ${binPath} ${processMode} ${orbitInfo} \"${extResol}\"\n" +
	"fi\n\n" ); 
	
	dataMonitorTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Data monitoring (plots)\n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_dataMonitor}\" -eq 1 ]] ; then\n" +
	"  qcRetrieval ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_abnormal_${satId}_${fileExt}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_namelist_${satId}_${fileExt}" + "\\\n" +
	"  \"${email}\" \"${website}\" ${controlDataPath}" + "\n\n" +
	"  dataQualityMonitor ${nedtSensor1Path} ${nedtList} ${nedtMonitorSrc} ${IDL}" + "\\\n" + 
	"  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} ${perfsMonitorPath}\n" +
	"fi\n\n" );
    }		


    /**
     * fwd task string change for FY3 MWRI, the same as AQUA AMSRE
     */
    private void fwdChopp2Fy3ri() {
	//fwdChopp2Aqua();
	fwdChoppOneSensor();
    }		

    /**
     * fwd task string change for GCOMW1 AMSR2, the same as AQUA AMSRE
     */
    private void fwdChopp2Gcomw1() {
	//fwdChopp2Aqua();
	fwdChoppOneSensor();
    }		

    /**
     * fwd task string change for TRMM TMI
     */
    private void fwdChopp2Trmm() {
	fwdChoppOneSensor();
    }		

    /**
     * fwd task string change for GPM GMI
     */
    private void fwdChopp2Gpm() {
	fwdChoppOneSensor();
    }		

    /**
     * fwd task string change for MADRAS
     */
    private void fwdChopp2Mtma() {
	fwdChoppOneSensor();
    }		

    /**
     * fwd task string change for SAPHIR
     */
    private void fwdChopp2Mtsa() {
	fwdChoppOneSensor();
    }		

    /**
     * fwd task string change for one sensor case
     */
    private void fwdChoppOneSensor() {
    
	fwdGdasTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP GDAS analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd}  " 		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${gdasData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdEcmwfTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP ECMWF analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd}  " 		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${ecmwfData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	fwdGfsTaskString = new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Application of forward operator on NWP GFS analyses\n" 			+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_fwd}\" -eq 1 ]] ; then\n" +
	"  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}" 	+ "\\\n" +
        "  ${fwdAnalysDir} ${CRTMcoeffPath} ${instrumentSensor1File} ${nProfs2Fwd}  " 		+ "\\\n" +
        "  ${addDeviceNoise} ${nedtAftFMFile} ${monitorFwd} ${logFile} ${fwdControlFile} "	+ "\\\n" + 
	"  ${binPath} ${satId} ${sensorId} ${gfsData} ${fwdCloudOffOrOn} \"${rdirExt}\"\n" +
	"fi\n\n" );
	
	choppRadFilesTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Chopping the FMSDR file into pieces for faster processing\n" 		+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_choppRadFiles}\" -eq 1 ]] ; then\n" +
	"  chopp ${fmsdrDir} ${fmsdr4ChoppList} ${sensor1} ${fmsdrChoppDir}" 		        + "\\\n" +
	"  ${nChoppedFilesPerOrbit} ${logFile} ${choppControlFile} ${choppSrc} ${makeOrNot}" 	+ "\\\n" + 
	"  ${binPath} ${processMode} ${orbitInfo} \"${extResol}\"\n" +
	"fi\n\n" ); 
	
	dataMonitorTaskString	    
    	= new String(
	"#--------------------------------------------------------------------------------\n" 	+
	"#      step: Data monitoring (plots)\n" 						+
	"#--------------------------------------------------------------------------------\n" 	+
	"if [[ \"${step_dataMonitor}\" -eq 1 ]] ; then\n" +
	"  qcRetrieval ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_abnormal_${satId}_${fileExt}" + "\\\n" +
	"  ${controlDataPath}/qcRetrieval_namelist_${satId}_${fileExt}" + "\\\n" +
	"  \"${email}\" \"${website}\" ${controlDataPath}" + "\n\n" +
	"  dataQualityMonitor ${nedtSensor1Path} ${nedtList} ${nedtMonitorSrc} ${IDL}" + "\\\n" + 
	"  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} ${perfsMonitorPath}\n" +
	"fi\n\n" );
    }		

    
    /**
     * Invoked when task's progress property changes.
     */
    public void propertyChange(PropertyChangeEvent evt) {
        if ("progress" == evt.getPropertyName()) {
            int progress = (Integer) evt.getNewValue();
            progressBar.setValue(progress);
            //taskOutput.append(String.format(
            //        "Completed %d%% of task.\n", task.getProgress()));
        } 
    }


    /**
     * Create the GUI and show it. As with all GUI code, this must run
     * on the event-dispatching thread.
     */
    private static void createAndShowGUI() {
	//Create and set up the window.
	JFrame frame = new JFrame("MIRS Control Panel");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	
	//Create and set up the content pane.
	JComponent newContentPane = new Monitor();
	newContentPane.setPreferredSize(new Dimension(1300,1000));
	newContentPane.setOpaque(true); //content panes must be opaque
	frame.setContentPane(newContentPane);

	//Display the window.
	frame.pack();
	frame.setSize(920,800);
	frame.setResizable(true);
	Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
	if (frame.getWidth()  > d.width ) frame.setSize(d.width,frame.getHeight());
	if (frame.getHeight() > d.height) frame.setSize(frame.getWidth(),d.height);
	
	frame.setVisible(true);
	frame.setLocationRelativeTo(null); 
    }

    /**
     * Main method to run
     */ 
    public static void main(String[] args) {
        //Schedule a job for the event-dispatching thread:
        //creating and showing this application's GUI.
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }
        
}

/**
 * Image panel to load logo image.
 */
class ImagePanel extends JPanel
{  
   public ImagePanel()
   {  
      // acquire the image
      image = Toolkit.getDefaultToolkit().getImage("../data/StaticData/Logos/noaalogo.jpg");
      MediaTracker tracker = new MediaTracker(this);
      tracker.addImage(image, 0);
      try { tracker.waitForID(0); }
      catch (InterruptedException exception) {}
   }
   
   public ImagePanel(String imgFile)
   {  
      // acquire the image
      image = Toolkit.getDefaultToolkit().getImage(imgFile);
      MediaTracker tracker = new MediaTracker(this);
      tracker.addImage(image, 0);
      try { tracker.waitForID(0); }
      catch (InterruptedException exception) {}
   }
   
   public void paintComponent(Graphics g)
   {  
      super.paintComponent(g);
      int imageWidth = image.getWidth(this);
      int imageHeight = image.getHeight(this);
	
      g.drawImage(image, 0, 0, null);
   }
   
   private Image image;
   private static final long serialVersionUID = 1L;
}


class ImageDialog extends JDialog 
{
	public ImageDialog(ImagePanel panel) {
		super();
		setSize(680,540);
    		Container contentPane = getContentPane();
    		contentPane.add(panel, "Center");
	}
}
