/*
 * Purpose: This is a helper class to configure paths 
 *          of various IAT packages.       
 *       
 * Author: Deyong Xu / RTI @ JCSDA
 * Last update: 1/27/2015, Initial coding
 *  
 */

package iatgui;

import java.io.File;

import javax.swing.JFileChooser;

public final class DirSetter {

	// Package Home directory (windows)
	public static String theGUI_Home;
	public static String theIAT_Home;
	public static String theVsdbHome;
	public static String theRadmonHome;
	public static String theGeHome;
	public static String theHitHome;
	public static String theFcstDiffHome;

	// Package Workspace directory (windows)
	public static String theGUI_Workspace;
	public static String theIAT_Workspace;
	public static String theVsdbWorkspace;
	public static String theRadmonWorkspace;
	public static String theGeWorkspace;
	public static String theHitWorkspace;
	public static String theFcstDiffWorkspace;

	public static String theOS;

	public DirSetter() {
	}

	// Use static block to initialize static variables
	static {
		// Get GUI Home directory
		theGUI_Home = System.getProperty("user.dir");

		// Get OS name in lower case.
		theOS = System.getProperty("os.name").toLowerCase();
		// Set path based on OS.
		if (isWindows()) {
			// Setting for testing IAT in windows
			// IAT Root directory, remove "GUI" ( 3 chars ) from string.
			theIAT_Home = theGUI_Home.substring(0, theGUI_Home.length() - 4);

			// Package HOME
			theVsdbHome = theIAT_Home + "\\vsdb_pkg\\vsdb_v17";
			theRadmonHome = theIAT_Home + "\\radmon_pkg\\radmon";
			theGeHome = theIAT_Home + "\\ge_pkg\\ge";
			theHitHome = theIAT_Home + "\\hit_pkg\\hit";
			theFcstDiffHome = theIAT_Home + "\\fcstDiff_pkg\\fcstDiff";

			// Package workspace
			theVsdbWorkspace = theIAT_Home + "\\workspace\\vsdb_workspace";
			theRadmonWorkspace = theIAT_Home + "\\workspace\\radmon_workspace";
			theGeWorkspace = theIAT_Home + "\\workspace\\ge_workspace";
			theHitWorkspace = theIAT_Home + "\\workspace\\hit_workspace";
			theFcstDiffWorkspace = theIAT_Home
					+ "\\workspace\\fcstDiff_workspace";

		} else {
			// Setting for Linux
			// Package HOME
			// theIAT_Home = theGUI_Home + "/../..";
			theIAT_Home = theGUI_Home.substring(0, theGUI_Home.length() - 8);

			theVsdbHome = theIAT_Home + "/vsdb_pkg/vsdb_v17";
			theRadmonHome = theIAT_Home + "/radmon_pkg/radmon";
			theGeHome = theIAT_Home + "/ge_pkg/ge";
			theHitHome = theIAT_Home + "/hit_pkg/hit";
			theFcstDiffHome = theIAT_Home + "/fcstDiff_pkg/fcstDiff";

			// Package workspace
			String upperLevelDir = theIAT_Home + "/..";
			String workspace = upperLevelDir + "/workspace";
			theVsdbWorkspace = workspace + "/vsdb_workspace";
			theRadmonWorkspace = workspace + "/radmon_workspace";
			theGeWorkspace = workspace + "/ge_workspace";
			theHitWorkspace = workspace + "/hit_workspace";
			theFcstDiffWorkspace = workspace + "/fcstDiff_workspace";
		}

	}

	// Get package home
	public static String getGUI_Root() {
		return theGUI_Home;
	}

	public static String getFcstDiffRoot() {
		return theFcstDiffHome;
	}

	public static String getGeRoot() {
		return theGeHome;
	}

	public static String getHitRoot() {
		return theHitHome;
	}

	public static String getRadmonRoot() {
		return theRadmonHome;
	}

	public static String getVsdbRoot() {
		return theVsdbHome;
	}

	// Get workspace
	public static String getFcstDiffWorkspace() {
		return theFcstDiffWorkspace;
	}

	public static String getGeWorkspace() {
		return theGeWorkspace;
	}

	public static String getHitWorkspace() {
		return theHitWorkspace;
	}

	public static String getRadmonWorkspace() {
		return theRadmonWorkspace;
	}

	public static String getVsdbWorkspace() {
		return theVsdbWorkspace;
	}

	// Check platform
	public static boolean isWindows() {
		if (theOS.indexOf("win") >= 0)
			return true;
		else
			return false;
	}

	public static boolean isLinux() {
		return !isWindows();
	}

}
