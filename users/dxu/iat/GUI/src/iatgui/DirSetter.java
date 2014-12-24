package iatgui;

public final class DirSetter {

	// Package Home directory (windows)
	public static String theGUI_Home = System.getProperty("user.dir");
	public static String theIAT_Home;
	public static String theVsdbHome;
	public static String theRadmonHome;
	public static String theGeHome;
	public static String theHitHome;
	public static String theFcstDiffHome;

	public static String osName;

	public DirSetter() {
	}

	public static void setDirs() {
		// Get OS name in lower case.
		osName = System.getProperty("os.name").toLowerCase();

		// Set path based on OS.
		if (osName.indexOf("win") >= 0) {
			// Setting for testing IAT in windows
			// IAT Root directory, remove "GUI" ( 3 chars ) from string.
			theIAT_Home = theGUI_Home.substring(0,
					theGUI_Home.length() - 3);
			
			theVsdbHome = theIAT_Home + "\\vsdb_pkg\\vsdb_v17";
			theRadmonHome = theIAT_Home
					+ "\\radmon_pkg\\radmon_pkg\\radmon\\util\\Radiance_Monitor";
			theGeHome = theIAT_Home + "\\ge_pkg\\ge_pkg\\ge";
			theHitHome = theIAT_Home + "\\hit_pkg\\hit";
			theFcstDiffHome = theIAT_Home + "\\fcstDiff_pkg\\fcstDiff";
			
			
			System.out.println("vsdb home with DirSetter");
			System.out.println(theVsdbHome);
		} else {
			// Setting for Linux
			// IAT Root directory
			theIAT_Home = theGUI_Home + "/../..";
			theVsdbHome = theIAT_Home + "/vsdb_pkg/vsdb_v17";
			theRadmonHome = theIAT_Home
					+ "/radmon_pkg/radmon_pkg/radmon/util/Radiance_Monitor";
			theGeHome = theIAT_Home + "/ge_pkg/ge_pkg/ge/";
			theHitHome = theIAT_Home + "/hit_pkg/hit";
			theFcstDiffHome = theIAT_Home
					+ "/fcstDiff_pkg/fcstDiff";
		}	
	}

	
	public static String getVsdbRoot()	{
		setDirs();
		return theVsdbHome;
	}

	public static String getRadmonRoot()	{
		setDirs();
		return theVsdbHome;
	}

	public static String getGeRoot()	{
		setDirs();
		return theVsdbHome;
	}

	public static String getHitRoot()	{
		setDirs();
		return theVsdbHome;
	}

	public static String getFcstDiffRoot()	{
		setDirs();
		return theVsdbHome;
	}
}
