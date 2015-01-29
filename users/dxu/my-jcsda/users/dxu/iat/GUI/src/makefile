JAVA_HOME=/usr/java/latest

all:
	${JAVA_HOME}/bin/javac  iatgui/IAT.java
	${JAVA_HOME}/bin/jar cvfm  ../bin/IAT.jar  IAT.mf  iatgui/*.class
	rm -f iatgui/*.class

run:
	${JAVA_HOME}/bin/java -Dawt.toolkit=sun.awt.X11.XToolkit -jar ../bin/IAT.jar &


clean:
	rm  -f iatgui/*.class ../bin/IAT.jar

