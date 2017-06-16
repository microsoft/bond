To build with a development version of this plugin, repeat as follows:
* make changes
* gradle build
* mvn install:install-file -Dfile=${BOND_JAVA_ROOT}/gradle-plugin/build/libs/bond-gradle-1.0.jar -DgroupId=com.microsoft.bond -DartifactId=gradle -Dversion=1.0 -Dpackaging=jar
This will install the plugin to your local maven repository, which the other
Java projects' build.gradles will use in preference to maven central.
