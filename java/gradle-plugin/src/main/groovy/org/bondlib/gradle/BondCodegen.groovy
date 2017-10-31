package org.bondlib.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.InputFiles
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.SourceSet
import org.gradle.api.tasks.TaskAction

abstract class BondCodegen extends DefaultTask {
    // main/test codegen is separate and identical except for a few params.
    abstract String dependentTaskName()
    abstract SourceSet dependentSourceSet()
    abstract String bondSrcRootPath()
    abstract String outputDirName()

    List<String> gbcBaseArgv = ['gbc', 'java']
    File bondSrcRoot = project.file(bondSrcRootPath())

    @Input
    List<String> gbcOptions = []

    @InputFiles
    Set<File> allBondfiles = defaultBondfiles()

    @OutputDirectory
    File outputDir = project.file("${project.buildDir.canonicalPath}/" + outputDirName())

    BondCodegen() {
        // Assumes we are being called from a Java or Java-inherited
        // (e.g., Kotlin) context.
        project.tasks.getByName(dependentTaskName()).dependsOn(this)

        // Needs to happen regardless of whether gradle decides we're up-to-date.
        // We can do this in the constructor for now, but if we expose the
        // ability to change our output dir, this has to happen there.
        dependentSourceSet().java.srcDir(outputDir.canonicalPath)
    }

    def options(Object... optionList) {
        optionList.each { option ->
            gbcOptions << option.toString()
        }
    }

    private def defaultBondfiles() {
        Set<File> defaultBondfiles = []

        if (bondSrcRoot.exists()) {
            project.fileTree(bondSrcRoot) {
                include '**/*.bond'
            }.each { bondfile ->
                defaultBondfiles.add(bondfile)
            }
        }

        return defaultBondfiles
    }

    def bondfiles(Object... bondfileList) {
        bondfileList.each { bondfile ->
            allBondfiles << project.file(bondfile)
        }
    }

    @TaskAction
    def codegen() {
        if (allBondfiles.empty) {
            return
        }

        def gbcArgv = gbcBaseArgv.clone()
        gbcArgv += ['--output-dir', outputDir.canonicalPath]
        gbcArgv += gbcOptions

        allBondfiles.each { bondfile ->
            gbcArgv << bondfile.canonicalPath
        }

        project.logger.info("BondCodegen: executing gbc argv ${gbcArgv}")
        def result = project.exec {
            commandLine gbcArgv
        }

        if (result.exitValue != 0) {
            def errmsg = "gbc returned ${result.exitValue}"
            project.logger.error(errmsg)
            throw new GradleException(errmsg)
        }
    }
}
