package com.microsoft.bond.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.plugins.JavaPlugin
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.InputFiles
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction

class BondCodegen extends DefaultTask {
    List<String> gbcBaseArgv = ['gbc', 'java']
    File defaultBondRoot = project.file('src/main/bond')

    @Input
    List<String> gbcOptions = []

    @InputFiles
    Set<File> allBondfiles = defaultBondfiles()

    @OutputDirectory
    File outputDir = project.file("${project.buildDir.canonicalPath}/generated-bond-src")

    BondCodegen() {
        // Assumes we are being called from a Java or Java-inherited
        // (e.g., Kotlin) context.
        project.tasks.getByName(JavaPlugin.COMPILE_JAVA_TASK_NAME).dependsOn this

        // Needs to happen regardless of whether gradle decides we're up-to-date.
        // We can do this in the constructor for now, but if we expose the
        // ability to change our output dir, this has to happen there.
        project.sourceSets.main.java.srcDir outputDir.canonicalPath
    }

    def options(Object... optionList) {
        optionList.each { option ->
            gbcOptions << option.toString()
        }
    }

    private def defaultBondfiles() {
        Set<File> defaultBondfiles = []

        if (defaultBondRoot.exists()) {
            project.fileTree(defaultBondRoot) {
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
            project.logger.warn("bond codegen enabled, but no bondfiles detected or configured")
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
