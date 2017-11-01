package org.bondlib.gradle

import org.gradle.api.plugins.JavaPlugin
import org.gradle.api.tasks.SourceSet

class BondProdCodegen extends BondCodegen {
    @Override
    String dependentTaskName() {
        return JavaPlugin.COMPILE_JAVA_TASK_NAME
    }

    @Override
    SourceSet dependentSourceSet() {
        return project.sourceSets.main
    }

    @Override
    String bondSrcRootPath() {
        return "src/main/bond"
    }

    @Override
    String outputDirName() {
        return "generated-bond-main-src"
    }
}
