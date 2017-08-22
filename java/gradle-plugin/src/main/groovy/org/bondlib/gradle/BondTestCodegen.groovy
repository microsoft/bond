package org.bondlib.gradle

import org.gradle.api.plugins.JavaPlugin
import org.gradle.api.tasks.SourceSet

class BondTestCodegen extends BondCodegen {
    @Override
    String dependentTaskName() {
        return JavaPlugin.COMPILE_TEST_JAVA_TASK_NAME
    }

    @Override
    SourceSet dependentSourceSet() {
        return project.sourceSets.test
    }

    @Override
    String bondSrcRootPath() {
        return "src/test/bond"
    }

    @Override
    String outputDirName() {
        return "generated-bond-test-src"
    }
}
