package org.jetbrains.plugins.cbt.runner

import java.util

import com.intellij.execution.Executor
import com.intellij.execution.configurations._
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.module.Module
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project

import scala.collection.JavaConversions._


class CbtRunConfiguration(val project: Project, val configurationFactory: ConfigurationFactory, val name: String)
  extends ModuleBasedConfiguration[RunConfigurationModule](name, new RunConfigurationModule(project), configurationFactory) {

  private def defaultWorkingDirectory = Option(project.getBaseDir).fold("")(_.getPath)

  override def getValidModules: util.Collection[Module] = List()

  override def getConfigurationEditor: SettingsEditor[_ <: RunConfiguration] = new CbtRunConfigurationEditor()

  override def getState(executor: Executor, environment: ExecutionEnvironment): RunProfileState = {
    new CbtComandLineState(this, environment)
  }
}

class CbtComandLineState(configuration: CbtRunConfiguration, environment: ExecutionEnvironment)
  extends CommandLineState(environment) {
  override def startProcess(): ProcessHandler = {
    null
  }
}