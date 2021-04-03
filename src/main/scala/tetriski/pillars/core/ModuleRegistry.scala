package tetriski.pillars.core

import chisel3.Module
import tetriski.pillars.archlib.{ElementAlu, ElementConst, ElementCounter, ElementLSU, ElementMux, ElementRF}

import scala.collection.mutable.ArrayBuffer

object ModuleRegistry {
  val classOfElements = new ArrayBuffer[Class[_ <: ElementTrait]]()
  val classOfModules = new ArrayBuffer[Class[_ <: Module]]()

  val defaultLSU = new ElementLSU("Default",List(16))

  val defaultAlu = new ElementAlu("Default", List(OpEnum.AND), false, List(16))

  val defaultRF = new ElementRF("Default", List(1, 1, 2, 16))

  val defaultConstUnit = new ElementConst("Default",List(16))

  val defaultMux = new ElementMux("Default",List(16))

  val defaultCounter = new ElementCounter("Default",List(16))

  def getID(element: ElementTrait): Int = {
    val EClass = element.getClass
    if (classOfElements != null) {
      if (classOfElements.contains(EClass)) {
        classOfElements.indexOf(EClass)
      } else {
        register(element)
        classOfElements.size - 1
      }
    } else {
      register(element)
      classOfElements.size - 1
    }

  }

  def register(element: ElementTrait): Unit = {
    val EClass = element.getClass
    val MClass = element.correlation

    if (classOfElements != null) {
      if (classOfElements.contains(EClass)) {
        println(EClass.getName + " has been registered.")
      } else {
        classOfElements.append(EClass)
        classOfModules.append(MClass)
      }
    } else {
      println("Registering correlation between " + EClass.getName + " and " + MClass.getName + ".")
      classOfElements.append(EClass)
      classOfModules.append(MClass)
    }

  }
}
