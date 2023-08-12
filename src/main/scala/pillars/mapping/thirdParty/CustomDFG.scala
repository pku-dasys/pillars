package pillars.mapping.thirdParty

import scala.collection.mutable.Map

object CustomDFG extends App {
  def initVadd(g: Digraph): Unit = {
    //Initialize nodes in this DFG
    //input index i of A(i) and B(i)
    g.node("input0", attrs = Map("opcode" -> "input"))
    //offset of array A in SRAM 0
    g.node("const0", attrs = Map("opcode" -> "const"))
    //offset of array B in SRAM 1
    g.node("const1", attrs = Map("opcode" -> "const"))
    //get the offset of A(i) by adding const0 to input0
    g.node("add0", attrs = Map("opcode" -> "add"))
    //get the offset of B(i) by adding const1 to input0
    g.node("add1", attrs = Map("opcode" -> "add"))
    //get the value of A(i) from SRAM 0
    g.node("load0", attrs = Map("opcode" -> "load", "sram" -> "0"))
    //get the value of B(i) from SRAM 1
    g.node("load1", attrs = Map("opcode" -> "load", "sram" -> "1"))
    //sum A(i) and B(i)
    g.node("add2", attrs = Map("opcode" -> "add"))
    //get (A(i) + B(i)) from output port
    g.node("output0", attrs = Map("opcode" -> "output"))

    //Initialize edges in this DFG
    //connect inputs of node add0
    g.edge("const0", "add0"
      , attrs = Map("operand" -> "0"))
    g.edge("input0", "add0"
      , attrs = Map("operand" -> "1"))
    //connect inputs of node add1
    g.edge("const1", "add1"
      , attrs = Map("operand" -> "0"))
    g.edge("input0", "add1"
      , attrs = Map("operand" -> "1"))
    //connect add0 to load0
    g.edge("add0", "load0"
      , attrs = Map("operand" -> "0"))
    //connect add1 to load1
    g.edge("add1", "load1"
      , attrs = Map("operand" -> "0"))
    //connect inputs of node add2
    g.edge("load0", "add2"
      , attrs = Map("operand" -> "0"))
    g.edge("load1", "add2"
      , attrs = Map("operand" -> "1"))
    //connect add2 to output0
    g.edge("add2", "output0"
      , attrs = Map("operand" -> "0"))

  }

  def initReverse(g: Digraph): Unit = {
    //Initialize nodes in this DFG
    g.node("incr0", attrs = Map("opcode" -> "incr"))
    g.node("const2", attrs = Map("opcode" -> "const"))
    g.node("sub0", attrs = Map("opcode" -> "sub"))
    g.node("const3", attrs = Map("opcode" -> "const"))
    g.node("const4", attrs = Map("opcode" -> "const"))
    g.node("add3", attrs = Map("opcode" -> "add"))
    g.node("add4", attrs = Map("opcode" -> "add"))
    g.node("load2", attrs = Map("opcode" -> "load"))
    g.node("store0", attrs = Map("opcode" -> "store", "sram" -> "3"))

    //Initialize edges in this DFG
    //connect inputs of node add3
    g.edge("incr0", "add3"
      , attrs = Map("operand" -> "0"))
    g.edge("const3", "add3"
      , attrs = Map("operand" -> "1"))
    //connect inputs of node sub0
    //operand of the minuend should be 0
    g.edge("const2", "sub0"
      , attrs = Map("operand" -> "0"))
    //operand of the subtrahend should be 1
    g.edge("incr0", "sub0"
      , attrs = Map("operand" -> "1"))
    //connect inputs of node add4
    g.edge("sub0", "add4"
      , attrs = Map("operand" -> "0"))
    g.edge("const4", "add4"
      , attrs = Map("operand" -> "1"))
    //connect add3 to load2
    g.edge("add3", "load2"
      , attrs = Map("operand" -> "0"))
    //connect add4 to store0, operand of the address should be 0
    g.edge("add4", "store0"
      , attrs = Map("operand" -> "0"))
    //connect load2 to store0, operand of the value to be saved should be 0
    g.edge("load2", "store0"
      , attrs = Map("operand" -> "1"))
  }

  val g = new Digraph("VAdd")
  initVadd(g)
  initReverse(g)
  //save the .dot file and visualize
  g.save("Vadd_Reverse_.dot", "./tutorial/", print = true)
  g.render(fileName = "Vadd_Reverse_.dot", directory = "./tutorial/", format = "jpg")
//  g.view(fileName = "Vadd_Reverse_.dot", directory = "./tutorial/")
}
