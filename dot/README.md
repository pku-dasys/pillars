DOT for Storing DFG
=====================

A DFG TXT is a text file storing an DFG (data-flow graph).

A DFG is written in a dot graph format that includes metadata, such as labeling inputs, outputs, operations,
      and operands within the computation. Some other CGRA frameworks such CGRA-ME and CCF also employ similar format.
      

A DFG TXT is in the following format, users can see examples in sub-folders.
   *          "digraph G {                                    "
   *          "the name of a DFG node [opcode = ?];           "
   *          ......
   *          "source DFG node -> sink DFG node [operand = ?];"
   *          ......
   *          "}                                              "