TXT for Storing MRRG
=====================

An MRRG TXT is a text file storing an MRRG (modulo routing resource graph).

An MRRG TXT is in the following format, users can some pre-generated MRRG in this folder.
   *          "number of nodes without opcode"
   *          "<the name of an MRRG node>    "
   *          "nIn (the number of fan-ins)   "
   *          "the name of fan-in 0          "
   *          ......
   *          "the name of fan-in nIn        "
   *          "nOut (the number of fan-outs) "
   *          "the name of fan-out 0         "
   *          ......
   *          "the name of fan-out nOut      "
   *          ......
   *          "number of nodes with opcodes  "
   *          "<the name of an MRRG node>    "
   *          "nIn (the number of fan-ins)   "
   *          "the name of fan-in 0          "
   *          ......
   *          "the name of fan-in nIn        "
   *          "nOut (the number of fan-outs) "
   *          "the name of fan-out 0         "
   *          ......
   *          "the name of fan-out nOut      "
   *          "nOp (the number of opcodes)   "
   *          "opcode 0                      "
   *          ......
   *          "opcode nOp                    "
   *          ......