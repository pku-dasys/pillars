TXTs for Storing Mapping Results
=====================

##Information TXT

An information TXT is a text file storing detail mapping results of modules.
Only MRRG nodes which routes data or perform functions 


An information TXT is in the following format.
 Every 3 lines contain an item of information.
 Users can some pre-generated information TXT in sub-folders. (*_i.txt)
   *          "<the name of MRRG inner node routing data>"
   *          "the serial number of fan-ins routing data "
   *          "the serial number of fan-outs routing data"
   *          ......
   
   or
   *          "<the name of MRRG inner node routing data>"
   *          "SELECTED_OP                               "
   *          "the selected opcode                       "
   *          ......
   
##Result TXT

A result TXT is a text file showing the mapping results of the origin nodes in DFG.

An information TXT is in the following format.
 Users can some prepared information TXT in sub-folders. (*_r.txt)
   *          "the name of DFG node" "the name of MRRG node mapped onto" "fire time" "skew"


