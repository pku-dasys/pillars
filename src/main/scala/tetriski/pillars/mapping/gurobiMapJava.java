package tetriski.pillars.mapping;

import gurobi.*;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;

import static java.lang.Math.abs;


/**
 * This class based on disjoint set is used to check whether there are useless rings in some direction graphs.
 */
class checkWithoutRing {
    List<Map<Integer, List<Integer>>> directionGraphs;

    /**
     * Constructed function.
     *
     * @param testedGraphs a list of direction graphs
     */
    checkWithoutRing(List<Map<Integer, List<Integer>>> testedGraphs) {
        directionGraphs = testedGraphs;
    }

    /**
     * check whether there are useless rings in targeted direction graphs.
     *
     * @return if all graphs pass simply connected checking, return true,
     * meaning there are not useless rings in targeted direction graphs.
     */
    Boolean check() {
        for (Map<Integer, List<Integer>> graph : directionGraphs) {
            if (!checkSimplyConnected(graph)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Connect two nodes (sharing the same root).
     *
     * @param unionMap the union map
     * @param sourceID the ID of source node
     * @param sinkID   the ID of sink node
     */
    void connect(Map<Integer, Integer> unionMap, Integer sourceID, Integer sinkID) {
        int rootSink = find(unionMap, sinkID);
        int rootSource = find(unionMap, sourceID);
        unionMap.put(Math.min(rootSink, rootSource), Math.max(rootSink, rootSource));
    }

    /**
     * Find root of a node.
     *
     * @param unionMap the union map
     * @param nodeID   the ID of node
     */
    int find(Map<Integer, Integer> unionMap, Integer nodeID) {
        if (!unionMap.containsKey(nodeID)) {
            unionMap.put(nodeID, nodeID);
        }
        int father = unionMap.get(nodeID);
        if (father == nodeID) {
            return nodeID;
        }
        father = find(unionMap, father);
        unionMap.put(nodeID, father);
        return father;
    }

    /**
     * check whether there are useless rings in a targeted direction graph.
     *
     * @param nodeConnects the direction graph
     * @return if the graph passes simply connected checking, return true,
     * meaning there are not useless rings in targeted direction graphs.
     */
    Boolean checkSimplyConnected(Map<Integer, List<Integer>> nodeConnects) {
        Map<Integer, Integer> unionMap = new HashMap();
        for (int sourceID : nodeConnects.keySet()) {
            unionMap.put(sourceID, sourceID);
        }
        for (int sourceID : nodeConnects.keySet()) {
            find(unionMap, sourceID);
            List<Integer> sinkIDs = nodeConnects.get(sourceID);
            for (int sinkID : sinkIDs) {
                connect(unionMap, sourceID, sinkID);
            }
        }
        Set<Integer> rootSet = new HashSet<>();
        for (Integer node : nodeConnects.keySet()) {
            rootSet.add(find(unionMap, node));
        }
        return rootSet.size() == 1;
    }
}

/**
 * A class doing core mapping process using Gurobi
 * <p>
 * Call {@code ILPMap} to do Mapping.
 * {@code getILPModel} is used to setup the Mapping model.
 */
public class gurobiMapJava {
    String filename;

    List<Integer> DFGOpNodeOut;
    List<Integer> DFGOpNodeOpcode;
    List<String> DFGOpNodeName;
    List<List<Integer>> DFGValNodeOut;
    List<List<Integer>> DFGValNodeOutputOperand;
    List<String> DFGValNodeName;

    List<String> MRRGFunctionName;
    List<String> MRRGRoutingName;
    List<List<Integer>> MRRGFunctionFanin;
    List<List<Integer>> MRRGFunctionFaninType;
    List<List<Integer>> MRRGRoutingFanin;
    List<List<Integer>> MRRGRoutingFaninType;
    List<List<Integer>> MRRGFunctionFanout;
    List<List<Integer>> MRRGFunctionFanoutType;
    List<List<Integer>> MRRGRoutingFanout;
    List<List<Integer>> MRRGRoutingFanoutType;
    List<List<Integer>> MRRGFunctionSupportOpcode;

    Map<String, Integer> MRRGLatency;
    Map<String, Integer> DFGValB2opMap;
    Set<Integer> DFGCommutativeSet;
    List<List<String>> connectList;
    Map<String, List<String>> DFGMultipleInputMap;
    Map<String, Integer> DFGSelfJoinMap;
    Map<String, Integer> waitSkewMap;
    Map<String, Integer> DFGRelativeSkewMap;
    Map<String, Integer> DFGLatencyMap;
    Map<List<String>, Integer> MRRGDistance;
    Map<String, Set<String>> MRRGNeighboringNode;
    Map<Integer, Integer> regMap;
    Map<Integer, List<Integer>> regConnect;
    Map<Integer, List<Integer>> func2regMap;
    Map<Integer, List<List<Integer>>> reg2funcMap;
    Map<Integer, List<List<Integer>>> funcDirect2funcMap;
    Set<String> DFGCommutatedSet;
    Map<Integer, List<Integer>> DFGOpOperand;
    int II = 1;
    int numDfgVals = 0;
    int numDfgOps = 0;
    int numMrrgR = 0;
    int numMrrgF = 0;
    int connectSize = 0;
    int countR = 0;
    int countF = 0;
    int iterationNum = 0;
    int iterationLimit = 100;
    int maxLatency = 15;
    int skewLimit = 2;
    Boolean ringCheckPass = false;
    Boolean useRelativeSkew = true;
    int ringCheckCount = 0;
    int ringCheckLimit = 20;
    Random RNG = new Random(1);
    String result = "fail";
    long elapsedTime = 0;
    long startTime = 0;
    long timeLimit = 3600000;
    int neighboringDistance = 20;
    int usedBypassALU = 0;
    int usedFuncALU = 0;

    /**
     * Set up the class.
     *
     * @param filename the file name used in printing result
     */
    gurobiMapJava(String filename) {
        this.filename = filename;

        DFGOpNodeName = new ArrayList<>();
        DFGOpNodeOpcode = new ArrayList<>();
        DFGOpNodeOut = new ArrayList<>();
        DFGValNodeName = new ArrayList<>();
        DFGValNodeOut = new ArrayList<>();
        DFGValNodeOutputOperand = new ArrayList<>();

        MRRGFunctionName = new ArrayList<>();
        MRRGFunctionFanin = new ArrayList<>();
        MRRGFunctionFaninType = new ArrayList<>();
        MRRGFunctionFanout = new ArrayList<>();
        MRRGFunctionFanoutType = new ArrayList<>();
        MRRGFunctionSupportOpcode = new ArrayList<>();
        MRRGRoutingName = new ArrayList<>();
        MRRGRoutingFanin = new ArrayList<>();
        MRRGRoutingFaninType = new ArrayList<>();
        MRRGRoutingFanout = new ArrayList<>();
        MRRGRoutingFanoutType = new ArrayList<>();

        MRRGLatency = new HashMap<>();
        DFGValB2opMap = new HashMap<>();
        DFGCommutativeSet = new HashSet<>();
        connectList = new ArrayList<>();
        DFGMultipleInputMap = new HashMap<>();
        DFGSelfJoinMap = new HashMap<>();
        waitSkewMap = new HashMap<>();
        DFGRelativeSkewMap = new HashMap<>();
        DFGLatencyMap = new HashMap<>();
        MRRGDistance = new HashMap<>();
        MRRGNeighboringNode = new HashMap<>();
        regMap = new HashMap<>();
        regConnect = new HashMap<>();
        func2regMap = new HashMap<>();
        reg2funcMap = new HashMap<>();
        funcDirect2funcMap = new HashMap<>();
        DFGCommutatedSet = new HashSet();
        DFGOpOperand = new HashMap<>();

    }

    /**
     * Build class from txt files, only used in debugging.
     *
     * @param DFGFile  the file name of txt file containing DFG(IR)
     * @param MRRGFile the file name of txt file containing MRRG
     * @throws IOException
     */
    gurobiMapJava(String DFGFile, String MRRGFile) throws IOException {
        DFGOpNodeName = new ArrayList<>();
        DFGOpNodeOpcode = new ArrayList<>();
        DFGOpNodeOut = new ArrayList<>();
        DFGValNodeName = new ArrayList<>();
        DFGValNodeOut = new ArrayList<>();
        DFGValNodeOutputOperand = new ArrayList<>();

        MRRGFunctionName = new ArrayList<>();
        MRRGFunctionFanin = new ArrayList<>();
        MRRGFunctionFaninType = new ArrayList<>();
        MRRGFunctionFanout = new ArrayList<>();
        MRRGFunctionFanoutType = new ArrayList<>();
        MRRGFunctionSupportOpcode = new ArrayList<>();
        MRRGRoutingName = new ArrayList<>();
        MRRGRoutingFanin = new ArrayList<>();
        MRRGRoutingFaninType = new ArrayList<>();
        MRRGRoutingFanout = new ArrayList<>();
        MRRGRoutingFanoutType = new ArrayList<>();

        BufferedReader DFGReader = new BufferedReader(new FileReader(DFGFile));
        int opnum = Integer.parseInt(DFGReader.readLine());
        for (int i = 0; i < opnum; i++) {
            DFGOpNodeName.add(DFGReader.readLine());
            DFGOpNodeOut.add(Integer.valueOf(DFGReader.readLine()));
            DFGOpNodeOpcode.add(Integer.valueOf(DFGReader.readLine()));
        }
        int valnum = Integer.parseInt(DFGReader.readLine());
        for (int i = 0; i < valnum; i++) {
            DFGValNodeName.add(DFGReader.readLine());
            int outsize = Integer.parseInt(DFGReader.readLine());
            List<Integer> valout = new ArrayList<>();
            for (int j = 0; j < outsize; j++)
                valout.add(Integer.valueOf(DFGReader.readLine()));
            DFGValNodeOut.add(valout);
            List<Integer> valoperand = new ArrayList<>();
            for (int j = 0; j < outsize; j++)
                valoperand.add(Integer.valueOf(DFGReader.readLine()));
            DFGValNodeOutputOperand.add(valoperand);
        }
        DFGReader.close();

        BufferedReader MRRGReader = new BufferedReader(new FileReader(MRRGFile));

        int fnum = Integer.parseInt(MRRGReader.readLine());
        for (int i = 0; i < fnum; i++) {
            MRRGFunctionName.add(MRRGReader.readLine());
            int faninsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanin = new ArrayList<>();
            List<Integer> fanintype = new ArrayList<>();
            for (int j = 0; j < faninsize; j++) {
                fanin.add(Integer.valueOf(MRRGReader.readLine()));
                fanintype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGFunctionFanin.add(fanin);
            MRRGFunctionFaninType.add(fanintype);

            int fanoutsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanout = new ArrayList<>();
            List<Integer> fanouttype = new ArrayList<>();
            for (int j = 0; j < fanoutsize; j++) {
                fanout.add(Integer.valueOf(MRRGReader.readLine()));
                fanouttype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGFunctionFanout.add(fanout);
            MRRGFunctionFanoutType.add(fanouttype);

            int sopsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> sop = new ArrayList<>();
            for (int j = 0; j < sopsize; j++) {
                sop.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGFunctionSupportOpcode.add(sop);
        }

        int rnum = Integer.parseInt(MRRGReader.readLine());
        for (int i = 0; i < rnum; i++) {
            MRRGRoutingName.add(MRRGReader.readLine());
            int faninsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanin = new ArrayList<>();
            List<Integer> fanintype = new ArrayList<>();
            for (int j = 0; j < faninsize; j++) {
                fanin.add(Integer.valueOf(MRRGReader.readLine()));
                fanintype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGRoutingFanin.add(fanin);
            MRRGRoutingFaninType.add(fanintype);

            int fanoutsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanout = new ArrayList<>();
            List<Integer> fanouttype = new ArrayList<>();
            for (int j = 0; j < fanoutsize; j++) {
                fanout.add(Integer.valueOf(MRRGReader.readLine()));
                fanouttype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGRoutingFanout.add(fanout);
            MRRGRoutingFanoutType.add(fanouttype);
        }
        MRRGReader.close();

        System.out.println(opnum);
        System.out.println(valnum);
        System.out.println(fnum);
        System.out.println(rnum);
    }

    /**
     * Get where R_i,j stores in GRBVar[] R.
     *
     * @param valIndex     the index of valNode
     * @param routingIndex the index of routingNode
     */
    int RIndex(int valIndex, int routingIndex) {
        return valIndex * MRRGRoutingName.size() + routingIndex;
    }

    /**
     * Get where F_p,q stores in GRBVar[] F.
     *
     * @param opIndex       the index of opNode
     * @param functionIndex the index of functionalNode
     */
    int FIndex(int opIndex, int functionIndex) {
        return opIndex * MRRGFunctionName.size() + functionIndex;
    }

    /**
     * Do ILP mapping and return the mapping result
     *
     * @param separatedPR     a parameter indicating whether ILP placement and routing should be separated
     * @param scheduleControl a parameter indicating whether the latency and skew should be controlled and obtained in ILP
     */
    List<Integer>[] ILPMap(Boolean separatedPR, Boolean scheduleControl) throws GRBException, IOException {
        startTime = System.currentTimeMillis();
        GRBModel[] models = getILPModel(separatedPR, scheduleControl);
        GRBModel modelP = models[0];
        GRBModel modelR = models[1];
        result = "fail\t\t" + iterationNum + "\t\t" + ringCheckCount;

//        int statusP = modelP.get(GRB.IntAttr.Status);
        int solcntP = modelP.get(GRB.IntAttr.SolCount);
        if (solcntP == 0) {
            return null;
        }

        GRBVar[] Vars = modelR.getVars();
        GRBVar[] R = new GRBVar[countR];
        System.arraycopy(Vars, 0, R, 0, countR);
        GRBVar[] F = new GRBVar[countF];
        int delayOffset = countR + countF;
        if (separatedPR) {
            System.arraycopy(modelP.getVars(), 0, F, 0, countF);
            delayOffset = countR;
        } else {
            System.arraycopy(Vars, countR, F, 0, countF);
        }
        GRBVar[] Delay = new GRBVar[connectSize];
        if (scheduleControl) {
            System.arraycopy(Vars, delayOffset, Delay, 0, connectSize);
        }

//        int status = modelR.get(GRB.IntAttr.Status);
        int solcntR = modelR.get(GRB.IntAttr.SolCount);


//        if (status == GRB.OPTIMAL || status == GRB.SUBOPTIMAL || status == GRB.SOLUTION_LIMIT) {
        if (solcntR != 0) {

            if (ringCheckPass) {
                result = "success\t\t" + iterationNum + "\t\t" + ringCheckCount;
            }
            int[] r_mapped = new int[numMrrgR];
            int[] f_mapped = new int[numMrrgF];
            int[] r_result = new int[numMrrgR];
            int[] f_result = new int[numMrrgF];

            for (int r = 0; r < numMrrgR; r++) {
                r_mapped[r] = 0;
                r_result[r] = -1;
            }
            for (int f = 0; f < numMrrgF; f++) {
                f_mapped[f] = 0;
                f_result[f] = -1;
            }

            if (scheduleControl) {
                for (int i = 0; i < numDfgOps; i++) {
                    DFGLatencyMap.put(DFGOpNodeName.get(i),
                            (int) modelR.getVarByName("Latency_" + i).get(GRB.DoubleAttr.X));
                }
                if (useRelativeSkew) {
                    for (String key : DFGMultipleInputMap.keySet()) {
                        DFGRelativeSkewMap.put(key,
                                (int) modelR.getVarByName("Skew_" + key).get(GRB.DoubleAttr.X));
                    }
                }
                for (String key : waitSkewMap.keySet()) {
                    waitSkewMap.replace(key, (int) modelR.getVarByName(key).get(GRB.DoubleAttr.X));
                }

            }

            /****Debug****/
            if (scheduleControl) {
                for (int i = 0; i < connectSize; i++) {
                    System.out.println("Delay " + connectList.get(i).get(0) + " -> " +
                            connectList.get(i).get(1) + ": " + modelR.getVarByName("Delay_" + i).get(GRB.DoubleAttr.X));
                }

                for (int i = 0; i < numDfgOps; i++) {
                    System.out.println("Latency " + DFGOpNodeName.get(i) + ": " +
                            modelR.getVarByName("Latency_" + i).get(GRB.DoubleAttr.X));
                }

                if (useRelativeSkew) {
                    for (String key : DFGMultipleInputMap.keySet()) {
                        System.out.println("Skew " + key + ": " +
                                modelR.getVarByName("Skew_" + key).get(GRB.DoubleAttr.X));
                    }
                }

                for (String key : waitSkewMap.keySet()) {
                    System.out.println("waitSkew " + key + ": " +
                            modelR.getVarByName(key).get(GRB.DoubleAttr.X));
                }

//                for (int r = 0; r < numMrrgR; r++) {
//                    if (modelR.getVarByName("R_" + r + "_22").get(GRB.DoubleAttr.X) > 0) {
//                        System.out.println("R_" + r + "_22" + ": " + MRRGRoutingName.get(r) + " " + DFGValNodeName.get(22));
//                    }
//                }
            }

//
//            for (int f = 0; f < num_mrrg_f; f++) {
//                if (model.getVarByName("F_3_" + f).get(GRB.DoubleAttr.X) > 0) {
//                    if (MRRGlatency.containsKey(MRRGfunctionname.get(f))) {
//                        System.out.println("F_3_" + f + ": " + MRRGfunctionname.get(f) + " " + DFGopnodename.get(3));
//                    }
//                }
//            }
            /****Debug****/

            Map<Integer, Integer> mappedOp2MrrgMap = new HashMap();

            for (int val = 0; val < numDfgVals; val++) {
                for (int r = 0; r < numMrrgR; r++)
                    if (abs(R[RIndex(val, r)].get(GRB.DoubleAttr.X) - 1.0) < 0.01) {
                        String name = MRRGRoutingName.get(r);
                        if (name.contains("alu") && name.contains("internalNode")) {
                            System.out.println("Routing ALU: " + MRRGRoutingName.get(r) + " " + DFGValNodeName.get(val)
                                    + " " + R[RIndex(val, r)].get(GRB.DoubleAttr.X));
                            usedBypassALU += 1;
                        }
                        r_mapped[r] = 1;
                        r_result[r] = val;
                    }
            }
            if (filename == null) {
                Date now = new Date();
                SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
                filename = "internalNodeinfo" + format.format(now);
            }
            FileWriter resultFile = new FileWriter(filename + "_r.txt");
            for (int op = 0; op < numDfgOps; op++)
                for (int f = 0; f < numMrrgF; f++) {
//                    if(op == 2){
//                        System.out.println(DFGOpNodeName.get(op) + " " +  f + " " + MRRGFunctionName.get(f) +
//                                " " + F[FIndex(op, f)].get(GRB.DoubleAttr.X));
//                    }
                    if (abs(F[FIndex(op, f)].get(GRB.DoubleAttr.X) - 1.0) < 0.01) {
                        String name = MRRGFunctionName.get(f);
                        String opName = DFGOpNodeName.get(op);
                        if(DFGMultipleInputMap.containsKey(opName)) {
                            int routingNodeOperand0 = MRRGFunctionFanin.get(f).get(0);
                            int valNodeOperand1 = DFGOpOperand.get(op).get(1);
                            if (abs(1.0 - modelR.getVarByName("R_" + routingNodeOperand0 + "_" + valNodeOperand1).get(GRB.DoubleAttr.X)) < 0.01) {
                                DFGCommutatedSet.add(opName);
                            }
                        }

                        if (name.contains("alu") && name.contains("internalNode")) {
                            System.out.println("Func ALU: " + MRRGFunctionName.get(f) + " " + F[FIndex(op, f)].get(GRB.DoubleAttr.X));
                            usedFuncALU += 1;
                        }
                        System.out.printf("%s->%s\n", DFGOpNodeName.get(op), MRRGFunctionName.get(f));
                        resultFile.write(DFGOpNodeName.get(op) + " " + MRRGFunctionName.get(f) + "\n");
                        f_mapped[f] = DFGOpNodeOpcode.get(op).intValue() + 1;
                        f_result[f] = op;
                        mappedOp2MrrgMap.put(op, f);
//                        int mrrgOperand0 = MRRGFunctionFanin.get(f).get(0);
//                        int dfgOperand0 = DFGMultipleInputMap.get(DFGOpNodeName.get(op)).get(0);
//                        if(abs(modelR.getVarByName("R_"+).get(GRB.DoubleAttr.X) - 1.0) < 0.01)
                    }
                }
            resultFile.flush();
            resultFile.close();


            FileWriter infoFile = new FileWriter(filename + "_i.txt");
            for (int r = 0; r < numMrrgR; r++)
                if (r_mapped[r] == 1 && MRRGRoutingName.get(r).indexOf("internalNode") != -1) {
                    List<Integer> fanin = new ArrayList<>();
                    List<Integer> fanout = new ArrayList<>();
                    for (int i = 0; i < MRRGRoutingFanin.get(r).size(); i++) {
                        if (r_mapped[MRRGRoutingFanin.get(r).get(i)] == 1 &&
                                r_result[MRRGRoutingFanin.get(r).get(i)] == r_result[r])
                            fanin.add(Integer.valueOf(i));
                    }
                    for (int i = 0; i < MRRGRoutingFanout.get(r).size(); i++) {
                        if (r_mapped[MRRGRoutingFanout.get(r).get(i)] == 1 &&
                                r_result[MRRGRoutingFanout.get(r).get(i)] == r_result[r])
                            fanout.add(Integer.valueOf(i));
                    }
                    if (fanin.size() > 1) {
                        System.out.println("   " + MRRGRoutingName.get(r) + "<-" + DFGValNodeName.get(r_mapped[r]));
                    }
                    if (fanin.size() > 0 && fanout.size() > 0) {
                        infoFile.write("<" + MRRGRoutingName.get(r) + ">\n");
                        for (int i = 0; i < fanin.size(); i++)
                            infoFile.write(fanin.get(i).toString() + " ");
                        infoFile.write("\n");
                        for (int i = 0; i < fanout.size(); i++)
                            infoFile.write(fanout.get(i).toString() + " ");
                        infoFile.write("\n");
                    }
                }
            for (int f = 0; f < numMrrgF; f++) {
                if (f_mapped[f] > 0 && MRRGFunctionName.get(f).indexOf("internalNode") != -1) {
                    infoFile.write("<" + MRRGFunctionName.get(f) + ">\nSELECTED_OP\n");
                    infoFile.write("" + (f_mapped[f] - 1) + "\n");
                }
            }
            infoFile.flush();
            infoFile.close();

            if(ringCheckPass) {
                int regCount = 0;
                for (int val = 0; val < numDfgVals; val++) {
                    for (int fanout = 0; fanout < DFGValNodeOut.get(val).size(); fanout++) {
                        Map<Integer, List<Integer>> graph = getGraph(modelR, val, fanout);
                        int op = DFGValB2opMap.get(DFGValNodeName.get(val));
//                    System.out.println(val + " " + DFGValNodeName.get(val) + " " + op + " " + DFGOpNodeName.get(op));
                        int mappedMrrgNode = mappedOp2MrrgMap.get(op);
                        List<Integer> functionFanout = MRRGFunctionFanout.get(mappedMrrgNode);
                        List<Integer> roots = new ArrayList<>();
                        for (Integer root : functionFanout) {
                            if (graph.containsKey(root)) {
                                roots.add(root);
                            }
                        }
                        if (roots.size() == 1) {
                            int root = roots.get(0);
                            int currentNode = root;
                            int previousReg = -1;
                            while (graph.get(currentNode).size() != 0) {
                                if (MRRGLatency.containsKey(MRRGRoutingName.get(currentNode))) {
                                    if (!regMap.containsKey(currentNode)) {
                                        regMap.put(currentNode, regCount++);
                                    }
                                    if (previousReg == -1) {
                                        if (func2regMap.containsKey(op)) {
                                            if (!func2regMap.get(op).contains(regMap.get(currentNode))) {
                                                func2regMap.get(op).add(regMap.get(currentNode));
                                            }
                                        } else {
                                            List<Integer> regList = new ArrayList<>();
                                            regList.add(regMap.get(currentNode));
                                            func2regMap.put(op, regList);
                                        }
                                    } else {
                                        if (regConnect.containsKey(previousReg)) {
                                            regConnect.get(previousReg).add(regMap.get(currentNode));
                                        } else {
                                            List<Integer> regList = new ArrayList<>();
                                            regList.add(regMap.get(currentNode));
                                            regConnect.put(previousReg, regList);
                                        }
                                    }
                                    previousReg = regMap.get(currentNode);
                                }
                                if (graph.get(currentNode).size() == 1) {
                                    currentNode = graph.get(currentNode).get(0);

                                } else {
                                    System.out.println("\033[31;4m" + "Fanout is not a chain." + "\033[0m");
                                }
                            }
                            int outOp = DFGValNodeOut.get(val).get(fanout);
                            int outOpOperand = DFGValNodeOutputOperand.get(val).get(fanout);
                            List<Integer> outOpPair = new LinkedList<>();
                            outOpPair.add(outOp);
                            outOpPair.add(outOpOperand);
                            if (previousReg == -1) {
                                if (funcDirect2funcMap.containsKey(op)) {
                                    funcDirect2funcMap.get(op).add(outOpPair);
                                } else {
                                    List<List<Integer>> funcList = new ArrayList<>();
                                    funcList.add(outOpPair);
                                    funcDirect2funcMap.put(op, funcList);
                                }
                            } else {
                                if (reg2funcMap.containsKey(previousReg)) {
                                    reg2funcMap.get(previousReg).add(outOpPair);
                                } else {
                                    List<List<Integer>> funcList = new ArrayList<>();
                                    funcList.add(outOpPair);
                                    reg2funcMap.put(previousReg, funcList);
                                }
                            }
                        } else {
                            System.out.println("\033[31;4m" + "Find root fail." + "\033[0m");
                        }
                    }
                }
            }

            List<Integer>[] result = new List[2];
            result[0] = new ArrayList<>();
            result[1] = new ArrayList<>();
            for (int r = 0; r < numMrrgR; r++)
                result[0].add(Integer.valueOf(r_result[r]));
            for (int f = 0; f < numMrrgF; f++)
                result[1].add(Integer.valueOf(f_result[f]));
            return result;
        }
        return null;

    }

    /**
     * Do ILP mapping and write num of variables and constraints into a file, return the runtime of mapping.
     */
    Double ILPMap(FileWriter fw) throws GRBException, IOException {
        GRBModel model = getILPModel(false, false)[0];

        int status = model.get(GRB.IntAttr.Status);

        if (status == GRB.OPTIMAL || status == GRB.SUBOPTIMAL || status == GRB.SOLUTION_LIMIT) {
            int vars = model.get(GRB.IntAttr.NumVars), constrs = model.get(GRB.IntAttr.NumConstrs);
            fw.write("Vars : " + vars + " Constrs : " + constrs + " ");
            return model.get(GRB.DoubleAttr.Runtime);
        }
        return -1.0;

    }

    /**
     * Get the mapping result of a valNode as a directed graph.
     */
    Map<Integer, List<Integer>> getGraph(GRBModel model, Integer val, Integer fanout) throws GRBException {
        Set<Integer> mappedRoutingNodes = new HashSet<>();
        int fanouts = DFGValNodeOut.get(val).size();
//        System.out.println(DFGValNodeName.get(val) + " " + DFGOpNodeName.get(DFGValNodeOut.get(val).get(fanout)));
        for (int r = 0; r < numMrrgR; r++) {
            String valueName = "R_" + r + "_" + val;
            if (fanouts > 1) {
                valueName = "R_" + r + "_" + val + "_" + fanout;
            }
            if (model.getVarByName(valueName).get(GRB.DoubleAttr.X) == 1) {
//                System.out.println(r + " " + MRRGRoutingName.get(r));
                mappedRoutingNodes.add(r);
            }
        }
        Map<Integer, List<Integer>> graph = new HashMap<>();
        for (Integer mappedRoutingNode : mappedRoutingNodes) {
            List<Integer> mappedOutNodes = new ArrayList<>();
            List<Integer> outNodes = MRRGRoutingFanout.get(mappedRoutingNode);
            for (int i = 0; i < outNodes.size(); i++) {
                if (MRRGRoutingFanoutType.get(mappedRoutingNode).get(i) == 0) {
                    int outNode = outNodes.get(i);
                    if (mappedRoutingNodes.contains(outNode)) {
                        mappedOutNodes.add(outNode);
                    }
                }
            }
            graph.put(mappedRoutingNode, mappedOutNodes);
        }
        return graph;
    }

    /**
     * Check whether the mapping results contain useless rings.
     */
    Boolean checkRoutingWithoutUselessRing(GRBModel model) throws GRBException {
        List<Map<Integer, List<Integer>>> testedGraphs = new ArrayList<>();
        for (int val = 0; val < numDfgVals; val++) {
            int fanouts = DFGValNodeOut.get(val).size();
            for (int fanout = 0; fanout < fanouts; fanout++) {
                Map<Integer, List<Integer>> graph = getGraph(model, val, fanout);
                testedGraphs.add(graph);
            }
        }
        return new checkWithoutRing(testedGraphs).check();
    }

    /**
     * Constraint: Routing Resource Usage.
     */
    void constrRoutingResource(GRBModel model, GRBVar[] R, List<GRBVar[]> S) throws GRBException {
        int constrcount = 0;
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                for (int k = 0; k < S.get(RIndex(val, r)).length; k++) {
                    model.addConstr(R[RIndex(val, r)], GRB.GREATER_EQUAL, S.get(RIndex(val, r))[k],
                            "sub_val" + (constrcount++));
                }
            }
    }

    /**
     * Constraint: Legality of Delay and WaitSkew.
     */
    <T> void constrDelay(GRBModel model, List<GRBVar[]> S, GRBVar[] Delays,
                         GRBVar[] Latencies, GRBVar[] WaitSkews, T[] F, Boolean VariableF) throws GRBException {

        int constrcount = 0;
        for (int val = 0; val < numDfgVals; val++) {
            for (int fanOut = 0; fanOut < DFGValNodeOut.get(val).size(); fanOut++) {
                GRBLinExpr constraint = new GRBLinExpr();
                String valName = DFGValNodeName.get(val);
                for (int r = 0; r < numMrrgR; r++) {
                    int coeff = 0;
                    if (MRRGLatency.containsKey(MRRGRoutingName.get(r))) {
                        coeff = MRRGLatency.get(MRRGRoutingName.get(r));
                    }
                    constraint.addTerm(coeff, S.get(RIndex(val, r))[fanOut]);
                }
//                if (DFGValNodeOut.get(val).size() > 1) {
//                    for (int r = 0; r < numMrrgR; r++) {
//                        int coeff = 0;
//                        if (MRRGLatency.containsKey(MRRGRoutingName.get(r))) {
//                            coeff = MRRGLatency.get(MRRGRoutingName.get(r));
//                        }
//                        constraint.addTerm(coeff, S.get(RIndex(val, r))[fanOut]);
//                    }
//                } else {
//                    for (int r = 0; r < numMrrgR; r++) {
//                        int coeff = 0;
//                        if (MRRGLatency.containsKey(MRRGRoutingName.get(r))) {
//                            coeff = MRRGLatency.get(MRRGRoutingName.get(r));
//                        }
//                        constraint.addTerm(coeff, R[RIndex(val, r)]);
//                    }
//                }
                for (int f = 0; f < numMrrgF; f++) {
                    int coeff = 0;
                    if (MRRGLatency.containsKey(MRRGFunctionName.get(f))) {
                        coeff = MRRGLatency.get(MRRGFunctionName.get(f));
                    }
                    if (VariableF) {
                        constraint.addTerm(coeff, (GRBVar) F[FIndex(DFGValB2opMap.get(valName), f)]);
                    } else {
                        constraint.addConstant(coeff * Integer.class.cast(F[FIndex(DFGValB2opMap.get(valName), f)]));
                    }

                }

                Delays[constrcount].set(GRB.StringAttr.VarName, "Delay_" + constrcount);

                model.addConstr(constraint, GRB.EQUAL, Delays[constrcount], "delay_" + (constrcount));

                /** Constraint delay and wait skew
                 */

                int sourceID = DFGValB2opMap.get(valName);
                int sinkID = DFGValNodeOut.get(val).get(fanOut);

                String sourceName = DFGOpNodeName.get(sourceID);
                String sinkName = DFGOpNodeName.get(sinkID);
                GRBLinExpr waitSkewConstraint = new GRBLinExpr();
                waitSkewConstraint.addTerm(1, Latencies[sourceID]);
                waitSkewConstraint.addTerm(1, Delays[constrcount]);
                waitSkewConstraint.addTerm(1, WaitSkews[constrcount]);

                if (DFGMultipleInputMap.containsKey(sinkName)) {

                    WaitSkews[constrcount].set(GRB.StringAttr.VarName, "WaitSkew_" + sourceName + "_" + sinkName);
                    waitSkewMap.put("WaitSkew_" + sourceName + "_" + sinkName, constrcount);
                    if (!useRelativeSkew) {
                        if (sourceID == sinkID) {
                            WaitSkews[constrcount].set(GRB.DoubleAttr.UB, skewLimit + II);
                        } else {
                            WaitSkews[constrcount].set(GRB.DoubleAttr.UB, skewLimit);
                        }
                    }

                    if (sourceID == sinkID) {
//                        GRBLinExpr ringConstraint = new GRBLinExpr();
//                        ringConstraint.addTerm(1.0, WaitSkews[constrcount]);
//                        ringConstraint.addConstant(II);
                        model.addConstr(WaitSkews[constrcount], GRB.EQUAL, Delays[constrcount], "waitSkew_" + constrcount);
                    } else {
                        model.addConstr(waitSkewConstraint, GRB.EQUAL, Latencies[sinkID], "waitSkew_" + constrcount);
                    }
                } else {
                    model.addConstr(waitSkewConstraint, GRB.EQUAL, Latencies[sinkID], "waitSkew_" + constrcount);
                    model.addConstr(WaitSkews[constrcount], GRB.EQUAL, 0, "singleInputWaitSkew_" + constrcount);
                }
                constrcount++;
            }
        }
    }

    /**
     * Set Latency Range.
     */
    void setLatencyRange(GRBVar[] Latencies, int maxLatency) throws GRBException {
        int minLatency = 0;
        for (int op = 0; op < numDfgOps; op++) {
            Latencies[op].set(GRB.StringAttr.VarName, "Latency_" + op);
            Latencies[op].set(GRB.DoubleAttr.LB, minLatency);
            Latencies[op].set(GRB.DoubleAttr.UB, maxLatency);
        }
    }

    /**
     * Constraint: Relative Skew Limit.
     */
    void constrRelativeSkew(GRBModel model, GRBVar[] RelativeSkews, GRBVar[] WaitSkews,
                            GRBVar[] SkewDirection, int skewLimit) throws GRBException {
        int constrcount = 0;
        for (String key : DFGMultipleInputMap.keySet()) {
            String sinkNodeName = key;
//            int sinkID = DFGopnodename.indexOf(sinkNodeName);
            RelativeSkews[constrcount].set(GRB.StringAttr.VarName, "Skew_" + sinkNodeName);
            RelativeSkews[constrcount].set(GRB.DoubleAttr.LB, -skewLimit);
            RelativeSkews[constrcount].set(GRB.DoubleAttr.UB, skewLimit);
//            GRBLinExpr skewConstraint = new GRBLinExpr();
//            skewConstraint.addConstant(skewLimit);
//            model.addConstr(skewConstraint, GRB.GREATER_EQUAL,
//                    RelativeSkews[constrcount], "maxSkew_" + constrcount);

//            GRBLinExpr minSkewConstraint = new GRBLinExpr();
//            minSkewConstraint.addConstant(-skewLimit);
//            model.addConstr(minSkewConstraint, GRB.LESS_EQUAL,
//                    RelativeSkews[constrcount], "minSkew_" + constrcount);

            List<String> sourceNodeNames = DFGMultipleInputMap.get(key);

            if (DFGSelfJoinMap.containsKey(sinkNodeName)) {
                int selfPort = 0;
                for (int j = 0; j < sourceNodeNames.size(); j++) {
                    if (sourceNodeNames.get(j) == sinkNodeName) {
                        selfPort = j;
                    }
                }
                int anotherPort = 1 - selfPort;
                int direction = 1;
                if (selfPort == 1) {
                    direction = -1;
                }

                GRBVar selfWaitSkew = WaitSkews[waitSkewMap
                        .get("WaitSkew_" + sinkNodeName + "_" + sinkNodeName)];
                GRBVar anotherWaitSkew = WaitSkews[waitSkewMap
                        .get("WaitSkew_" + sourceNodeNames.get(anotherPort) + "_" + sinkNodeName)];

                GRBLinExpr selfJoinConstraint = new GRBLinExpr();
                selfJoinConstraint.addTerm(direction, selfWaitSkew);
                selfJoinConstraint.addConstant(-II * direction);
                model.addConstr(selfJoinConstraint, GRB.EQUAL,
                        RelativeSkews[constrcount], "relativeSkew_" + constrcount);

                model.addConstr(anotherWaitSkew, GRB.EQUAL, 0, "anotherWaitSkew_" + constrcount);
            } else {

                SkewDirection[constrcount].set(GRB.StringAttr.VarName, "SkewDirection_" + sinkNodeName);
                int sourceNum = sourceNodeNames.size();
                if (sourceNum == 2) {
                    GRBLinExpr relativeConstraint = new GRBLinExpr();
                    GRBVar waitSkew0 = WaitSkews[waitSkewMap
                            .get("WaitSkew_" + sourceNodeNames.get(0) + "_" + sinkNodeName)];
                    GRBVar waitSkew1 = WaitSkews[waitSkewMap
                            .get("WaitSkew_" + sourceNodeNames.get(1) + "_" + sinkNodeName)];
                    relativeConstraint.addTerm(-1, waitSkew0);
                    relativeConstraint.addTerm(1, waitSkew1);

                    model.addConstr(relativeConstraint, GRB.EQUAL,
                            RelativeSkews[constrcount], "relativeSkew_" + constrcount);

                    GRBLinExpr directionConstraint0 = new GRBLinExpr();
                    directionConstraint0.addTerm(skewLimit, SkewDirection[constrcount]);
                    model.addConstr(directionConstraint0, GRB.GREATER_EQUAL,
                            waitSkew0, "direction0_" + constrcount);

                    GRBLinExpr directionConstraint1 = new GRBLinExpr();
                    directionConstraint1.addTerm(-skewLimit, SkewDirection[constrcount]);
                    directionConstraint1.addConstant(skewLimit);
                    model.addConstr(directionConstraint1, GRB.GREATER_EQUAL,
                            waitSkew1, "direction1_" + constrcount);
                }
            }
            constrcount++;
        }
    }

    /**
     * Set objective for routing.
     */
    void setRoutingObjective(GRBModel model, GRBVar[] R) throws GRBException {
        GRBLinExpr objective = new GRBLinExpr();
        double[] coeffs = new double[countR];
        for (int i = 0; i < countR; i++) coeffs[i] = 1.0;

        objective.addTerms(coeffs, R);
        model.setObjective(objective, GRB.MINIMIZE);
    }

    /**
     * Set objective for placement.
     */
    void setPlacementObjective(GRBModel model, GRBVar[] F, boolean routingDriven) throws GRBException {
        GRBLinExpr objective = new GRBLinExpr();
        for (int pSource = 0; pSource < numMrrgF; pSource++) {
            for (int qSource = 0; qSource < numDfgOps; qSource++) {
                String sourceDFGName = DFGOpNodeName.get(qSource);
                for (int pSink = 0; pSink < numMrrgF; pSink++) {
                    for (int qSink = 0; qSink < numDfgOps; qSink++) {
                        String sinkDFGName = DFGOpNodeName.get(qSink);
                        List<String> namePair = new LinkedList<String>() {{
                            add(sourceDFGName);
                            add(sinkDFGName);
                        }};
                        if (connectList.contains(namePair) &&
                                MRRGFunctionSupportOpcode.get(pSource).contains(DFGOpNodeOpcode.get(qSource)) &&
                                MRRGFunctionSupportOpcode.get(pSink).contains(DFGOpNodeOpcode.get(qSink))) {
                            String varName = "concurrentF_" + pSource + "_" + qSource + "_" + pSink + "_" + qSink;
                            GRBVar concurrentF = model.addVar(0, 1, 1, 'B', varName);
                            String sourceMRRGName = MRRGFunctionName.get(pSource);
                            String sinkMRRGName = MRRGFunctionName.get(pSink);
                            int distence = MRRGDistance.get(new LinkedList<String>() {{
                                add(sourceMRRGName);
                                add(sinkMRRGName);
                            }});
                            GRBVar[] tempF = {F[FIndex(qSource, pSource)], F[FIndex(qSink, pSink)]};
                            model.addGenConstrAnd(concurrentF, tempF,
                                    "and_" + varName);
                            if (routingDriven && distence < neighboringDistance) {
                                objective.addTerm(MRRGNeighboringNode.get(sourceMRRGName).size()
                                                * iterationNum / 80.0
                                        , concurrentF);
                            }
                            objective.addTerm(distence, concurrentF);
                        }

                    }
                }

            }
        }

//        if (routingDriven) {
//            for (int p = 0; p < numMrrgF; p++) {
//                for (int q = 0; q < numDfgOps; q++){
//                    objective.addTerm(MRRGNeighboringNode.get(MRRGFunctionName.get(p)).size()
//                                    * iterationNum / 20.0
//                            , F[FIndex(q, p)]);
//                }
//            }
//        }

        model.setObjective(objective, GRB.MINIMIZE);
    }

    /**
     * Constraint: Routing Exclusivity.
     */
    void constrRoutingExclusivity(GRBModel model, GRBVar[] R) throws GRBException {
        int constrcount = 0;
        for (int r = 0; r < numMrrgR; r++) {
            GRBLinExpr constraint = new GRBLinExpr();
            for (int val = 0; val < numDfgVals; val++)
                constraint.addTerm(1.0, R[RIndex(val, r)]);
            model.addConstr(constraint, GRB.LESS_EQUAL, 1, "route_exclusivity_" + (constrcount++));
        }
    }

    /**
     * Constraint: Functional Unit Exclusivity.
     */
    void constrFunctionExclusivity(GRBModel model, GRBVar[] F) throws GRBException {
        int constrcount = 0;
        for (int p = 0; p < numMrrgF; p++) {
            GRBLinExpr constraint = new GRBLinExpr();
            for (int q = 0; q < numDfgOps; q++)
                constraint.addTerm(1.0, F[FIndex(q, p)]);
            model.addConstr(constraint, GRB.LESS_EQUAL, 1, "function_unit_exclusivity_" + (constrcount++));
        }
    }

    /**
     * Constraint: All Operation Placement.
     */
    void constrOperationPlacement(GRBModel model, GRBVar[] F) throws GRBException {
        int constrcount = 0;
        for (int q = 0; q < numDfgOps; q++) {
            GRBLinExpr constraint = new GRBLinExpr();
            for (int p = 0; p < numMrrgF; p++)
                constraint.addTerm(1.0, F[FIndex(q, p)]);
            model.addConstr(constraint, GRB.EQUAL, 1, "ensure_all_ops_mapped_" + (constrcount++));
        }
    }

//    /**
//     * Constraint: Acyclic Routing.
//     */
//    void constrAcyclic(GRBModel model, GRBVar[] R) throws GRBException {
//        int MRRG_NODE_ROUTING = 0;
//        for (int val = 0; val < numDfgVals; val++) {
//            GRBLinExpr sum_of_edges = new GRBLinExpr();
//            GRBLinExpr sum_of_nodes = new GRBLinExpr();
//            for (int r = 0; r < numMrrgR; r++) {
//                sum_of_nodes.addTerm(1.0, R[RIndex(val, r)]);
//
//                List<Integer> fanins = MRRGRoutingFanin.get(r);
//                List<Integer> faninTypes = MRRGRoutingFaninType.get(r);
//                for (int in = 0; in < fanins.size(); in++) {
//                    if (faninTypes.get(in) == MRRG_NODE_ROUTING) {
//                        GRBVar edgeIn = model.addVar(0.0, 1.0, 0.0, 'B',
//                                "edgeIn_" + r + "_" + val + "_" + in);
//                        GRBVar[] tempNodes = {R[RIndex(val, r)], R[RIndex(val, fanins.get(in))]};
//                        model.addGenConstrAnd(edgeIn, tempNodes, "constrEdgeIn" + r + "_" + val + "_" + in);
//                        sum_of_edges.addTerm(1.0, edgeIn);
//                    }
//                }
//
////                List<Integer> fanouts = MRRGRoutingFanout.get(r);
////                List<Integer> fanoutTypes = MRRGFunctionFanoutType.get(r);
////                for(int out=0;out<fanins.size();out++){
////                    if(fanoutTypes.get(out)== MRRG_NODE_ROUTING) {
////                        GRBVar edgeOut = model.addVar(1.0, 0.0,0.0,'B',
////                                "edgeOut_" + r + "_" + val + "_" + out);
////                        GRBVar[] tempNodes = {R[RIndex(val, r)], R[RIndex(val,fanouts.get(out))]};
////                        model.addGenConstrAnd(edgeOut, tempNodes, "constrEdgeOut" + r + "_" + val + "_" + out);
////                        sum_of_edges.addTerm(1.0, edgeOut);
////                    }
////                }
//            }
//            sum_of_nodes.addConstant(1);
//            model.addConstr(sum_of_edges, GRB.EQUAL, sum_of_nodes, "acyclic_routing_" + val);
//        }
//    }

    /**
     * Constraint: Fanout_Routing.
     */
    <T> void constrFanoutRouting(GRBModel model, List<GRBVar[]> S, T[] F, Boolean VariableF) throws GRBException {
        int constrcount = 0;
        int MRRG_NODE_ROUTING = 0;
        int MRRG_NODE_FUNCTION = 1;
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                int val_fanouts = DFGValNodeOut.get(val).size();

                for (int i = 0; i < val_fanouts; i++) {
                    GRBLinExpr sum_of_fanouts = new GRBLinExpr();
                    int fanoutsize = MRRGRoutingFanout.get(r).size();
                    for (int mrrg_fanout = 0; mrrg_fanout < fanoutsize; mrrg_fanout++) {
                        if (MRRGRoutingFanoutType.get(r).get(mrrg_fanout) == MRRG_NODE_ROUTING) {
                            sum_of_fanouts.addTerm(1.0, S.get(RIndex(val, MRRGRoutingFanout.get(r).
                                    get(mrrg_fanout)))[i]);
                        } else if (MRRGRoutingFanoutType.get(r).get(mrrg_fanout) == MRRG_NODE_FUNCTION) {
                            int op = DFGValNodeOut.get(val).get(i).intValue(), operand =
                                    DFGValNodeOutputOperand.get(val).get(i).intValue();
                            int outOpNode = MRRGRoutingFanout.get(r).get(mrrg_fanout);
                            if (MRRGFunctionFanin.get(outOpNode).size() > operand) {
                                if (DFGCommutativeSet.contains(op)
                                        || MRRGFunctionFanin.get(outOpNode).get(operand) == r) {
                                    if (VariableF) {
                                        sum_of_fanouts.addTerm(1.0,
                                                (GRBVar) F[FIndex(op, outOpNode)]);
                                    } else {
                                        sum_of_fanouts.addConstant(Integer.class.cast(F[FIndex(op,
                                                MRRGRoutingFanout.get(r).get(mrrg_fanout))]));
                                    }
                                }
                            }
                        }
                    }
                    model.addConstr(sum_of_fanouts, GRB.GREATER_EQUAL, S.get(RIndex(val, r))[i],
                            "fanout_routing_" + (constrcount++));
                }
            }

    }

    /**
     * Constraint: Multiplexer Input Exclusivity.
     */
    void constrMultiplexerExclusivity(GRBModel model, GRBVar[] R) throws GRBException {
        int constrcount = 0;
        int MRRG_NODE_ROUTING = 0;
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                GRBLinExpr sum_of_fanins = new GRBLinExpr();
                int fanin_count = MRRGRoutingFanin.get(r).size();
                if (fanin_count > 1) {
                    for (int fanin = 0; fanin < fanin_count; fanin++) {
                        if (MRRGRoutingFaninType.get(r).get(fanin) == MRRG_NODE_ROUTING)
                            sum_of_fanins.addTerm(1.0, R[RIndex(val, MRRGRoutingFanin.get(r).get(fanin))]);
                    }
                    model.addConstr(sum_of_fanins, GRB.GREATER_EQUAL, R[RIndex(val, r)],
                            "mux_exclusivity_lower_" + (constrcount++));
                    model.addConstr(sum_of_fanins, GRB.LESS_EQUAL, 1,
                            "mux_exclusivity_upper_" + (constrcount));
                }
            }
    }

    /**
     * Constraint: Initial Fanout.
     */
    <T> void constrInitialFanout(GRBModel model, List<GRBVar[]> S, T[] F, Boolean VariableF) throws GRBException {
        int constrcount = 0;
        for (int op = 0; op < numDfgOps; op++)
            for (int f = 0; f < numMrrgF; f++) {
                if (DFGOpNodeOut.get(op).intValue() != -1) {
                    int val = DFGOpNodeOut.get(op).intValue();
                    int f_fanouts = MRRGFunctionFanout.get(f).size();
                    for (int r = 0; r < f_fanouts; r++) {
                        int val_fanouts = DFGValNodeOut.get(val).size();
                        for (int i = 0; i < val_fanouts; i++)
                            if (VariableF) {
                                model.addConstr((GRBVar) F[FIndex(op, f)], GRB.EQUAL,
                                        S.get(RIndex(val, MRRGFunctionFanout.get(f).get(r)))[i],
                                        "function_unit_fanout_" + (constrcount++));
                            } else {
                                model.addConstr(Integer.class.cast(F[FIndex(op, f)]), GRB.EQUAL,
                                        S.get(RIndex(val, MRRGFunctionFanout.get(f).get(r)))[i],
                                        "function_unit_fanout_" + (constrcount++));
                            }
                    }
                }
            }
    }

    /**
     * Constraint: Functional Unit Legality.
     */
    void constrFunctionalLegality(GRBModel model, GRBVar[] F) throws GRBException {
        int constrcount = 0;
        for (int op = 0; op < numDfgOps; op++)
            for (int f = 0; f < numMrrgF; f++) {
                int flag = 0;
                for (int i = 0; i < MRRGFunctionSupportOpcode.get(f).size(); i++)
                    if (MRRGFunctionSupportOpcode.get(f).get(i) == DFGOpNodeOpcode.get(op)) {
                        flag = 1;
                        break;
                    }
                if (flag == 0)
                    model.addConstr(F[FIndex(op, f)], GRB.EQUAL, 0, "op_support_" + (constrcount++));
            }
    }

    /**
     * Core process of this class, set up the Gurobi mapping model.
     *
     * @param separatedPR     a parameter indicating whether ILP placement and routing should be separated
     * @param scheduleControl a parameter indicating whether the latency and skew should be controlled and obtained in ILP
     */
    GRBModel[] getILPModel(Boolean separatedPR, Boolean scheduleControl) throws GRBException, IOException {
        GRBEnv env = new GRBEnv();
        int timelimit = (int) ((timeLimit - elapsedTime) / 1000);
        double grb_mipgap = 0.2;
        int grb_solnlimit = 2;
        if (separatedPR) {
            grb_solnlimit = 5;
        }

        env.set(GRB.IntParam.Seed, abs(RNG.nextInt()));
        env.set(GRB.DoubleParam.MIPGap, grb_mipgap);
        if (separatedPR) {
            env.set(GRB.DoubleParam.TimeLimit, timelimit / 2);
        } else {
            env.set(GRB.DoubleParam.TimeLimit, timelimit);
        }
        env.set(GRB.IntParam.SolutionLimit, grb_solnlimit);

        //focus on time
        env.set(GRB.IntParam.MIPFocus, 1);

        //a time-quality trade off
//        env.set(GRB.IntParam.MIPFocus, 2);

        GRBModel modelP = new GRBModel(env);
        GRBModel modelR = modelP;
        if (separatedPR) {
            grb_mipgap = 0.001;
            env.set(GRB.DoubleParam.MIPGap, grb_mipgap);
            modelR = new GRBModel(env);
        }

        numDfgVals = DFGValNodeName.size();
        numDfgOps = DFGOpNodeName.size();
        numMrrgR = MRRGRoutingName.size();
        numMrrgF = MRRGFunctionName.size();
        connectSize = connectList.size();
        countR = numDfgVals * numMrrgR;
        countF = numDfgOps * numMrrgF;

        GRBVar[] R = modelR.addVars(countR, 'B');
        GRBVar[] F = modelP.addVars(countF, 'B');
        Integer[] afterPlacementF = new Integer[countF];


        List<GRBVar[]> S = new ArrayList<>();
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                int num_fanouts = DFGValNodeOut.get(val).size();
                S.add(modelR.addVars(num_fanouts, 'B'));
//                if (num_fanouts > 1) {
//                    S.add(modelR.addVars(num_fanouts, 'B'));
//                } else {
//                    S.add(null);
//                }
            }
        modelR.update();

        /** Set VarName of R_i_j, R_i_j_k and F_p_q.
         */
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                R[RIndex(val, r)].set(GRB.StringAttr.VarName, "R_" + r + "_" + val);
                if (S.get(RIndex(val, r)) != null) {
                    for (int k = 0; k < S.get(RIndex(val, r)).length; k++) {
                        S.get(RIndex(val, r))[k].set(GRB.StringAttr.VarName, "R_" + r + "_" + val + "_" + k);
                    }
                } else {
                    S.set(RIndex(val, r), new GRBVar[1]);
                    S.get(RIndex(val, r))[0] = R[RIndex(val, r)];
                }
            }

        for (int op = 0; op < numDfgOps; op++)
            for (int f = 0; f < numMrrgF; f++)
                F[FIndex(op, f)].set(GRB.StringAttr.VarName, "F_" + f + "_" + op);

        modelR.update();

        File init = new File("./importantBak/MappingSAT/vadd-24.res");
        Scanner s = new Scanner(init);
        String nodeName;
        while(s.hasNext()){
            nodeName = s.next();
            GRBVar var = modelR.getVarByName(nodeName);
            var.set(GRB.DoubleAttr.Start, 1);
        }


        /** Constraints of placement.
         */
        constrFunctionExclusivity(modelP, F);
        constrOperationPlacement(modelP, F);
        constrFunctionalLegality(modelP, F);

        if (separatedPR) {
//            GRBVar[] concurrentF = modelP.addVars(num_mrrg_f * num_mrrg_f * connectSize, 'B');
            setPlacementObjective(modelP, F, true);
            modelP.update();
            modelP.write("problem_java_P.lp");
            modelP.optimize();
            int status = modelP.get(GRB.IntAttr.Status);
            int solcnt = modelP.get(GRB.IntAttr.SolCount);
            if (status == GRB.INFEASIBLE || (status == GRB.TIME_LIMIT && solcnt == 0)) {
                return new GRBModel[]{modelP, modelR};
            }
            System.arraycopy(modelP.getVars(), 0, F, 0, countF);
            for (int i = 0; i < F.length; i++) {
                afterPlacementF[i] = (int) F[i].get(GRB.DoubleAttr.X);
//                if (afterPlacementF[i] > 0) {
//                    System.out.println(DFGOpNodeName.get(i / numMrrgF) +
//                            " map to " + MRRGFunctionName.get(i % numMrrgF));
//                }
            }

        }

        /** Constraints of routing.
         */
        constrRoutingResource(modelR, R, S);
        constrRoutingExclusivity(modelR, R);
        constrMultiplexerExclusivity(modelR, R);
//        constrAcyclic(modelR, R);
        if (separatedPR) {
            constrFanoutRouting(modelR, S, afterPlacementF, false);
            constrInitialFanout(modelR, S, afterPlacementF, false);
        } else {
            constrFanoutRouting(modelR, S, F, true);
            constrInitialFanout(modelR, S, F, true);
        }


        /** Constraints of latencies and skews.
         */
        if (scheduleControl) {
            GRBVar[] Delays = modelR.addVars(connectSize, 'I');
            GRBVar[] Latencies = modelR.addVars(numDfgOps, 'I');

            int WaitSkewNum = connectSize;
            int RelativeSkewNum = DFGMultipleInputMap.size();

            GRBVar[] WaitSkews = modelR.addVars(WaitSkewNum, 'I');
            GRBVar[] RelativeSkews = modelR.addVars(RelativeSkewNum, 'I');
            GRBVar[] SkewDirection = modelR.addVars(RelativeSkewNum, 'B');


            if (separatedPR) {
                constrDelay(modelR, S, Delays, Latencies, WaitSkews, afterPlacementF, false);
            } else {
                constrDelay(modelR, S, Delays, Latencies, WaitSkews, F, true);
            }

            setLatencyRange(Latencies, maxLatency);
            if (useRelativeSkew) {
                constrRelativeSkew(modelR, RelativeSkews, WaitSkews,
                        SkewDirection, skewLimit);
            }
        }

        /** Set objective.
         */
        setRoutingObjective(modelR, R);

        modelR.update();
        modelR.write("problem_java_R.lp");
        modelR.optimize();
        System.out.println("Placement status: " + modelP.get(GRB.IntAttr.Status));
        System.out.println("Routing status: " + modelR.get(GRB.IntAttr.Status));
        elapsedTime = System.currentTimeMillis() - startTime;
        int remainTile = (int) ((timeLimit - elapsedTime) / 1000);
        if (remainTile < 1) {
            return new GRBModel[]{modelP, modelR};
        }
        int status = modelR.get(GRB.IntAttr.Status);
        int solcnt = modelR.get(GRB.IntAttr.SolCount);

        if (status == GRB.TIME_LIMIT && solcnt == 0) {
            System.out.println("\033[31;4m" + "Time limit reached, mapping fail." + "\033[0m");
            return new GRBModel[]{modelP, modelR};
        } else if (status == GRB.INFEASIBLE) {
            if (separatedPR) {
                if (iterationNum < iterationLimit) {
                    System.out.println("\033[33;4m" + "Iteration " + iterationNum + ": fail, continue...\033[0m");
                    iterationNum++;
                    return getILPModel(separatedPR, scheduleControl);
                } else {
                    System.out.println("\033[31;4m" + "Iteration limit reached, mapping fail." + "\033[0m");
                }
            } else {
                System.out.println("\033[31;4m" + "Integrated P&R mapping fail." + "\033[0m");
            }
        } else {
            ringCheckPass = checkRoutingWithoutUselessRing(modelR);
            if (separatedPR) {
//                if(!ringCheckPass) {
//                    Random tempRNG = new Random(100);
//                    for (int it = 0; it < 5; it++) {
//                        System.out.println("\033[33;4m" + "Ring check fail times with this placement: "
//                                + (it + 1) + "\033[0m");
//                        modelR.set(GRB.IntParam.Seed, abs(tempRNG.nextInt()));
//                        modelR.reset();
//                        modelR.update();
//                        modelR.optimize();
//                        ringCheckPass = checkRoutingWithoutUselessRing(modelR);
//                        if (ringCheckPass) break;
//                    }
//                }
                if (ringCheckPass) {
                    System.out.println("\033[34;4m" + "Iteration " + iterationNum + ": mapping success!" + "\033[0m");
                } else {
                    System.out.println("\033[33;4m" + "Iteration " + iterationNum + ": success," +
                            " but there remain useless rings of routingNodes." + "\033[0m");
                    if (ringCheckCount < ringCheckLimit) {
                        ringCheckCount++;
                        iterationNum++;
                        System.out.println("\033[33;4m" + "Ring check fail times: " + ringCheckCount + "\033[0m");
                        return getILPModel(separatedPR, scheduleControl);
                    } else {
                        System.out.println("\033[33;4m" + "Ring check limit reached," +
                                " Useless rings of routingNodes accept." +
                                " There remain useless rings of routingNodes and the skew limit may be ruined.\n" +
                                "You can enlarge the ring check limit or skew limit," +
                                " or increase routing resources of the CGRA architecture." + "\033[0m");
                    }
                }
            } else {
                if (ringCheckPass) {
                    System.out.println("\033[34;4m" + "Integrated P&R mapping success!" + "\033[0m");
                } else {
                    System.out.println("\033[33;4m" + "Integrated P&R mapping success," +
                            " but there remain useless rings of routingNodes." + "\033[0m");
                    if (scheduleControl) {
                        if (ringCheckCount < ringCheckLimit) {
                            ringCheckCount++;
                            iterationNum++;
                            System.out.println("\033[33;4m" + "Ring check fail times: " + ringCheckCount + "\033[0m");
                            return getILPModel(separatedPR, scheduleControl);
                        } else {
                            System.out.println("\033[33;4m" + "Ring check limit reached," +
                                    " Useless rings of routingNodes accept." +
                                    " There remain useless rings of routingNodes and the skew limit may be ruined.\n" +
                                    "You can enlarge the ring check limit or skew limit," +
                                    " or increase routing resources of the CGRA architecture." + "\033[0m");
                        }
                    }
                }
            }
        }

        return new GRBModel[]{modelP, modelR};
    }

}
