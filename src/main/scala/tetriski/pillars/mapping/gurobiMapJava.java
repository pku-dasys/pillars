package tetriski.pillars.mapping;

import gurobi.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
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
     * @param sinkID the ID of sink node
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
     * @param nodeID the ID of node
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
     * @param  nodeConnects the direction graph
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

    List<Integer> DFGopNodeOut;
    List<Integer> DFGopNodeOpcode;
    List<String> DFGopNodeName;
    List<List<Integer>> DFGvalNodeOut;
    List<List<Integer>> DFGvalNodeOutputOperand;
    List<String> DFGvalNodeName;

    List<String> MRRGfunctionName;
    List<String> MRRGroutingName;
    List<List<Integer>> MRRGfunctionFanin;
    List<List<Integer>> MRRGfunctionFaninType;
    List<List<Integer>> MRRGroutingFanin;
    List<List<Integer>> MRRGroutingFaninType;
    List<List<Integer>> MRRGfunctionFanout;
    List<List<Integer>> MRRGfunctionFanoutType;
    List<List<Integer>> MRRGroutingFanout;
    List<List<Integer>> MRRGroutingFanoutType;
    List<List<Integer>> MRRGfunctionSupportOpcode;

    Map<String, Integer> MRRGlatency;
    Map<String, Integer> DFGvalB2opMap;
    List<List<String>> connectList;
    Map<String, List<String>> DFGMultipleInputMap;
    Map<String, Integer> DFGSelfJoinMap;
    Map<String, Integer> waitSkewMap;
    Map<String, Integer> DFGskewMap;
    Map<String, Integer> DFGlatencyMap;
    Map<List<String>, Integer> MRRGdistence;
    Map<Integer, Integer> regMap;
    Map<Integer, List<Integer>> regConnect;
    Map<Integer, List<Integer>> func2regMap;
    Map<Integer, List<List<Integer>>> reg2funcMap;
    Map<Integer, List<List<Integer>>> funcDirect2funcMap;
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
    int ringCheckCount = 0;
    int ringCheckLimit = 5;
    Random RNG = new Random(1);

    /**
     * Set up the class.
     *
     * @param filename the file name used in printing result
     */
    gurobiMapJava(String filename) {
        this.filename = filename;

        DFGopNodeName = new ArrayList<>();
        DFGopNodeOpcode = new ArrayList<>();
        DFGopNodeOut = new ArrayList<>();
        DFGvalNodeName = new ArrayList<>();
        DFGvalNodeOut = new ArrayList<>();
        DFGvalNodeOutputOperand = new ArrayList<>();

        MRRGfunctionName = new ArrayList<>();
        MRRGfunctionFanin = new ArrayList<>();
        MRRGfunctionFaninType = new ArrayList<>();
        MRRGfunctionFanout = new ArrayList<>();
        MRRGfunctionFanoutType = new ArrayList<>();
        MRRGfunctionSupportOpcode = new ArrayList<>();
        MRRGroutingName = new ArrayList<>();
        MRRGroutingFanin = new ArrayList<>();
        MRRGroutingFaninType = new ArrayList<>();
        MRRGroutingFanout = new ArrayList<>();
        MRRGroutingFanoutType = new ArrayList<>();

        MRRGlatency = new HashMap<>();
        DFGvalB2opMap = new HashMap<>();
        connectList = new ArrayList<>();
        DFGMultipleInputMap = new HashMap<>();
        DFGSelfJoinMap = new HashMap<>();
        waitSkewMap = new HashMap<>();
        DFGskewMap = new HashMap<>();
        DFGlatencyMap = new HashMap<>();
        MRRGdistence = new HashMap<>();
        regMap = new HashMap<>();
        regConnect = new HashMap<>();
        func2regMap = new HashMap<>();
        reg2funcMap = new HashMap<>();
        funcDirect2funcMap = new HashMap<>();
    }

    /**
     * Build class from txt files, only used in debugging.
     *
     * @param DFGFile  the file name of txt file containing DFG(IR)
     * @param MRRGFile the file name of txt file containing MRRG
     * @throws IOException
     */
    gurobiMapJava(String DFGFile, String MRRGFile) throws IOException {
        DFGopNodeName = new ArrayList<>();
        DFGopNodeOpcode = new ArrayList<>();
        DFGopNodeOut = new ArrayList<>();
        DFGvalNodeName = new ArrayList<>();
        DFGvalNodeOut = new ArrayList<>();
        DFGvalNodeOutputOperand = new ArrayList<>();

        MRRGfunctionName = new ArrayList<>();
        MRRGfunctionFanin = new ArrayList<>();
        MRRGfunctionFaninType = new ArrayList<>();
        MRRGfunctionFanout = new ArrayList<>();
        MRRGfunctionFanoutType = new ArrayList<>();
        MRRGfunctionSupportOpcode = new ArrayList<>();
        MRRGroutingName = new ArrayList<>();
        MRRGroutingFanin = new ArrayList<>();
        MRRGroutingFaninType = new ArrayList<>();
        MRRGroutingFanout = new ArrayList<>();
        MRRGroutingFanoutType = new ArrayList<>();

        BufferedReader DFGReader = new BufferedReader(new FileReader(DFGFile));
        int opnum = Integer.parseInt(DFGReader.readLine());
        for (int i = 0; i < opnum; i++) {
            DFGopNodeName.add(DFGReader.readLine());
            DFGopNodeOut.add(Integer.valueOf(DFGReader.readLine()));
            DFGopNodeOpcode.add(Integer.valueOf(DFGReader.readLine()));
        }
        int valnum = Integer.parseInt(DFGReader.readLine());
        for (int i = 0; i < valnum; i++) {
            DFGvalNodeName.add(DFGReader.readLine());
            int outsize = Integer.parseInt(DFGReader.readLine());
            List<Integer> valout = new ArrayList<>();
            for (int j = 0; j < outsize; j++)
                valout.add(Integer.valueOf(DFGReader.readLine()));
            DFGvalNodeOut.add(valout);
            List<Integer> valoperand = new ArrayList<>();
            for (int j = 0; j < outsize; j++)
                valoperand.add(Integer.valueOf(DFGReader.readLine()));
            DFGvalNodeOutputOperand.add(valoperand);
        }
        DFGReader.close();

        BufferedReader MRRGReader = new BufferedReader(new FileReader(MRRGFile));

        int fnum = Integer.parseInt(MRRGReader.readLine());
        for (int i = 0; i < fnum; i++) {
            MRRGfunctionName.add(MRRGReader.readLine());
            int faninsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanin = new ArrayList<>();
            List<Integer> fanintype = new ArrayList<>();
            for (int j = 0; j < faninsize; j++) {
                fanin.add(Integer.valueOf(MRRGReader.readLine()));
                fanintype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGfunctionFanin.add(fanin);
            MRRGfunctionFaninType.add(fanintype);

            int fanoutsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanout = new ArrayList<>();
            List<Integer> fanouttype = new ArrayList<>();
            for (int j = 0; j < fanoutsize; j++) {
                fanout.add(Integer.valueOf(MRRGReader.readLine()));
                fanouttype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGfunctionFanout.add(fanout);
            MRRGfunctionFanoutType.add(fanouttype);

            int sopsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> sop = new ArrayList<>();
            for (int j = 0; j < sopsize; j++) {
                sop.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGfunctionSupportOpcode.add(sop);
        }

        int rnum = Integer.parseInt(MRRGReader.readLine());
        for (int i = 0; i < rnum; i++) {
            MRRGroutingName.add(MRRGReader.readLine());
            int faninsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanin = new ArrayList<>();
            List<Integer> fanintype = new ArrayList<>();
            for (int j = 0; j < faninsize; j++) {
                fanin.add(Integer.valueOf(MRRGReader.readLine()));
                fanintype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGroutingFanin.add(fanin);
            MRRGroutingFaninType.add(fanintype);

            int fanoutsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanout = new ArrayList<>();
            List<Integer> fanouttype = new ArrayList<>();
            for (int j = 0; j < fanoutsize; j++) {
                fanout.add(Integer.valueOf(MRRGReader.readLine()));
                fanouttype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGroutingFanout.add(fanout);
            MRRGroutingFanoutType.add(fanouttype);
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
        return valIndex * MRRGroutingName.size() + routingIndex;
    }

    /**
     * Get where F_p,q stores in GRBVar[] F.
     *
     * @param opIndex       the index of opNode
     * @param functionIndex the index of functionalNode
     */
    int FIndex(int opIndex, int functionIndex) {
        return opIndex * MRRGfunctionName.size() + functionIndex;
    }

    /**
     * Do ILP mapping and return the mapping result
     *
     * @param separatedPR     a parameter indicating whether ILP placement and routing should be separated
     * @param scheduleControl a parameter indicating whether the latency and skew should be controlled and obtained in ILP
     */
    List<Integer>[] ILPMap(Boolean separatedPR, Boolean scheduleControl) throws GRBException, IOException {
        GRBModel[] models = getILPModel(separatedPR, scheduleControl);
        GRBModel modelP = models[0];
        GRBModel modelR = models[1];

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

        int status = modelR.get(GRB.IntAttr.Status);

        if (status == GRB.OPTIMAL || status == GRB.SUBOPTIMAL || status == GRB.SOLUTION_LIMIT) {

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
                    DFGlatencyMap.put(DFGopNodeName.get(i),
                            (int) modelR.getVarByName("Latency_" + i).get(GRB.DoubleAttr.X));
                }
                for (String key : DFGMultipleInputMap.keySet()) {
                    DFGskewMap.put(key,
                            (int) modelR.getVarByName("Skew_" + key).get(GRB.DoubleAttr.X));
                }
            }

            /****Debug****/
            if (scheduleControl) {
                for (int i = 0; i < connectSize; i++) {
                    System.out.println("Delay " + connectList.get(i).get(0) + " -> " +
                            connectList.get(i).get(1) + ": " + modelR.getVarByName("Delay_" + i).get(GRB.DoubleAttr.X));
                }

                for (int i = 0; i < numDfgOps; i++) {
                    System.out.println("Latency " + DFGopNodeName.get(i) + ": " +
                            modelR.getVarByName("Latency_" + i).get(GRB.DoubleAttr.X));
                }


                for (String key : DFGMultipleInputMap.keySet()) {
                    System.out.println("Skew " + key + ": " +
                            modelR.getVarByName("Skew_" + key).get(GRB.DoubleAttr.X));
                }

                for (String key : waitSkewMap.keySet()) {
                    System.out.println("waitSkew " + key + ": " +
                            modelR.getVarByName(key).get(GRB.DoubleAttr.X));
                }

//                for (int r = 0; r < num_mrrg_r; r++) {
//                    if (modelR.getVarByName("R_r_" + 0).get(GRB.DoubleAttr.X) > 0) {
//                        System.out.println("R_r_" + 0 + ": " + MRRGroutingname.get(r) + " " + DFGvalnodename.get(0));
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
                    if (R[RIndex(val, r)].get(GRB.DoubleAttr.X) == 1.0) {
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
                for (int f = 0; f < numMrrgF; f++)
                    if (F[FIndex(op, f)].get(GRB.DoubleAttr.X) == 1.0) {
                        System.out.printf("%s->%s\n", DFGopNodeName.get(op), MRRGfunctionName.get(f));
                        resultFile.write(DFGopNodeName.get(op) + " " + MRRGfunctionName.get(f) + "\n");
                        f_mapped[f] = DFGopNodeOpcode.get(op).intValue() + 1;
                        f_result[f] = op;
                        mappedOp2MrrgMap.put(op, f);
                    }
            resultFile.flush();
            resultFile.close();


            FileWriter infoFile = new FileWriter(filename + "_i.txt");
            for (int r = 0; r < numMrrgR; r++)
                if (r_mapped[r] == 1 && MRRGroutingName.get(r).indexOf("internalNode") != -1) {
                    List<Integer> fanin = new ArrayList<>();
                    List<Integer> fanout = new ArrayList<>();
                    for (int i = 0; i < MRRGroutingFanin.get(r).size(); i++) {
                        if (r_mapped[MRRGroutingFanin.get(r).get(i)] == 1 &&
                                r_result[MRRGroutingFanin.get(r).get(i)] == r_result[r])
                            fanin.add(Integer.valueOf(i));
                    }
                    for (int i = 0; i < MRRGroutingFanout.get(r).size(); i++) {
                        if (r_mapped[MRRGroutingFanout.get(r).get(i)] == 1 &&
                                r_result[MRRGroutingFanout.get(r).get(i)] == r_result[r])
                            fanout.add(Integer.valueOf(i));
                    }
                    if (fanin.size() > 1) {
                        System.out.println("   " + MRRGroutingName.get(r) + "<-" + DFGvalNodeName.get(r_mapped[r]));
                    }
                    if (fanin.size() > 0 && fanout.size() > 0) {
                        infoFile.write("<" + MRRGroutingName.get(r) + ">\n");
                        for (int i = 0; i < fanin.size(); i++)
                            infoFile.write(fanin.get(i).toString() + " ");
                        infoFile.write("\n");
                        for (int i = 0; i < fanout.size(); i++)
                            infoFile.write(fanout.get(i).toString() + " ");
                        infoFile.write("\n");
                    }
                }
            for (int f = 0; f < numMrrgF; f++) {
                if (f_mapped[f] > 0 && MRRGfunctionName.get(f).indexOf("internalNode") != -1) {
                    infoFile.write("<" + MRRGfunctionName.get(f) + ">\nSELECTED_OP\n");
                    infoFile.write("" + (f_mapped[f] - 1) + "\n");
                }
            }
            infoFile.flush();
            infoFile.close();

            int regCount = 0;
            for (int val = 0; val < numDfgVals; val++) {
                for (int fanout = 0; fanout < DFGvalNodeOut.get(val).size(); fanout++) {
                    Map<Integer, List<Integer>> graph = getGraph(modelR, val, fanout);
                    int op = DFGvalB2opMap.get(DFGvalNodeName.get(val));
                    int mappedMrrgNode = mappedOp2MrrgMap.get(op);
                    List<Integer> functionFanout = MRRGfunctionFanout.get(mappedMrrgNode);
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
                            if (MRRGlatency.containsKey(MRRGroutingName.get(currentNode))) {
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
                        int outOp = DFGvalNodeOut.get(val).get(fanout);
                        int outOpOperand = DFGvalNodeOutputOperand.get(val).get(fanout);
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
        int fanouts = DFGvalNodeOut.get(val).size();
        for (int r = 0; r < numMrrgR; r++) {
            String valueName = "R_" + r + "_" + val;
            if (fanouts > 1) {
                valueName = "R_" + r + "_" + val + "_" + fanout;
            }
            if (model.getVarByName(valueName).get(GRB.DoubleAttr.X) == 1) {
                mappedRoutingNodes.add(r);
            }
        }
        Map<Integer, List<Integer>> graph = new HashMap<>();
        for (Integer mappedRoutingNode : mappedRoutingNodes) {
            List<Integer> mappedOutNodes = new ArrayList<>();
            List<Integer> outNodes = MRRGroutingFanout.get(mappedRoutingNode);
            for (int i = 0; i < outNodes.size(); i++) {
                if (MRRGroutingFanoutType.get(mappedRoutingNode).get(i) == 0) {
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
            int fanouts = DFGvalNodeOut.get(val).size();
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
//                R[RINDEX(val, r)].set(GRB.StringAttr.VarName, "R_" + val + "_" + r);
                if (S.get(RIndex(val, r)) != null) {
                    for (int k = 0; k < S.get(RIndex(val, r)).length; k++) {
//                        S.get(RINDEX(val, r))[k].set(GRB.StringAttr.VarName, "R_" + val + "_" + r + "_" + k);
                        model.addConstr(R[RIndex(val, r)], GRB.GREATER_EQUAL, S.get(RIndex(val, r))[k],
                                "sub_val" + (constrcount++));
                    }
                } else {
                    S.set(RIndex(val, r), new GRBVar[1]);
                    S.get(RIndex(val, r))[0] = R[RIndex(val, r)];
                }
            }
    }

    /**
     * Constraint: Legality of Delay and WaitSkew.
     */
    <T> void constrDelay(GRBModel model, GRBVar[] R, List<GRBVar[]> S, GRBVar[] Delays,
                         GRBVar[] Latencies, GRBVar[] WaitSkews, T[] F, Boolean VariableF) throws GRBException {

        int constrcount = 0;
        for (int val = 0; val < numDfgVals; val++) {
            for (int fanOut = 0; fanOut < DFGvalNodeOut.get(val).size(); fanOut++) {
                GRBLinExpr constraint = new GRBLinExpr();
                String valName = DFGvalNodeName.get(val);
                if (DFGvalNodeOut.get(val).size() > 1) {
                    for (int r = 0; r < numMrrgR; r++) {
                        int coeff = 0;
                        if (MRRGlatency.containsKey(MRRGroutingName.get(r))) {
                            coeff = MRRGlatency.get(MRRGroutingName.get(r));
                        }
                        constraint.addTerm(coeff, S.get(RIndex(val, r))[fanOut]);
                    }
                } else {
                    for (int r = 0; r < numMrrgR; r++) {
                        int coeff = 0;
                        if (MRRGlatency.containsKey(MRRGroutingName.get(r))) {
                            coeff = MRRGlatency.get(MRRGroutingName.get(r));
                        }
                        constraint.addTerm(coeff, R[RIndex(val, r)]);
                    }
                }
                for (int f = 0; f < numMrrgF; f++) {
                    int coeff = 0;
                    if (MRRGlatency.containsKey(MRRGfunctionName.get(f))) {
                        coeff = MRRGlatency.get(MRRGfunctionName.get(f));
                    }
                    if (VariableF) {
                        constraint.addTerm(coeff, (GRBVar) F[FIndex(DFGvalB2opMap.get(valName), f)]);
                    } else {
                        constraint.addConstant(coeff * Integer.class.cast(F[FIndex(DFGvalB2opMap.get(valName), f)]));
                    }

                }

                Delays[constrcount].set(GRB.StringAttr.VarName, "Delay_" + constrcount);

                model.addConstr(constraint, GRB.EQUAL, Delays[constrcount], "delay_" + (constrcount));

                /** Constraint delay and wait skew
                 */

                int sourceID = DFGvalB2opMap.get(valName);
                int sinkID = DFGvalNodeOut.get(val).get(fanOut);

                String sourceName = DFGopNodeName.get(sourceID);
                String sinkName = DFGopNodeName.get(sinkID);
                GRBLinExpr waitSkewConstraint = new GRBLinExpr();
                waitSkewConstraint.addTerm(1, Latencies[sourceID]);
                waitSkewConstraint.addTerm(1, Delays[constrcount]);
                waitSkewConstraint.addTerm(1, WaitSkews[constrcount]);

                if (DFGMultipleInputMap.containsKey(sinkName)) {

                    WaitSkews[constrcount].set(GRB.StringAttr.VarName, "WaitSkew_" + sourceName + "_" + sinkName);
                    waitSkewMap.put("WaitSkew_" + sourceName + "_" + sinkName, constrcount);

                    if (sourceID == sinkID) {
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
    void setPlacementObjective(GRBModel model, GRBVar[] F) throws GRBException {
        GRBLinExpr objective = new GRBLinExpr();
        for (int pSource = 0; pSource < numMrrgF; pSource++) {
            for (int qSource = 0; qSource < numDfgOps; qSource++) {
                String sourceDFGName = DFGopNodeName.get(qSource);
                for (int pSink = 0; pSink < numMrrgF; pSink++) {
                    for (int qSink = 0; qSink < numDfgOps; qSink++) {
                        String sinkDFGName = DFGopNodeName.get(qSink);
                        List<String> namePair = new LinkedList<String>() {{
                            add(sourceDFGName);
                            add(sinkDFGName);
                        }};
                        if (connectList.contains(namePair) &&
                                MRRGfunctionSupportOpcode.get(pSource).contains(DFGopNodeOpcode.get(qSource)) &&
                                MRRGfunctionSupportOpcode.get(pSink).contains(DFGopNodeOpcode.get(qSink))) {
                            String varName = "concurrentF_" + pSource + "_" + qSource + "_" + pSink + "_" + qSink;
                            GRBVar concurrentF = model.addVar(0, 1, 1, 'B', varName);
                            String sourceMRRGName = MRRGfunctionName.get(pSource);
                            String sinkMRRGName = MRRGfunctionName.get(pSink);
                            int distence = MRRGdistence.get(new LinkedList<String>() {{
                                add(sourceMRRGName);
                                add(sinkMRRGName);
                            }});
                            GRBVar[] tempF = {F[FIndex(qSource, pSource)], F[FIndex(qSink, pSink)]};
                            model.addGenConstrAnd(concurrentF, tempF,
                                    "and_" + varName);
                            objective.addTerm(distence, concurrentF);
                        }

                    }
                }

            }
        }

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

    /**
     * Constraint: Fanout_Routing.
     */
    <T> void constrFanoutRouting(GRBModel model, List<GRBVar[]> S, T[] F, Boolean VariableF) throws GRBException {
        int constrcount = 0;
        int MRRG_NODE_ROUTING = 0;
        int MRRG_NODE_FUNCTION = 1;
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                int val_fanouts = DFGvalNodeOut.get(val).size();

                for (int i = 0; i < val_fanouts; i++) {
                    GRBLinExpr sum_of_fanouts = new GRBLinExpr();
                    int fanoutsize = MRRGroutingFanout.get(r).size();
                    for (int mrrg_fanout = 0; mrrg_fanout < fanoutsize; mrrg_fanout++) {
                        if (MRRGroutingFanoutType.get(r).get(mrrg_fanout) == MRRG_NODE_ROUTING) {
                            sum_of_fanouts.addTerm(1.0, S.get(RIndex(val, MRRGroutingFanout.get(r).
                                    get(mrrg_fanout)))[i]);
                        } else if (MRRGroutingFanoutType.get(r).get(mrrg_fanout) == MRRG_NODE_FUNCTION) {
                            int op = DFGvalNodeOut.get(val).get(i).intValue(), operand =
                                    DFGvalNodeOutputOperand.get(val).get(i).intValue();
                            if (MRRGfunctionFanin.get(MRRGroutingFanout.get(r).get(mrrg_fanout)).size() > operand &&
                                    MRRGfunctionFanin.get(MRRGroutingFanout.get(r).get(mrrg_fanout)).get(operand) == r) {
                                if (VariableF) {
                                    sum_of_fanouts.addTerm(1.0,
                                            (GRBVar) F[FIndex(op, MRRGroutingFanout.get(r).get(mrrg_fanout))]);
                                } else {
                                    sum_of_fanouts.addConstant(Integer.class.cast(F[FIndex(op,
                                            MRRGroutingFanout.get(r).get(mrrg_fanout))]));
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
                int fanin_count = MRRGroutingFanin.get(r).size();
                if (fanin_count > 1) {
                    for (int fanin = 0; fanin < fanin_count; fanin++) {
                        if (MRRGroutingFaninType.get(r).get(fanin) == MRRG_NODE_ROUTING)
                            sum_of_fanins.addTerm(1.0, R[RIndex(val, MRRGroutingFanin.get(r).get(fanin))]);
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
                if (DFGopNodeOut.get(op).intValue() != -1) {
                    int val = DFGopNodeOut.get(op).intValue();
                    int f_fanouts = MRRGfunctionFanout.get(f).size();
                    for (int r = 0; r < f_fanouts; r++) {
                        int val_fanouts = DFGvalNodeOut.get(val).size();
                        for (int i = 0; i < val_fanouts; i++)
                            if (VariableF) {
                                model.addConstr((GRBVar) F[FIndex(op, f)], GRB.EQUAL,
                                        S.get(RIndex(val, MRRGfunctionFanout.get(f).get(r)))[i],
                                        "function_unit_fanout_" + (constrcount++));
                            } else {
                                model.addConstr(Integer.class.cast(F[FIndex(op, f)]), GRB.EQUAL,
                                        S.get(RIndex(val, MRRGfunctionFanout.get(f).get(r)))[i],
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
                for (int i = 0; i < MRRGfunctionSupportOpcode.get(f).size(); i++)
                    if (MRRGfunctionSupportOpcode.get(f).get(i) == DFGopNodeOpcode.get(op)) {
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
        int timelimit = 7200;
        double grb_mipgap = 0.2;
        int grb_solnlimit = 2;
        if (separatedPR) {
            grb_solnlimit = 5;
        }

        env.set(GRB.IntParam.Seed, abs(RNG.nextInt()));
        env.set(GRB.DoubleParam.MIPGap, grb_mipgap);
        env.set(GRB.DoubleParam.TimeLimit, timelimit);
        env.set(GRB.IntParam.SolutionLimit, grb_solnlimit);

        //focus on time
        env.set(GRB.IntParam.MIPFocus, 1);

        //a time-quality trade off
//        env.set(GRB.IntParam.MIPFocus, 2);

        GRBModel modelP = new GRBModel(env);
        GRBModel modelR = modelP;
        if (separatedPR) {
            modelR = new GRBModel(env);
        }

        numDfgVals = DFGvalNodeName.size();
        numDfgOps = DFGopNodeName.size();
        numMrrgR = MRRGroutingName.size();
        numMrrgF = MRRGfunctionName.size();
        connectSize = connectList.size();
        countR = numDfgVals * numMrrgR;
        countF = numDfgOps * numMrrgF;

        GRBVar[] R = modelR.addVars(countR, 'B');
        GRBVar[] F = modelP.addVars(countF, 'B');
        Integer[] afterPlacementF = new Integer[countF];


        List<GRBVar[]> S = new ArrayList<>();
        for (int val = 0; val < numDfgVals; val++)
            for (int r = 0; r < numMrrgR; r++) {
                int num_fanouts = DFGvalNodeOut.get(val).size();
                if (num_fanouts > 1) {
                    S.add(modelR.addVars(num_fanouts, 'B'));
                } else {
                    S.add(null);
                }
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
                }
            }

        for (int op = 0; op < numDfgOps; op++)
            for (int f = 0; f < numMrrgF; f++)
                F[FIndex(op, f)].set(GRB.StringAttr.VarName, "F_" + f + "_" + op);

        /** Constraints of placement.
         */
        constrFunctionExclusivity(modelP, F);
        constrOperationPlacement(modelP, F);
        constrFunctionalLegality(modelP, F);

        if (separatedPR) {
//            GRBVar[] concurrentF = modelP.addVars(num_mrrg_f * num_mrrg_f * connectSize, 'B');
            setPlacementObjective(modelP, F);
            modelP.update();
            modelP.write("problem_java_P.lp");
            modelP.optimize();
            System.arraycopy(modelP.getVars(), 0, F, 0, countF);
            for (int i = 0; i < F.length; i++) {
                afterPlacementF[i] = (int) F[i].get(GRB.DoubleAttr.X);
//                if (afterPlacementF[i] > 0) {
//                    System.out.println(DFGopnodename.get(i / num_mrrg_f) +
//                            " map to " + MRRGfunctionname.get(i % num_mrrg_f));
//                }
            }

        }

        /** Constraints of routing.
         */
        constrRoutingResource(modelR, R, S);
        constrRoutingExclusivity(modelR, R);
        constrMultiplexerExclusivity(modelR, R);
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
                constrDelay(modelR, R, S, Delays, Latencies, WaitSkews, afterPlacementF, false);
            } else {
                constrDelay(modelR, R, S, Delays, Latencies, WaitSkews, F, true);
            }

            setLatencyRange(Latencies, maxLatency);
            constrRelativeSkew(modelR, RelativeSkews, WaitSkews,
                    SkewDirection, skewLimit);
        }

        /** Set objective.
         */
        setRoutingObjective(modelR, R);

        modelR.update();
        modelR.write("problem_java_R.lp");
        modelR.optimize();
        System.out.println("Placement status: " + modelP.get(GRB.IntAttr.Status));
        System.out.println("Routing status: " + modelR.get(GRB.IntAttr.Status));
        int status = modelR.get(GRB.IntAttr.Status);
        if (status == GRB.INFEASIBLE) {
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
                }
            }
        }

        return new GRBModel[]{modelP, modelR};
    }

}
