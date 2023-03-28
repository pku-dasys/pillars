package pillars.mapping;

import java.io.*;
import java.lang.reflect.Array;
import java.text.SimpleDateFormat;
import java.util.*;

import static java.util.Arrays.asList;

import org.apache.logging.log4j.LogManager;

import java.util.zip.InflaterInputStream;

public class searchMap {
    org.apache.logging.log4j.Logger logger = LogManager.getLogger(searchMap.class);

    //for DFG
    int singlePathTimeLimit = 5;
    int singlePathEdgeLimit = 25;
    int MaxDelay = 4;
    int numDFG;
    String filename;
    Map<String, Integer> waitSkewMap;
    Map<String, Integer> DFGRelativeSkewMap;
    Map<String, Integer> DFGLatencyMap;
    Set<String> DFGCommutatedSet;
    List<String> DFGOpNodeName;
    Set<Integer> DFGCommutativeSet;
    Map<Integer, List<Integer>> DFGOpOperand;
    Map<Integer, Set<Integer>> fixedMapRelation;

    List<Integer> dfgOPNodes;
    List<Integer> dfgValNodes;
    List<Integer>[] DFGout;
    List<Integer>[] DFGin;
    Set<Integer>[] DFGop;
    int II;
    //for MRRG
    int numMRRG;
    List<Integer> mrrgFuncNodes;
    List<Integer> mrrgRoutingNodes;
    String[] MRRGFunctionName;
    String[] MRRGRoutingName;
    Set<List<Integer>> MRRGPathUsed;
    Map<Integer, Set<List<Integer>>> potentialAffectedNodes;

    List<Integer>[] MRRGout;
    List<Integer>[] MRRGin;
    Set<Integer>[] MRRGop;
    List<Integer> MRRGlatency;//need to init

    public boolean matchOP(int x, int y) {
        for (Integer k : DFGop[x]) {
            if (MRRGop[y].contains(k)) return true;
        }
        return false;
    }

    public int getLatency(int a, int b) {
        //latency from mrrg node a->b
        return MRRGlatency.get(a);
    }

    boolean[][] matchTable;
    boolean[] used;
    int[] routingUsed;
    int[] match;
    int[] reMatch;

    int[] toposort() {
        //the dfg without self circle is a DAG
        //get the toporank of the DAG
        int[] q = new int[numDFG];
        int qhead = 0;
        int[] du = new int[numDFG];
        for (int i = 0; i < numDFG; i++) du[i] = 0;
        for (int x = 0; x < numDFG; x++)
            for (int y : DFGout[x])
                if (x != y) {
                    //System.out.printf("%d -> %d\n",x,y);
                    du[y]++;
                }
        for (int x = 0; x < numDFG; x++) if (du[x] == 0) q[qhead++] = x;
        for (int d = 0; d < numDFG; d++) {
            if (d >= qhead) {
                System.out.println("warning : topo sort fail");
                return new int[0];
            }
            int x = q[d];
            for (int y : DFGout[x])
                if (x != y) {
                    du[y] -= 1;
                    if (du[y] == 0) {
                        q[qhead++] = y;
                    }
                }
        }
        return q;
    }

    int resultPath[][][];
    int[][] waitPath;

    boolean isFirstOuter(int a, int b) {
        for (int i = 0; i < topoque.length; i++) {
            int x = topoque[i];
            if (DFGin[x].contains(a)) {
                return x == b;
            }
        }
        return false;
    }

    void usePath(int a, int b, int[] path) {
        //a,b: dfg node
        //System.out.printf("use path : %d to %d , %s\n",a,b,Arrays.toString(path));
        resultPath[a][b] = new int[path.length];
        if (isFirstOuter(a, b)) {
            waitPath[a] = path;
        }
        System.arraycopy(path, 0, resultPath[a][b], 0, path.length);
        for (int x : path) {
            used[x] = true;
            routingUsed[x] = a;
        }
    }

    int getTime(int[] path) {
        int ans = 0;
        for (int i = 1; i < path.length; i++) ans += getLatency(path[i - 1], path[i]);
        return ans;
    }

    boolean[] backUpUsed() {
        boolean[] me = new boolean[numMRRG];
        System.arraycopy(used, 0, me, 0, numMRRG);
        return me;
    }

    void restoreUsed(boolean[] me) {
        System.arraycopy(me, 0, used, 0, numMRRG);
    }

    int[] backUpRoutingUsed() {
        int[] me = new int[numMRRG];
        System.arraycopy(routingUsed, 0, me, 0, numMRRG);
        return me;
    }

    void restoreRoutingUsed(int[] me) {
        System.arraycopy(me, 0, routingUsed, 0, numMRRG);
    }

    int[] time;

    boolean ValidateMatchOP() {
        for (int i = 0; i < numDFG; i++) {
            if (!(0 <= match[i] && match[i] < numMRRG)) return false;
        }
        for (int i = 0; i < numDFG; i++)
            for (int j = i + 1; j < numDFG; j++)
                if (match[i] == match[j]) {
                    System.out.printf("Function Nodes use the same MRRG node\n");
                    return false;
                }
        for (int x = 0; x < numDFG; x++) {
            int y = match[x];
            if (!matchOP(x, y)) {
                System.out.printf("OP cant match %d %d table result : %d\n", x, y, matchTable[x][y] ? 1 : 0);
                return false;
            }
        }
        return true;
    }

    boolean ValidateFiretime() {
        int[] FireTime = new int[numDFG];
        Arrays.fill(FireTime, 0);
        for (int x : topoque) {
            int ti = -1;
            DFGRelativeSkewMap.put(DFGOpNodeName.get(x), 0);
            if (DFGin[x].size() == 0) {
//                FireTime[x] = time[x];
                int matched = match[x];
                String name = MRRGFunctionName[matched];
                String[] sArray = name.split(":");
                int rc = Integer.parseInt(sArray[0]);
                FireTime[x] = rc;
            }
            for (int y : DFGin[x])
                if (y != x) {
                    int nt = FireTime[y] + getTime(resultPath[y][x]);

                    System.out.printf(DFGOpNodeName.get(y) + " -> " + DFGOpNodeName.get(x) +
                            ": " + getTime(resultPath[y][x]) + "\n");

                    if (ti == -1) ti = nt;
                    else if (ti != nt) {
                        if (Math.abs(ti - nt) <= MaxDelay) {
                            boolean flag = (routingUsed[MRRGin[match[x]].get(0)] == y);
                            int skew = 0;
                            if (!flag) {
                                skew = ti - nt;
                            } else {
                                skew = nt - ti;
                            }
                            DFGRelativeSkewMap.put(DFGOpNodeName.get(x), skew);
                            if (ti < nt) {
                                ti = nt;
                            }
                        } else {
                            System.out.printf("Fire time cant match\n");
                            return false;
                        }
                    }
                } else {
                    if (getTime(resultPath[x][x]) != II) {
                        System.out.printf("self circle time cant match II\n");
                        return false;
                    }
                }
            if (ti != -1) FireTime[x] = ti;
            DFGLatencyMap.put(DFGOpNodeName.get(x), FireTime[x]);
        }
        return true;
    }

    boolean ValidatePathUse() {
        int[] pu = new int[numMRRG];
        Arrays.fill(pu, -1);
        for (int x : topoque) {
            for (int y : DFGin[x]) {
                pu[match[x]] = x;
                pu[match[y]] = y;
                int[] path = resultPath[y][x];
                if (path[0] != match[y] || path[path.length - 1] != match[x]) {
                    System.out.printf("ends of path incorrect\n");
                    return false;
                }
                for (int i = 1; i < path.length - 1; i++) {
                    if (pu[path[i]] != -1 && pu[path[i]] != y) {
                        System.out.printf("path intersect\n");
                        return false;
                    }
                    pu[path[i]] = y;
                }
                for (int i = 0; i < path.length - 1; i++) {
                    if (!MRRGout[path[i]].contains(path[i + 1])) {
                        System.out.printf("path edge incorrect\n");
                        return false;
                    }
                }
                for (int i = 0; i < path.length - 1; i++) {
                    MRRGPathUsed.add(asList(path[i], path[i + 1]));
                }
            }
        }
        return true;
    }

    void ValidateSolution() {
        boolean ok = true;
        ok &= ValidateMatchOP();
        ok &= ValidateFiretime();
        ok &= ValidatePathUse();
        if (ok) {
            logger.info("niu bi!");
            //System.out.println("niu bi!");
        }
    }

    void OutputSolution() throws IOException {
        logger.info("I find the solution!\n");
        logger.info("match : " + Arrays.toString(match));
//        System.out.printf("I find the solution!\n");
//        System.out.printf("match : %s\n", Arrays.toString(match));
        for (int x = 0; x < numDFG; x++) {
            logger.debug("out(" + x + ") : " + DFGout[x].toString());
//            System.out.printf("out(%d) : %s\n", x, DFGout[x].toString());
            for (int y : DFGout[x]) {
                logger.debug("path(" + x + "->" + y + "): " + Arrays.toString(resultPath[x][y]));
                logger.debug("path(" + DFGOpNodeName.get(x) + "->" + DFGOpNodeName.get(y) + "): ");
//                System.out.printf("path(%d->%d): %s\n", x, y, Arrays.toString(resultPath[x][y]));
//                System.out.printf("path(%s->%s): ", DFGOpNodeName.get(x), DFGOpNodeName.get(y));
                for (int i = 1; i < resultPath[x][y].length - 1; i++) {
                    logger.debug(MRRGRoutingName[resultPath[x][y][i]] + ", ");
//                    System.out.printf(MRRGRoutingName[resultPath[x][y][i]] + ", ");
                }
                logger.debug("Path found\n");
//                System.out.printf("\n");
            }
        }

        FileWriter resultFile = new FileWriter(filename + "_r.txt");

        for (int op = 0; op < numDFG; op++) {
//            for (int f = 0; f < numMRRG; f++) {
            int f = match[op];
//                if () {
            String name = MRRGFunctionName[f];
//                    String opName = DFGOpNodeName.get(op);
//                    if (DFGMultipleInputMap.containsKey(opName)) {
//                        int routingNodeOperand0 = MRRGFunctionFanin.get(f).get(0);
//                        int valNodeOperand1 = DFGOpOperand.get(op).get(1);
//                        if (abs(1.0 - modelR.getVarByName("R_" + routingNodeOperand0 + "_" + valNodeOperand1).get(GRB.DoubleAttr.X)) < 0.01) {
//                            DFGCommutatedSet.add(opName);
//                        }
//                    }
//
//                    if (name.contains("alu") && name.contains("internalNode")) {
//                        System.out.println("Func ALU: " + MRRGFunctionName.get(f) + " " + F[FIndex(op, f)].get(GRB.DoubleAttr.X));
//                        usedFuncALU += 1;
//                    }
//                    System.out.printf("%s->%s\n", DFGOpNodeName.get(op), MRRGFunctionName[f]);
            resultFile.write(DFGOpNodeName.get(op) + " " + MRRGFunctionName[f] + "\n");
//                    f_mapped[f] = DFGOpNodeOpcode.get(op).intValue() + 1;
//                    f_result[f] = op;
//                    mappedOp2MrrgMap.put(op, f);
        }
        resultFile.flush();
        resultFile.close();


        FileWriter infoFile = new FileWriter(filename + "_i.txt");
        List<String> funcOutput = new ArrayList<>();
        for (int r = 0; r < numMRRG; r++) {
            if (mrrgFuncNodes.contains(r)) {
                if (used[r] && MRRGFunctionName[r].indexOf("internalNode") != -1) {
                    String output = "<" + MRRGFunctionName[r] + ">\nSELECTED_OP\n" +
                            DFGop[reMatch[r]].toArray()[0] + "\n";
                    funcOutput.add(output);
                }
            } else {
                if (used[r] && MRRGRoutingName[r].indexOf("internalNode") != -1) {
                    List<Integer> fanin = new ArrayList<>();
                    List<Integer> fanout = new ArrayList<>();
                    for (int i = 0; i < MRRGin[r].size(); i++) {
                        if (used[MRRGin[r].get(i)] &&
                                routingUsed[MRRGin[r].get(i)] == routingUsed[r] &&
                                MRRGPathUsed.contains(asList(MRRGin[r].get(i), r)))
                            fanin.add(Integer.valueOf(i));
                    }
                    for (int i = 0; i < MRRGout[r].size(); i++) {
                        if (used[MRRGout[r].get(i)] &&
                                routingUsed[MRRGout[r].get(i)] == routingUsed[r] &&
                                MRRGPathUsed.contains(asList(r, MRRGout[r].get(i))))
                            fanout.add(Integer.valueOf(i));
                    }
                    if (fanin.size() > 1) {
                        logger.debug("   " + MRRGRoutingName[r] + "<-" + DFGOpNodeName.get(routingUsed[r]));
//                        System.out.println("   " + MRRGRoutingName[r] + "<-" + DFGOpNodeName.get(routingUsed[r]));
                    }
                    if (fanin.size() > 0 && fanout.size() > 0) {
                        infoFile.write("<" + MRRGRoutingName[r] + ">\n");
                        for (int i = 0; i < fanin.size(); i++)
                            infoFile.write(fanin.get(i).toString() + " ");
                        infoFile.write("\n");
                        for (int i = 0; i < fanout.size(); i++)
                            infoFile.write(fanout.get(i).toString() + " ");
                        infoFile.write("\n");
                    }
                }
            }

        }
        for (int i = 0; i < funcOutput.size(); i++) {
            infoFile.write(funcOutput.get(i));
        }

        infoFile.flush();
        infoFile.close();
    }

    void report() throws IOException {
        findSolution = true;
        ValidateSolution();
        OutputSolution();
    }

    boolean findSolution = false;
    int[] topoque;

    class dfsCaller implements Caller {
        int num;

        public dfsCaller(int num) {
            this.num = num;
        }

        public void go() throws IOException {
            dfs(num);
        }
    }

    ;

    class SinglePath implements Caller {
        int a, b, len;
        Caller nxt;

        public SinglePath(int a, int b, int len, Caller nxt) {
            this.a = a;
            this.b = b;
            this.len = len;
            this.nxt = nxt;
        }

        public void go() throws IOException {
            SearchSinglePath(a, b, len, nxt);
        }
    }

    int TRYS = 0;

    void SearchSinglePath(int a, int b, int len, Caller nxt) throws IOException {
        int[] ranks = GetRank(a, b, reMatch[a]);
        int[][] toArray = GetToArray(b, len, ranks);
        SearchPath(a, b, len, ranks, toArray, nxt);
    }

    void dfsFireTime(int x, int num) throws IOException {
        dfs(num + 1);
        time[x] = 0;
        /*
        for(int i=-1;i<=1;i++){
            time[x]=i;
            dfs(num+1);
        }

         */
    }

    void dfsInputEdge(int x, int num) throws IOException {
        if (findSolution) return;
        List<Integer> trueInput = new ArrayList<Integer>();
        for (int y : DFGin[x])
            if (y != x) {
                trueInput.add(y);
            }

        if (trueInput.size() == 0) {
            time[x] = 0;
            dfsFireTime(x, num);
            int matched = match[x];
            String name = MRRGFunctionName[matched];
            String[] sArray = name.split(":");
            int rc = Integer.parseInt(sArray[0]);
            time[x] = rc;
        } else if (trueInput.size() == 1) {
            int y = trueInput.get(0);
            int[] ranks = GetRank(match[y], match[x], y);
            int[][] toArray = GetToArray(match[x], singlePathTimeLimit, ranks);
            for (int i = 0; i <= singlePathTimeLimit; i++) {
                time[x] = time[y] + i;
                SearchPath(match[y], match[x], i, ranks, toArray, new dfsCaller(num + 1));
            }
        } else if (trueInput.size() == 2) {
            int l = trueInput.get(0);
            int r = trueInput.get(1);
            //System.out.printf("two input , time is %d %d\n",time[l],time[r]);
            int limitL = singlePathTimeLimit;
            int limitR = singlePathTimeLimit;
            if (time[l] < time[r]) limitL += time[r] - time[l];
            else limitR += time[l] - time[r];
            int[] ranks = GetRank(match[l], match[x], l);
            int[][] toArray = GetToArray(match[x], limitL, ranks);

            for (int i = 0; i <= limitL; i++) {
                if (time[l] + i >= time[r]) {
                    time[x] = time[l] + i;
                    for (int dr = 0; dr <= 0; dr++)
                        if (time[x] - time[r] + dr >= 0) {
                            SearchPath(match[l], match[x], i, ranks, toArray, new SinglePath(match[r],
                                    match[x], time[x] - time[r] + dr, new dfsCaller(num + 1)));
                        }
                }
            }
        }
    }

    int[][] GetToArray(int T, int len, int[] ranks) {
        int[][] toArrray = new int[len + 1][numMRRG];
        for (int i = 0; i <= len; i++) for (int j = 0; j < numMRRG; j++) toArrray[i][j] = -1;

        Integer[] id = new Integer[numMRRG];
        for (int i = 0; i < numMRRG; i++) id[i] = i;
        Arrays.sort(id, new Comparator<Integer>() {
            public int compare(Integer x, Integer y) {
                return ranks[x] - ranks[y];
            }
        });
        toArrray[0][T] = 0;
        for (int i = 0; i <= len; i++) {
            {
                //maybe T's rank is very small (in situation S=T)
                int x = T;
                if (toArrray[i][x] == -1) continue;
                if (used[x] && x != T) continue;
                for (int y : MRRGin[x])
                    if (ranks[y] < ranks[x] || x == T) {
                        int newLen = i + getLatency(y, x);
                        if (newLen <= len) {
                            if (toArrray[newLen][y] == -1) {
                                toArrray[newLen][y] = x;
                            }
                        }
                    }
            }
            for (int j = numMRRG - 1; j >= 0; j--) {
                int x = id[j];
                if (toArrray[i][x] == -1) continue;
                if (used[x] && x != T) continue;
                for (int y : MRRGin[x])
                    if (ranks[y] < ranks[x] || x == T) {
                        int newLen = i + getLatency(y, x);
                        if (newLen <= len) {
                            if (toArrray[newLen][y] == -1) {
                                toArrray[newLen][y] = x;
                            }
                        }
                    }
            }
        }
        return toArrray;
    }

    int[] GetRank(int a, int b, int DFGin) {
        List<Integer> q = new ArrayList<Integer>();
        boolean[] inq = new boolean[numMRRG];
        for (int i = 0; i < numMRRG; i++) inq[i] = false;
        int head = 0;
        q.add(a);
        inq[a] = true;
        while (head < q.size()) {
            int x = q.get(head);
            head += 1;
            for (int y : MRRGout[x]) {
                if (!inq[y]) if (y != b) if (!used[y] || routingUsed[y] == DFGin) {
                    inq[y] = true;
                    q.add(y);
                }
            }
        }
        q.add(b);
        int[] rank = new int[numMRRG];
        for (int i = 0; i < numMRRG; i++) rank[i] = numMRRG + 1;
        for (int i = 0; i < q.size(); i++) rank[q.get(i)] = i;
        rank[b] = 100000000;
        rank[a] = -1;
        return rank;
    }

    int[] ListInt2Array(List<Integer> p) {
        int[] a = new int[p.size()];
        for (int i = 0; i < p.size(); i++) a[i] = p.get(i);
        return a;
    }

    int[] GetPathTo(int[][] toArray, int a, int b, int len) {
        List<Integer> p = new ArrayList<Integer>();
        logger.trace("path " + a + "->" + b + " (len " + len + "), toArray" + toArray[len][a]);
//        System.out.printf("path %d->%d (len %d), toArray %d\n", a, b, len, toArray[len][a]);
        while (!(a == b && len == 0)) {
            p.add(a);
            int na = toArray[len][a];
            if (na == -1) {
                logger.trace("cant find path to");
//                System.out.println("cant find path to");
                return new int[0];
            }
            len -= getLatency(a, na);
            a = na;
        }
        p.add(b);
        return ListInt2Array(p);
    }

    int[] PathMerge(int[] path1, int[] path2) {
        if (path1.length == 0) return Arrays.copyOf(path2, path2.length);
        if (path2.length == 0) return Arrays.copyOf(path1, path1.length);

        int allLen = path1.length + path2.length;
        if (path1[path1.length - 1] == path2[0]) {
            allLen -= 1;
        }
        int[] path = new int[allLen];
        System.arraycopy(path1, 0, path, 0, path1.length);
        int t = 0;
        if (path1[path1.length - 1] == path2[0]) {
            t = 1;
        }

        System.arraycopy(path2, t, path, t - t + path1.length, path2.length - t);
        return path;
    }

    void Go(int S, int a, int T, int restLen, int restEdge, List<Integer> path, int[] ranks, int[][] ToArray, Caller nextCall) throws IOException {
        if (findSolution) return;
        if (ToArray[restLen][a] == -1) return;
        {
            int[] restPath = GetPathTo(ToArray, a, T, restLen);
            int[] allPath = PathMerge(ListInt2Array(path), restPath);
            boolean[] bp = backUpUsed();
            usePath(reMatch[S], reMatch[T], allPath);
            nextCall.go();
            restoreUsed(bp);
        }
        if (restEdge > 0) {
            for (int x : MRRGout[a])
                if ((ranks[x] > ranks[a]) || (x == T)) {
                    if (ToArray[restLen][a] == x) continue;
                    if (restLen < getLatency(a, x)) continue;
                    List<Integer> p = new ArrayList<Integer>(path);
                    p.add(x);
                    Go(S, x, T, restLen - getLatency(a, x), restEdge - 1, p, ranks, ToArray, nextCall);
                }
        }
    }

    void NaiveSearch(int S, int x, int T, int len, int rest, List<Integer> path, Caller next) throws IOException {
        if (x == T && len == 0) {
            usePath(reMatch[S], reMatch[T], ListInt2Array(path));
            next.go();
            return;
        }
        if (rest > 0)
            for (int y : MRRGout[x])
                if ((!used[y]) || y == T) if (len >= getLatency(x, y)) {
                    used[y] = true;
                    List<Integer> newpath = new ArrayList<Integer>(path);
                    newpath.add(y);
                    NaiveSearch(S, y, T, len - getLatency(x, y), rest - 1, newpath, next);
                    used[y] = false;
                }
    }

    boolean Contain(int[] x, int y) {
        for (int z : x) if (z == y) return true;
        return false;
    }

    int[][][] trivialDPto(int S, int Maxlen, int DFGin) {
        boolean fixFlag = false;
        int DFGSink = reMatch[S];
        //for non-swapable operators like sub
        if (!DFGCommutativeSet.contains(DFGSink) && DFGOpOperand.containsKey(DFGSink)) {
            if (DFGOpOperand.get(DFGSink).size() > 1)
                fixFlag = true;
        }


        //{dp[len][x] union y} -> dp[len+w(x,y)][y]
        int[][][] dp = new int[Maxlen + 2][numMRRG][];
        dp[0][S] = new int[]{S};
        List<int[]> Que = new ArrayList<int[]>();
        Que.add(new int[]{0, S});
        for (int i = 0; i < Que.size(); i++) {
            int Len = Que.get(i)[0];
            int x = Que.get(i)[1];
            for (int y : MRRGin[x]) {
                if (x == S) {
                    if (fixFlag) {
                        int operand = DFGOpOperand.get(DFGSink).indexOf(DFGin);
                        if (MRRGin[x].get(operand) != y) {
                            continue;
                        }
                    }
                }
                if (getLatency(y, x) + Len <= Maxlen) {
                    if (dp[getLatency(y, x) + Len][y] == null)
                        if ((!Contain(dp[Len][x], y)) || (y == S)) {
                            dp[getLatency(y, x) + Len][y] = PathMerge(new int[]{y}, dp[Len][x]);
                            if ((!used[y] || routingUsed[y] == DFGin) && (y != S) && !mrrgFuncNodes.contains(y)) {
                                Que.add(new int[]{getLatency(y, x) + Len, y});
                            }
                        }
                }
            }
        }
        //System.out.printf("Que size is %d, start len is %d\n",Que.size(),getLatency(S,S));
        return dp;
    }

    void GreedySearch(int S, int x, int T, int len, int rest, List<Integer> path, Caller next) throws IOException {
        int DFGin = reMatch[S];
        int[][][] dp = trivialDPto(T, len, DFGin);
        for (int delay = MaxDelay; delay >= 0; delay--) {
            //for(int delay=0;delay<=MaxDelay;delay++){
            if (delay % II == 0) {
                if ((len - delay > 0) || (len - delay == 0 && S != T)) {
                    if (dp[len - delay][x] != null) {
                        boolean[] bkp = backUpUsed();
                        int[] bkr = backUpRoutingUsed();
                        usePath(reMatch[S], reMatch[T], PathMerge(ListInt2Array(path), dp[len - delay][x]));
                        next.go();
                        restoreUsed(bkp);
                        restoreRoutingUsed(bkr);
                        //return;
                    }
                }
            }
        }
        /*
        if(dp[len][x]!=null){
            boolean[] bkp=backUpUsed();
            usePath(reMatch[S],reMatch[T],PathMerge(ListInt2Array(path),dp[len][x]));
            next.go();
            restoreUsed(bkp);
        }
        */
    }

    void SearchPath(int a, int b, int len, int[] ranks, int[][] ToArray, Caller nextCall) throws IOException {
        if (!isFirstOuter(reMatch[a], reMatch[b])) {
            for (int i = 0; i < waitPath[reMatch[a]].length - 1; i++) {
                List<Integer> path = new ArrayList<Integer>();
                int rlen = 0;
                for (int j = 0; j <= i; j++) {
                    path.add(waitPath[reMatch[a]][j]);
                    if (j < i) rlen += getLatency(waitPath[reMatch[a]][j], waitPath[reMatch[a]][j + 1]);
                }
                if (rlen <= len)
                    GreedySearch(a, path.get(path.size() - 1), b, len - rlen, singlePathEdgeLimit, path, nextCall);
                //NaiveSearch(a, path.get(path.size()-1), b, len, singlePathEdgeLimit, path, nextCall);
            }
        } else {
            List<Integer> path = new ArrayList<Integer>();
            path.add(a);
            GreedySearch(a, path.get(path.size() - 1), b, len, singlePathEdgeLimit, path, nextCall);
            //NaiveSearch(a, path.get(path.size()-1), b, len, singlePathEdgeLimit, path, nextCall);
        }

        //System.out.printf(" Try %d %d %d\n",a,b,len);

        //Go(a,a,b,len,singlePathEdgeLimit,new ArrayList<Integer>(),ranks,ToArray,nextCall);
    }

    interface Caller {
        void go() throws IOException;
    }

    class DfsInputEdge implements Caller {
        int x, num;

        public DfsInputEdge(int x, int num) {
            this.x = x;
            this.num = num;
        }

        public void go() throws IOException {
            dfsInputEdge(x, num);
        }
    }

    void dfsSelfCircle(int x, int num) throws IOException {
        if (findSolution) return;
        if (DFGout[x].contains(x)) {
            //not used
            int[] ranks = GetRank(match[x], match[x], x);
            //not used
            int[][] ToArray = GetToArray(match[x], II, ranks);
            SearchPath(match[x], match[x], II, ranks, ToArray, new DfsInputEdge(x, num));

            //dfsInputEdge(x,num);
        } else {
            dfsInputEdge(x, num);
        }
    }

    void linkEdgeDfs(int x, int num) throws IOException {
        if (findSolution) return;
        dfsSelfCircle(x, num);
    }

    int CutTrys(int x, int num) {
        int P = (num - x) * 5;
        if (P <= 5) P = 5;
        //if(P>2000)P=2000;
        return P;
    }

    int inf = 10000000;
    int LM_TRYS = 10000000;

    void dfs(int x) throws IOException {
        //if(LM_TRYS<TRYS)return;
        if (findSolution) return;
        //overfitting ?
        TRYS++;
        if (TRYS >= 100) return;
        //System.out.printf("%d\n",x);
//        System.out.println(TRYS);
//        System.out.println(Arrays.toString(match));

        logger.debug("Trys:" + TRYS);
        logger.trace(Arrays.toString(match));

        String[] debug;
        debug = new String[match.length];
        for (int j = 0; j < match.length; j++) {
            if (match[j] == 0) {
                debug[j] = " ";
            } else {
                debug[j] = MRRGFunctionName[match[j]];
            }
        }
        logger.trace(Arrays.toString(debug));
//        System.out.println(Arrays.toString(debug));

        if (x == dfgOPNodes.size()) {
            report();
            return;
        }
        int NowTry = TRYS;
        Integer[] id = new Integer[mrrgFuncNodes.size()];
        for (int i = 0; i < id.length; i++) id[i] = i;


        Arrays.sort(id, new Comparator<Integer>() {
            public int compare(Integer a, Integer b) {

                int suma = 0;
                for (int i = 0; i < x; i++) {
                    if (DFGin[topoque[x]].contains(topoque[i]))
                        suma += shortestDis[match[topoque[i]]][mrrgFuncNodes.get(a)];
                }

                Set<List<Integer>> affectedA = potentialAffectedNodes.get(a);
                if (affectedA != null) {
                    for (List<Integer> item : affectedA) {
                        int dfgIndex = item.get(1);
                        double factor = 1.0 / (item.get(0) * fixedMapRelation.get(dfgIndex).size());
                        double value = 0.0;
                        for (Integer mrrgNode : fixedMapRelation.get(dfgIndex)) {
                            value += shortestDis[mrrgNode][mrrgFuncNodes.get(a)];
                        }
                        value = value * factor;
                        suma += (int) value;
                    }
                }


                int sumb = 0;
                for (int i = 0; i < x; i++)
                    if (DFGin[topoque[x]].contains(topoque[i]))
                        sumb += shortestDis[match[topoque[i]]][mrrgFuncNodes.get(b)];

                Set<List<Integer>> affectedB = potentialAffectedNodes.get(b);
                if (affectedB != null) {
                    for (List<Integer> item : affectedB) {
                        int dfgIndex = item.get(1);
                        double factor = 1.0 / (item.get(0) * fixedMapRelation.get(dfgIndex).size());
                        double value = 0.0;
                        for (Integer mrrgNode : fixedMapRelation.get(dfgIndex)) {
                            value += shortestDis[mrrgNode][mrrgFuncNodes.get(b)];
                        }
                        value = value * factor;
                        sumb += (int) value;
                    }
                }


                return suma - sumb;

                //return mrrgFuncNodes.get(a)-mrrgFuncNodes.get(b);
            }
        });

        int count = 0;
        for (int _1 = 0; _1 < mrrgFuncNodes.size(); _1++)
            if (matchTable[topoque[x]][id[_1]])
                if (!used[mrrgFuncNodes.get(id[_1])]) {
                    //LM_TRYS=NowTry+CutTrys(x,dfgOPNodes.size());
                    int i = id[_1];
                    if (findSolution) return;
                    int suma = 0;

                    for (int j = 0; j < x; j++)
                        if (DFGin[topoque[x]].contains(topoque[j])) {
                            suma += shortestDis[match[topoque[j]]][mrrgFuncNodes.get(i)];
                        }

                    if (suma >= inf) continue;
//                    if (suma >= 26 + count) {
//                        count += 1;
//                        continue;
//                    }

                    match[topoque[x]] = mrrgFuncNodes.get(i);
                    reMatch[mrrgFuncNodes.get(i)] = topoque[x];
                    boolean[] bkp = backUpUsed();
                    used[mrrgFuncNodes.get(i)] = true;
                    linkEdgeDfs(topoque[x], x);
                    restoreUsed(bkp);
                    match[topoque[x]] = 0;
                    if (TRYS - NowTry >= CutTrys(x, dfgOPNodes.size())) {
                        return;
                    }
                }
    }

    int[][] shortestDis;

    int[] GetShortestFrom(int S) {
        List<Integer> Q = new ArrayList<Integer>();
        Q.add(S);
        int[] dis = new int[numMRRG];
        Arrays.fill(dis, inf);
        dis[S] = 0;
        for (int i = 0; i < Q.size(); i++) {
            int x = Q.get(i);
            for (int y : MRRGout[x])
                if (dis[y] > numMRRG) {
                    dis[y] = dis[x] + 1;
                    Q.add(y);
                }
        }
        return dis;
    }

    public ArrayList<Integer> Array2List(int[] p) {
        ArrayList<Integer> me = new ArrayList<>();
        for (int i = 0; i < p.length; i++) me.add(p[i]);
        return me;
    }

    public void get() throws IOException {
        //overfitting ?
        while (!findSolution) {
            Collections.shuffle(mrrgFuncNodes);
            get2();
        }
    }

    public void get2() throws IOException {
        //ReconstructDFG();
        time = new int[numDFG];
        logger.info("start to searchMap, II is " + II);
//        System.out.printf("start to searchMap, II is %d\n", II);
        waitPath = new int[numDFG][];
        int numLatency0 = 0;
        List<Integer> latency1 = new ArrayList<Integer>();
        for (int i = 0; i < numMRRG; i++) {
            if (MRRGlatency.get(i) == 0) {
                numLatency0 += 1;
            } else latency1.add(i);
        }
        logger.info("MRRG node: " + numMRRG);
        logger.info("latency 1 node: " + latency1.toString());
        logger.info("Func nodes is " + Arrays.toString(mrrgFuncNodes.toArray()));
//        System.out.printf("MRRG node: %d\n", numMRRG);
//        System.out.printf("latency 1 node: %s\n", latency1.toString());
//        System.out.printf("Func nodes is %s\n", Arrays.toString(mrrgFuncNodes.toArray()));
        /*
        for(int x:mrrgFuncNodes){
            System.out.printf(" %d op : %s\n",x,MRRGop[x].toString());
        }
        */

        for (int i = 0; i < numDFG; i++) {
            logger.debug("dfg in (" + i + ")= " + DFGin[i].toString());
//            System.out.printf("dfg in (%d)= %s\n", i, DFGin[i].toString());
        }
        shortestDis = new int[numMRRG][numMRRG];

        /*
        int[] tmp=ListInt2Array(mrrgFuncNodes);
        for(int i=0;i<tmp.length;i++)if(tmp[i]==1601) {
            int x=tmp[0];
            tmp[0]=tmp[i];
            tmp[i]=x;
            break;
        }
        mrrgFuncNodes=Array2List(tmp);

         */


        for (int i = 0; i < mrrgFuncNodes.size(); i++) {
            int[] dis = GetShortestFrom(mrrgFuncNodes.get(i));
            for (int j = 0; j < numMRRG; j++) shortestDis[mrrgFuncNodes.get(i)][j] = dis[j];
        }

        for (int x : mrrgFuncNodes) {
            int sum = 0;
            int cnt = 0;
            for (int y : mrrgFuncNodes)
                if (shortestDis[x][y] < 1000) {
                    sum += shortestDis[x][y];
                    cnt++;
                }
            logger.debug(x + " :" + sum + " " + cnt);
//            System.out.printf("%d :%d %d\n", x, sum, cnt);
        }


        matchTable = new boolean[dfgOPNodes.size()][mrrgFuncNodes.size()];
        for (int i = 0; i < dfgOPNodes.size(); i++)
            for (int j = 0; j < mrrgFuncNodes.size(); j++) {
                if (fixedMapRelation.containsKey(i)) {
                    if (fixedMapRelation.get(i).contains(mrrgFuncNodes.get(j))) {
                        matchTable[i][j] = true;
                    } else {
                        matchTable[i][j] = false;
                    }
                } else {
                    matchTable[i][j] = matchOP(dfgOPNodes.get(i), mrrgFuncNodes.get(j));
                }
            }

        for (int i = 0; i < dfgOPNodes.size(); i++) {
            potentialAffectedNodes.put(i, new HashSet<>());
            Queue<List<Integer>> queue = new LinkedList<List<Integer>>();
            for (Integer out : DFGout[i]) {
                queue.offer(asList(out));
            }
            while (!queue.isEmpty()) {
                List<Integer> node = queue.poll();
                if (fixedMapRelation.containsKey(node.get(0))) {
                    potentialAffectedNodes.get(i).add(asList(node.size(), node.get(0)));
                }
                if (DFGout[node.get(0)].size() > 0) {
                    for (Integer next : DFGout[node.get(0)]) {
                        if (next != node.get(0)) {
                            List<Integer> nextList = new ArrayList<Integer>();
                            nextList.add(next);
                            nextList.addAll(node);
                            queue.offer(nextList);
                        }
                    }
                }
            }
        }

        used = new boolean[numMRRG];
        routingUsed = new int[numMRRG];
        for (int i = 0; i < numMRRG; i++) {
            used[i] = false;
            routingUsed[i] = -1;
        }
        match = new int[dfgOPNodes.size()];
        reMatch = new int[numMRRG];

        resultPath = new int[numDFG][numDFG][];


        //MM cant work
        topoque = toposort();
        TRYS = 0;

        dfs(0);
    }

    public searchMap(int DFGnodes, int MRRGnodes) {
        numDFG = DFGnodes;
        DFGin = new List[numDFG];
        DFGout = new List[numDFG];
        DFGop = new Set[numDFG];
        dfgOPNodes = new ArrayList<Integer>();
        dfgValNodes = new ArrayList<Integer>();
        for (int i = 0; i < numDFG; i++) {
            DFGin[i] = new ArrayList<Integer>();
            DFGout[i] = new ArrayList<Integer>();
            DFGop[i] = new HashSet<Integer>();
        }


        waitSkewMap = new HashMap<>();
        DFGRelativeSkewMap = new HashMap<>();
        DFGLatencyMap = new HashMap<>();
        DFGCommutatedSet = new HashSet();
        DFGOpNodeName = new ArrayList<>();
        DFGCommutativeSet = new HashSet();
        DFGOpOperand = new HashMap<>();
        fixedMapRelation = new HashMap<>();
        potentialAffectedNodes = new HashMap<>();


        numMRRG = MRRGnodes;
        MRRGin = new List[numMRRG];
        MRRGout = new List[numMRRG];
        MRRGop = new Set[numMRRG];
        MRRGlatency = new ArrayList<Integer>();

        mrrgFuncNodes = new ArrayList<Integer>();
        mrrgRoutingNodes = new ArrayList<Integer>();
        MRRGFunctionName = new String[numMRRG];
        MRRGRoutingName = new String[numMRRG];
        MRRGPathUsed = new HashSet<>();

        for (int i = 0; i < MRRGnodes; i++) {
            MRRGin[i] = new ArrayList<Integer>();
            MRRGout[i] = new ArrayList<Integer>();
            MRRGop[i] = new HashSet<Integer>();
            //MRRGlatency.add(0);
        }
    }
}

