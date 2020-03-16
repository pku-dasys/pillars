package tetriski.pillars.Mapping;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;

import gurobi.*;

public class gurobimap_java {
    String filename;

    List<Integer> DFGopnodeout;
    List<Integer> DFGopnodeopcode;
    List<String> DFGopnodename;
    List<List<Integer>> DFGvalnodeout;
    List<List<Integer>> DFGvalnodeoutputoperand;
    List<String> DFGvalnodename;

    List<String> MRRGfunctionname;
    List<String> MRRGroutingname;
    List<List<Integer>> MRRGfunctionfanin;
    List<List<Integer>> MRRGfunctionfanintype;
    List<List<Integer>> MRRGroutingfanin;
    List<List<Integer>> MRRGroutingfanintype;
    List<List<Integer>> MRRGfunctionfanout;
    List<List<Integer>> MRRGfunctionfanouttype;
    List<List<Integer>> MRRGroutingfanout;
    List<List<Integer>> MRRGroutingfanouttype;
    List<List<Integer>> MRRGfunctionSupportop;

    gurobimap_java(String filename) {
        this.filename = filename;

        DFGopnodename = new ArrayList<>();
        DFGopnodeopcode = new ArrayList<>();
        DFGopnodeout = new ArrayList<>();
        DFGvalnodename = new ArrayList<>();
        DFGvalnodeout = new ArrayList<>();
        DFGvalnodeoutputoperand = new ArrayList<>();

        MRRGfunctionname = new ArrayList<>();
        MRRGfunctionfanin = new ArrayList<>();
        MRRGfunctionfanintype = new ArrayList<>();
        MRRGfunctionfanout = new ArrayList<>();
        MRRGfunctionfanouttype = new ArrayList<>();
        MRRGfunctionSupportop = new ArrayList<>();
        MRRGroutingname = new ArrayList<>();
        MRRGroutingfanin = new ArrayList<>();
        MRRGroutingfanintype = new ArrayList<>();
        MRRGroutingfanout = new ArrayList<>();
        MRRGroutingfanouttype = new ArrayList<>();
    }

    gurobimap_java(String DFGFile, String MRRGFile) throws IOException {
        DFGopnodename = new ArrayList<>();
        DFGopnodeopcode = new ArrayList<>();
        DFGopnodeout = new ArrayList<>();
        DFGvalnodename = new ArrayList<>();
        DFGvalnodeout = new ArrayList<>();
        DFGvalnodeoutputoperand = new ArrayList<>();

        MRRGfunctionname = new ArrayList<>();
        MRRGfunctionfanin = new ArrayList<>();
        MRRGfunctionfanintype = new ArrayList<>();
        MRRGfunctionfanout = new ArrayList<>();
        MRRGfunctionfanouttype = new ArrayList<>();
        MRRGfunctionSupportop = new ArrayList<>();
        MRRGroutingname = new ArrayList<>();
        MRRGroutingfanin = new ArrayList<>();
        MRRGroutingfanintype = new ArrayList<>();
        MRRGroutingfanout = new ArrayList<>();
        MRRGroutingfanouttype = new ArrayList<>();

        BufferedReader DFGReader = new BufferedReader(new FileReader(DFGFile));
        int opnum = Integer.parseInt(DFGReader.readLine());
        for (int i = 0; i < opnum; i++) {
            DFGopnodename.add(DFGReader.readLine());
            DFGopnodeout.add(Integer.valueOf(DFGReader.readLine()));
            DFGopnodeopcode.add(Integer.valueOf(DFGReader.readLine()));
        }
        int valnum = Integer.parseInt(DFGReader.readLine());
        for (int i = 0; i < valnum; i++) {
            DFGvalnodename.add(DFGReader.readLine());
            int outsize = Integer.parseInt(DFGReader.readLine());
            List<Integer> valout = new ArrayList<>();
            for (int j = 0; j < outsize; j++)
                valout.add(Integer.valueOf(DFGReader.readLine()));
            DFGvalnodeout.add(valout);
            List<Integer> valoperand = new ArrayList<>();
            for (int j = 0; j < outsize; j++)
                valoperand.add(Integer.valueOf(DFGReader.readLine()));
            DFGvalnodeoutputoperand.add(valoperand);
        }
        DFGReader.close();

        BufferedReader MRRGReader = new BufferedReader(new FileReader(MRRGFile));

        int fnum = Integer.parseInt(MRRGReader.readLine());
        for (int i = 0; i < fnum; i++) {
            MRRGfunctionname.add(MRRGReader.readLine());
            int faninsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanin = new ArrayList<>();
            List<Integer> fanintype = new ArrayList<>();
            for (int j = 0; j < faninsize; j++) {
                fanin.add(Integer.valueOf(MRRGReader.readLine()));
                fanintype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGfunctionfanin.add(fanin);
            MRRGfunctionfanintype.add(fanintype);

            int fanoutsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanout = new ArrayList<>();
            List<Integer> fanouttype = new ArrayList<>();
            for (int j = 0; j < fanoutsize; j++) {
                fanout.add(Integer.valueOf(MRRGReader.readLine()));
                fanouttype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGfunctionfanout.add(fanout);
            MRRGfunctionfanouttype.add(fanouttype);

            int sopsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> sop = new ArrayList<>();
            for (int j = 0; j < sopsize; j++) {
                sop.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGfunctionSupportop.add(sop);
        }

        int rnum = Integer.parseInt(MRRGReader.readLine());
        for (int i = 0; i < rnum; i++) {
            MRRGroutingname.add(MRRGReader.readLine());
            int faninsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanin = new ArrayList<>();
            List<Integer> fanintype = new ArrayList<>();
            for (int j = 0; j < faninsize; j++) {
                fanin.add(Integer.valueOf(MRRGReader.readLine()));
                fanintype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGroutingfanin.add(fanin);
            MRRGroutingfanintype.add(fanintype);

            int fanoutsize = Integer.parseInt(MRRGReader.readLine());
            List<Integer> fanout = new ArrayList<>();
            List<Integer> fanouttype = new ArrayList<>();
            for (int j = 0; j < fanoutsize; j++) {
                fanout.add(Integer.valueOf(MRRGReader.readLine()));
                fanouttype.add(Integer.valueOf(MRRGReader.readLine()));
            }
            MRRGroutingfanout.add(fanout);
            MRRGroutingfanouttype.add(fanouttype);
        }
        MRRGReader.close();

        System.out.println(opnum);
        System.out.println(valnum);
        System.out.println(fnum);
        System.out.println(rnum);
    }

    int RINDEX(int valindex, int routingindex) {
        return valindex * MRRGroutingname.size() + routingindex;
    }

    int FINDEX(int opindex, int functionindex) {
        return opindex * MRRGfunctionname.size() + functionindex;
    }

    List<Integer>[] ILPMap() throws GRBException, IOException {
        GRBModel model = getILPModel();
        int num_dfg_vals = DFGvalnodename.size();
        int num_dfg_ops = DFGopnodename.size();
        int num_mrrg_r = MRRGroutingname.size();
        int num_mrrg_f = MRRGfunctionname.size();

        int count_R = num_dfg_vals * num_mrrg_r;
        int count_F = num_dfg_ops * num_mrrg_f;

        GRBVar[] Vars = model.getVars();
        GRBVar[] R = new GRBVar[count_R];
        System.arraycopy(Vars, 0, R, 0, count_R);
        GRBVar[] F = new GRBVar[count_F];
        System.arraycopy(Vars, count_R, F, 0, count_F);

        int status = model.get(GRB.IntAttr.Status);

        if (status == GRB.OPTIMAL || status == GRB.SUBOPTIMAL || status == GRB.SOLUTION_LIMIT) {
            int[] r_mapped = new int[num_mrrg_r];
            int[] f_mapped = new int[num_mrrg_f];
            int[] r_result = new int[num_mrrg_r];
            int[] f_result = new int[num_mrrg_f];

            for (int r = 0; r < num_mrrg_r; r++) {
                r_mapped[r] = 0;
                r_result[r] = -1;
            }
            for (int f = 0; f < num_mrrg_f; f++) {
                f_mapped[f] = 0;
                f_result[f] = -1;
            }

            for (int val = 0; val < num_dfg_vals; val++)
                for (int r = 0; r < num_mrrg_r; r++)
                    if (R[RINDEX(val, r)].get(GRB.DoubleAttr.X) == 1.0) {
                        //System.out.printf("%s->%s\n", DFGvalnodename.get(val), MRRGroutingname.get(r));
                        r_mapped[r] = 1;
                        r_result[r] = val;
                    }
            if (filename == null) {
                Date now = new Date();
                SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
                filename = "internalNodeinfo" + format.format(now);
            }
            FileWriter resultFile = new FileWriter(filename + "_r.txt");
            for (int op = 0; op < num_dfg_ops; op++)
                for (int f = 0; f < num_mrrg_f; f++)
                    if (F[FINDEX(op, f)].get(GRB.DoubleAttr.X) == 1.0) {
                        System.out.printf("%s->%s\n", DFGopnodename.get(op), MRRGfunctionname.get(f));
                        resultFile.write(DFGopnodename.get(op) + " " + MRRGfunctionname.get(f) + "\n");
                        f_mapped[f] = DFGopnodeopcode.get(op).intValue() + 1;
                        f_result[f] = op;
                    }
            resultFile.flush();
            resultFile.close();


            FileWriter infoFile = new FileWriter(filename + "_i.txt");
            for (int r = 0; r < num_mrrg_r; r++)
                if (r_mapped[r] == 1 && MRRGroutingname.get(r).indexOf("internalNode") != -1) {
                    List<Integer> fanin = new ArrayList<>();
                    List<Integer> fanout = new ArrayList<>();
                    for (int i = 0; i < MRRGroutingfanin.get(r).size(); i++) {
                        if (r_mapped[MRRGroutingfanin.get(r).get(i)] == 1 &&
                                r_result[MRRGroutingfanin.get(r).get(i)] == r_result[r])
                            fanin.add(Integer.valueOf(i));
                    }
                    for (int i = 0; i < MRRGroutingfanout.get(r).size(); i++) {
                        if (r_mapped[MRRGroutingfanout.get(r).get(i)] == 1 &&
                                r_result[MRRGroutingfanout.get(r).get(i)] == r_result[r])
                            fanout.add(Integer.valueOf(i));
                    }
                    if (fanin.size() > 1) {
                        System.out.println("   " + MRRGroutingname.get(r) + "<-" + DFGvalnodename.get(r_mapped[r]));
                    }
                    if (fanin.size() > 0 && fanout.size() > 0) {
                        infoFile.write("<" + MRRGroutingname.get(r) + ">\n");
                        for (int i = 0; i < fanin.size(); i++)
                            infoFile.write(fanin.get(i).toString() + " ");
                        infoFile.write("\n");
                        for (int i = 0; i < fanout.size(); i++)
                            infoFile.write(fanout.get(i).toString() + " ");
                        infoFile.write("\n");
                    }
                }
            for (int f = 0; f < num_mrrg_f; f++) {
                if (f_mapped[f] > 0 && MRRGfunctionname.get(f).indexOf("internalNode") != -1) {
                    infoFile.write("<" + MRRGfunctionname.get(f) + ">\nSELECTED_OP\n");
                    infoFile.write("" + (f_mapped[f] - 1) + "\n");
                }
            }
            infoFile.flush();
            infoFile.close();

            List<Integer>[] result = new List[2];
            result[0] = new ArrayList<>();
            result[1] = new ArrayList<>();
            for (int r = 0; r < num_mrrg_r; r++)
                result[0].add(Integer.valueOf(r_result[r]));
            for (int f = 0; f < num_mrrg_f; f++)
                result[1].add(Integer.valueOf(f_result[f]));
            return result;
        }
        return null;

    }

    Double ILPMap(FileWriter fw) throws GRBException, IOException {
        GRBModel model = getILPModel();

        int status = model.get(GRB.IntAttr.Status);

        if (status == GRB.OPTIMAL || status == GRB.SUBOPTIMAL || status == GRB.SOLUTION_LIMIT) {
            int vars = model.get(GRB.IntAttr.NumVars), constrs = model.get(GRB.IntAttr.NumConstrs);
            fw.write("Vars : " + vars + " Constrs : " + constrs + " ");
            return model.get(GRB.DoubleAttr.Runtime);
        }
        return -1.0;

    }

    GRBModel getILPModel()throws GRBException, IOException {
        GRBEnv env = new GRBEnv();
        int timelimit = 7200;
        double grb_mipgap = 0.2;
        int grb_solnlimit = 1;
        env.set(GRB.DoubleParam.MIPGap, grb_mipgap);
        env.set(GRB.DoubleParam.TimeLimit, timelimit);
        env.set(GRB.IntParam.SolutionLimit, grb_solnlimit);

        //focus on time
        env.set(GRB.IntParam.MIPFocus, 1);

        //a time-quality trade off
//        env.set(GRB.IntParam.MIPFocus, 2);

        GRBModel model = new GRBModel(env);

        int num_dfg_vals = DFGvalnodename.size();
        int num_dfg_ops = DFGopnodename.size();
        int num_mrrg_r = MRRGroutingname.size();
        int num_mrrg_f = MRRGfunctionname.size();

        int count_R = num_dfg_vals * num_mrrg_r;
        int count_F = num_dfg_ops * num_mrrg_f;
        GRBVar[] R = model.addVars(count_R, 'B');
        GRBVar[] F = model.addVars(count_F, 'B');

        List<GRBVar[]> S = new ArrayList<>();
        for (int val = 0; val < num_dfg_vals; val++)
            for (int r = 0; r < num_mrrg_r; r++) {
                int num_fanouts = DFGvalnodeout.get(val).size();
                if (num_fanouts > 1) {
                    S.add(model.addVars(num_fanouts, 'B'));
                } else {
                    S.add(null);
                }
            }
        model.update();

        int constrcount = 0;
        for (int val = 0; val < num_dfg_vals; val++)
            for (int r = 0; r < num_mrrg_r; r++) {
                R[RINDEX(val, r)].set(GRB.StringAttr.VarName, "R_" + val + "_" + r);
                if (S.get(RINDEX(val, r)) != null) {
                    for (int k = 0; k < S.get(RINDEX(val, r)).length; k++) {
                        S.get(RINDEX(val, r))[k].set(GRB.StringAttr.VarName, "R_" + val + "_" + r + "_" + k);
                        model.addConstr(R[RINDEX(val, r)], GRB.GREATER_EQUAL, S.get(RINDEX(val, r))[k],
                                "sub_val" + (constrcount++));
                    }
                } else {
                    S.set(RINDEX(val, r), new GRBVar[1]);
                    S.get(RINDEX(val, r))[0] = R[RINDEX(val, r)];
                }
            }
        for (int op = 0; op < num_dfg_ops; op++)
            for (int f = 0; f < num_mrrg_f; f++)
                F[FINDEX(op, f)].set(GRB.StringAttr.VarName, "F_" + op + "_" + f);

        GRBLinExpr objective = new GRBLinExpr();
        double[] coeffs = new double[count_R];
        for (int i = 0; i < count_R; i++) coeffs[i] = 1.0;

        objective.addTerms(coeffs, R);
        model.setObjective(objective, GRB.MINIMIZE);

        constrcount = 0;
        for (int i = 0; i < num_mrrg_r; i++) {
            GRBLinExpr constraint = new GRBLinExpr();
            for (int j = 0; j < num_dfg_vals; j++)
                constraint.addTerm(1.0, R[RINDEX(j, i)]);
            model.addConstr(constraint, GRB.LESS_EQUAL, 1, "route_exclusivity_" + (constrcount++));
        }

        constrcount = 0;
        for (int p = 0; p < num_mrrg_f; p++) {
            GRBLinExpr constraint = new GRBLinExpr();
            for (int q = 0; q < num_dfg_ops; q++)
                constraint.addTerm(1.0, F[FINDEX(q, p)]);
            model.addConstr(constraint, GRB.LESS_EQUAL, 1, "function_unit_exclusivity_" + (constrcount++));
        }

        constrcount = 0;
        for (int q = 0; q < num_dfg_ops; q++) {
            GRBLinExpr constraint = new GRBLinExpr();
            for (int p = 0; p < num_mrrg_f; p++)
                constraint.addTerm(1.0, F[FINDEX(q, p)]);
            model.addConstr(constraint, GRB.EQUAL, 1, "ensure_all_ops_mapped_" + (constrcount++));
        }

        constrcount = 0;
        int MRRG_NODE_ROUTING = 0;
        int MRRG_NODE_FUNCTION = 1;
        for (int val = 0; val < num_dfg_vals; val++)
            for (int r = 0; r < num_mrrg_r; r++) {
                int val_fanouts = DFGvalnodeout.get(val).size();

                for (int i = 0; i < val_fanouts; i++) {
                    GRBLinExpr sum_of_fanouts = new GRBLinExpr();
                    int fanout_count = 0;
                    int fanoutsize = MRRGroutingfanout.get(r).size();
                    for (int mrrg_fanout = 0; mrrg_fanout < fanoutsize; mrrg_fanout++) {
                        if (MRRGroutingfanouttype.get(r).get(mrrg_fanout) == MRRG_NODE_ROUTING) {
                            sum_of_fanouts.addTerm(1.0, S.get(RINDEX(val, MRRGroutingfanout.get(r).
                                    get(mrrg_fanout)))[i]);
                            fanout_count++;
                        } else if (MRRGroutingfanouttype.get(r).get(mrrg_fanout) == MRRG_NODE_FUNCTION) {
                            int op = DFGvalnodeout.get(val).get(i).intValue(), operand =
                                    DFGvalnodeoutputoperand.get(val).get(i).intValue();
                            if (MRRGfunctionfanin.get(MRRGroutingfanout.get(r).get(mrrg_fanout)).size() > operand &&
                                    MRRGfunctionfanin.get(MRRGroutingfanout.get(r).get(mrrg_fanout)).get(operand) == r) {
                                sum_of_fanouts.addTerm(1.0, F[FINDEX(op, MRRGroutingfanout.get(r).get(mrrg_fanout))]);
                                fanout_count++;
                            }
                        }
                    }

                    model.addConstr(sum_of_fanouts, GRB.GREATER_EQUAL, S.get(RINDEX(val, r))[i],
                            "fanout_routing_" + (constrcount++));
                }
            }

        constrcount = 0;
        for (int val = 0; val < num_dfg_vals; val++)
            for (int r = 0; r < num_mrrg_r; r++) {
                GRBLinExpr sum_of_fanins = new GRBLinExpr();
                int fanin_count = MRRGroutingfanin.get(r).size();
                if (fanin_count > 1) {
                    for (int fanin = 0; fanin < fanin_count; fanin++) {
                        if (MRRGroutingfanintype.get(r).get(fanin) == MRRG_NODE_ROUTING)
                            sum_of_fanins.addTerm(1.0, R[RINDEX(val, MRRGroutingfanin.get(r).get(fanin))]);
                    }
                    model.addConstr(sum_of_fanins, GRB.GREATER_EQUAL, R[RINDEX(val, r)],
                            "mux_exclusivity_lower_" + (constrcount++));
                    model.addConstr(sum_of_fanins, GRB.LESS_EQUAL, 1,
                            "mux_exclusivity_upper_" + (constrcount));
                }
            }

        constrcount = 0;
        for (int op = 0; op < num_dfg_ops; op++)
            for (int f = 0; f < num_mrrg_f; f++) {
                if (DFGopnodeout.get(op).intValue() != -1) {
                    int val = DFGopnodeout.get(op).intValue();
                    int f_fanouts = MRRGfunctionfanout.get(f).size();
                    for (int r = 0; r < f_fanouts; r++) {
                        int val_fanouts = DFGvalnodeout.get(val).size();
                        for (int i = 0; i < val_fanouts; i++)
                            model.addConstr(F[FINDEX(op, f)], GRB.EQUAL,
                                    S.get(RINDEX(val, MRRGfunctionfanout.get(f).get(r)))[i],
                                    "function_unit_fanout_" + (constrcount++));
                    }
                }
            }

        constrcount = 0;
        for (int op = 0; op < num_dfg_ops; op++)
            for (int f = 0; f < num_mrrg_f; f++) {
                //    System.out.printf("%d %d\n", f, MRRGfunctionSupportop.size());
                int flag = 0;
                for (int i = 0; i < MRRGfunctionSupportop.get(f).size(); i++)
                    if (MRRGfunctionSupportop.get(f).get(i) == DFGopnodeopcode.get(op)) {
                        flag = 1;
                        break;
                    }
                if (flag == 0)
                    model.addConstr(F[FINDEX(op, f)], GRB.EQUAL, 0, "op_support_" + (constrcount++));
            }

        model.update();

        model.write("problem_java.lp");

        model.optimize();

        return model;

    }

}