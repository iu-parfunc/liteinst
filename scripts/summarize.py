#!/usr/bin/python

import csv
from decimal import Decimal

benches = [ "blackscholes", "bzip", "fluid", "h264", "hmmer", "hull", "nbody", "perlbench", "sjeng" ]
experiments = [ "Init_Costs-Table6", "Layout_Distribution-Table2", "Mem_Util-Table5", "Injection_Costs-Fig4" ]

def format_e(n):
  a = '%e' % n
  return a.split('e')[0].rstrip('0').rstrip('.') + 'e' + a.split('e')[1]

def summarize_injection_costs():
  prefix = "../results/"+experiments[3]
  f = open(prefix+"/injection_costs_fig4.csv", 'w');
  f.write("threads, Dyninst Insertion, LiteInst Patching, LiteInst Metadata, LiteInst Punning, LiteInst Total\n")
  liteinst = list(csv.reader(open(prefix+"/raw/liteinst.out")))
  dyninst  = list(csv.reader(open(prefix+"/raw/dyninst.out")))

  liteinst_dict = {}
  for row in liteinst:
    n_threads = row[0]
    if n_threads in liteinst_dict:
      summary_data = liteinst_dict[n_threads]
      for i, elem in enumerate(row[1:]):
        summary_data[1][i] = int(summary_data[1][i]) + int(elem)
      summary_data[0] += 1
      liteinst_dict[n_threads] = summary_data
    else:
      liteinst_dict[n_threads] = [1, row[1:]]

  liteinst_avg = []
  for n_threads, data in liteinst_dict.iteritems():
    t_data = []
    t_data.append(n_threads)
    for val in data[1]:
      t_data.append(int(val) / data[0])
    liteinst_avg.append(t_data)

  dyninst_dict = {}
  for row in dyninst:
    n_threads = row[0]
    if n_threads in dyninst_dict:
      summary_data = dyninst_dict[n_threads]
      for i, elem in enumerate(row[1:]):
        summary_data[1][i] = int(summary_data[1][i]) + int(elem)
      summary_data[0] += 1
      dyninst_dict[n_threads] = summary_data
    else:
      dyninst_dict[n_threads] = [1, row[1:]]

  dyninst_avg = []
  for n_threads, data in dyninst_dict.iteritems():
    t_data = []
    t_data.append(n_threads)
    for val in data[1]:
      t_data.append(int(val) / data[0])
    dyninst_avg.append(t_data)
 
  injection_summary = []
  for i, row in enumerate(liteinst_avg):
    dyninst_row = dyninst_avg[i]
    data = []
    data.append(row[0])
    data.append(dyninst_row[2])
    data.append(row[2])
    data.append(row[3])
    data.append(row[4])
    data.append(row[1])
    injection_summary.append(data)

  for row in injection_summary:
    f.write("{0}, {1}, {2}, {3}, {4}, {5}\n".format(
        row[0], row[1], row[2], row[3], row[4], row[5]))
  f.close()

  f = open(prefix+"/injection_costs_table3.csv", 'w');
  f.write("#threads, Process Attach, Dyninst Probe Insertion, Dyninst Total Cost, LiteInst Cost, Cost Ratio (Dyn-/Liteinst)\n")

  injection_comparison = []
  for i, row in enumerate(liteinst_avg):
    dyninst_row = dyninst_avg[i]
    data = []
    data.append(row[0])
    data.append(format_e(Decimal(dyninst_row[1])))
    data.append(format_e(Decimal(dyninst_row[2])))
    data.append(format_e(Decimal((int(dyninst_row[1]) + int(dyninst_row[2]) + int(dyninst_row[3])))))
    data.append(format_e(Decimal(row[1])))
    data.append(format_e(Decimal((int(dyninst_row[1]) + int(dyninst_row[2]) + int(dyninst_row[3])) / int(row[1]))))
    injection_comparison.append(data) 

  for row in injection_comparison:
    f.write("{0}, {1}, {2}, {3}, {4}, {5}\n".format(
        row[0], row[1], row[2], row[3], row[4], row[5]))
  f.close()

def summarize_init_table(summary):
  prefix = "../results/"+experiments[0]
  f = open(prefix+"/init_costs_table_6.csv", 'w')
  f.write("Benchmark, #probes, Init Cost (cycles), User Time (%)\n")
  for bench in summary:
    f.write("{0}, {1}, {2}, {3:.2f}\n".format(
          bench, 
          summary[bench][0],  
          format_e(Decimal(summary[bench][1])),
          float(summary[bench][1]) / (float(summary[bench][6]) * 1000000000 * 2.6) * 100))
  f.close()

def summarize_mem_table(summary):
  prefix = "../results/"+experiments[2]
  f = open(prefix+"/mem_util_table_5.csv", 'w')
  f.write("Benchmark, Allocated Memory(KB), #Trampolines, Fixed Util(%), Arena Util(%), Max Residency(KB), Overhead (%)\n")
  for bench in summary:
    f.write("{0}, {1}, {2}, {3:.2f}, {4:.2f}, {5}, {6:.2f}\n".format(
          bench, 
          summary[bench][2],  
          summary[bench][3],  
          float(summary[bench][4]),  
          float(summary[bench][5]),  
          summary[bench][7],
          float(summary[bench][2]) / float(summary[bench][7]) * 100))
  f.close()

def main():
  summary = dict()
  prefix = "../results/"+experiments[0]+"/raw"
  for bench in benches:
    data = list(csv.reader(open(prefix+"/"+bench)))
    with open(prefix+"/"+ bench+"_time") as f:
      lines = f.readlines()
      for line in lines[:-1]:
        cells = line.split()
        if cells[0] == "user":
          data[0].append(cells[1])
        elif cells[0] == "RES_SIZE:":
          data[0].append(cells[1])
    summary[bench] = data[0]
  # summarize_init_table(summary)
  # summarize_mem_table(summary)
  summarize_injection_costs()

if __name__ == "__main__":
  main()
