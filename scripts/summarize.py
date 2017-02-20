#!/usr/bin/python

import csv
from decimal import Decimal

benches = [ "blackscholes", "bzip-1.0.3", "fluid", "h264ref-9.3", "hmmer", "hull", "nbody", "perl-5.8.7", "sjeng" ]
experiments = [ "Init_Costs-Table6", "Layout_Distribution-Table2", "Mem_Util-Table5" ]

def format_e(n):
  a = '%e' % n
  return a.split('e')[0].rstrip('0').rstrip('.') + 'e' + a.split('e')[1]

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
  summarize_init_table(summary)
  summarize_mem_table(summary)

if __name__ == "__main__":
  main()
