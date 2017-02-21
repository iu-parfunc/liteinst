#!/usr/bin/python

import matplotlib.pyplot as plt
import sys

def main(argv):
  inputfile = argv[0]
  outputfile= argv[1]

  data = [1, 2]
  plt.table(cellText=data[1:],
            colLabels=data[0])
  plt.show()

if __name__ == "__main__":
  main(sys.argv[1:])

