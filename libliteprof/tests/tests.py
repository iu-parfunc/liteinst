#!/usr/bin/python

import os;

pwd = os.getcwd()
lib = "%s/../../build/lib/libliteprof.so" % pwd

bins = ['test_app_0_O0.exe', 'test_app_1_O3.exe']

for test in bins:
  cmd = "LD_PRELOAD=%s ./apps/%s" % (lib, test)
  os.system(cmd)
